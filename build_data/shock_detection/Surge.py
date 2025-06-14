import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn import tree,ensemble, neighbors
from sklearn import linear_model, svm
from sklearn import metrics,model_selection
from sklearn import decomposition
from sklearn.ensemble import AdaBoostClassifier
import xgboost as xgb
from IPython.display import display, Markdown
from sklearn import tree
import json
from sklearn import tree as tr
import data,visualization,training
from toolz.functoolz import pipe,compose
from datetime import datetime,date
import warnings
warnings.filterwarnings('ignore')
from peak_detector import PeakDetector
from tensorflow.keras.models import load_model
import matplotlib.dates as mdates
import tensorflow as tf
from keras import Input, Model
from keras.layers import TFSMLayer
import re

# Global Variables Inputs
today = date.today()

day = today.strftime("%d")
month = today.strftime("%m")
year = today.strftime("%Y")

# Filesystem env variables
data_folder = '../data'
result_folder = '../result'
cutoff = 0.2 
lag = 7
mode = 'top'
cv = 10
country_list = ["Belarus"]

#Function to get most recent folders and files
def get_updated_files(path='.'):
    #path = "/Users/mahda.soltani/forecast-surges-pipeline/data"
    dirs = [d for d in os.listdir(path) if os.path.isdir(os.path.join(path, d))]

    # Parse folder names as dates and sort
    date_dirs = []
    for d in dirs:
        try:
            parsed_date = datetime.strptime(d, "%Y-%m-%d")
            date_dirs.append((d, parsed_date))
        except ValueError:
            pass  # Ignore directories that don't have a date format

    if not date_dirs:
        return None, []

    # Get the latest directory based on date
    latest_dir = max(date_dirs, key=lambda x: x[1])[0]
    latest_subdir = os.path.join(path, latest_dir)
    files = os.listdir(latest_subdir)

    remove_files = ['.ipynb_checkpoints', 'full-data.csv', 'full-data.rds']
    for file in remove_files:        
        if file in files:
            files.remove(file)
 
    return latest_subdir + '/', files

def add_RAI_cat_norms(raw_data: pd.DataFrame) -> pd.DataFrame:
    # Find which columns are available for each category
    backlash_cols = [
        'bribery_economic_corruptionNorm',
        'transnational_organization_crimeNorm'
    ]
    backlash_avail = [
        col for col in backlash_cols if col in raw_data.columns
    ]
    diplomacy_cols = [
        'diplomatic_recognitionNorm', 'diplomatic_sanctionNorm',
        'diplomatic_statementNorm', 'diplomatic_visitNorm'
    ]
    diplomacy_avail = [
        col for col in diplomacy_cols if col in raw_data.columns
    ]
    domestic_interference_cols = [
        'intelligence_counterintelligenceNorm',
        'political_process_policy_interventionNorm', 'surveillanceNorm'
    ]
    domestic_interference_avail = [
        col for col in domestic_interference_cols if col in raw_data.columns
    ]
    economic_power_cols = [
        'economic_aid_assistanceNorm', 'investmentNorm',
        'tech_transfer_investmentNorm',
        'trade_agreement_exchangeNorm', 'trade_financial_sanctionNorm'
    ]
    economic_power_avail = [
        col for col in economic_power_cols if col in raw_data.columns
    ]
    hard_power_cols = [
        'arms_transfer_security_aid_assistanceNorm',
        'security_cooperationNorm', 'joint_security_force_exerciseNorm',
        'security_force_facility_presenceNorm'
    ]
    hard_power_avail = [
        col for col in hard_power_cols if col in raw_data.columns
    ]
    soft_power_cols = [
        'diaspora_activationNorm', 'media_campaign_interventionNorm',
        'professional_cultural_exchangeNorm'
    ]
    soft_power_avail = [
        col for col in soft_power_cols if col in raw_data.columns
    ]
    
    # Calculate norm sums for each category
    raw_data['backlash_sumNorm'] = raw_data[backlash_avail].sum(axis=1)
    raw_data['diplomacy_sumNorm'] = raw_data[diplomacy_avail].sum(axis=1)
    raw_data['domestic_interference_sumNorm'] = \
        raw_data[domestic_interference_avail].sum(axis=1)
    raw_data['economic_power_sumNorm'] = \
        raw_data[economic_power_avail].sum(axis=1)
    raw_data['hard_power_sumNorm'] = raw_data[hard_power_avail].sum(axis=1)
    raw_data['soft_power_sumNorm'] = raw_data[soft_power_avail].sum(axis=1)
    
    return raw_data


def convert_to_training_data_2(X, Y, country, event, peak_detector, mode='cutoff'):
    # plot_dir='/Users/mahda.soltani/forecast-surges-pipeline/plots'
    script_dir = os.path.dirname(__file__)
    model_path = os.path.join(script_dir, 'content', 'model_version1')
    layer = TFSMLayer(model_path, call_endpoint='serving_default')
    
    #I needed to define a Keras model with this layer because TFSMLayer doesnt directly expose .predict()
    input_layer = Input(shape=(2,))  #based on the input tensor's shape from model signature (None, 2)
    output_layer = layer(input_layer)
    loaded_model = Model(inputs=input_layer, outputs=output_layer)

    
    X_values = peak_detector.peak_detection(Y)    
    X_values = np.nan_to_num(np.array(X_values))  # Replace NaN in input data
    X_values = X_values.reshape(-1, 2)  # Reshape to have 2 values per data point
    
    predictions = loaded_model.predict(X_values)
    predictions = predictions['dense_5']
    
    binary_predictions_NN = (predictions > 0.5).astype(int)
    binary_predictions_NN  = [item for sublist in binary_predictions_NN for item in sublist]
    
    binary_predictions_algorithm = peak_detector.peak_detection_conservative(Y)
    binary_predictions_algorithm = [int(value) for value in binary_predictions_algorithm]
    new_list = [1 if x or y else 0 for x, y in zip(binary_predictions_NN, binary_predictions_algorithm)]
    top_3_indices = sorted(range(len(Y)), key=lambda i: abs(Y[i]), reverse=True)[:3]
    for idx in top_3_indices:
        new_list[idx] = 1
    for i in range(1, len(Y) - 1):
        if new_list[i] == 1:
            left = Y[i-1] if i > 0 else float('-inf')
            right = Y[i+1] if i < len(Y) - 1 else float('-inf')

            if left > Y[i] and right > Y[i]:
                # Both neighbors are larger, remove peak
                new_list[i] = 0
            else:
                # Label larger neighbors as peaks
                if left > Y[i]:
                    new_list[i-1] = 1
                if right > Y[i]:
                    new_list[i+1] = 1
                    
    
    
    return X_values, binary_predictions_algorithm, new_list

def detect_peaks(folder, countries, date):
    # match = re.search(r'\d{4}-\d{2}-\d{2}', folder)
    # date = match.group(0)
    plot_dir = f'../plots2/{date}'
    os.makedirs(plot_dir, exist_ok=True)
    
    lookaround = 12
    std_dev = 0.88
    normalise = 1
    alpha = 0.05
    beta = 0.2
    peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    
    peak_results = {}
    for country in country_list:
        if country in countries:
            file = country + '.csv'
            pre_data = pd.read_csv(folder+file)
            raw_data = add_RAI_cat_norms(pre_data)
            
            cols = data.targets[:]
            cols.append('date')
            cols = [cols[-1]] + cols[:-1]
            
            peaks_df = pd.DataFrame(columns=cols)
            peaks_df['date'] = raw_data['date'].tolist()
            
        
        
            for event in data.targetsNew: 
                if event in raw_data.columns:
                    X, Y = data.prep_data(raw_data, event, encode_date=True, lag=1)
                    peaks, peaks_conservative, peaks_detected = convert_to_training_data_2(X, Y, country, event, peak_detector)
                    peaks_df[event] =  peaks_detected
                    data_for_plotting = raw_data.copy()
                    data_for_plotting[event + '_peaks'] = peaks_detected
                    
                    
                    # if event in data.targets:
                    peak_results[(country, event)] = peaks_detected
                    
                    data_for_plotting['date'] = pd.to_datetime(data_for_plotting['date'])
    
                    # Create the plot
                    fig, ax2 = plt.subplots(figsize=(15, 12))
                    
                    # Plot normalized counts on ax2
                    ax2.plot(data_for_plotting['date'], data_for_plotting[event + 'Norm'], label='Normalized Number of Articles', color='green')
                    ax2.scatter(data_for_plotting['date'][data_for_plotting[event + '_peaks'] == 1], 
                                data_for_plotting[event + 'Norm'][data_for_plotting[event + '_peaks'] == 1], 
                                color='red', label='Detected Peaks', zorder=5)
                    ax2.set_xlabel('Date')
                    ax2.set_ylabel('Normalized Number of Articles')
                    ax2.legend(loc='upper right')
                    
                    # Format x-axis for dates
                    ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
                    ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
                    plt.xticks(rotation=45)
                    
                    # Set title
                    plt.title(f'Peak Detection for {country} - {event}')
                    
                    # Save the plot
                    plt.savefig(os.path.join(plot_dir, f'{country}_{event}_peaks2.png'))
                    plt.close()

                        
                    
            for event2 in data.targetsRAI:
                if event2 in raw_data.columns:
                    # print('hi')
                    # print(event2)
                    X, Y = data.prep_data(raw_data, event2, encode_date=True, lag=1)
                    peaks, peaks_conservative, peaks_detected = convert_to_training_data_2(X, Y, country, event2, peak_detector)
                    peaks_df[event2] =  peaks_detected
                    
                    # Create plot of these peaks
                    data_for_plotting = raw_data.copy()
                    data_for_plotting[event2 + '_peaks'] = peaks_detected
                    
                    # if event2 in data.targets:
                    peak_results[(country, event2)] = peaks_detected
                    
                    data_for_plotting['date'] = pd.to_datetime(data_for_plotting['date'])
    
                    # Create the plot
                    fig, ax2 = plt.subplots(figsize=(15, 12))
                    
                    # Plot normalized counts on ax2
                    ax2.plot(data_for_plotting['date'], data_for_plotting[event2], label='Normalized Number of Articles', color='green')
                    ax2.scatter(data_for_plotting['date'][data_for_plotting[event2 + '_peaks'] == 1], 
                                data_for_plotting[event2][data_for_plotting[event2 + '_peaks'] == 1], 
                                color='red', label='Detected Peaks', zorder=5)
                    ax2.set_xlabel('Date')
                    ax2.set_ylabel('Normalized Number of Articles')
                    ax2.legend(loc='upper right')
                    
                    # Format x-axis for dates
                    ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
                    ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
                    plt.xticks(rotation=45)
                    
                    # Set title
                    plt.title(f'RAI peak Detection for {country} - {event2}')
                    
                    # Save the plot
                    plt.savefig(os.path.join(plot_dir, f'{country}_{event2}_peaks2.png'))
                    plt.close()
            
            if not os.path.exists(f'../result/{year}-{month}-{day}/peaks'):
                        os.makedirs(f'../result/{year}-{month}-{day}/peaks') 

            
            peaks_df.to_csv(
                f'../result/{year}-{month}-{day}/peaks/{country}.csv',
                index = False
            )
            
            #peaks_df.to_csv(f'/Users/mahda.soltani/Desktop/MLP/Peaks/{country}.csv')
            
            
    return peak_results
                    

def run_peak_detection(path):
    folder, files = get_updated_files(path)
    if folder is None or len(files) == 0:
        print("No recent data folders found.")
        return
    countries = [file[:-4] for file in files]  
    detect_peaks(folder, countries, f'{year}-{month}-{day}')
    print("Peak detection completed.")
    
run_peak_detection(data_folder)    
