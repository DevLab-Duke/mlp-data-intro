import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
from sklearn import tree
from sklearn import tree as tr
import data
from datetime import datetime,date
import warnings
warnings.filterwarnings('ignore')
from peak_detector import PeakDetector
import matplotlib.dates as mdates
from keras import Input, Model
from keras.layers import TFSMLayer

# Global Variables Inputs
today = date.today()

day = today.strftime("%d")
month = today.strftime("%m")
year = today.strftime("%Y")

# Filesystem env variables
civic_data_folder = '../../data/1-civic-aggregate'
rai_data_folder = '../../data/1-rai-aggregate'
civic_result_folder = '../../data/2-civic-shock'
rai_result_folder = '../../data/2-rai-shock'
country_list = ["Panama", "Costa Rica", "Solomon Islands", "Dominican Republic",
                      "Peru", "Nicaragua", "El Salvador", "Honduras", "Jamaica",
                      "Paraguay", "Ecuador", "Colombia"]

#Function to get files from static folder
def get_updated_files(path='.'):
    if not os.path.exists(path):
        return None, []
    
    files = os.listdir(path)
    
    # Filter to only CSV files
    csv_files = [f for f in files if f.endswith('.csv')]
    
    remove_files = ['.ipynb_checkpoints', 'full-data.csv', 'full-data.rds']
    for file in remove_files:        
        if file in csv_files:
            csv_files.remove(file)
 
    return path + '/', csv_files


def convert_to_training_data_2(Y, country, event, peak_detector, mode='cutoff'):
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
    civic_plot_dir = f'../../data/2-civic-shock'
    rai_plot_dir = f'../../data/2-rai-shock'
    os.makedirs(civic_plot_dir, exist_ok=True)
    os.makedirs(rai_plot_dir, exist_ok=True)
    
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
            civic_data = pd.read_csv(folder+file)
            
            cols = data.civic[:]
            cols.append('date')
            cols = [cols[-1]] + cols[:-1]
            
            peaks_df = pd.DataFrame(columns=cols)
            peaks_df['date'] = civic_data['date'].tolist()
            
            # Process civic events
            for event in data.civic: 
                if event in civic_data.columns:
                    Y = civic_data[event]
                    peaks, peaks_conservative, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector)
                    peaks_df[event] =  peaks_detected
                    data_for_plotting = civic_data.copy()
                    data_for_plotting[event + '_peaks'] = peaks_detected
                    
                    peak_results[(country, event)] = peaks_detected
                    
                    data_for_plotting['date'] = pd.to_datetime(data_for_plotting['date'])
    
                    # Create the plot
                    fig, ax2 = plt.subplots(figsize=(15, 12))
                    
                    # Plot normalized counts on ax2
                    ax2.plot(data_for_plotting['date'], data_for_plotting[event], label='Normalized Number of Articles', color='green')
                    ax2.scatter(data_for_plotting['date'][data_for_plotting[event + '_peaks'] == 1], 
                                data_for_plotting[event][data_for_plotting[event + '_peaks'] == 1], 
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
                    plt.savefig(os.path.join(civic_plot_dir, f'{country}_{event}_peaks.png'))
                    plt.close()

            # Save civic peaks to civic folder
            peaks_df.to_csv(
                f'{civic_result_folder}/{country}.csv',
                index = False
            )
            
    return peak_results

def detect_rai_peaks_by_influencer(folder, countries, date):
    """Process RAI shock detection for each influencer CSV file separately."""
    rai_plot_dir = f'../../data/2-rai-shock'
    os.makedirs(rai_plot_dir, exist_ok=True)
    
    lookaround = 12
    std_dev = 0.88
    normalise = 1
    alpha = 0.05
    beta = 0.2
    peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    
    peak_results = {}
    
    # Get all RAI files in the directory
    _, rai_files = get_updated_files(folder)
    
    # Process each influencer file
    for rai_file in rai_files:
        # Parse filename to get country and influencer (e.g., "Belarus_russia.csv")
        if '_' not in rai_file or not rai_file.endswith('.csv'):
            continue
            
        filename_parts = rai_file[:-4].split('_', 1)  # Remove .csv and split on first underscore
        if len(filename_parts) != 2:
            continue
            
        country, influencer = filename_parts
        
        # Only process countries in our list
        if country not in country_list:
            continue
            
        print(f"Processing RAI peaks for {country} - {influencer}")
        
        rai_data = pd.read_csv(os.path.join(folder, rai_file))
        # RAI themes are now pre-calculated in the data pipeline
        
        # Create peaks dataframe for this influencer
        cols = data.rai[:]
        cols.append('date')
        cols = [cols[-1]] + cols[:-1]
        
        peaks_df = pd.DataFrame(columns=cols)
        peaks_df['date'] = rai_data['date'].tolist()
        
        # Process RAI events
        for event in data.rai:
            if event in rai_data.columns:
                Y = rai_data[event]
                peaks, peaks_conservative, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector)
                peaks_df[event] = peaks_detected
                
                # Create plot
                data_for_plotting = rai_data.copy()
                data_for_plotting[event + '_peaks'] = peaks_detected
                
                peak_results[(country, influencer, event)] = peaks_detected
                
                data_for_plotting['date'] = pd.to_datetime(data_for_plotting['date'])

                # Create the plot
                fig, ax2 = plt.subplots(figsize=(15, 12))
                
                # Plot normalized counts
                ax2.plot(data_for_plotting['date'], data_for_plotting[event], label='Normalized Number of Articles', color='green')
                ax2.scatter(data_for_plotting['date'][data_for_plotting[event + '_peaks'] == 1], 
                            data_for_plotting[event][data_for_plotting[event + '_peaks'] == 1], 
                            color='red', label='Detected Peaks', zorder=5)
                ax2.set_xlabel('Date')
                ax2.set_ylabel('Normalized Number of Articles')
                ax2.legend(loc='upper right')
                
                # Format x-axis for dates
                ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
                ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
                plt.xticks(rotation=45)
                
                # Set title with influencer info
                plt.title(f'RAI Peak Detection for {country} ({influencer}) - {event}')
                
                # Save the plot with influencer in filename
                plt.savefig(os.path.join(rai_plot_dir, f'{country}_{influencer}_{event}_peaks.png'))
                plt.close()
        
        # Save peaks to RAI folder with influencer in filename
        peaks_df.to_csv(
            f'../../data/2-rai-shock/{country}_{influencer}.csv',
            index=False
        )
        
    return peak_results

def run_peak_detection(path):
    folder, files = get_updated_files(path)
    if folder is None or len(files) == 0:
        print("No recent data folders found.")
        return
    countries = [file[:-4] for file in files]  
    detect_peaks(folder, countries, f'{year}-{month}-{day}')
    print("Civic peak detection completed.")

def run_rai_peak_detection(path):
    folder, files = get_updated_files(path)
    if folder is None or len(files) == 0:
        print("No recent RAI data folders found.")
        return
    detect_rai_peaks_by_influencer(folder, [], f'{year}-{month}-{day}')
    print("RAI peak detection completed.")
    
# Run both civic and RAI peak detection
run_peak_detection(civic_data_folder)    
run_rai_peak_detection(rai_data_folder)    
