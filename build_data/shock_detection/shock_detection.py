# import numpy as np
# import os
# import matplotlib.pyplot as plt
# import pandas as pd
# from sklearn import tree
# from sklearn import tree as tr
# import data
# from datetime import datetime,date
# import warnings
# warnings.filterwarnings('ignore')
# from peak_detector import PeakDetector
# import matplotlib.dates as mdates
# from keras import Input, Model
# # from keras.layers import TFSMLayer
# from tensorflow.keras.layers import TFSMLayer
# import tensorflow as tf
# TFSMLayer = tf.keras.layers.TFSMLayer

# # Global Variables Inputs
# today = date.today()

# day = today.strftime("%d")
# month = today.strftime("%m")
# year = today.strftime("%Y")

# # Filesystem env variables
# # civic_data_folder = '../../data/1-civic-aggregate'
# # rai_data_folder = '../../data/1-rai-aggregate'
# # civic_result_folder = '../../data/2-civic-shock'
# # rai_result_folder = '../../data/2-rai-shock'

# #relative paths
# SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# civic_data_folder = os.path.join(SCRIPT_DIR, '../../data/1-civic-aggregate')
# rai_data_folder   = os.path.join(SCRIPT_DIR, '../../data/1-rai-aggregate')
# civic_result_folder = os.path.join(SCRIPT_DIR, '../../data/2-civic-shock')
# rai_result_folder   = os.path.join(SCRIPT_DIR, '../../data/2-rai-shock')

# # Stage remaining countries (New Civic but no New RAI as of 6/26)
# # country_list = ['Armenia', 'Belarus', 'Hungary', 'Azerbaijan', 'Moldova', 'Macedonia', 'Turkey', 
# #                 'Uzbekistan', 'Kyrgyzstan', 'Kazakhstan', 'Algeria', 'Guatemala', 'Bangladesh', 
# #                 'Cambodia', 'India', 'Indonesia', 'Malaysia', 'Nepal', 'Pakistan', 'Philippines', 
# #                 'Sri Lanka', 'Timor Leste', 'Angola', 'Burkina Faso', 'Cameroon', 'DR Congo', 'Ghana', 
# #                 'Liberia', 'Malawi', 'Mozambique', 'Namibia', 'Nigeria', 'Rwanda', 'South Africa', 
# #                 'South Sudan', 'Tunisia', 'Uganda']

# country_list = ['Albania', 'Algeria', 'Angola', 'Armenia', 'Azerbaijan', 'Bangladesh', 'Belarus', 'Benin', 'Burkina Faso',
#                 'Cambodia', 'Cameroon', 'Colombia', 'Costa Rica', 'DR Congo', 'Dominican Republic', 'Ecuador', 'El Salvador',
#                 'Ethiopia', 'Georgia', 'Ghana', 'Guatemala', 'Honduras', 'Hungary', 'India', 'Indonesia', 'Jamaica', 'Kazakhstan',
#                 'Kenya', 'Kosovo', 'Kyrgyzstan', 'Liberia', 'Macedonia', 'Malawi', 'Malaysia', 'Mali', 'Mauritania', 'Mexico', 'Moldova',
#                 'Morocco', 'Mozambique', 'Namibia', 'Nepal', 'Nicaragua', 'Niger', 'Nigeria', 'Pakistan', 'Panama', 'Paraguay',
#                 'Peru', 'Philippines', 'Rwanda', 'Senegal', 'Serbia', 'Solomon Islands', 'South Africa', 'South Sudan', 'Sri Lanka',
#                 'Tanzania', 'Timor Leste', 'Tunisia', 'Turkey', 'Uganda', 'Ukraine', 'Uzbekistan', 'Zambia', 'Zimbabwe']
# # country_list = ['Mexico']

# #Function to get files from static folder
# def get_updated_files(path='.'):
#     if not os.path.exists(path):
#         return None, []
    
#     files = os.listdir(path)
    
#     # Filter to only CSV files
#     csv_files = [f for f in files if f.endswith('.csv')]
    
#     remove_files = ['.ipynb_checkpoints', 'full-data.csv', 'full-data.rds']
#     for file in remove_files:        
#         if file in csv_files:
#             csv_files.remove(file)
 
#     return path + '/', csv_files


# def convert_to_training_data_2(Y, country, event, peak_detector, mode='cutoff'):
#     # plot_dir='/Users/mahda.soltani/forecast-surges-pipeline/plots'
#     script_dir = os.path.dirname(__file__)
#     model_path = os.path.join(script_dir, 'content', 'model_version1')
#     layer = TFSMLayer(model_path, call_endpoint='serving_default')
    
#     #I needed to define a Keras model with this layer because TFSMLayer doesnt directly expose .predict()
#     input_layer = Input(shape=(2,))  #based on the input tensor's shape from model signature (None, 2)
#     output_layer = layer(input_layer)
#     loaded_model = Model(inputs=input_layer, outputs=output_layer)

    
#     X_values = peak_detector.peak_detection(Y)    
#     X_values = np.nan_to_num(np.array(X_values))  # Replace NaN in input data
#     X_values = X_values.reshape(-1, 2)  # Reshape to have 2 values per data point
    
#     predictions = loaded_model.predict(X_values)
#     predictions = predictions['dense_5']
    
#     binary_predictions_NN = (predictions > 0.5).astype(int)
#     binary_predictions_NN  = [item for sublist in binary_predictions_NN for item in sublist]
    
#     binary_predictions_algorithm = peak_detector.peak_detection_conservative(Y)
#     binary_predictions_algorithm = [int(value) for value in binary_predictions_algorithm]
#     new_list = [1 if x or y else 0 for x, y in zip(binary_predictions_NN, binary_predictions_algorithm)]
#     top_3_indices = sorted(range(len(Y)), key=lambda i: abs(Y[i]), reverse=True)[:3]
#     for idx in top_3_indices:
#         new_list[idx] = 1
#     for i in range(1, len(Y) - 1):
#         if new_list[i] == 1:
#             left = Y[i-1] if i > 0 else float('-inf')
#             right = Y[i+1] if i < len(Y) - 1 else float('-inf')

#             if left > Y[i] and right > Y[i]:
#                 # Both neighbors are larger, remove peak
#                 new_list[i] = 0
#             else:
#                 # Label larger neighbors as peaks
#                 if left > Y[i]:
#                     new_list[i-1] = 1
#                 if right > Y[i]:
#                     new_list[i+1] = 1
                    
    
    
#     return X_values, binary_predictions_algorithm, new_list

# def detect_peaks(folder, countries, date):
#     # match = re.search(r'\d{4}-\d{2}-\d{2}', folder)
#     # date = match.group(0)
#     # civic_plot_dir = f'../../data/2-civic-shock'
#     # rai_plot_dir = f'../../data/2-rai-shock'
#     # os.makedirs(civic_plot_dir, exist_ok=True)
#     # os.makedirs(rai_plot_dir, exist_ok=True)

#     civic_plot_dir = os.path.join(SCRIPT_DIR, '../../data/2-civic-shock')
#     rai_plot_dir   = os.path.join(SCRIPT_DIR, '../../data/2-rai-shock')
#     os.makedirs(civic_plot_dir, exist_ok=True)
#     os.makedirs(rai_plot_dir,   exist_ok=True)
    
#     lookaround = 12
#     std_dev = 0.88
#     normalise = 1
#     alpha = 0.05
#     beta = 0.2
#     peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    
#     peak_results = {}
#     for country in country_list:
#         if country in countries:
#             file = country + '.csv'
#             civic_data = pd.read_csv(folder+file)
            
#             cols = data.civic[:]
#             cols.append('date')
#             cols = [cols[-1]] + cols[:-1]
            
#             peaks_df = pd.DataFrame(columns=cols)
#             peaks_df['date'] = civic_data['date'].tolist()
            
#             # Process civic events
#             for event in data.civic: 
#                 if event in civic_data.columns:
#                     Y = civic_data[event]
#                     peaks, peaks_conservative, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector)
#                     peaks_df[event] =  peaks_detected
#                     data_for_plotting = civic_data.copy()
#                     data_for_plotting[event + '_peaks'] = peaks_detected
                    
#                     peak_results[(country, event)] = peaks_detected
                    
#                     data_for_plotting['date'] = pd.to_datetime(data_for_plotting['date'])
    
#                     # Create the plot
#                     fig, ax2 = plt.subplots(figsize=(15, 12))
                    
#                     # Plot normalized counts on ax2
#                     ax2.plot(data_for_plotting['date'], data_for_plotting[event], label='Normalized Number of Articles', color='green')
#                     ax2.scatter(data_for_plotting['date'][data_for_plotting[event + '_peaks'] == 1], 
#                                 data_for_plotting[event][data_for_plotting[event + '_peaks'] == 1], 
#                                 color='red', label='Detected Peaks', zorder=5)
#                     ax2.set_xlabel('Date')
#                     ax2.set_ylabel('Normalized Number of Articles')
#                     ax2.legend(loc='upper right')
                    
#                     # Format x-axis for dates
#                     ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
#                     ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
#                     plt.xticks(rotation=45)
                    
#                     # Set title
#                     plt.title(f'Peak Detection for {country} - {event}')
                    
#                     # Save the plot
#                     plt.savefig(os.path.join(civic_plot_dir, f'{country}_{event}_peaks.png'))
#                     plt.close()

#             # Save civic peaks to civic folder
#             # peaks_df.to_csv(
#             #     f'{civic_result_folder}/{country}.csv',
#             #     index = False
#             # )
#             outfile = os.path.join(civic_result_folder, f'{country}.csv')
#             peaks_df.to_csv(outfile, index=False)
            
#     return peak_results

# def detect_rai_peaks_by_influencer(folder, countries, date):
#     """Process RAI shock detection for each influencer CSV file separately."""
#     # rai_plot_dir = f'../../data/2-rai-shock'
#     # os.makedirs(rai_plot_dir, exist_ok=True)
#     rai_plot_dir = os.path.join(SCRIPT_DIR, '../../data/2-rai-shock')
#     os.makedirs(rai_plot_dir, exist_ok=True)
    
#     lookaround = 12
#     std_dev = 0.88
#     normalise = 1
#     alpha = 0.05
#     beta = 0.2
#     peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    
#     peak_results = {}
    
#     # Get all RAI files in the directory
#     _, rai_files = get_updated_files(folder)
    
#     # Process each influencer file
#     for rai_file in rai_files:
#         # Parse filename to get country and influencer (e.g., "Belarus_russia.csv")
#         if '_' not in rai_file or not rai_file.endswith('.csv'):
#             continue
            
#         filename_parts = rai_file[:-4].split('_', 1)  # Remove .csv and split on first underscore
#         if len(filename_parts) != 2:
#             continue
            
#         country, influencer = filename_parts
        
#         # Only process countries in our list
#         if country not in country_list:
#             continue
            
#         print(f"Processing RAI peaks for {country} - {influencer}")
        
#         rai_data = pd.read_csv(os.path.join(folder, rai_file))
#         # RAI themes are now pre-calculated in the data pipeline
        
#         # Create peaks dataframe for this influencer
#         cols = data.rai[:]
#         cols.append('date')
#         cols = [cols[-1]] + cols[:-1]
        
#         peaks_df = pd.DataFrame(columns=cols)
#         peaks_df['date'] = rai_data['date'].tolist()
        
#         # Process RAI events
#         for event in data.rai:
#             if event in rai_data.columns:
#                 Y = rai_data[event]
#                 peaks, peaks_conservative, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector)
#                 peaks_df[event] = peaks_detected
                
#                 # Create plot
#                 data_for_plotting = rai_data.copy()
#                 data_for_plotting[event + '_peaks'] = peaks_detected
                
#                 peak_results[(country, influencer, event)] = peaks_detected
                
#                 data_for_plotting['date'] = pd.to_datetime(data_for_plotting['date'])

#                 # Create the plot
#                 fig, ax2 = plt.subplots(figsize=(15, 12))
                
#                 # Plot normalized counts
#                 ax2.plot(data_for_plotting['date'], data_for_plotting[event], label='Normalized Number of Articles', color='green')
#                 ax2.scatter(data_for_plotting['date'][data_for_plotting[event + '_peaks'] == 1], 
#                             data_for_plotting[event][data_for_plotting[event + '_peaks'] == 1], 
#                             color='red', label='Detected Peaks', zorder=5)
#                 ax2.set_xlabel('Date')
#                 ax2.set_ylabel('Normalized Number of Articles')
#                 ax2.legend(loc='upper right')
                
#                 # Format x-axis for dates
#                 ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
#                 ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
#                 plt.xticks(rotation=45)
                
#                 # Set title with influencer info
#                 plt.title(f'RAI Peak Detection for {country} ({influencer}) - {event}')
                
#                 # Save the plot with influencer in filename
#                 plt.savefig(os.path.join(rai_plot_dir, f'{country}_{influencer}_{event}_peaks.png'))
#                 plt.close()
        
#         # Save peaks to RAI folder with influencer in filename
#         # peaks_df.to_csv(
#         #     f'../../data/2-rai-shock/{country}_{influencer}.csv',
#         #     index=False
#         # )
#         outfile = os.path.join(rai_result_folder, f'{country}_{influencer}.csv')
#         peaks_df.to_csv(outfile, index=False)
        
#     return peak_results

# def run_peak_detection(path):
#     folder, files = get_updated_files(path)
#     if folder is None or len(files) == 0:
#         print("No recent data folders found.")
#         return
#     countries = [file[:-4] for file in files]  
#     detect_peaks(folder, countries, f'{year}-{month}-{day}')
#     print("Civic peak detection completed.")

# def run_rai_peak_detection(path):
#     folder, files = get_updated_files(path)
#     if folder is None or len(files) == 0:
#         print("No recent RAI data folders found.")
#         return
#     detect_rai_peaks_by_influencer(folder, [], f'{year}-{month}-{day}')
#     print("RAI peak detection completed.")
    
# # Run both civic and RAI peak detection
# # run_peak_detection(civic_data_folder)    
# # run_rai_peak_detection(rai_data_folder)

# # ZR's adding new variable to run peak detection
# RUN_PEAK_DETECTION = True   # flip to True when you need fresh outputs

# if RUN_PEAK_DETECTION:
#     run_peak_detection(civic_data_folder)
#     run_rai_peak_detection(rai_data_folder)
# else:
#     print("⚠️  Skipping peak‑detection step (RUN_PEAK_DETECTION = False)")

# # Combine all CSV files into single files
# def combine_civic_shock_files():
#     """Combine all civic shock detection CSV files into a single CSV file."""
#     civic_files = []
    
#     # Get all CSV files from civic shock folder
#     for filename in os.listdir(civic_result_folder):
#         if filename.endswith('.csv'):
#             file_path = os.path.join(civic_result_folder, filename)
#             country = filename[:-4]  # Remove .csv extension
            
#             # Read the CSV file
#             df = pd.read_csv(file_path)
#             df['country'] = country  # Add country column
#             civic_files.append(df)
    
#     if civic_files:
#         # Combine all dataframes
#         combined_civic = pd.concat(civic_files, ignore_index=True)
        
#         # Reorder columns to put country first, then date
#         cols = ['country', 'date'] + [col for col in combined_civic.columns 
#                                       if col not in ['country', 'date']]
#         combined_civic = combined_civic[cols]
        
#         # Jeremy's
#         # Save combined file to final-counts folder
#         # final_counts_folder = '../../data/final-counts'
#         # output_path = os.path.join(final_counts_folder, 'shock-civic-data.csv')
#         # combined_civic.to_csv(output_path, index=False)

#         # ZR's relative paths
#         final_counts_folder = os.path.join(SCRIPT_DIR, '../../data/final-counts')
#         os.makedirs(final_counts_folder, exist_ok=True)          # <‑‑ add this
#         output_path = os.path.join(final_counts_folder, 'shock-civic-data.csv')
#         combined_civic.to_csv(output_path, index=False)
#         print(f"Combined civic shock data saved to: {output_path}")
#     else:
#         print("No civic shock CSV files found to combine.")

# def combine_rai_shock_files():
#     """Combine all RAI shock detection CSV files into a single file."""
#     rai_files = []
    
#     # Get all CSV files from RAI shock folder
#     for filename in os.listdir(rai_result_folder):
#         if filename.endswith('.csv') and '_' in filename:
#             file_path = os.path.join(rai_result_folder, filename)
            
#             # Parse filename to get country and influencer
#             filename_parts = filename[:-4].split('_', 1)  # Remove .csv and split on first underscore
#             if len(filename_parts) == 2:
#                 country, influencer = filename_parts
                
#                 # Read the CSV file
#                 df = pd.read_csv(file_path)
#                 df['country'] = country  # Add country column
#                 df['influencer'] = influencer  # Add influencer column
#                 rai_files.append(df)
    
#     if rai_files:
#         # Combine all dataframes
#         combined_rai = pd.concat(rai_files, ignore_index=True)
        
#         # Reorder columns to put country and influencer first, then date
#         cols = ['country', 'influencer', 'date'] + [col for col in combined_rai.columns 
#                                                     if col not in ['country', 'influencer', 'date']]
#         combined_rai = combined_rai[cols]
        
#         # Jeremy's
#         # Save combined file to final-counts folder
#         # final_counts_folder = '../../data/final-counts'
#         # output_path = os.path.join(final_counts_folder, 'shock-rai-data.csv')
#         # combined_rai.to_csv(output_path, index=False)

#         # ZR's relative paths
#         final_counts_folder = os.path.join(SCRIPT_DIR, '../../data/final-counts')
#         os.makedirs(final_counts_folder, exist_ok=True)          # <‑‑ add this
#         output_path = os.path.join(final_counts_folder, 'shock-rai-data.csv')
#         combined_rai.to_csv(output_path, index=False)
#         print(f"Combined RAI shock data saved to: {output_path}")
#     else:
#         print("No RAI shock CSV files found to combine.")

# # Run the combination functions
# print("Combining shock detection files...")
# combine_civic_shock_files()
# combine_rai_shock_files()
# print("File combination completed.")    
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
from tensorflow.keras.layers import TFSMLayer
import tensorflow as tf
TFSMLayer = tf.keras.layers.TFSMLayer

# =========================
# Global Variables Inputs
# =========================
today = date.today()
day = today.strftime("%d")
month = today.strftime("%m")
year = today.strftime("%Y")

# =========================
# Paths (relative)
# =========================
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

civic_data_folder = os.path.join(SCRIPT_DIR, '../../data/1-civic-aggregate')
rai_data_folder   = os.path.join(SCRIPT_DIR, '../../data/1-rai-aggregate')
mleed_data_folder = os.path.join(SCRIPT_DIR, '../../data/1-mleed-aggregate')   # <-- NEW

civic_result_folder = os.path.join(SCRIPT_DIR, '../../data/2-civic-shock')
rai_result_folder   = os.path.join(SCRIPT_DIR, '../../data/2-rai-shock')
mleed_result_folder = os.path.join(SCRIPT_DIR, '../../data/2-mleed-shock')     # <-- NEW

os.makedirs(civic_result_folder, exist_ok=True)
os.makedirs(rai_result_folder,   exist_ok=True)
os.makedirs(mleed_result_folder, exist_ok=True)  # <-- NEW

# =========================
# Countries
# =========================
country_list = [

                'Guatemala', 'Honduras',  'El Salvador', 'Nicaragua'
    
                # 'Albania', 'Algeria', 'Angola', 'Armenia', 'Azerbaijan', 'Bangladesh', 'Belarus', 'Benin', 'Burkina Faso',
                # 'Cambodia', 'Cameroon', 'Colombia', 'Costa Rica', 'DR Congo', 'Dominican Republic', 'Ecuador', 'El Salvador',
                # 'Ethiopia', 'Georgia', 'Ghana', 'Guatemala', 'Honduras', 'Hungary', 'India', 'Indonesia', 'Jamaica', 'Kazakhstan',
                # 'Kenya', 'Kosovo', 'Kyrgyzstan', 'Liberia', 'Macedonia', 'Malawi', 'Malaysia', 'Mali', 'Mauritania', 'Mexico', 'Moldova',
                # 'Morocco', 'Mozambique', 'Namibia', 'Nepal', 'Nicaragua', 'Niger', 'Nigeria', 'Pakistan', 'Panama', 'Paraguay',
                # 'Peru', 'Philippines', 'Rwanda', 'Senegal', 'Serbia', 'Solomon Islands', 'South Africa', 'South Sudan', 'Sri Lanka',
                # 'Tanzania', 'Timor Leste', 'Tunisia', 'Turkey', 'Uganda', 'Ukraine', 'Uzbekistan', 'Zambia', 'Zimbabwe'
                ]

# =========================
# Helpers
# =========================
def get_updated_files(path='.'):
    if not os.path.exists(path):
        return None, []
    files = os.listdir(path)
    csv_files = [f for f in files if f.endswith('.csv')]
    remove_files = ['.ipynb_checkpoints', 'full-data.csv', 'full-data.rds']
    for file in remove_files:
        if file in csv_files:
            csv_files.remove(file)
    return path + '/', csv_files

def _load_tfsm_model():
    """Load TF SavedModel for peak fusion (same as your civic/rai path)."""
    script_dir = os.path.dirname(__file__)
    model_path = os.path.join(script_dir, 'content', 'model_version1')
    layer = TFSMLayer(model_path, call_endpoint='serving_default')
    inp = Input(shape=(2,))
    out = layer(inp)
    return Model(inputs=inp, outputs=out)

def convert_to_training_data_2(Y, country, event, peak_detector, loaded_model=None):
    if loaded_model is None:
        loaded_model = _load_tfsm_model()

    X_values = peak_detector.peak_detection(Y)
    X_values = np.nan_to_num(np.array(X_values))
    X_values = X_values.reshape(-1, 2)

    predictions = loaded_model.predict(X_values)
    predictions = predictions['dense_5']
    binary_predictions_NN = (predictions > 0.5).astype(int)
    binary_predictions_NN = [item for sublist in binary_predictions_NN for item in sublist]

    binary_predictions_algorithm = peak_detector.peak_detection_conservative(Y)
    binary_predictions_algorithm = [int(v) for v in binary_predictions_algorithm]

    # OR of NN + conservative algorithm
    new_list = [1 if x or y else 0 for x, y in zip(binary_predictions_NN, binary_predictions_algorithm)]

    # force top-3 magnitude spikes on
    top_3_indices = sorted(range(len(Y)), key=lambda i: abs(Y[i]), reverse=True)[:3]
    for idx in top_3_indices:
        new_list[idx] = 1

    # neighborhood cleanup: drop pits & allow neighbor switch
    for i in range(1, len(Y) - 1):
        if new_list[i] == 1:
            left = Y[i-1] if i > 0 else float('-inf')
            right = Y[i+1] if i < len(Y) - 1 else float('-inf')
            if left > Y[i] and right > Y[i]:
                new_list[i] = 0
            else:
                if left > Y[i]:
                    new_list[i-1] = 1
                if right > Y[i]:
                    new_list[i+1] = 1

    return X_values, binary_predictions_algorithm, new_list

# =========================
# CIVIC peak detection
# =========================
def detect_peaks(folder, countries, date):
    civic_plot_dir = civic_result_folder
    os.makedirs(civic_plot_dir, exist_ok=True)

    lookaround = 12
    std_dev = 0.88
    normalise = 1
    alpha = 0.05
    beta = 0.2
    peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    loaded_model = _load_tfsm_model()

    peak_results = {}
    for country in country_list:
        if country in countries:
            file = country + '.csv'
            path = os.path.join(folder, file)
            if not os.path.exists(path):
                continue
            civic_data = pd.read_csv(path)

            cols = data.civic[:]  # expected normalized civic series (e.g., arrestNorm, etc.)
            cols.append('date')
            cols = [cols[-1]] + cols[:-1]

            peaks_df = pd.DataFrame(columns=cols)
            peaks_df['date'] = civic_data['date'].tolist()

            for event in data.civic:
                if event in civic_data.columns:
                    Y = civic_data[event]
                    _, _, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector, loaded_model)
                    peaks_df[event] =  peaks_detected

                    # Plot
                    dfp = civic_data.copy()
                    dfp[event + '_peaks'] = peaks_detected
                    dfp['date'] = pd.to_datetime(dfp['date'])

                    fig, ax2 = plt.subplots(figsize=(15, 12))
                    ax2.plot(dfp['date'], dfp[event], label='Normalized Number of Articles', color='green')
                    ax2.scatter(dfp['date'][dfp[event + '_peaks'] == 1],
                                dfp[event][dfp[event + '_peaks'] == 1],
                                color='red', label='Detected Peaks', zorder=5)
                    ax2.set_xlabel('Date')
                    ax2.set_ylabel('Normalized Number of Articles')
                    ax2.legend(loc='upper right')
                    ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
                    ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
                    plt.xticks(rotation=45)
                    plt.title(f'Peak Detection for {country} - {event}')
                    plt.savefig(os.path.join(civic_plot_dir, f'{country}_{event}_peaks.png'))
                    plt.close()

            outfile = os.path.join(civic_result_folder, f'{country}.csv')
            peaks_df.to_csv(outfile, index=False)

    return peak_results

# =========================
# RAI peak detection
# =========================
def detect_rai_peaks_by_influencer(folder, countries, date):
    rai_plot_dir = rai_result_folder
    os.makedirs(rai_plot_dir, exist_ok=True)

    lookaround = 12
    std_dev = 0.88
    normalise = 1
    alpha = 0.05
    beta = 0.2
    peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    loaded_model = _load_tfsm_model()

    peak_results = {}
    _, rai_files = get_updated_files(folder)

    for rai_file in rai_files:
        if '_' not in rai_file or not rai_file.endswith('.csv'):
            continue
        country, influencer = rai_file[:-4].split('_', 1)
        if country not in country_list:
            continue

        path = os.path.join(folder, rai_file)
        if not os.path.exists(path):
            continue
        rai_data = pd.read_csv(path)

        cols = data.rai[:]  # expected normalized RAI series (events/themes) already prepared
        cols.append('date')
        cols = [cols[-1]] + cols[:-1]
        peaks_df = pd.DataFrame(columns=cols)
        peaks_df['date'] = rai_data['date'].tolist()

        for event in data.rai:
            if event in rai_data.columns:
                Y = rai_data[event]
                _, _, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector, loaded_model)
                peaks_df[event] = peaks_detected

                dfp = rai_data.copy()
                dfp[event + '_peaks'] = peaks_detected
                dfp['date'] = pd.to_datetime(dfp['date'])

                fig, ax2 = plt.subplots(figsize=(15, 12))
                ax2.plot(dfp['date'], dfp[event], label='Normalized Number of Articles', color='green')
                ax2.scatter(dfp['date'][dfp[event + '_peaks'] == 1],
                            dfp[event][dfp[event + '_peaks'] == 1],
                            color='red', label='Detected Peaks', zorder=5)
                ax2.set_xlabel('Date')
                ax2.set_ylabel('Normalized Number of Articles')
                ax2.legend(loc='upper right')
                ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
                ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
                plt.xticks(rotation=45)
                plt.title(f'RAI Peak Detection for {country} ({influencer}) - {event}')
                plt.savefig(os.path.join(rai_plot_dir, f'{country}_{influencer}_{event}_peaks.png'))
                plt.close()

        outfile = os.path.join(rai_result_folder, f'{country}_{influencer}.csv')
        peaks_df.to_csv(outfile, index=False)

    return peak_results

# =========================
# MLEED peak detection  <-- NEW
# =========================
def detect_mleed_peaks(folder, countries, date):
    """
    Same logic as civic/rai, but dynamically selects MLEED normalized series
    as all columns that end with 'Norm' in each {Country}.csv under 1-mleed-aggregate.
    """
    mleed_plot_dir = mleed_result_folder
    os.makedirs(mleed_plot_dir, exist_ok=True)

    lookaround = 12
    std_dev = 0.88
    normalise = 1
    alpha = 0.05
    beta = 0.2
    peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)
    loaded_model = _load_tfsm_model()

    _, csv_files = get_updated_files(folder)
    for fname in csv_files:
        # {Country}.csv only
        if '_' in fname or not fname.endswith('.csv'):
            continue
        country = fname[:-4]
        if country not in country_list:
            continue

        path = os.path.join(folder, fname)
        if not os.path.exists(path):
            continue
        df = pd.read_csv(path)

        if 'date' not in df.columns:
            continue

        # Pick MLEED normalized columns in this file
        mleed_cols = [c for c in df.columns if c.endswith('Norm')]
        if not mleed_cols:
            print(f"[MLEED] {country}: no *Norm columns found; skipped.")
            continue

        cols = ['date'] + mleed_cols
        peaks_df = pd.DataFrame(columns=cols)
        peaks_df['date'] = df['date'].tolist()

        for event in mleed_cols:
            Y = df[event].values
            _, _, peaks_detected = convert_to_training_data_2(Y, country, event, peak_detector, loaded_model)
            peaks_df[event] = peaks_detected

            # Plot
            dfp = df.copy()
            dfp[event + '_peaks'] = peaks_detected
            dfp['date'] = pd.to_datetime(dfp['date'])

            fig, ax2 = plt.subplots(figsize=(15, 12))
            ax2.plot(dfp['date'], dfp[event], label='Normalized Number of Articles', color='green')
            ax2.scatter(dfp['date'][dfp[event + '_peaks'] == 1],
                        dfp[event][dfp[event + '_peaks'] == 1],
                        color='red', label='Detected Peaks', zorder=5)
            ax2.set_xlabel('Date')
            ax2.set_ylabel('Normalized Number of Articles')
            ax2.legend(loc='upper right')
            ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=5))
            ax2.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
            plt.xticks(rotation=45)
            plt.title(f'MLEED Peak Detection for {country} - {event}')
            plt.savefig(os.path.join(mleed_plot_dir, f'{country}_{event}_peaks.png'))
            plt.close()

        outfile = os.path.join(mleed_result_folder, f'{country}.csv')
        peaks_df.to_csv(outfile, index=False)

# =========================
# Runners
# =========================
def run_peak_detection(path):
    folder, files = get_updated_files(path)
    if folder is None or len(files) == 0:
        print("No recent civic data files found.")
        return
    countries = [file[:-4] for file in files]
    detect_peaks(folder, countries, f'{year}-{month}-{day}')
    print("Civic peak detection completed.")

def run_rai_peak_detection(path):
    folder, files = get_updated_files(path)
    if folder is None or len(files) == 0:
        print("No recent RAI data files found.")
        return
    detect_rai_peaks_by_influencer(folder, [], f'{year}-{month}-{day}')
    print("RAI peak detection completed.")

def run_mleed_peak_detection(path):  # <-- NEW
    folder, files = get_updated_files(path)
    if folder is None or len(files) == 0:
        print("No recent MLEED data files found.")
        return
    # countries derive from filenames; detection function picks normalized series dynamically
    detect_mleed_peaks(folder, [], f'{year}-{month}-{day}')
    print("MLEED peak detection completed.")

# =========================
# Run peak detection
# =========================
RUN_PEAK_DETECTION = True

if RUN_PEAK_DETECTION:
    run_peak_detection(civic_data_folder)
    run_rai_peak_detection(rai_data_folder)
    run_mleed_peak_detection(mleed_data_folder)  # <-- NEW
else:
    print("⚠️  Skipping peak‑detection step (RUN_PEAK_DETECTION = False)")

# =========================
# Combine outputs
# =========================
def combine_civic_shock_files():
    civic_files = []
    for filename in os.listdir(civic_result_folder):
        if filename.endswith('.csv'):
            file_path = os.path.join(civic_result_folder, filename)
            country = filename[:-4]
            df = pd.read_csv(file_path)
            df['country'] = country
            civic_files.append(df)
    if civic_files:
        combined_civic = pd.concat(civic_files, ignore_index=True)
        cols = ['country', 'date'] + [c for c in combined_civic.columns if c not in ['country', 'date']]
        combined_civic = combined_civic[cols]
        final_counts_folder = os.path.join(SCRIPT_DIR, '../../data/final-counts')
        os.makedirs(final_counts_folder, exist_ok=True)
        output_path = os.path.join(final_counts_folder, 'shock-civic-data.csv')
        combined_civic.to_csv(output_path, index=False)
        print(f"Combined civic shock data saved to: {output_path}")
    else:
        print("No civic shock CSV files found to combine.")

def combine_rai_shock_files():
    rai_files = []
    for filename in os.listdir(rai_result_folder):
        if filename.endswith('.csv') and '_' in filename:
            file_path = os.path.join(rai_result_folder, filename)
            country, influencer = filename[:-4].split('_', 1)
            df = pd.read_csv(file_path)
            df['country'] = country
            df['influencer'] = influencer
            rai_files.append(df)
    if rai_files:
        combined_rai = pd.concat(rai_files, ignore_index=True)
        cols = ['country', 'influencer', 'date'] + [c for c in combined_rai.columns if c not in ['country', 'influencer', 'date']]
        combined_rai = combined_rai[cols]
        final_counts_folder = os.path.join(SCRIPT_DIR, '../../data/final-counts')
        os.makedirs(final_counts_folder, exist_ok=True)
        output_path = os.path.join(final_counts_folder, 'shock-rai-data.csv')
        combined_rai.to_csv(output_path, index=False)
        print(f"Combined RAI shock data saved to: {output_path}")
    else:
        print("No RAI shock CSV files found to combine.")

def combine_mleed_shock_files():  # <-- NEW
    mleed_files = []
    for filename in os.listdir(mleed_result_folder):
        if filename.endswith('.csv') and '_' not in filename:
            file_path = os.path.join(mleed_result_folder, filename)
            country = filename[:-4]
            df = pd.read_csv(file_path)
            df['country'] = country
            mleed_files.append(df)
    if mleed_files:
        combined_mleed = pd.concat(mleed_files, ignore_index=True)
        cols = ['country', 'date'] + [c for c in combined_mleed.columns if c not in ['country', 'date']]
        combined_mleed = combined_mleed[cols]
        final_counts_folder = os.path.join(SCRIPT_DIR, '../../data/final-counts')
        os.makedirs(final_counts_folder, exist_ok=True)
        output_path = os.path.join(final_counts_folder, 'shock-mleed-data.csv')
        combined_mleed.to_csv(output_path, index=False)
        print(f"Combined MLEED shock data saved to: {output_path}")
    else:
        print("No MLEED shock CSV files found to combine.")

print("Combining shock detection files...")
combine_civic_shock_files()
combine_rai_shock_files()
combine_mleed_shock_files()   # <-- NEW
print("File combination completed.")
