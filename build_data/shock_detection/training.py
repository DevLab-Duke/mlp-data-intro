
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.model_selection import TimeSeriesSplit
from sklearn import tree,ensemble, neighbors
from sklearn import linear_model, svm
from sklearn import metrics,model_selection
from sklearn import decomposition
from sklearn.ensemble import AdaBoostClassifier
from xgboost import XGBClassifier
from tensorflow.keras.models import load_model
from scipy.stats import boxcox, zscore
from scipy.stats.mstats import winsorize
import math
import matplotlib
matplotlib.use('Agg')

# class PeakDetector:
#     def __init__(self, lookaround, std_dev, normalize, alpha, beta):
#         self.lookaround = lookaround
#         self.std_dev = std_dev
#         self.normalise = normalize
#         self.alpha = alpha
#         self.beta = beta
        
#     @staticmethod
#     def equalize_lists(lst1, lst2):
#         len1, len2 = len(lst1), len(lst2)
#         difference = abs(len1 - len2)
#         if len1 < len2:
#             lst1 = np.concatenate((lst1, [0] * difference))
#         elif len2 < len1:
#             lst2 = np.concatenate((lst2, [0] * difference))
#         return lst1, lst2

#     @staticmethod
#     def apply_transformation(values, method, limits=0.05):
#         if method == 'log':
#             return [np.log(v) if v > 0 else 0 for v in values]
#         elif method == 'boxcox':
#             return boxcox(values)[0]
#         elif method == 'sqrt':
#             return [np.sqrt(v) if v >= 0 else -np.sqrt(-v) for v in values]
#         elif method == 'winsorize':
#             return winsorize(values, limits=limits)
#         return values

#     def peakiness(self, left, post_value, right, pre_value):
#         w_left = [math.exp(-j * self.beta) for j in range(1, 13)]
#         w_right = [1 - self.alpha * j for j in range(1, 13)]
#         w_left = [w / sum(w_left) for w in w_left]
#         w_right = [w / sum(w_right) for w in w_right]

#         left, weights_left = self.equalize_lists(left, w_left)
#         right, weights_right = self.equalize_lists(right, w_right)

#         left_mean = np.dot(weights_left, left)
#         right_mean = np.dot(weights_right, right)
#         left_std = math.sqrt(np.dot(weights_left, (left - left_mean) ** 2))
#         right_std = math.sqrt(np.dot(weights_right, (right - right_mean) ** 2))

#         left_score = (pre_value - left_mean) / left_std if left_std != 0 else 0
#         right_score = (pre_value - right_mean) / right_std if right_std != 0 else 0

#         return left_score, right_score

#     def peakiness_conservative(self, left, post_value, right, pre_value):
#         # right, weights = self.equalize_lists(right, weights)

#         w_left = [round(math.exp(-j * self.beta),2) for j in range(1,13)]
#         w_right = [round(1-self.alpha*j,2) for j in range(1,13)]
#         w_left_sum = sum(w_left)
#         w_left = [weight/w_left_sum for weight in w_left]
#         w_right_sum = sum(w_right)
#         w_right = [weight/w_right_sum for weight in w_right]
#         left, weights_left = self.equalize_lists(left, w_left)
#         right, weights_right = self.equalize_lists(right, w_right)
#         reverse_weights = list(reversed(weights_left))

#         result_left = [(a * ((post_value - b) ** 2)) for a, b in zip(reverse_weights, left)]
#         left_weighted_std_squared = np.sum(result_left) if  len(left) > 0 else 0
#         left_weighted_std = left_weighted_std_squared ** 0.5

#         result_right = [(a * ((post_value - b) ** 2)) for a, b in zip(weights_right, right)]
#         right_weighted_std_squared = np.sum(result_right) if len(right)>0 else 0
#         right_weighted_std = right_weighted_std_squared ** 0.5

#         left_mean_list = [a * b for a, b in zip(reverse_weights, left)]
#         left_mean = np.sum(left_mean_list) if  len(left) > 0 else 0

#         right_mean_list = [a * b for a, b in zip(weights_right, right)]
#         right_mean = np.sum(right_mean_list) if  len(right) > 0 else 0

#         left_total = (left_weighted_std * self.std_dev) + left_mean
#         right_total = (right_weighted_std * self.std_dev) + right_mean

#         average_total = (left_total + right_total) / 2

#         return pre_value > max(left_total, right_total)

#     def peak_detection(self, series):
#         pre_values = series.tolist()
#         z_scores = zscore(pre_values)
#         num_outliers = np.sum(np.abs(z_scores) > 2)

#         winsorizing_proportion = min(math.ceil(num_outliers / len(pre_values)), 0.1)
#         method = ['none', 'winsorize', 'sqrt'][self.normalise]
#         values = self.apply_transformation(series, method, winsorizing_proportion)

#         peak_scores = [self.peakiness(values[max(0, i - self.lookaround): i],
#                                       values[i],
#                                       values[i + 1: i + self.lookaround + 1],
#                                       pre_values[i]) for i in range(len(values))]
#         return peak_scores


#     def peak_detection_conservative(self, series):
#         pre_values = series.tolist()
#         z_scores = zscore(pre_values)
#         num_outliers = np.sum(np.abs(z_scores) > 2)

#         winsorizing_proportion = min(math.ceil(num_outliers / len(pre_values)), 0.1)
#         method = ['none', 'winsorize', 'sqrt'][self.normalise]
#         values = self.apply_transformation(series, method, winsorizing_proportion)

#         peak_scores = [self.peakiness_conservative(values[max(0, i - self.lookaround): i],
#                                       values[i],
#                                       values[i + 1: i + self.lookaround + 1],
#                                       pre_values[i]) for i in range(len(values))]
#         return peak_scores
    
    
#     def refine_peaks(Y, peaks):
#         # Ensure top 3 spikes are always included
#         top_three_peaks = np.argsort(-np.abs(Y[peaks]))[:3]
#         peaks = np.union1d(peaks, top_three_peaks)

#         # Label adjacent points
#         new_peaks = []
#         for peak in peaks:
#             new_peaks.append(peak)
#             if peak > 0 and Y[peak - 1] > Y[peak]:
#                 new_peaks.append(peak - 1)
#             if peak < len(Y) - 1 and Y[peak + 1] > Y[peak]:
#                 new_peaks.append(peak + 1)

#         # Remove duplicates
#         new_peaks = np.unique(new_peaks)

#         # Remove peaks if both neighbors are larger
#         final_peaks = []
#         for peak in new_peaks:
#             if (peak > 0 and Y[peak - 1] > Y[peak]) and (peak < len(Y) - 1 and Y[peak + 1] > Y[peak]):
#                 continue
#             final_peaks.append(peak)

#         return np.array(final_peaks)


def convert_to_training_data(X, Y, new_list, lag, cutoff, country, event, mode='cutoff'):
    # lookaround = 12
    # std_dev = 0.88
    # normalise = 1
    # alpha = 0.05
    # beta = 0.2
    # peak_detector = PeakDetector(lookaround, std_dev, normalise, alpha, beta)

    if mode == 'cutoff':
        mu = Y_diff.mean()
        sigma = Y_diff.std()

        Y_diff = (((Y_diff-mu)/sigma) > cutoff).astype(int)
        #print(f'Pos rate: {Y_diff.mean()}')

        #Y_diff1 = (((Y_diff-mu)/sigma) > 0.2)
        #Y_diff2 = (Y_diff > 0.5*sigma)
        #Y_diff = (Y_diff1 & Y_diff2).astype(int)

        if Y_diff.mean() < 0.2:
            return None,None

    #Leaving this sigma bit because it was present in the last code
    Y_sigma = np.array([Y[i] - Y[i-lag] for i in range(lag,len(Y))])
    sigma = Y_sigma.std()


    # script_dir = os.path.dirname(__file__)
    # model_path = os.path.join(script_dir, 'content', 'model_version1')

    # Load the model
    # loaded_model = load_model(model_path)

    # X_values = peak_detector.peak_detection(Y)
    # X_values = np.nan_to_num(np.array(X_values))  # Replace NaN in input data
    # X_values = X_values.reshape(-1, 2)  # Reshape to have 2 values per data point


    # Predict using the NN model
    # predictions = loaded_model.predict(X_values)
    # binary_predictions_NN = (predictions > 0.5).astype(int)
    # binary_predictions_NN  = [item for sublist in binary_predictions_NN for item in sublist]
    
    # binary_predictions_algorithm = peak_detector.peak_detection_conservative(Y)
    # binary_predictions_algorithm = [int(value) for value in binary_predictions_algorithm]
    
    X_drop = X.drop([len(X)-(i+1) for i in range(lag)])
    
    graphs_dir = 'graphs'
    if not os.path.exists(graphs_dir):
        os.makedirs(graphs_dir)
    
    
    #combine the threshold model peaks with the NN model peaks
    # new_list = [1 if x or y else 0 for x, y in zip(binary_predictions_NN, binary_predictions_algorithm)]
    # top_3_indices = sorted(range(len(Y)), key=lambda i: abs(Y[i]), reverse=True)[:3]
    # for idx in top_3_indices:
    #     new_list[idx] = 1
         

    # Step 2 & 3: Adjust peaks based on adjacent values
    # for i in range(1, len(Y) - 1):
    #     if new_list[i] == 1:
    #         left = Y[i-1] if i > 0 else float('-inf')
    #         right = Y[i+1] if i < len(Y) - 1 else float('-inf')

    #         if left > Y[i] and right > Y[i]:
    #             # Both neighbors are larger, remove peak
    #             new_list[i] = 0
    #         else:
    #             # Label larger neighbors as peaks
    #             if left > Y[i]:
    #                 new_list[i-1] = 1
    #             if right > Y[i]:
    #                 new_list[i+1] = 1
    
    
    
#     plt.figure(figsize=(10, 6))
#     plt.plot(Y, label='Data')
#     plt.scatter(np.where(np.array(new_list) == 1), np.array(Y)[np.array(new_list) == 1], color='red', label='Peaks')  # Highlight Peaks
#     plt.title(f'Peak Detection in {country} for {event}')
#     plt.xlabel('Time')
#     plt.ylabel('Values')
#     plt.legend()
    
#     country_dir = os.path.join('graphs', country)
#     event_dir = os.path.join(country_dir, event)

#     # Check if the country directory exists, if not create it
#     if not os.path.exists(country_dir):
#         os.makedirs(country_dir)

#     # Check if the event directory exists, if not create it
#     if not os.path.exists(event_dir):
#         os.makedirs(event_dir)

#     # Modify file paths to include the country and event directories
#     graph_file_name = os.path.join(event_dir, f"{country}_{event}_peak_graph.jpg")
#     csv_file_name = os.path.join(event_dir, f"{country}_{event}_peak_graph.csv")

#     # Save the graph
#     plt.savefig(graph_file_name)
#     plt.close()
#     df = pd.DataFrame({
#     'value': Y,
#     'peaks': new_list
#     })
    

#     # Save the DataFrame as a CSV file
#     df.to_csv(csv_file_name, index=False)

    
  
   
    new_list = new_list[lag:]
    
    print(country," ",event)
    

    return X_drop,np.array(new_list),sigma

def train(X, Y, new_list, cutoff, lag, country, event, mode='cutoff', cv=2, convert_data=True, f1_beta=1):

    if convert_data:
        X,Y_diff,sigma = convert_to_training_data(X,Y, new_list, lag,cutoff,country,event, mode=mode)
        
    else:
        Y_diff = Y
        sigma = 0
    if X is None:
        return -1,-1,-1,-1,-1
    
    #X_train, X_test, y_train, y_test = TimeSeriesSplit(X, Y_diff, test_size=0.30, random_state=42)
    
    # def f1_beta(y_true,y_pred):
    #     return metrics.fbeta_score(y_true,y_pred,beta=f1_beta)
     
  
    scoring = {'f1':metrics.make_scorer(metrics.fbeta_score,beta=f1_beta),'precision':'precision','recall':'recall'}
    
    clf = AdaBoostClassifier(algorithm='SAMME')
   
    tscv = TimeSeriesSplit(n_splits=2)
    
    params = {"n_estimators":[i*10 for i in range(1,11)],"learning_rate":[i*0.1 for i in range(1,21)]}

    grid_search = model_selection.GridSearchCV(clf, params,scoring=scoring,n_jobs=-1,cv=cv,
                                               verbose=1,return_train_score=True,refit='f1')
    grid_search.fit(X,Y_diff)

    print(f'CV f1: {grid_search.best_score_} {grid_search.best_params_}')
    print(f'CV precision: {grid_search.cv_results_["mean_test_precision"][grid_search.best_index_]}')
    print(f'CV recall: {grid_search.cv_results_["mean_test_recall"][grid_search.best_index_]}')
    
    #predictions = grid_search.best_estimator_.predict(X_test)
    
    #print(f'Accuracy : {metrics.accuracy_score(y_test,predictions)}')
    
     
    
    return grid_search.best_score_, grid_search.cv_results_["mean_test_precision"][grid_search.best_index_],\
grid_search.cv_results_["mean_test_recall"][grid_search.best_index_],Y_diff.sum(), grid_search,sigma,Y_diff
