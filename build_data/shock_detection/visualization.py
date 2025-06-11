import re
import numpy as np
from sklearn import tree as tr 
from collections import defaultdict


def tree_info(tree):
    cut_off = tree.threshold[0]
    feat = tree.feature[0]
    left = np.argmax(tree.value[1])*2-1
    right = np.argmax(tree.value[2])*2-1
    return [feat,float(cut_off),int(left),int(right)]

def model_info(model,X):
    feat_labels = X.columns.values
    info = []
    for idx,sub_tree in enumerate(model.estimators_):
        sub_info = tree_info(sub_tree.tree_)
        weight = model.estimator_weights_[idx]
        sub_info[0] = feat_labels[sub_info[0]]
        sub_info.append(float(weight))
        info.append(sub_info)
    return info


def insert_point(points,to_add):
    i = 0
    already_in = False
    while points[i][0] <= to_add[0]:
        if points[i][0] == to_add[0]:
            already_in = True
        points[i][1] += to_add[1] * to_add[3]
        i += 1
        
    if not already_in:
        points.insert(i,[to_add[0]+1e-6,points[i][1]])
        points.insert(i,[to_add[0],points[i-1][1]])
        i += 1
    for j in range(i,len(points)):
        points[j][1] += to_add[2] * to_add[3]
    


def generate_curve(model,features):
    curves = dict()
    used_feat = set(i[0] for i in model)
    for feat in used_feat:
        min_val = min([i[1] for i in model if i[0] == feat]+[features[feat].min()])
        max_val = max([i[1] for i in model if i[0] == feat]+[features[feat].max()])
        curves[feat] = [[min_val,0],[max_val,0]]
       
        for stump in model:
            if stump[0] == feat:
                insert_point(curves[feat],stump[1:])
              
    return curves


def generate_curve_check(model,features):
    curves = dict()
    used_feat = set(i[0] for i in model)
    for feat in used_feat:
        min_val = min([i[1] for i in model if i[0] == feat]+[features[feat].min()])
        max_val = max([i[1] for i in model if i[0] == feat]+[features[feat].max()])
        step = (max_val-min_val)/1000
        curves[feat] = [[min_val+i*step,0] for i in range(1001)]
        for stump in model:
            if stump[0] == feat:
                for point in curves[feat]:
                    if point[0] <= stump[1]:
                        point[1] += stump[2]*stump[4]
                    else:
                        point[1] += stump[3]*stump[4]
    return curves

def inverse_query(model,x,y=None):
    feat_labels = x.columns.values
    result = defaultdict(float)
    cutoff = defaultdict(lambda : [1e-6,1e6])
    for idx,sub_tree in enumerate(model.estimators_):
        pred = sub_tree.predict(x)[0]*2-1
        tree_tuple = tree_info(sub_tree.tree_)
        tree_tuple[0] = feat_labels[tree_tuple[0]]

        result[tree_tuple[0]] += pred*model.estimator_weights_[idx]


        if pred > 0:
            if x[tree_tuple[0]].iloc[0] > tree_tuple[1]:
                cutoff[tree_tuple[0]][0] = max(cutoff[tree_tuple[0]][0],tree_tuple[1])
            else:
                cutoff[tree_tuple[0]][1] = min(cutoff[tree_tuple[0]][1],tree_tuple[1])

    pred_label = int(np.sum([i for i in result.values()])>0)
    #assert pred_label == model.predict(x)[0], f'pred_val: {np.sum([i for i in result.values()])} model: {model.predict(x)} {result}'

    if pred_label == 1:
        print(f'Prediction: {pred_label*2-1}')
        if y is not None:
            print(f'True label: {y}')
        for feature,value in sorted(result.items(),key=lambda x:x[1],reverse=True):
            
            if value <= 0:
                continue
            else:
                print(f'{feature} = {x[feature].iloc[0]} is in range ({cutoff[feature][0]},{cutoff[feature][1]}). Weight: {value}')
    return result,cutoff