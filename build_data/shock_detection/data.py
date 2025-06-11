import pandas as pd
import numpy as np

counts = ['arrest',
'censor',
'changeelection',
'changepower',
'cooperate',
'coup',
'defamationcase',
'disaster',
'legalaction',
'legalchange',
'martiallaw',
'mobilizesecurity',
'praise',
'protest',
'purge',
'raid',
'threaten',
'violencelethal',
'violencenonlethal'
]

targets = ['arrest',
 'activism',
'censor',
 'corruption',
# 'changeelection',
# 'changepower',
# 'cooperate',
# 'coup',
'defamationcase',
'disaster',
'legalaction',
'legalchange',
# 'martiallaw',
'mobilizesecurity',
# 'praise',
'protest',
'purge',
'raid',
#'threaten',
'violencelethal',
'violencenonlethal',
#'f1'
]

targetsForecast = ['arrest',
 'activism',
'censor',
 'corruption',
# 'changeelection',
# 'changepower',
# 'cooperate',
# 'coup',
'defamationcase',
# 'disaster',
'legalaction',
'legalchange',
# 'martiallaw',
'mobilizesecurity',
# 'praise',
'protest',
'purge',
'raid',
#'threaten',
'violencelethal',
'violencenonlethal',
#'f1'
]

targetsNew = ['arrest',
              'censor',
              'electionactivity',
              'electionirregularities',
              'cooperate',
              'coup',
              'defamationcase',
              'disaster',
              'legalaction',
              'legalchange',
              'martiallaw',
              'mobilizesecurity',
              'corruption',
              'activism',
              'protest',
              'purge',
              'raid',
              'threaten',
              'violencelethal',
              'violencenonlethal'
            ]

targetsRAI = [
    'arms_transfer_security_aid_assistanceNorm',
    'bribery_economic_corruptionNorm',
    'diaspora_activationNorm',
    'diplomatic_mediationNorm',
    'diplomatic_recognitionNorm',
    'diplomatic_sanctionNorm',
    'diplomatic_statementNorm',
    'diplomatic_visitNorm',
    "economic_aid_assistanceNorm", 
    "intelligence_counterintelligenceNorm",
    "investmentNorm", 
    "joint_security_force_exerciseNorm", 
    "media_campaign_interventionNorm",
    "political_process_policy_interventionNorm", 
    "professional_cultural_exchangeNorm",
    "security_cooperationNorm", 
    "security_force_facility_presenceNorm",
    "surveillanceNorm", 
    "tech_transfer_investmentNorm", 
    "trade_agreement_exchangeNorm",
    "trade_financial_sanctionNorm", 
    "transnational_organization_crimeNorm",
    'backlash_sumNorm',
    'diplomacy_sumNorm',
    'domestic_interference_sumNorm',
    'economic_power_sumNorm',
    'hard_power_sumNorm',
    'soft_power_sumNorm'
]

to_normal = ['balance.of.trade',
            'consumer.price.index.cpi',
            'cpi.transportation',
            'exports',
            'fiscal.expenditure',
            'government.revenues',
            'imports']

def prep_data(data,target='f1',encode_date=False,lag=1):
    X = data.drop(['date','country'],axis=1)
    
    for i in data.columns.values:
        if i in counts:
            X[i+'_count'] = X[i]
            X[i] = X[i+'Norm']
            X = X.drop([i+'Norm'],axis=1)


        # if 'Norm' in i:
        #     X[i[:-4]] = X[i]
        #     X = X.drop([i],axis=1)
        #     print(i)

    #returning target's normalized counts        
    Y = X[target]
    
    

#     for i in X.columns.values:
#         if i in to_normal:
#             X[i] = (X[i]-X[i].mean())/X[i].std()
#         elif i not in ['cs_999','article_total','rai_999']+targets and np.abs(X[i]).mean() > 100:
#             X[i] = (X[i]-X[i].mean())/X[i].std()
     
    #corr_matrix = X.corr().abs()

    #upper = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(np.bool))
    #cols = upper.columns
    #to_drop = []
    #x = upper.loc[target]
                
    #mean_corr = x.mean()
    #sigma_corr = x.std()
    #limit = mean_corr + 2*sigma_corr
                
    #for col in cols:
    #    if col != target:
    #        if x[col] > limit :
    #            to_drop.append(col)
    #X = X.drop(X[to_drop], axis=1)
      
    # What is the point of this??           
    if encode_date:
        #taking the month
        month = pd.to_numeric(data['date'].map(lambda x:x.split('-')[1]))
        #takes each month value x and adds the lag to it
        month = month.map(lambda x:(x+lag)%12 if x != 12-lag else 12)  
       
        month = np.ceil(month/3).astype(int)
        #X's quarter
        for i in range(1,5):
            X[f'q{i}'] = (month == i).astype(int)
        
        x = [0 for i in range(lag)]
        
        for i in range(lag,len(X)):
            if X[target][i] > X[target][i-lag]:
                x.append(1)
            else:
                x.append(0)
        X['surge'] = x
    return X,Y
