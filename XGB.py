# -*- coding: utf-8 -*-
"""
Created on Mon Mar 21 21:06:01 2022

@author: admin
"""

from sklearn.preprocessing import StandardScaler
import numpy as np
import pandas as pd
import os
import xgboost as xgb
from sklearn.feature_selection import RFE
from xgboost import XGBRegressor as XGBR
from sklearn.model_selection import KFold, cross_val_score, train_test_split
from sklearn.metrics import mean_squared_error
from tqdm import tqdm,trange
import random
import matplotlib.pyplot as plt
from sklearn.model_selection import ShuffleSplit
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import StandardScaler
from tqdm import tqdm,trange
import random


def scatter_loss_plot():
    plt.subplot(1,2,1)
    plt.ylim(-1,1)
    plt.xlim(-1,1)
    plt.plot(y_test[colnames[0]],y_test_pred[0],'.')

    
    plt.subplot(1,2,2)
    plt.ylim(-1,1)
    plt.xlim(-1,1)
    plt.plot(y_train[colnames[0]],y_train_pred[0],'.')

    
def rmse(obs,pre):
    return np.sqrt(mean_squared_error(obs, pre))
    
def caculate_cor():
    global r_test,r_train,y_test_pred,y_train_pred,rmse_test,rmse_train
    y_test_pred=pd.DataFrame(model.predict(x_test).reshape(y_test.shape),index=test_index)
    r_test=np.corrcoef(y_test_pred[0],y_test[colnames[0]])
    y_train_pred=pd.DataFrame(model.predict(x_train).reshape(y_train.shape),index=train_index)
    r_train=np.corrcoef(y_train_pred[0],y_train[colnames[0]])
    rmse_test=rmse(y_test[colnames[0]],y_test_pred[0])
    rmse_train=rmse(y_train[colnames[0]],y_train_pred[0])

xl = pd.ExcelFile('plant_data.xlsx')
namelist=xl.sheet_names[0:13]

names = list(('Length','DW','RS','APX','H2O2','MDA','SOD','Chla',
                'Chlb','Content','TF','RCF','SCF'))
writer1=pd.ExcelWriter('all-xgb-op.xlsx')
writer2=pd.ExcelWriter('all-xgb-cor.xlsx')
for i in trange(0,13):
    frame=pd.read_excel('plant_data.xlsx',sheet_name=i)
    random.seed(12345)
    n=frame.shape[1]

    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    valdata=frame.iloc[val,:]
    val_x=valdata.iloc[:,0:(n-1)]
    val_y=valdata.iloc[:,(n-1):]
    frame=frame.iloc[model_index,:]
    stdsc=StandardScaler()
    x_data=frame.iloc[:,0:(n-1)]
    x_data=pd.DataFrame(stdsc.fit_transform(x_data))
    y_data=frame.iloc[:,(n-1):]
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    #stdsc=StandardScaler()
    prelist=[]
    vallist=[]
    corlist_train=[]
    corlist_test=[]
    rmsel_train=[]
    rmsel_test=[]
    o=[]
    model = XGBR(n_estimators=100,random_state=0)
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]  
            
            x_test=x_data.iloc[test_index,:]
            x_test.columns=x_names        
            y_test=y_data.iloc[test_index,:]            
            model.fit(x_train,np.array(y_train).ravel())

            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)
            #scatter_loss_plot()
            o.append(y_train[colnames[0]])
            o.append(y_train_pred[0])
            o.append(y_test[colnames[0]])
            o.append(y_test_pred[0])
            pbar.update()           

    plt.show()        
    cordf=pd.DataFrame({'train':corlist_train,'test':corlist_test,
                        'rmse_train':rmsel_train,'rmse_test':rmsel_test})
    obs_pre_df=pd.DataFrame([y_data[colnames[0]],o[1],o[5],o[9],o[13],o[17],o[21],o[25],o[29],o[33],o[37],
                            o[3],o[7],o[11],o[15],o[19],o[23],o[27],o[31],o[35],o[39]]).T
    obs_pre_df.columns=(colnames[0],'train1','train2','train3','train4','train5',
                        'train6','train7','train8','train9','train10',
                        'test1','test2','test3','test4','test5',
                        'test6','test7','test8','test9','test10')

    obs_pre_df.to_excel(writer1,sheet_name=names[i])
    cordf.to_excel(writer2,sheet_name=names[i])
       
writer1.save()
writer2.save()
