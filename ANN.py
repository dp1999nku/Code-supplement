# -*- coding: utf-8 -*-
"""
Created on Sat Sep  7 15:11:45 2019

@author: dp
"""

from tensorflow.keras import Sequential
from tensorflow.keras.layers import Dense,Activation,Dropout
from tensorflow.keras import optimizers
from sklearn.model_selection import train_test_split
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import KFold
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import ShuffleSplit
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import StandardScaler
from tqdm import tqdm,trange

def write_result(t):
    pd.DataFrame(train.history).to_excel(writer,startcol=5*t,sheet_name='loss&mae')

    pd.DataFrame(globals()['r_test_'+colnames[0]]).to_excel(writer,startcol=13*t,sheet_name='cor&score')
    pd.DataFrame(globals()['r_test_'+colnames[1]]).to_excel(writer,startcol=13*t+3,sheet_name='cor&score')
    pd.DataFrame(globals()['r_train_'+colnames[0]]).to_excel(writer,startcol=13*t+6,sheet_name='cor&score')
    pd.DataFrame(globals()['r_train_'+colnames[1]]).to_excel(writer,startcol=13*t+9,sheet_name='cor&score')
    pd.DataFrame(score).to_excel(writer,startcol=3*t,sheet_name='score')

    pred_obv=pd.DataFrame({'y_test_'+colnames[0]:y_test[colnames[0]].reset_index(drop=True),
                       'y_test_'+colnames[1]:y_test[colnames[1]].reset_index(drop=True),
                       'y_pred_'+colnames[0]:y_pred[0],
                       'y_pred_'+colnames[1]:y_pred[1],
                       'y_train_'+colnames[0]:y_train[colnames[0]].reset_index(drop=True),
                       'y_train_'+colnames[1]:y_train[colnames[1]].reset_index(drop=True),
                       'y_train_pred_'+colnames[0]:y_train_pred[0],
                       'y_train_pred_'+colnames[1]:y_train_pred[1]     })

    pred_obv.to_excel(writer,sheet_name='obv&pre',startcol=t*9)
    


def scatter_loss_plot():
    plt.subplot(2,3,1)
    plt.ylim(-1,1)
    plt.xlim(-1,1)
    plt.plot(y_test[colnames[0]],y_test_pred[0],'.')

    
    plt.subplot(2,3,2)
    plt.ylim(-1,1)
    plt.xlim(-1,1)
    plt.plot(y_train[colnames[0]],y_train_pred[0],'.')

    
    plt.subplot(2,3,4)
    #plt.ylim()
    plt.plot(train.history['loss'],'-')    
    plt.subplot(2,3,5)
    #plt.ylim()
    plt.plot(train.history['mean_absolute_error'],'-')    
    plt.subplot(2,3,6)
    plt.plot(train.history['val_loss'],'-')    
    
def Network_train(opt,Setlr,dlcs,sjsl,nepochs):
    global train,score
    Adam=optimizers.Adam(lr=Setlr, beta_1=0.9, beta_2=0.999, epsilon=1e-08, decay=sjsl, amsgrad=True)
    sgd=optimizers.SGD(lr=Setlr, momentum=dlcs, decay=sjsl, nesterov=False)
    Adagrad=optimizers.Adagrad(lr=Setlr, epsilon=1e-06)
    ###bulid network
    model.compile(loss='mean_squared_error', optimizer=opt,metrics=['mae'])
    train=model.fit(x_train,y_train,validation_split=0.1,epochs=nepochs,batch_size=10,verbose=0)
    score=model.evaluate(x_test,y_test)

def Set_network(n_hide,n_input):
    global model
    model=Sequential()
    model.add(Dense(input_dim=n_input,units=n_hide,kernel_initializer='normal',activation='relu'))
    #model.add(PReLU(alpha_initializer='zeros', alpha_regularizer=None, alpha_constraint=None, shared_axes=None))
    #model.add(LeakyReLU(alpha=1))
    model.add(Dropout(0.2))
    #model.add(Dense(input_dim=n_hide,output_dim=n_hide,activation='relu',kernel_initializer='normal'))
    #model.add(Dense(input_dim=n_hide,output_dim=n_hide,kernel_initializer='normal'))
    #model.add(PReLU(alpha_initializer='zeros', alpha_regularizer=None, alpha_constraint=None, shared_axes=None))
    #model.add(Dropout(0.2))
    model.add(Dense(input_dim=n_hide,units=1,kernel_initializer='normal'))
    #model.add(PReLU(alpha_initializer'zeros', alpha_regularizer=None, alpha_constraint=None, shared_axes=None))
    #model.add(LeakyReLU(alpha=1))

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


#################################################################################################3
frame=pd.read_excel('ANN.xlsx',sheet_name=1)

#dummies_method=pd.get_dummies(frame['Method'],columns='Method')#

x_data=frame.iloc[:,:31]
y_data=frame.iloc[:,31:]

x_train,x_test,y_train,y_test=train_test_split(x_data,y_data,test_size=0.2,random_state=1234)
x_train,x_val,y_train,y_val=train_test_split(x_train,y_train,test_size=0.25,random_state=1234)
colnames=y_data.columns.values.tolist()
#xdata[['Carbon','Macromolecular Compound','Com-1','Com-2','Oxide','Salt','Dim','Hollow','Zeta Potential (mV)']]= xdata['Carbon'].apply(pd.to_numeric)#

file1='predict.xlsx'
wb1=xlrd.open_workbook(filename=file1)
ws1=wb1.sheet_by_name('Sheet1')
predictdata=[]
for i in range(ws1.nrows):
    col=[]
    for j in range(ws1.ncols):
        col.append(ws1.cell(i,j).value)
    predictdata.append(col)
predf=pd.DataFrame(predictdata[1:],columns=predictdata[0],dtype='float64')




writer1=pd.ExcelWriter('ann-op.xlsx')
writer2=pd.ExcelWriter('ann-cor.xlsx')
names = list(('Length','DW','RS','APX','H2O2','MDA','SOD','Chla',
                'Chlb','Content','TF','RCF','SCF'))
for i in trange(0,13):
    frame=pd.read_excel('plant_data.xlsx',sheet_name=i)
    n=frame.shape[1]
    x_data=frame.iloc[:,0:(n-1)]
    y_data=frame.iloc[:,(n-1):]
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()

    #kf=KFold(n_splits=10,shuffle=False)
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    stdsc=StandardScaler()
    x_data=pd.DataFrame(stdsc.fit_transform(x_data))
    x_data.columns=x_names
    
    prelist=[]
    corlist_train=[]
    corlist_test=[]
    o=[]
    
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            global x_train,y_train,x_test,y_test
            x_train=x_data.iloc[train_index,:]
            y_train=y_data.iloc[train_index,:]
            
            x_test=x_data.iloc[test_index,:]
            y_test=y_data.iloc[test_index,:]
            
            Set_network(2*(n-1),(n-1))
            Network_train('Adam',0.1,0.9,0.0001,200)
            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            #scatter_loss_plot()
            o.append(y_train[colnames[0]])
            o.append(y_train_pred[0])
            o.append(y_test[colnames[0]])
            o.append(y_test_pred[0])
            pbar.update(1)

    cordf=pd.DataFrame({'tarin':corlist_train,'test':corlist_test})
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