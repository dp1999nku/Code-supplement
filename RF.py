# -*- coding: utf-8 -*-
"""
Created on Tue Jul  7 20:41:03 2020

@author: dp
"""

from sklearn.ensemble import RandomForestRegressor
import pandas as pd
import numpy as np
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
    

    
    
xl=pd.ExcelFile('plant_data.xlsx')
namelist=xl.sheet_names
    
"""
RFECV
"""
"""
from sklearn.feature_selection import RFECV
from sklearn.model_selection import KFold

rf=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,max_features=5)
rfecv = RFECV(estimator=rf, cv=KFold(n_splits=2), scoring='r2',
              min_features_to_select=14,n_jobs=-1)
    
for i in trange(1,13):
    frame=pd.read_excel('RF20210202.xlsx',sheet_name=i) 
    x_names=frame.iloc[:,0:31].columns.values.tolist()
    x_data=frame.iloc[:,0:31].values
    y_data=frame.iloc[:,31].values
    rfecv.fit(x_data,y_data)
    """
    print(rfecv.n_features_)
    print(rfecv.ranking_)
    print(rfecv.support_)
    print(rfecv.estimator_)
    print(rfecv.grid_scores_)
    """
    plt.figure()
    plt.xlabel("Number of features selected")
    plt.ylabel("Cross validation score")
    plt.plot(range(14, len(rfecv.grid_scores_) + 14), rfecv.grid_scores_)
    plt.show()
    
    globals()['colindex'+str(i)]=[]
    for j in range(29):
        if rfecv.support_[j]==True:
            globals()['colindex'+str(i)].append(x_names[j])
"""    

            
"""
SBS
"""
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split
from sklearn.base import clone
from itertools import combinations

class SBS():
    def __init__(self, estimator, k_features, scoring,
                 test_size=0.25, random_state=1):
        self.scoring = scoring
        self.estimator = clone(estimator)
        self.k_features = k_features
        self.test_size = test_size
        self.random_state = random_state

    def fit(self, X, y):
        
        X_train, X_test, y_train, y_test =             train_test_split(X, y, test_size=self.test_size,
                             random_state=self.random_state)

        dim = X_train.shape[1]
        self.indices_ = tuple(range(dim))
        self.subsets_ = [self.indices_]
        score = self._calc_score(X_train, y_train, 
                                 X_test, y_test, self.indices_)
        self.scores_ = [score]

        while dim > self.k_features:
            scores = []
            subsets = []

            for p in combinations(self.indices_, r=dim - 1):
                score = self._calc_score(X_train, y_train, 
                                         X_test, y_test, p)
                scores.append(score)
                subsets.append(p)

            best = np.argmax(scores)
            self.indices_ = subsets[best]
            self.subsets_.append(self.indices_)
            dim -= 1

            self.scores_.append(scores[best])
        self.k_score_ = self.scores_[-1]

        return self

    def transform(self, X):
        return X[:, self.indices_]

    def _calc_score(self, X_train, y_train, X_test, y_test, indices):
        self.estimator.fit(X_train[:, indices], y_train)
        y_pred = self.estimator.predict(X_test[:, indices])
        score = self.scoring(y_test, y_pred)
        return score
    

rf=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,max_features=5)
sbs=SBS(rf,k_features=15,scoring=r2_score)
score_list=[]
for i in trange(0,13):
    frame=pd.read_excel('plant_data.xlsx',sheet_name=i)
    x_names=frame.iloc[:,0:(n-1)].columns.values.tolist()
    x_data=frame.iloc[:,0:(n-1)].values
    y_data=frame.iloc[:,(n-1)].values
    sbs.fit(x_data,y_data)
    
    k_feat = [len(k) for k in sbs.subsets_]
    score_list.append(sbs.scores_)
    
    plt.plot(k_feat, sbs.scores_, marker='o')
    plt.ylim([0, 1])
    plt.ylabel('r2')
    plt.xlabel('Number of features')
    plt.grid()
    plt.tight_layout()
    # plt.savefig('images/04_08.png', dpi=300)
    plt.show()
    
    k=sbs.scores_.index(max(sbs.scores_))
    globals()['k'+str(i)]=list(sbs.subsets_[k])

    globals()['colindex'+str(i)]=[]
    for j in globals()['k'+str(i)]:
        globals()['colindex'+str(i)].append(x_names[j])
    #feature_remian=frame.columns[1:][globals()['k'+str(29-k)]]
    #print(feature_remian)
    
score_df=pd.DataFrame(score_list)
score_df=score_df.stack().unstack(0)
xl=pd.ExcelFile('RF20210202.xlsx')

score_df.to_excel('sbs-score.xlsx',sheet_name='sheet1')


"""
rf-regression
"""
xl = pd.ExcelFile('plant_data.xlsx')
namelist=xl.sheet_names[0:13]
model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=20#,min_samples_leaf=2
                                )
#predf=pd.read_excel('experiment materials.xlsx',sheet_name=1).iloc[0:5,0:31]

writer1=pd.ExcelWriter('all-rf-op.xlsx')
writer2=pd.ExcelWriter('all-rf-cor.xlsx')
writer3=pd.ExcelWriter('all-rf-imp.xlsx')
writer4=pd.ExcelWriter('all-pre.xlsx')
writer5=pd.ExcelWriter('all-val.xlsx')
#writer6=pd.ExcelWriter('ss-index-all.xlsx')
names = list(('Length','DW','RS','APX','H2O2','MDA','SOD','Chla',
                'Chlb','Content','TF','RCF','SCF'))

for i in trange(0,13):
    frame=pd.read_excel('plant_data.xlsx',sheet_name=i)
    random.seed(12345)
    
    n=frame.shape[1]
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    valdata=frame.loc[val,:]
    val_x=valdata.iloc[:,0:(n-1)]
    val_y=valdata.iloc[:,(n-1):]
    frame=frame.loc[model_index,:]
    x_data=frame.iloc[:,0:(n-1)]
    #x_data=frame[globals()['colindex'+str(i)]]
    x_data.index=range(len(x_data))
    y_data=frame.iloc[:,(n-1):]
    y_data.index=range(len(y_data))
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
    imp=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=20#,min_samples_leaf=2
                                )
    #with tqdm(total=10) as pbar:
    for train_index , test_index in ss.split(x_data,y_data):
        x_train=x_data.iloc[train_index,:]
        x_train.columns=x_names
        
        y_train=y_data.iloc[train_index,:]
        
        x_test=x_data.iloc[test_index,:]
        x_test.columns=x_names
    
        y_test=y_data.iloc[test_index,:]
        model.fit(x_train,np.array(y_train).ravel())
        val_one=model.predict(val_x)
        vallist.append(val_one.T)
        #pre_one=model.predict(predf)
        #prelist.append(pre_one.T)
        caculate_cor()
        corlist_train.append(r_train[1,0])
        corlist_test.append(r_test[1,0])
        rmsel_train.append(rmse_train)
        rmsel_test.append(rmse_test)
       # scatter_loss_plot()
        o.append(y_train[colnames[0]])
        o.append(y_train_pred[0])
        o.append(y_test[colnames[0]])
        o.append(y_test_pred[0])
        #pbar.update()           
        imp.append(model.feature_importances_)

    #plt.show()        
    cordf=pd.DataFrame({'train':corlist_train,'test':corlist_test,
                        'rmse_train':rmsel_train,'rmse_test':rmsel_test})
    obs_pre_df=pd.DataFrame([y_data[colnames[0]],o[1],o[5],o[9],o[13],o[17],o[21],o[25],o[29],o[33],o[37],
                            o[3],o[7],o[11],o[15],o[19],o[23],o[27],o[31],o[35],o[39]]).T
    obs_pre_df.columns=(colnames[0],'train1','train2','train3','train4','train5',
                        'train6','train7','train8','train9','train10',
                        'test1','test2','test3','test4','test5',
                        'test6','test7','test8','test9','test10')
    presult=pd.DataFrame(prelist,columns=['T','C','S','M','L']).T
    vresult=pd.DataFrame(vallist,columns=val).T
    
    print(np.corrcoef(np.array(np.mean(vresult.T)).ravel(),
                          np.array(val_y).ravel())[0,1])
    vresult['predict']=np.array(np.mean(vresult.T)).ravel()
    vresult['observe']=val_y
    vresult['error']=vresult['predict']-vresult['observe']

    #imp_df=pd.DataFrame(imp,columns=globals()['colindex'+str(i)])
    imp_df=pd.DataFrame(imp,columns=x_names)
    obs_pre_df.to_excel(writer1,sheet_name=names[i])
    cordf.to_excel(writer2,sheet_name=names[i])
    imp_df.to_excel(writer3,sheet_name=names[i])
    presult.to_excel(writer4,sheet_name=names[i])
    vresult.to_excel(writer5,sheet_name=names[i])
    
writer1.save()
writer2.save()
writer3.save()
writer4.save()
writer5.save()



"""
ss-index output
"""
writer6=pd.ExcelWriter('ss-index-all.xlsx')
l_train_list=[]
l_test_list=[]
for i in trange(1,13):
    frame=pd.read_excel('RF20210202.xlsx',sheet_name=i)
    corresult=pd.read_excel('all-rf-cor.xlsx',sheet_name=i-1)
    max_index=np.argmax(corresult['train'],axis=0)   
    
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    frame=frame.iloc[model_index,:]
    x_data=frame.iloc[:,0:31]
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    index_list=[]
    for train_index in ss.split(x_data):
        index_list.append(train_index)
    index_train_list=index_list[max_index][0]
    index_df=pd.DataFrame(index_train_list)
    index_df.to_excel(writer6,sheet_name=namelist[i-1])

    l_train=len(index_list[0][0]) 
    l_test=len(index_list[0][1])
    l_train_list.append(l_train)
    l_test_list.append(l_test) 
writer6.save()

len_df=pd.DataFrame({'train':l_train_list,'test':l_test_list})
len_df.to_excel('ss_len.xlsx')
'''

"""
ss-length output
"""

l_train_list=[]
l_test_list=[]
for i in range(1,13):
    frame=pd.read_excel('RF20210202.xlsx',sheet_name=i)
    #corresult=pd.read_excel('sbs-rf-cor.xlsx',sheet_name=i-1)
    #max_index=np.argmax(corresult['test'],axis=0)
    
    x_data=frame[globals()['colindex'+str(i)]]
    y_data=frame.iloc[:,31:]
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()

    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)

    index_list=[]
    for train_index in ss.split(x_data):
        index_list.append(train_index)
    l_train=len(index_list[0][0]) 
    l_test=len(index_list[0][1])
    l_train_list.append(l_train)
    l_test_list.append(l_test)

    #index_train_list=index_list[max_index][0]
    
    #index_df=pd.DataFrame(index_train_list)
    #index_df=index_df.stack()
    #index_df=index_df.unstack(0)

    #index_df.to_excel(writer3,sheet_name=colnames[0])

"""
colindex output
"""
writer5=pd.ExcelWriter('colindex.xlsx')
for i in range(1,13):
    colindex=pd.DataFrame(globals()['colindex'+str(i)])
    colindex.to_excel(writer5,sheet_name=namelist[i-1])
writer5.save()
    



"""
predict
"""
prelist=[]
model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=5#,min_samples_leaf=2
                                )
predf=pd.read_excel('experiment materials.xlsx',sheet_name=1).iloc[0:5,0:31]

for i in trange(1,13):
    frame=pd.read_excel('RF20210202.xlsx',sheet_name=i)
    x_data=frame[globals()['colindex'+str(i)]]
    #x_data=frame.iloc[:,0:29]
    y_data=frame.iloc[:,31:]
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()    
    prei=predf[globals()['colindex'+str(i)]]

    index=pd.read_excel('ss-index.xlsx',sheet_name=i-1).iloc[:,1]
    x_train=x_data.iloc[index,:]
    x_train.columns=x_names        
    y_train=y_data.iloc[index,:]        
    x_test=x_data.iloc[-index,:]
    x_test.columns=x_names    
    y_test=y_data.iloc[-index,:]

    model.fit(x_train,y_train)
    pre=model.predict(prei)
    prelist.append(pre.T)
        
presult=pd.DataFrame(np.array(prelist).reshape(12,5),columns=['T','C','S','M','L'])
presult.index=namelist
'''








'''
permutation test
'''
xl = pd.ExcelFile('plant_data.xlsx')
namelist=xl.sheet_names[0:13]
model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=20#,min_samples_leaf=2
                                )
names = list(('Length','DW','RS','APX','H2O2','MDA','SOD','Chla',
                'Chlb','Content','TF','RCF','SCF'))
writer1=pd.ExcelWriter('permutation.xlsx')

for i in trange(0,13):
    frame=pd.read_excel('plant_data.xlsx',sheet_name=i)
    n=frame.shape[1]
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    valdata=frame.iloc[val,:]
    val_x=valdata.iloc[:,0:(n-1)]
    val_y=valdata.iloc[:,(n-1):]
    frame=frame.iloc[model_index,:]
    x_data=frame.iloc[:,0:(n-1)]
    x_data.index=range(len(x_data))
    y_data=frame.iloc[:,(n-1):]
    y_data.index=range(len(y_data))
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    #stdsc=StandardScaler()
    r2_list=[]
    q2_list=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=15#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]
            
            x_test=x_data.iloc[test_index,:]
            x_test.columns=x_names        
            y_test=y_data.iloc[test_index,:]
            
            for j in trange(5):
                for k in range(10):
                    random.seed(i+j+k)
                    per_index=np.random.choice(train_index,round((j+1)*0.2*len(x_train)),False)
                    y_train_per=y_train.copy()
                    for i_index in per_index:
                        y_train_per.loc[i_index,:]=np.random.uniform(-1,1)#######################################设置数据的范围
                    model.fit(x_train,np.array(y_train_per).ravel())
                    r2_list.append(
                        np.corrcoef(y_train.iloc[:,0],y_train_per.iloc[:,0])[0,1])
                    y_array=np.array(y_test).ravel()
                    rss=np.sum((y_array-model.predict(x_test))**2)                          
                    tss=np.sum((y_array-np.mean(y_array))**2)
                    q2=1-rss/tss
                    q2_list.append(q2)
                
            model.fit(x_train,np.array(y_train).ravel())
            r2_list.append(1)
            rss=np.sum((y_array-model.predict(x_test))**2)                          
            tss=np.sum((y_array-np.mean(y_array))**2)
            q2=1-rss/tss
            q2_list.append(q2)
            pbar.update() 
             
    perdf=pd.DataFrame({'r2':r2_list,'q2':q2_list})
    
    plt.ylim(-2,1)
    plt.xlim(0,1)
    plt.plot(perdf['r2'],perdf['q2'],'.')
    plt.show()
    perdf.to_excel(writer1,sheet_name=names[i])
writer1.save() 

########范围不是-1到1
##RS
i = 12
    frame=pd.read_excel('plant_data.xlsx',sheet_name=i)
    n=frame.shape[1]
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    valdata=frame.iloc[val,:]
    val_x=valdata.iloc[:,0:(n-1)]
    val_y=valdata.iloc[:,(n-1):]
    frame=frame.iloc[model_index,:]
    x_data=frame.iloc[:,0:(n-1)]
    x_data.index=range(len(x_data))
    y_data=frame.iloc[:,(n-1):]
    y_data.index=range(len(y_data))
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    #stdsc=StandardScaler()
    r2_list=[]
    q2_list=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=15#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]
            
            x_test=x_data.iloc[test_index,:]
            x_test.columns=x_names        
            y_test=y_data.iloc[test_index,:]
            
            for j in trange(5):
                for k in range(10):
                    random.seed(i+j+k)
                    per_index=np.random.choice(train_index,round((j+1)*0.2*len(x_train)),False)
                    y_train_per=y_train.copy()
                    for i_index in per_index:
                        y_train_per.loc[i_index,:]=np.random.uniform(0,15)#######################################设置数据的范围
                    model.fit(x_train,np.array(y_train_per).ravel())
                    r2_list.append(
                        np.corrcoef(y_train.iloc[:,0],y_train_per.iloc[:,0])[0,1])
                    y_array=np.array(y_test).ravel()
                    rss=np.sum((y_array-model.predict(x_test))**2)                          
                    tss=np.sum((y_array-np.mean(y_array))**2)
                    q2=1-rss/tss
                    q2_list.append(q2)
                
            model.fit(x_train,np.array(y_train).ravel())
            r2_list.append(1)
            rss=np.sum((y_array-model.predict(x_test))**2)                          
            tss=np.sum((y_array-np.mean(y_array))**2)
            q2=1-rss/tss
            q2_list.append(q2)
            pbar.update() 
             
    perdf=pd.DataFrame({'r2':r2_list,'q2':q2_list})
    
    plt.ylim(-2,1)
    plt.xlim(0,1)
    plt.plot(perdf['r2'],perdf['q2'],'.')
    plt.show()
    perdf.to_excel(writer1,sheet_name=names[i])

















































