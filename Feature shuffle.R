library(xlsx)
library(readxl)
library(openxlsx)
library(writexl)
library(randomForestExplainer)
library(randomForest)
library(Metrics)
library(ggplot2)
library(pdp)
library(RColorBrewer) 
library(dplyr)
library(caret)
library(igraph)
library(Metrics)
library(patchwork)

setwd("D:/master/project ing/plant-NP/model0118/R rf for plant")
label <- c(excel_sheets('plant_data.xlsx'))

#####################################################################
#####                    Feature shuffle                        #####
#####################################################################
setwd("D:/master/project ing/plant-NP/model0118/R rf for plant")
datalist<-list()
for (i in 1:13){
  data<-read.xlsx('plant_data.xlsx',i)
  datalist[[label[i]]]<-data
}
save(datalist,file='Rda/data.rda')


shuffle_list<-list()
for (i in 1:13){
  rmse_shuffle <- 0
  for (h in 1:10){
  set.seed(h)
  data<-datalist[[i]]
  rf<-randomForest(label~.,data=data,importance=TRUE,proximity=T,ntree=500,mtry=14)
  pre_train<-predict(rf)
  rmse_shuffle<-append(rmse_shuffle,rmse(pre_train,data$label))
  }
  
  for (j in 1:(ncol(data)-1)){
    for (t in 1:10){
      set.seed(t)
      of<-data[,j]
      sf<-sample(of,size=length(of),replace = FALSE)
      data[j]<-sf
      rf<-randomForest(label~.,data=data,importance=TRUE,proximity=T,ntree=500,mtry=14)
      pre_train<-predict(rf)
      rmse_shuffle<-append(rmse_shuffle,rmse(pre_train,data$label))
    }
  }
  rmse_shuffle <- rmse_shuffle[-1]
  shuffle_frame<-data.frame(n=c(rep(0:(ncol(data)-1),each=10)),rmse=rmse_shuffle,increace=NA)
  for (j in 1:nrow(shuffle_frame)){
    aver <- mean(shuffle_frame[1:10,2])
    shuffle_frame[j,3]<-(shuffle_frame[j,2]-aver)/aver*100
  }
  shuffle_list[[label[i]]]<-shuffle_frame
  print(i)
}
save(shuffle_list,file = 'Rda/shuffle_list.rda')


shuffleplot_r<-list()
for (i in 1:13){
  shuffle_frame<-shuffle_list[[i]]
  colnames(shuffle_frame)<-c('n_feature','rmse','increase')
  shuffleplot_r[[label[i]]]<-local({
    shuffle_frame=shuffle_frame
    ggplot(shuffle_frame,aes(n_feature,increase))+
      stat_summary(fun.data="mean_cl_boot", geom="ribbon", width = 0.01,
                   alpha=I(.5), color = '#7670a6', fill='#7670a6') +
      stat_summary(fun="mean", geom="line",color = '#ca6321', width=0.01)+
      #stat_summary(fun="mean", geom="point",color = '#ca6321',size=1)+
      theme_bw()+
      labs(title=label[i] )+
      theme(axis.line=element_line(color='black'))+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      #coord_fixed(ratio=nrow(shuffle_frame)/
      #              (max(shuffle_frame$increase)-min(shuffle_frame$increase)))+
      theme(legend.position="none",plot.title = element_text(hjust = 0.5))
  })
}
save(shuffleplot_r,file = 'Rda/shuffleplot_r.rda')
p<-shuffleplot_r[[1]]
for (i in 2:13){
  p<-p+shuffleplot_r[[i]]
}
p<-p+plot_layout(ncol=4)
p
ggsave('plot/feature-shuffle-r.pdf',width = 12,height=12)
