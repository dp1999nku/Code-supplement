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

#------------------------------------------------------------------------------#
#-----------------------------------RF model-----------------------------------#
#------------------------------------------------------------------------------#
###计算 R2 R2_10
###参数设置:ntree=500  mtry=14
setwd("D:/master/project ing/plant-NP/model0118/R rf for plant")

r2_10_all <- data.frame()
r2_all <- data.frame()
for (i in 1:13){
  data <- read.xlsx('plant_data.xlsx', i)
  set.seed(12345);disorder <- sample(length(data[,2]),replace=F)
  fold_num <- floor(length(data[,2])/10)
  r2_10 <- data.frame()
  predict <- data.frame()
  for (k in 1:10){
    o <- disorder[(fold_num*(k-1)+1):(fold_num*k)]
    rf.data <- data[-o,]
    rf <- randomForest(label~. , data = rf.data, 
                       ntree=500,mtry=20,
                       proximity = F,
                       importance = F)
    p <- predict(rf, data[o,])
    a <- data[o,length(data[1,])]
    predict <- rbind(predict,cbind(a,p))
    
    r2_1 <- cor(p,a)
    r2_10 <- rbind(r2_10, r2_1)
  }
  r2_10 <- mean(r2_10[,1])
  r2 <- cor(predict)[1,2]
  
  r2_10_all <- rbind(r2_10_all, r2_10)
  r2_all <- rbind(r2_all, r2)
  print(i)
}
colnames(r2_10_all) <- "R2"
colnames(r2_all) <- "R2"
label_data <- as.data.frame(label)
r2_10_all <- cbind(label_data,r2_10_all)
r2_all <- cbind(label_data,r2_all)
colnames(r2_10_all)[1] <- "label"
colnames(r2_all)[1] <- "label"

write.csv(r2_10_all, 'r2_10.csv')
write.csv(r2_all, 'r2.csv')

###计算test train R2(10fold&predict)
setwd("D:/master/project ing/plant-NP/model0118/R rf for plant")

###train###
Train_predict_wb <- createWorkbook()
Importance_wb <- createWorkbook()
Imframe_wb <- createWorkbook()
for (i in 1:13){
  addWorksheet(Train_predict_wb,sheetName = label[i])
}
for (i in 1:13){
  addWorksheet(Importance_wb,sheetName = label[i])
}

rf.list<-list()
for (i in 1:length(label)){
  data <- read.xlsx('plant_data.xlsx', i)
  rf.list[[label[i]]]<-local({
    randomForest(label~. , data = data, 
                 ntree=500,mtry=14,
                 proximity = T,
                 importance = T)})
  rf<-rf.list[[label[i]]]
  p <- predict(rf, data)
  a <- data[,length(data[1,])]
  predict <- cbind(a,p)
  imp <- as.data.frame(rf$importance)
  imp[,3] <- rownames(imp)
  colnames(imp) <- c("MSE","Node","variables")
  
  writeData(Train_predict_wb, sheet = i, predict)
  writeData(Importance_wb, sheet = i, imp)
  print(i)
}
save(rf.list,file='Rda/rflist.rda')
saveWorkbook(Train_predict_wb, "predict/Train_predict.xlsx", overwrite = TRUE)
saveWorkbook(Importance_wb, "predict/Importance.xlsx", overwrite = TRUE)

###test###
Test_predict_wb <- createWorkbook()
Test_predict_10fold_wb <- createWorkbook()
Train_predict_10fold_wb <- createWorkbook()
for (i in 1:13){
  addWorksheet(Test_predict_wb,sheetName = label[i])
}
for (i in 1:13){
  addWorksheet(Test_predict_10fold_wb,sheetName = label[i])
}
for (i in 1:13){
  addWorksheet(Train_predict_10fold_wb,sheetName = label[i])
}

for (i in 1:length(label)){
  data <- read.xlsx('plant_data.xlsx', i)
  set.seed(12345);disorder <- sample(length(data[,2]),replace=F)
  fold_num <- floor(length(data[,2])/10)
  
  n <- data.frame()
  n_train <- data.frame()
  predict <- data.frame()
  for (k in 1:10){
    o <- disorder[(fold_num*(k-1)+1):(fold_num*k)]
    rf.data <- data[-o,]
    rf <- randomForest(label~. , data = rf.data, 
                       ntree=500,mtry=14,
                       proximity = F,
                       importance = F)
    p <- predict(rf, data[o,])
    a <- data[o,length(data[1,])]
    p_train <- predict(rf, rf.data)
    a_train <- rf.data[,length(data[1,])]
    
    r2 <- cor(p,a)
    rm <- rmse(p,a)
    predict <- rbind(predict,cbind(a,p))
    
    n <- rbind(n,cbind(r2,rm))
    r2_train <- cor(p_train,a_train)
    rm_train <- rmse(p_train,a_train)
    n_train <- rbind(n_train,cbind(r2_train,rm_train))
  }
  colnames(n_train) <- c('r2','rm')
  writeData(Test_predict_wb, sheet = i, predict)
  writeData(Test_predict_10fold_wb, sheet = i, n)
  writeData(Train_predict_10fold_wb, sheet = i, n_train)

  print(i)
}
saveWorkbook(Test_predict_wb, "predict/Test_predict.xlsx", overwrite = TRUE)
saveWorkbook(Test_predict_10fold_wb, "predict/Test_predict_10fold.xlsx", overwrite = TRUE)
saveWorkbook(Train_predict_10fold_wb, "predict/Train_predict_10fold.xlsx", overwrite = TRUE)

###scatter plot
####见scatter plot code

###R2 distribution plot
###见R2 distribution code

###MSE single importance analysis
###见MSE single

###Feature shuffle
###见Feature shuffle

###permutation
###见permutation


##############Importance analysis#####################
###multi-way importance analysis###
load(file='Rda/rflist.rda')
md<-list()
mi<-list()
#colindex <- c('Carbide', 'Metal', 'MC', 'Oxide', 'Com1', 'Com2', 'Dim0', 'Dim1', 'Dim2',
#              'Hollow', 'Size', 'Purity', 'HD', 'Zeta', 'Species', 'TO', 'MO', 'Age', 'GS',
#              'Cultured', 'Category', 'TC', 'Duration', 'Photoperiod', 'illumination', 'Humidity',
#              'DT', 'NT', 'label')
#colindexf<-factor(colindex[-29],levels=colindex[-29])
for (i in 1:13){
  print(i)
  
  data <- read.xlsx('plant_data.xlsx', i)
  colindex <- c(colnames(data))
  colindexf<-factor(colindex[-length(colindex)],levels=colindex[-length(colindex)])

  rf<-rf.list[[i]]
  #incmse<-data.frame(index=names(imp[,3]),incmse=imp[,1]/max(imp[,1]))
  #colnames(incmse)[2]<-paste0(i)

  min_depth_frame<-min_depth_distribution(rf)
  md[[label[i]]]<-min_depth_frame
  
  im_frame<-measure_importance(rf)
  im_frame[4]<-im_frame[4]/max(im_frame[4])
  im_frame[5]<-im_frame[5]/max(im_frame[5])
  mi[[label[i]]]<-im_frame
}
save(md,mi,file='Rda/multi-importance.rda')
###multi-wayplot
load(file='Rda/rflist.rda')
load(file='Rda/multi-importance.rda')
mdplot<-list()
miplot<-list()
###min_depth
for (i in 1:13){
  print(i)
  min_depth_frame<-md[[i]]
  mdplot[[label[i]]]<-local({
    min_depth_frame=min_depth_frame
    plot_min_depth_distribution(min_depth_frame,k=14)+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      theme(legend.key.size = unit(0.5,'line'),legend.title = element_text(size=rel(0.6)),
            legend.text = element_text(size=rel(0.5)))
  })
  ggsave(paste0('plot/md/md_',label[i],'.pdf'),width=7,height=7)
  
#  im_frame=mi[[i]]
#  im_frame$p_value<-im_frame$p_value/5
#  miplot[[label[i]]]<-local({
#    im_frame=im_frame
#    plot_multi_way_importance(im_frame, x_measure = "mse_increase",
#                              y_measure = "node_purity_increase",
#                              size_measure = "p_value", no_of_labels = 5)+
#      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
#      theme(axis.line=element_line(color='black'),
#            axis.ticks.length=unit(0.5,'line'))+
#      coord_fixed(ratio=1)+
#      theme(legend.position=c(0.1,0.8))
#  })
#  ggsave(paste0('plot/mi/m_im_',label[i],'.pdf'),width=5,height=5)
}
save(mdplot,miplot,file='Rda/importanceplot.rda')
###multi-indicator
#PCA 
#library(factoextra)
#library(FactoMineR)
'''
for (i in 1:13){
  data <- mi[[i]]
  data[is.na(data)] <-  0
  
  for (k in 2:7){
    data[,k] <- data[,k]/max(data[,k])
  }
  data[,2] <- 1-data[,2]
  data[,9] <- 0
  for (w in 1:length(data[,1])){
    if (data[w,8] < 0.01) {
      data[w,9] <- 'A'
    } else if (data[w,8] < 0.1 & data[w,8] > 0.01) {
      data[w,9] <- 'B'
    }else {
      data[w,9] <- 'C'
    }
  }
  
  df <- data[,2:7]
  df.pca <- PCA(df, graph = F)
  fviz_pca_ind(df.pca,
               geom.ind = 'point',
               pointsize=3, pointshape=21,fill.ind = data$V9,
               addEllipses = T,
               label = T
  )+theme_grey()
  ggsave(paste0('plot/mi/pca_',label[i],'.pdf'),width=7,height=7)
}
'''


#####Feature Interaction Calculate######
load(file='Rda/rflist.rda')
load(file='Rda/multi-importance.rda')

inter_list<-list()
for (i in 1:13){
  print(i)
  im_frame<-mi[[i]]
  rf<-rf.list[[i]]
  vars <- important_variables(im_frame, k = 5, measures = c("mean_min_depth","no_of_trees"))
  interactions_frame <- min_depth_interactions(rf, vars)
  interactions_frame <- arrange(interactions_frame,-interactions_frame[,4])
  inter_list[[label[i]]]<-interactions_frame
}
###ps:depth or occurrence
save(inter_list,file='Rda/inter.rda')
###plot
fiplot<-list()
for (i in 1:13){
  interactions_frame<-inter_list[[i]]
  hlim<-ceiling(max(interactions_frame[1:25,3],interactions_frame[1:25,6]))
  fip<-plot_min_depth_interactions(interactions_frame,k=25)+
    scale_y_continuous(limits=c(0,hlim+1.5),expand=c(0,0))+
    scale_fill_gradient(low='#00d538',high='#ff5e24')+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    theme(legend.position=c(0.3,0.8),legend.box="horizontal")
  fiplot[[label[i]]]<-fip
  ggsave(paste0('plot/inter/inter',label[i],'.pdf'),width=7,height=4)
}
save(fiplot,file='Rda/inter_plot.rda')

###pdp analysis  PS:分析必须使用数值型变量
load(file='Rda/inter.rda')
pdplist<-list()
pdpplot<-list()
for (i in 1:13){
  data <- read.xlsx('plant_data.xlsx', i)
  rf <- randomForest(label~. , data = data, 
                ntree=500,mtry=14,
                proximity = T,
                importance = T)
  inter_frame<-inter_list[[i]]
  j=1
  k=1
  subpdp<-list()
  subpdpplot<-list()
  while (j<=4){
    interpair<-inter_frame$interaction[k]
    v1<-strsplit(interpair,':')[[1]][1]
    v2<-strsplit(interpair,':')[[1]][2]
    k=k+1
    if (v1!=v2) {
      par<-pdp::partial(rf,pred.var = c(v1, v2), chull = TRUE, progress = "text")
      subpdp[[j]]<-par
      j<-j+1
    } else {j<-j}
  }
  print(i)
  pdplist[[label[i]]]<-subpdp
  pdpplot[[label[i]]]<-subpdpplot
}

save(pdplist,file='Rda/pdplist.rda')
save(pdpplot,file='Rda/pdpplot.rda')

for (i in 1:13){
  subpdp<-pdplist[[i]]
  for (j in 1:4){
    par<-subpdp[[j]]
    subpdpplot[[j]]<-local({
      par=par
      ggplot(par, aes(x = par[[1L]], y = par[[2L]],
                      z = par[["yhat"]], fill = par[["yhat"]])) +
        geom_tile()+
        geom_contour(color = 'white')+
        viridis::scale_fill_viridis(name =label[i], option = 'D') +
        theme_bw()+
        xlab(colnames(par)[1])+
        ylab(colnames(par)[2])+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        theme(axis.line=element_line(color='black'),axis.text = element_text(size = rel(0.6)),
              axis.ticks.length=unit(0.3,'line'),axis.title = element_text(size = rel(0.6)))+
        theme(legend.key.size = unit(0.5,'line'),legend.title = element_text(size=rel(0.6)),
              legend.text = element_text(size=rel(0.5)),legend.position = c(0.9,0.8))
      ggsave(paste0('plot/dvpdp/',label[i],'-',
      colnames(par)[1],'-',colnames(par)[2],
      '.pdf'),width=5,height=5)
    })
  }
  pdpplot[[label[i]]]<-subpdpplot
}


##########################
i=2
data <- read.xlsx('plant_data.xlsx', i)
rf <- randomForest(label~. , data = data, 
                   ntree=500,mtry=14,
                   proximity = T,
                   importance = T)

#par<-pdp::partial(rf,pred.var = c('DT', 'NT'), chull = TRUE, progress = "text")
rf %>%
  partial(pred.var = c("TC", "HD"), chull = TRUE, progress = "text") %>%
  plotPartial(contour = TRUE, legend.title = "NT")
ice <- partial(rf, pred.var = c("TC", "Duration"), chull = TRUE, progress = "text")
plotPartial(ice)

age.ice <- partial(rf, pred.var = "TC", ice = F)
p1 <- plotPartial(age.ice, alpha = 0.5)
p2 <- plotPartial(age.ice, center = TRUE, alpha = 0.5)
grid.arrange(p1, p2, ncol = 2)






###single pdp
enzyme <- data.frame()
for (k in c('TC', 'Size', 'illumination')){
  enzyme <- data.frame()
  for (i in 10:13){
    data <- read.xlsx('plant_data.xlsx', i)
    rf <- randomForest(label~. , data = data, 
                       ntree=500,mtry=14,
                       proximity = T,
                       importance = T)
    p <- partial(rf, pred.var = k)
    enzyme <- rbind(enzyme, p)
  }
  #growth[,3] <- c(rep('Length',length(growth[,1])/3),
  #                rep('DW',length(growth[,1])/3),
  #                rep('RS',length(growth[,1])/3))
  colnames(enzyme) <- c('Label', 'indicators')
  #assign(paste0("p",k), ggplot(growth, aes(x = Label, y = indicators, color = type)) +
  #         geom_line() +theme_bw()) 
  write.csv(enzyme, paste0('single pdp/absorption-', k, '.csv'))
}
#grid.arrange(pTC, pDuration, pAge)





####3 features interaction analysis
###DW
setwd("C:/Users/admin/Desktop/plant-NP/model0118/R rf for plant")
data <- read.xlsx('3interaction/DW-TC-Duration-DT.xlsx',1)
model_data <- read.xlsx('plant_data.xlsx', 2)
rf <- randomForest(label~. , data = model_data, 
                   ntree=500,mtry=14,
                   proximity = T,
                   importance = T)

frame_all <- data.frame()
for (i in seq(0,2,0.2)){
  data$TC <- i
  for (k in seq(7,107,2)){
    data$Duration <- k
    for (r in seq(22,32,0.2)){
      data$DT <- r
      p <- predict(rf, data)
      frame <- cbind(i,k,r,p)
      frame_all <- rbind(frame_all, frame)
    }
    print(k)
  }
  print(i)
}
write.csv(frame_all, '3interaction/DW_result/1.csv')



###MDA
setwd("C:/Users/admin/Desktop/plant-NP/model0118/R rf for plant")
data <- read.xlsx('3interaction/MDA-TC-Duration-Age.xlsx',1)
model_data <- read.xlsx('plant_data.xlsx', 6)
rf <- randomForest(label~. , data = model_data, 
                   ntree=500,mtry=14,
                   proximity = T,
                   importance = T)

frame_all <- data.frame()
for (i in seq(0,2,0.2)){
  data$TC <- i
  for (k in seq(2,102,2)){
    data$Duration <- k
    for (r in seq(22,32,0.2)){
      data$DT <- r
      p <- predict(rf, data)
      frame <- cbind(i,k,r,p)
      frame_all <- rbind(frame_all, frame)
    }
    print(k)
  }
  print(i)
}
write.csv(frame_all, '3interaction/DW_result/1.csv')





###global plants-NPs
###temperature

###
setwd("C:/Users/admin/Desktop")
data <- read.csv('dayT.csv', header = F)
data <- read.csv('April NT.csv', header =F)
data <- as.matrix(data)
listdata <- data[,-1]
listdata <- listdata[-1,]

c1 <- rep(data[2:721,1], times = (ncol(data)-1))
c2 <- rep(data[1,2:1441], times = (nrow(data)-1))
repvalue <- data.frame()
for (i in 1:(ncol(data)-1)){
  o <- as.numeric(data[1,(i+1)])
  rep1 <- rep(o, times = (nrow(data)-1))
  repvalue <- rbind(repvalue, rep1)
}
c2 <- unlist(as.data.frame(t(as.matrix(repvalue))))
c3 <- unlist(as.data.frame(listdata))

result <- data.frame(c1,c2,c3)
colnames(result) <- c('lat', 'lon', 'value')
write.csv(result, 'April NT.csv')














##########网格搜索法算最优参数########
#####对模型的ntree和mtry求最优解
setwd("C:/Users/admin/Desktop/plant-NP/TClass-SLabel-optimizedmodel")
c_ntree <- c(300,400,500,600,700,800)
c_mtry <- c(8:15)

result_wb <- createWorkbook()
for (i in 1:17){
  addWorksheet(result_wb,sheetName = label[i])
}
for (i in 1:length(label)){
  data <- read.xlsx('plant_data.xlsx', i)
  set.seed(12345);disorder <- sample(length(data[,2]),replace=F)
  fold_num <- floor(length(data[,2])/10)
  r2_10 <- data.frame()
  predict <- data.frame()
  result <- data.frame()
  for (x in c_ntree){
    for (y in c_mtry) {
      for (k in 1:10){
      o <- disorder[(fold_num*(k-1)+1):(fold_num*k)]
      rf.data <- data[-o,]
      rf <- randomForest(label~. , data = rf.data, 
                        ntree= x, mtry = y,
                        proximity = F,
                        importance = F)
      p <- predict(rf, data[o,])
      a <- data[o,length(data[1,])]
      predict <- rbind(predict,cbind(a,p))

      r2_1 <- cor(p,a)
      r2_10 <- rbind(r2_10, r2_1)
      }
      r2_10 <- mean(r2_10[,1])
      r2 <- cor(predict)[1,2]
      result <- rbind(result, cbind(x,y,r2,r2_10))
      print(y)
    }
    print(x)
  }
  colnames(result) <- c('ntree','mtry','R2','R2_10')
  writeData(result_wb, sheet = i, result)
  print(i)
}
saveWorkbook(result_wb, "grid_result2.xlsx", overwrite = TRUE)
###找出最大R2的参数
result <- data.frame()
for (i in 1:17){
  data <- read.xlsx('grid_result2.xlsx', i)
  r2_one <- data[which(data[,3] == max(data[,3])),]
  result <- rbind(result, r2_one)
}
colnames(max_result) <- c('R2','R2_10')
rownames(result) <- c(label)

















































