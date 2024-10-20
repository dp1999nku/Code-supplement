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
