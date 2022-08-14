###global plants-NPs
###temperature
##NT (PS:April only have 29 data point)
#setwd("D:/climate dataset/水蒸气")
#month30 <- c('April', 'June', 'September', 'November')
#month31 <- c('January', 'March', 'May',
#             'July', 'August', 'October', 'December')
#month28 <- c('February')

#for (i in 1:4){
#  data <- data.frame()
#  data[1:720, 1:1440] <- 0
#  month <- month30[i]
#  for (n in 1:30){
#    data1 <- read.csv(paste0(month, ' ', n, ', 2018VAP', '.csv'), header = F)
#    data1 <- as.data.frame(data1)
#    listdata <- data1[,-1]
#    listdata <- listdata[-1,]
#  
#    data <- data + listdata
#  }
#  result <- data/29
#  write.csv(result, paste0('month/', month, '-NT', '.csv'))
#}

###illumination CSV → tiff
setwd("D:/master/project ing/plant-NP/model0118/Global/Climate raw data/illumination")
for (n in 1:12){
  data <- read.csv(paste0('CSV/IL ',n,'.csv'), header =F)
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
  colnames(result) <- c('Y', 'X', 'value')
  result[,3] <- as.numeric(result[,3])
  result[result[,3]==99999.000000,3]<-NA
  result <- na.omit(result)
  
  write.csv(result, paste0('Arc tran/IL ',n,'.csv'), row.names = FALSE)
}
###DT/NT CSV → tiff
setwd("D:/master/project ing/plant-NP/model0118/Global/Climate raw data/NIGHT T")
for (n in 1:12){
  data <- read.csv(paste0('CSV/NT ',n,'.csv'), header =F)
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
  colnames(result) <- c('Y', 'X', 'value')
  result[,3] <- as.numeric(result[,3])
  result[result[,3]==99999.000000,3]<-NA
  result <- na.omit(result)
  
  write.csv(result, paste0('Arc tran/NT ',n,'.csv'), row.names = FALSE)
}



###raster data
library(raster)
library(rasterVis)
library(RColorBrewer)
library(ncdf4)
library(rgdal)
library(caret)
library(randomForest)
library(xlsx)
library(readxl)
library(openxlsx)
library(writexl)
library(factoextra)
library(FactoMineR)

##################
######BEAN########
##################
setwd("D:/master/project ing/plant-NP/model0118/Global")
###bean dataset
bean <- raster('crop map/bean.tif')
plot(bean)
bean_frame <- as.data.frame(bean, xy=T)
coor <- na.omit(as.data.frame(bean, xy=T))
coor <- coor[rowSums(coor==0)==0,]
coor <- coor[,-3]
###date
plant_date <- raster('Climate raw data/plant date/Bean date.tif')
date_value <- as.data.frame(raster::extract(plant_date, coor))
date_result <- data.frame()
date_result <- rbind(date_result, date_value)
date_result <- cbind(coor, date_result)
colnames(date_result) <- c('X', 'Y', 'date')
date_result[,3] <- as.numeric(date_result[,3])
date_result <- na.omit(date_result)

date_result_month1 <- subset(date_result, date > 0 & date <= 31)
date_result_month2 <- subset(date_result, date > 31 & date <= 59)
date_result_month3 <- subset(date_result, date > 59 & date <= 90)
date_result_month4 <- subset(date_result, date > 90 & date <= 120)
date_result_month5 <- subset(date_result, date > 120 & date <= 151)
date_result_month6 <- subset(date_result, date > 151 & date <= 181)
date_result_month7 <- subset(date_result, date > 181 & date <= 212)
date_result_month8 <- subset(date_result, date > 212 & date <= 243)
date_result_month9 <- subset(date_result, date > 243 & date <= 273)
date_result_month10 <- subset(date_result, date > 273 & date <= 304)
date_result_month11 <- subset(date_result, date > 304 & date <= 334)
date_result_month12 <- subset(date_result, date > 334 & date <= 365)

bean_climate_data <- data.frame()
for (i in 1:12){
  data <- get(paste0('date_result_month', i))
  coor <- data[,1:2]
  
  if (length(coor[,1]) > 0){
    #pH
    pH <- raster('Climate raw data/soil pH/soil pH.tif')
    pH_value <- as.data.frame(raster::extract(pH, coor))
    pH_result <- data.frame()
    pH_result <- rbind(pH_result, pH_value)
    pH_result <- cbind(coor, pH_result)
    colnames(pH_result) <- c('X', 'Y', 'pH')
    pH_result[,3] <- as.numeric(pH_result[,3])
    pH_result[pH_result[,3]==0,3]<-NA
    pH_result <- na.omit(pH_result)
    #DAY T
    coor <- pH_result[,1:2]
    DAY_T <- raster(paste0('Climate raw data/DAY T/DT ',i,'.tif'))
    DAY_T_value <- as.data.frame(raster::extract(DAY_T, coor))
    DAY_T_result <- cbind(pH_result, DAY_T_value)
    DAY_T_result <- na.omit(DAY_T_result)
    colnames(DAY_T_result) <- c('X', 'Y', 'pH', 'DT')
    #NIGHT T
    coor <- DAY_T_result[,1:2]
    NIGHT_T <- raster(paste0('Climate raw data/NIGHT T/NT ',i,'.tif'))
    NIGHT_T_value <- as.data.frame(raster::extract(NIGHT_T, coor))
    NIGHT_T_result <- cbind(DAY_T_result, NIGHT_T_value)
    NIGHT_T_result <- na.omit(NIGHT_T_result)
    colnames(NIGHT_T_result) <- c('X', 'Y', 'pH', 'DT', 'NT')
    #illumination
    coor <- NIGHT_T_result[,1:2]
    illumination <- raster(paste0('Climate raw data/illumination/IL ',i,'.tif'))
    illumination_value <- as.data.frame(raster::extract(illumination, coor))
    illumination_result <- cbind(NIGHT_T_result, illumination_value)
    illumination_result <- na.omit(illumination_result)
    colnames(illumination_result) <- c('X', 'Y', 'pH', 'DT', 'NT','illumination')
    #Humidity
    coor <- illumination_result[,1:2]
    Humidity <- raster(paste0('Climate raw data/RH/RH ',i,'.tif'))
    Humidity_value <- as.data.frame(raster::extract(Humidity, coor))
    Humidity_result <- cbind(illumination_result, Humidity_value)
    Humidity_result <- na.omit(Humidity_result)
    colnames(Humidity_result) <- c('X', 'Y', 'pH', 'DT', 'NT','illumination', 'Humidity')
    
    bean_climate_data <- rbind(bean_climate_data, Humidity_result)
    print(i)
  } else {
    print(i)
  }
}
bean_coor <- bean_climate_data[,1:2]
write.csv(bean_climate_data, 'crop climate data/raw bean climate data.csv', row.names = FALSE)

final_data <- bean_climate_data
#pH translation
final_data[,8] <- 0 
for (i in 1:length(final_data[,1])){
  if (final_data[i,3] < 65) {
    final_data[i,8] <- 'Acidic Soil'
  } else if (final_data[i,3] > 75) {
    final_data[i,8] <- 'Calcareous Soil'
  } else{
    final_data[i,8] <- 'Soil'
  }
}
final_data <- final_data[,-3]
#illumination translation
final_data[,5] <- final_data[,5]/0.51
colnames(final_data) <- c('X', 'Y', 'DT', 'NT','illumination', 'Humidity', 'Cultured')
write.csv(final_data, 'crop climate data/bean climate data.csv', row.names = FALSE)


##################
######WHEAT#######
##################
setwd("D:/master/project ing/plant-NP/model0118/Global")
###wheat dataset
wheat <- raster('crop map/wheat.tif')
plot(wheat)
wheat_frame <- as.data.frame(wheat, xy=T)
coor <- na.omit(as.data.frame(wheat, xy=T))
coor <- coor[rowSums(coor==0)==0,]
coor <- coor[,-3]
###date
plant_date <- raster('Climate raw data/plant date/wheat date.tif')
date_value <- as.data.frame(raster::extract(plant_date, coor))
date_result <- data.frame()
date_result <- rbind(date_result, date_value)
date_result <- cbind(coor, date_result)
colnames(date_result) <- c('X', 'Y', 'date')
date_result[,3] <- as.numeric(date_result[,3])
date_result <- na.omit(date_result)

date_result_month1 <- subset(date_result, date > 0 & date <= 31)
date_result_month2 <- subset(date_result, date > 31 & date <= 59)
date_result_month3 <- subset(date_result, date > 59 & date <= 90)
date_result_month4 <- subset(date_result, date > 90 & date <= 120)
date_result_month5 <- subset(date_result, date > 120 & date <= 151)
date_result_month6 <- subset(date_result, date > 151 & date <= 181)
date_result_month7 <- subset(date_result, date > 181 & date <= 212)
date_result_month8 <- subset(date_result, date > 212 & date <= 243)
date_result_month9 <- subset(date_result, date > 243 & date <= 273)
date_result_month10 <- subset(date_result, date > 273 & date <= 304)
date_result_month11 <- subset(date_result, date > 304 & date <= 334)
date_result_month12 <- subset(date_result, date > 334 & date <= 365)

wheat_climate_data <- data.frame()
for (i in 1:12){
  data <- get(paste0('date_result_month', i))
  coor <- data[,1:2]
  
  if (length(coor[,1]) > 0){
    #pH
    pH <- raster('Climate raw data/soil pH/soil pH.tif')
    pH_value <- as.data.frame(raster::extract(pH, coor))
    pH_result <- data.frame()
    pH_result <- rbind(pH_result, pH_value)
    pH_result <- cbind(coor, pH_result)
    colnames(pH_result) <- c('X', 'Y', 'pH')
    pH_result[,3] <- as.numeric(pH_result[,3])
    pH_result[pH_result[,3]==0,3]<-NA
    pH_result <- na.omit(pH_result)
    #DAY T
    coor <- pH_result[,1:2]
    DAY_T <- raster(paste0('Climate raw data/DAY T/DT ',i,'.tif'))
    DAY_T_value <- as.data.frame(raster::extract(DAY_T, coor))
    DAY_T_result <- cbind(pH_result, DAY_T_value)
    DAY_T_result <- na.omit(DAY_T_result)
    colnames(DAY_T_result) <- c('X', 'Y', 'pH', 'DT')
    #NIGHT T
    coor <- DAY_T_result[,1:2]
    NIGHT_T <- raster(paste0('Climate raw data/NIGHT T/NT ',i,'.tif'))
    NIGHT_T_value <- as.data.frame(raster::extract(NIGHT_T, coor))
    NIGHT_T_result <- cbind(DAY_T_result, NIGHT_T_value)
    NIGHT_T_result <- na.omit(NIGHT_T_result)
    colnames(NIGHT_T_result) <- c('X', 'Y', 'pH', 'DT', 'NT')
    #illumination
    coor <- NIGHT_T_result[,1:2]
    illumination <- raster(paste0('Climate raw data/illumination/IL ',i,'.tif'))
    illumination_value <- as.data.frame(raster::extract(illumination, coor))
    illumination_result <- cbind(NIGHT_T_result, illumination_value)
    illumination_result <- na.omit(illumination_result)
    colnames(illumination_result) <- c('X', 'Y', 'pH', 'DT', 'NT','illumination')
    #Humidity
    coor <- illumination_result[,1:2]
    Humidity <- raster(paste0('Climate raw data/RH/RH ',i,'.tif'))
    Humidity_value <- as.data.frame(raster::extract(Humidity, coor))
    Humidity_result <- cbind(illumination_result, Humidity_value)
    Humidity_result <- na.omit(Humidity_result)
    colnames(Humidity_result) <- c('X', 'Y', 'pH', 'DT', 'NT','illumination', 'Humidity')
    
    wheat_climate_data <- rbind(wheat_climate_data, Humidity_result)
    print(i)
  } else {
    print(i)
  }
}
wheat_coor <- wheat_climate_data[,1:2]
write.csv(wheat_climate_data, 'crop climate data/raw wheat climate data.csv', row.names = FALSE)

final_data <- wheat_climate_data
#pH translation
final_data[,8] <- 0 
for (i in 1:length(final_data[,1])){
  if (final_data[i,3] < 65) {
    final_data[i,8] <- 'Acidic Soil'
  } else if (final_data[i,3] > 75) {
    final_data[i,8] <- 'Calcareous Soil'
  } else{
    final_data[i,8] <- 'Soil'
  }
}
final_data <- final_data[,-3]
#illumination translation
final_data[,5] <- final_data[,5]/0.51
colnames(final_data) <- c('X', 'Y', 'DT', 'NT','illumination', 'Humidity', 'Cultured')
write.csv(final_data, 'crop climate data/wheat climate data.csv', row.names = FALSE)


##################
######MAIZE#######
##################
setwd("D:/master/project ing/plant-NP/model0118/Global")
###maize dataset
maize <- raster('crop map/maize.tif')
plot(maize)
maize_frame <- as.data.frame(maize, xy=T)
coor <- na.omit(as.data.frame(maize, xy=T))
coor <- coor[rowSums(coor==0)==0,]
coor <- coor[,-3]
###date
plant_date <- raster('Climate raw data/plant date/maize date.tif')
date_value <- as.data.frame(raster::extract(plant_date, coor))
date_result <- data.frame()
date_result <- rbind(date_result, date_value)
date_result <- cbind(coor, date_result)
colnames(date_result) <- c('X', 'Y', 'date')
date_result[,3] <- as.numeric(date_result[,3])
date_result <- na.omit(date_result)

date_result_month1 <- subset(date_result, date > 0 & date <= 31)
date_result_month2 <- subset(date_result, date > 31 & date <= 59)
date_result_month3 <- subset(date_result, date > 59 & date <= 90)
date_result_month4 <- subset(date_result, date > 90 & date <= 120)
date_result_month5 <- subset(date_result, date > 120 & date <= 151)
date_result_month6 <- subset(date_result, date > 151 & date <= 181)
date_result_month7 <- subset(date_result, date > 181 & date <= 212)
date_result_month8 <- subset(date_result, date > 212 & date <= 243)
date_result_month9 <- subset(date_result, date > 243 & date <= 273)
date_result_month10 <- subset(date_result, date > 273 & date <= 304)
date_result_month11 <- subset(date_result, date > 304 & date <= 334)
date_result_month12 <- subset(date_result, date > 334 & date <= 365)

maize_climate_data <- data.frame()
for (i in 1:12){
  data <- get(paste0('date_result_month', i))
  coor <- data[,1:2]
  
  if (length(coor[,1]) > 0){
    #pH
    pH <- raster('Climate raw data/soil pH/soil pH.tif')
    pH_value <- as.data.frame(raster::extract(pH, coor))
    pH_result <- data.frame()
    pH_result <- rbind(pH_result, pH_value)
    pH_result <- cbind(coor, pH_result)
    colnames(pH_result) <- c('X', 'Y', 'pH')
    pH_result[,3] <- as.numeric(pH_result[,3])
    pH_result[pH_result[,3]==0,3]<-NA
    pH_result <- na.omit(pH_result)
    #DAY T
    coor <- pH_result[,1:2]
    DAY_T <- raster(paste0('Climate raw data/DAY T/DT ',i,'.tif'))
    DAY_T_value <- as.data.frame(raster::extract(DAY_T, coor))
    DAY_T_result <- cbind(pH_result, DAY_T_value)
    DAY_T_result <- na.omit(DAY_T_result)
    colnames(DAY_T_result) <- c('X', 'Y', 'pH', 'DT')
    #NIGHT T
    coor <- DAY_T_result[,1:2]
    NIGHT_T <- raster(paste0('Climate raw data/NIGHT T/NT ',i,'.tif'))
    NIGHT_T_value <- as.data.frame(raster::extract(NIGHT_T, coor))
    NIGHT_T_result <- cbind(DAY_T_result, NIGHT_T_value)
    NIGHT_T_result <- na.omit(NIGHT_T_result)
    colnames(NIGHT_T_result) <- c('X', 'Y', 'pH', 'DT', 'NT')
    #illumination
    coor <- NIGHT_T_result[,1:2]
    illumination <- raster(paste0('Climate raw data/illumination/IL ',i,'.tif'))
    illumination_value <- as.data.frame(raster::extract(illumination, coor))
    illumination_result <- cbind(NIGHT_T_result, illumination_value)
    illumination_result <- na.omit(illumination_result)
    colnames(illumination_result) <- c('X', 'Y', 'pH', 'DT', 'NT','illumination')
    #Humidity
    coor <- illumination_result[,1:2]
    Humidity <- raster(paste0('Climate raw data/RH/RH ',i,'.tif'))
    Humidity_value <- as.data.frame(raster::extract(Humidity, coor))
    Humidity_result <- cbind(illumination_result, Humidity_value)
    Humidity_result <- na.omit(Humidity_result)
    colnames(Humidity_result) <- c('X', 'Y', 'pH', 'DT', 'NT','illumination', 'Humidity')
    
    maize_climate_data <- rbind(maize_climate_data, Humidity_result)
    print(i)
  } else {
    print(i)
  }
}
maize_coor <- maize_climate_data[,1:2]
write.csv(maize_climate_data, 'crop climate data/raw maize climate data.csv', row.names = FALSE)

final_data <- maize_climate_data
#pH translation
final_data[,8] <- 0 
for (i in 1:length(final_data[,1])){
  if (final_data[i,3] < 65) {
    final_data[i,8] <- 'Acidic Soil'
  } else if (final_data[i,3] > 75) {
    final_data[i,8] <- 'Calcareous Soil'
  } else{
    final_data[i,8] <- 'Soil'
  }
}
final_data <- final_data[,-3]
#illumination translation
final_data[,5] <- final_data[,5]/0.51
colnames(final_data) <- c('X', 'Y', 'DT', 'NT','illumination', 'Humidity', 'Cultured')
write.csv(final_data, 'crop climate data/maize climate data.csv', row.names = FALSE)




########################################global prediction############################
###Zno-Content
setwd("D:/master/project ing/plant-NP/model0118")
label <- c(excel_sheets('Global/predict result/Content/Content-stress.xlsx'))
train_data <- read.xlsx('R rf for plant/plant_data.xlsx', 10)
rf <- randomForest(label~. , data =train_data, 
                   ntree=500,mtry=20,
                   proximity = F,
                   importance = F)
for (i in 1:3){
  final_data <- read.csv(paste0('Global/crop climate data/',label[i],' climate data','.csv'))
  data <- read.xlsx('Global/predict result/Content/Content-stress.xlsx',i)
  data[2:length(final_data[,1]),] <- data[1,]
  data$illumination <- final_data$illumination
  data$Humidity <- final_data$Humidity
  data$DT <- final_data$DT
  data$NT <- final_data$NT
  
  dmy <- dummyVars(" ~ .", data = final_data)
  trdata <- data.frame(predict(dmy, newdata = final_data))
  Cultured <- data.frame(NO=(1:length(final_data[,1])))
  Cultured[,1] <- trdata$CulturedAcidic.Soil
  Cultured[,2] <- trdata$CulturedCalcareous.Soil
  Cultured[,3] <- 0
  Cultured[,4] <- 0
  Cultured[,5] <- trdata$CulturedSoil
  data[,31:35] <- Cultured
  
  predict_value <- as.data.frame(predict(rf, data))
  map_data <- cbind(final_data[,1:2], predict_value)
  colnames(map_data) <- c('X','Y','value')
  write.csv(map_data, paste0('Global/predict result/Content/plot/',label[i],'-ZnO-Content.csv'), 
            row.names = FALSE)
}

###Fe2O3-Length
setwd("D:/master/project ing/plant-NP/model0118")
label <- c(excel_sheets('Global/predict result/Length/Length-stress.xlsx'))
train_data <- read.xlsx('R rf for plant/plant_data.xlsx', 1)
rf <- randomForest(label~. , data =train_data, 
                   ntree=500,mtry=20,
                   proximity = F,
                   importance = F)
for (i in 1:3){
  final_data <- read.csv(paste0('Global/crop climate data/',label[i],' climate data','.csv'))
  data <- read.xlsx('Global/predict result/Length/Length-stress.xlsx',i)
  data[2:length(final_data[,1]),] <- data[1,]
  data$illumination <- final_data$illumination
  data$Humidity <- final_data$Humidity
  data$DT <- final_data$DT
  data$NT <- final_data$NT
  
  predict_value <- as.data.frame(predict(rf, data))
  map_data <- cbind(final_data[,1:2], predict_value)
  colnames(map_data) <- c('X','Y','value')
  write.csv(map_data, paste0('Global/predict result/Length/',label[i],'-Fe2O3-Length.csv'), 
            row.names = FALSE)
}


###ZnO-Oxidative stress
setwd("D:/master/project ing/plant-NP/model0118")
model <- c(4,5,6,7)
label <- c('bean', 'wheat', 'maize')
label_index <- c(excel_sheets('Global/predict result/Oxidative/Oxidative-stress.xlsx'))
for (i in 1:4){
  map_wb <- createWorkbook()
  for (m in 1:3){
    addWorksheet(map_wb,sheetName = label[m])
  }
  
  train_data <- read.xlsx('R rf for plant/plant_data.xlsx', model[i])
  rf <- randomForest(label~. , data =train_data, 
                     ntree=500,mtry=20,
                     proximity = F,
                     importance = F)
  data_crop <- read.xlsx('Global/predict result/Oxidative/Oxidative-stress.xlsx',i)
  for (n in 1:3){
    final_data <- read.csv(paste0('Global/crop climate data/',label[n],' climate data','.csv'))
    data <- data_crop[n,]
    data[2:length(final_data[,1]),] <- data[1,]
    data$illumination <- final_data$illumination
    data$Humidity <- final_data$Humidity
    data$DT <- final_data$DT
    data$NT <- final_data$NT
    
    if (length(data[1,]) == 45){
      dmy <- dummyVars(" ~ .", data = final_data)
      trdata <- data.frame(predict(dmy, newdata = final_data))
      Cultured <- data.frame(NO=(1:length(final_data[,1])))
      Cultured[,1] <- trdata$CulturedAcidic.Soil
      Cultured[,2] <- trdata$CulturedCalcareous.Soil
      Cultured[,3] <- 0
      Cultured[,4] <- 0
      Cultured[,5] <- trdata$CulturedSoil
      data[,32:36] <- Cultured
    } else{
      data <- data
    }
    
    predict_value <- as.data.frame(predict(rf, data))
    map_data <- cbind(final_data[,1:2], predict_value)
    colnames(map_data) <- c('X','Y','value')
    writeData(map_wb, sheet = n, map_data)
    print(n)
  }
  saveWorkbook(map_wb, paste0('Global/predict result/Oxidative/',label_index[i],'-ZnO-Oxidative.xlsx'),
               overwrite = TRUE)
  print(i)
}
#combine
wb <- createWorkbook()
for (m in 1:3){
  addWorksheet(wb,sheetName = label[m])
}
len <- c(412572, 440706, 537906)
for (n in 1:3){
  indicator <- data.frame()
  indicator[1:len[n],1] <- 0
  for (i in 1:4){
    data <- read.xlsx(paste0('Global/predict result/Oxidative/', label_index[i], '-ZnO-Oxidative.xlsx'), n)
    indicator <- cbind(indicator, data[,3])
  }
  indicator <- indicator[,-1]
  colnames(indicator) <- label_index
  writeData(wb, sheet = n, indicator)
}
saveWorkbook(wb, 'Global/predict result/Oxidative/indicator.xlsx', overwrite = TRUE)
###4 index → 1 index
change_percent<-function(x){
  if (x>0) {
    z <- x/(1-x)
  } else {
    z <- -x
  }
  y <- z
  return(y)
}
label <- c('bean', 'wheat', 'maize')
for (i in 1:3){
  data<-read.xlsx('Global/predict result/Oxidative/indicator.xlsx', i)
  data_tran1 <- as.data.frame(sapply(data$APX,change_percent))
  data_tran2 <- as.data.frame(sapply(data$H2O2,change_percent))
  data_tran3 <- as.data.frame(sapply(data$MDA,change_percent))
  data_tran4 <- as.data.frame(sapply(data$SOD,change_percent))
  crop_data <- cbind(data_tran1, data_tran2, data_tran3, data_tran4)
  colnames(crop_data) <- c('APX', 'H2O2', 'MDA', 'SOD')
  crop_data$APX <- crop_data$APX/max(crop_data$APX)
  crop_data$H2O2 <- crop_data$H2O2/max(crop_data$H2O2)
  crop_data$MDA <- crop_data$MDA/max(crop_data$MDA)
  crop_data$SOD <- crop_data$SOD/max(crop_data$SOD)
  crop_data[,5] <- (crop_data[,1]+crop_data[,2]+crop_data[,3]+crop_data[,4])/4
  coor_data <- read.csv(paste0('Global/crop climate data/',label[i],' climate data.csv'))
  map_data <- data.frame(coor_data[,1:2], crop_data[,5])
  colnames(map_data) <- c('X', 'Y', 'value')
  
  value <- unique(as.data.frame(map_data[,3]))
  colnames(value) <- 'index'
  value <- arrange(value, index)
  num <- floor(length(value[,1])*0.05)
  point_set <- data.frame()
  for (n in 1:19){
    point <- value[num*n,1]
    point_set <- rbind(point_set, point)
  }
  colnames(point_set) <- 'breakpoint'
  set <-data.frame()
  for (a in 1:20){
    if (a == 1){
      sub <- subset(map_data, map_data$value < point_set[a,1])
    } else if (a == 20){
      sub <- subset(map_data, map_data$value >= point_set[(a-1),1])
    } else {
      sub <- subset(map_data, map_data$value >= point_set[(a-1),1] & 
                      map_data$value < point_set[a,1])
    }
    sub[,4] <- a
    set <- rbind(set, sub)
  }
  colnames(set)[4] <- 'class'

  write.csv(set, paste0('Global/predict result/Oxidative/',label[i],'-ZnO-Oxidative-class-20.csv'), 
            row.names = FALSE)
}

###class
map_data <- arrange(map_data, -value) 
map_data[,4] <- "zero"
num <- floor(length(map_data[,1])*0.1)
type <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten')
for (n in 1:5){
  if (n < 10){
  map_data[(num*(n-1)+1):num*n, 4] <- type[n]
  } else {
    map_data[(num*(n-1)+1):length(map_data[,1]),4] <- type[n]
  }
}

value <- unique(as.data.frame(map_data[,3]))
colnames(value) <- 'index'
value <- arrange(value, index)
num <- floor(length(value[,1])*0.1)
point_set <- data.frame()
for (n in 1:9){
  point <- value[num*n,1]
  point_set <- rbind(point_set, point)
}
colnames(point_set) <- 'breakpoint'
set <-data.frame()
for (a in 1:10){
  if (a == 1){
    sub <- subset(map_data, map_data$value < point_set[a,1])
  } else if (a == 10){
    sub <- subset(map_data, map_data$value >= point_set[(a-1),1])
  } else {
    sub <- subset(map_data, map_data$value >= point_set[(a-1),1] & 
                            map_data$value < point_set[a,1])
  }
  sub[,4] <- a
  set <- rbind(set, sub)
}
colnames(set)[4] <- 'class'



###Fe2O3-Chlab
setwd("D:/master/project ing/plant-NP/model0118")
model <- c(8,9)
label <- c('bean', 'wheat', 'maize')
label_index <- c(excel_sheets('Global/predict result/Chl/Chl-stress.xlsx'))
for (i in 1:2){
  map_wb <- createWorkbook()
  for (m in 1:3){
    addWorksheet(map_wb,sheetName = label[m])
  }
  
  train_data <- read.xlsx('R rf for plant/plant_data.xlsx', model[i])
  rf <- randomForest(label~. , data =train_data, 
                     ntree=500,mtry=20,
                     proximity = F,
                     importance = F)
  data_crop <- read.xlsx('Global/predict result/Chl/Chl-stress.xlsx',i)
  for (n in 1:3){
    final_data <- read.csv(paste0('Global/crop climate data/',label[n],' climate data','.csv'))
    data <- data_crop[n,]
    data[2:length(final_data[,1]),] <- data[1,]
    data$illumination <- final_data$illumination
    data$Humidity <- final_data$Humidity
    data$DT <- final_data$DT
    data$NT <- final_data$NT
    
    dmy <- dummyVars(" ~ .", data = final_data)
    trdata <- data.frame(predict(dmy, newdata = final_data))
    Cultured <- data.frame(NO=(1:length(final_data[,1])))
    Cultured[,1] <- trdata$CulturedAcidic.Soil
    Cultured[,2] <- trdata$CulturedCalcareous.Soil
    Cultured[,3] <- 0
    Cultured[,4] <- 0
    Cultured[,5] <- trdata$CulturedSoil
    data[,30:34] <- Cultured

    predict_value <- as.data.frame(predict(rf, data))
    map_data <- cbind(final_data[,1:2], predict_value)
    colnames(map_data) <- c('X','Y','value')
    writeData(map_wb, sheet = n, map_data)
    print(n)
  }
  saveWorkbook(map_wb, paste0('Global/predict result/Chl/',label_index[i],'-Fe2O3-Chl.xlsx'),
               overwrite = TRUE)
  print(i)
}
#combine
wb <- createWorkbook()
for (m in 1:3){
  addWorksheet(wb,sheetName = label[m])
}
len <- c(412572, 440706, 537906)
for (n in 1:3){
  indicator <- data.frame()
  indicator[1:len[n],1] <- 0
  for (i in 1:2){
    data <- read.xlsx(paste0('Global/predict result/Chl/', label_index[i], '-Fe2O3-Chl.xlsx'), n)
    indicator <- cbind(indicator, data[,3])
  }
  indicator <- indicator[,-1]
  colnames(indicator) <- label_index
  writeData(wb, sheet = n, indicator)
}
saveWorkbook(wb, 'Global/predict result/Chl/indicator.xlsx', overwrite = TRUE)




























































tran<-function(x){
  if (x>0) {
    z <- x/(1-x)
  } else {
    z <- x
  }
  y <- z
  return(y)
}
data<-read.xlsx('Global/predict result/Oxidative/indicator.xlsx', 1)
data_tran1 <- as.data.frame(sapply(data$APX,tran))
data_tran2 <- as.data.frame(sapply(data$H2O2,tran))
data_tran3 <- as.data.frame(sapply(data$MDA,tran))
data_tran4 <- as.data.frame(sapply(data$SOD,tran))
data_tran <- cbind(data_tran1, data_tran2, data_tran3, data_tran4)
colnames(data_tran) <- c('APX', 'H2O2', 'MDA', 'SOD')

dt<-as.matrix(scale(data))
head(dt)
rm1<-cor(dt)
rs1<-eigen(rm1)

val <- rs1$values
(Standard_deviation <- sqrt(val))
(Proportion_of_Variance <- val/sum(val))
(Cumulative_Proportion <- cumsum(Proportion_of_Variance))

(U<-as.matrix(rs1$vectors))
PC <-dt %*% U
colnames(PC) <- c("PC1","PC2","PC3","PC4")
head(PC)

PC <- as.data.frame(PC)
PC[,5] <- 'A' 
df <- PC

library(ggplot2)
p1<-ggplot(data = df,aes(x=PC1,y=PC2))+
  #stat_ellipse(aes(),type ="norm", geom ="polygon",alpha=0.2,color=NA)+
  geom_point()
p1

library(scatterplot3d)
color = c(rep('purple',50),rep('orange',50),rep('blue',50))
scatterplot3d(df[,1:3],
              pch = 16,angle=30,
              box=T,type="p",
              lty.hide=2,lty.grid = 2)
legend("topleft",c('Setosa','Versicolor','Virginica'),
       fill=c('purple','orange','blue'),box.col=NA)

tran<-function(x){
  if (x>0) {
    z <- x/(1-x)
  } else {
    z <- x
  }
  y <- z
  return(y)
}
data<-read.xlsx('Global/predict result/Oxidative/indicator.xlsx', 1)
data_tran1 <- as.data.frame(sapply(data$APX,tran))
data_tran2 <- as.data.frame(sapply(data$H2O2,tran))
data_tran3 <- as.data.frame(sapply(data$MDA,tran))
data_tran4 <- as.data.frame(sapply(data$SOD,tran))
data_tran <- cbind(data_tran1, data_tran2, data_tran3, data_tran4)
colnames(data_tran) <- c('APX', 'H2O2', 'MDA', 'SOD')
coor_data <- read.csv('Global/crop climate data/bean climate data.csv')
data_pca <- data.frame(coor_data[,1:2], data_tran)
data_pca[,1] <- sapply(data_pca$X,trans)
data_pca[,2] <- sapply(data_pca$Y,trans)

region_map <- raster('Global/crop map/world region.tif')
region_frame <- as.data.frame(region_map, xy=T)
colnames(region_frame) <- c('X', 'Y', 'Region')
region_frame <- na.omit(region_frame)
region_frame[,1] <- sapply(region_frame$X,trans)
region_frame[,2] <- sapply(region_frame$Y,trans)

region_frame <- subset(region_frame, region_frame$Region == 1&
                         region_frame$Region == 2&
                         region_frame$Region == 3&
                         region_frame$Region == 4&
                         region_frame$Region == 7&
                         region_frame$Region == 8&
                         region_frame$Region == 9)
region_frame[,3] <- sapply(region_frame$Region, region_trans)
region_label <- c('Asia', 'Europe', 'Africa', 'Latin America', 'Australia', 'North America')
map <- merge(region_frame, data_pca, by = c("X","Y"), , all = TRUE)
map_frame <- na.omit(map)
map_frame[,3] <- sapply(map_frame$Region, region_trans)
map_frame[,8] <- map_frame[,3]
colnames(map_frame)[8] <- 'Color'

com <- prcomp(map_frame[,4:7], center = TRUE,scale. = TRUE)
summary(com)
PC <- as.data.frame(com$x)
p <- ggplot(data = PC,aes(x=PC1,y=PC2, color=map_frame$Region))+
  #stat_ellipse(aes(),type ="norm", geom ="polygon",alpha=0.2,color=NA)+
     geom_point(size = 0.1)
p

###color tran
color_trans<-function(x){
  if (x == 'Asia') {
    z <- '#feda77'
  } else if (x == 'Europe') {
    z <- '#8134af'
  } else if (x == 'Africa') {
    z <- '#515bd4'
  } else if (x == 'Africa'){
    z <- '#515bd4'
  } else if (x == 'Latin America'){
    z <- '#dd2a7b'
  } else if (x == 'Australia'){
    z <- '#716e77'
  } else {
    z <- '#e79686'
  }
  y=z
  return(y)
}
map_frame[,8] <- sapply(map_frame$Color, color_trans)

library(scatterplot3d)
color = map_frame$Color
scatterplot3d(PC[,1:3], color = color,
              pch = 16,angle=20,
              box=T,type="p",
              lty.hide=2,lty.grid = 2)
legend("topleft",c('Setosa','Versicolor','Virginica'),
       fill=c('purple','orange','blue'),box.col=NA)








library(kernlab)
kpc <- kpca(data)
print(kpc)




###maize-oxidative stress data indictor
factor_data <- data.frame()
factor_data[1:542287,1] <- 1
for (i in 1:4){
  data <- read.xlsx('Raster/maize-oxidative stress map.xlsx', i)
  factor <- as.data.frame(data[,3])
  factor_data <- cbind(factor_data,factor)
}
factor_data <- factor_data[,-1]
colnames(factor_data) <- c(label[4:7])
factor_data <- abs(factor_data)
#factor_data[,1] <- factor_data[,1]*-1
#factor_data[,4] <- factor_data[,4]*-1
#factor_data <- scale(factor_data,center=T,scale=T)

###PCA
library(factoextra)
library(FactoMineR)
pcaa <- PCA(factor_data,scale.unit = T,ncp=4,graph = T)
print(pcaa)
summary(pcaa)
score <- pcaa$ind$coord
(w <- pcaa$eig[1:4]/sum(pcaa$eig[1:4]))
value <- score %*% w
pca_result <- cbind(data[,1:2], value)
colnames(pca_result) <- c('X', 'Y', 'value')
write.csv(pca_result, 'Raster/maize pca abs analysis.csv', row.names = FALSE)















