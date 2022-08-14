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
library(tidyverse)

###2018 AMT→AYT
setwd("D:/master/project ing/plant-NP/model0118")
tiff_1 <- raster(paste0('Global/Climate raw data/future T/2018 AMT/',1,'.tif'))
for (i in 2:12){
  tiff_one <- raster(paste0('Global/Climate raw data/future T/2018 AMT/',i,'.tif'))
  tiff_1 <- tiff_1+tiff_one
}
tiff_2018 <- tiff_1/12

###AMT~DT+NT
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
    #AMT
    coor <- NIGHT_T_result[,1:2]
    AMT <- raster(paste0('Climate raw data/AMT/',i,'.tif'))
    AMT_value <- as.data.frame(raster::extract(AMT, coor))
    AMT_result <- cbind(NIGHT_T_result, AMT_value)
    AMT_result <- na.omit(AMT_result)
    colnames(AMT_result) <- c('X', 'Y', 'pH', 'DT', 'NT', 'AMT')
    
    bean_climate_data <- rbind(bean_climate_data, AMT_result)
    print(i)
  } else {
    print(i)
  }
}
bean_T_data <- bean_climate_data[,-3]

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
    #AMT
    coor <- NIGHT_T_result[,1:2]
    AMT <- raster(paste0('Climate raw data/AMT/',i,'.tif'))
    AMT_value <- as.data.frame(raster::extract(AMT, coor))
    AMT_result <- cbind(NIGHT_T_result, AMT_value)
    AMT_result <- na.omit(AMT_result)
    colnames(AMT_result) <- c('X', 'Y', 'pH', 'DT', 'NT', 'AMT')
    
    wheat_climate_data <- rbind(wheat_climate_data, AMT_result)
    print(i)
  } else {
    print(i)
  }
}
wheat_T_data <- wheat_climate_data[,-3]

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
    #AMT
    coor <- NIGHT_T_result[,1:2]
    AMT <- raster(paste0('Climate raw data/AMT/',i,'.tif'))
    AMT_value <- as.data.frame(raster::extract(AMT, coor))
    AMT_result <- cbind(NIGHT_T_result, AMT_value)
    AMT_result <- na.omit(AMT_result)
    colnames(AMT_result) <- c('X', 'Y', 'pH', 'DT', 'NT', 'AMT')
    
    maize_climate_data <- rbind(maize_climate_data, AMT_result)
    print(i)
  } else {
    print(i)
  }
}
maize_T_data <- maize_climate_data[,-3]

T_data <- rbind(bean_T_data,  wheat_T_data, maize_T_data)
T_data$DT <- T_data$DT/2
LM_MODEL <- lm(AMT~DT+NT, data=T_data)
summary(LM_MODEL)####NT: 0.7951   DT: 0.1456


###extract data
label <- c('bean', 'wheat', 'maize')
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')
for (i in 1:3){
  for (n in 1:4){
    coor <- read.csv(paste0('Global/crop climate data/',label[i],' climate data','.csv'))
    coor <- coor[,1:2]
    T_2018 <- as.data.frame(raster::extract(tiff_2018, coor))
    result_2018 <- cbind(coor, T_2018)
    result_2018 <- na.omit(result_2018)
    colnames(result_2018) <- c('X', 'Y', 'value')
    tiff_2100 <- raster(paste0('Global/Climate raw data/future T/2100 T/', SSP[n],'.tif'))
    coor <- result_2018[,1:2]
    T_2100 <- as.data.frame(raster::extract(tiff_2100, coor))
    result <- cbind(result_2018, T_2100)
    result <- na.omit(result)
    colnames(result) <- c('X', 'Y', 'value_2018', 'value_2100')
    result[,5] <- (result$value_2100 - result$value_2018)
    colnames(result) <- c('X', 'Y', 'value_2018', 'value_2100', 'differ')
    write.csv(result, paste0('Global/predict result/future/differ/',label[i], '-', SSP[n],'.csv'), 
              row.names = FALSE)
    print(n)
  }
  print(i)
}

######################################predict 2100#############################
####NR: 0.7951   DR: 0.2913
###Length
setwd("D:/master/project ing/plant-NP/model0118")
label <- c('bean', 'wheat', 'maize')
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')
train_data <- read.xlsx('R rf for plant/plant_data.xlsx', 1)
for (i in 1:3){
  output_wb <- createWorkbook()
  for (o in 1:4){
    addWorksheet(output_wb,sheetName = SSP[o])
  }
  for (n in 1:4){
    final_data <- read.csv(paste0('Global/crop climate data/',label[i],' climate data','.csv'))
    final_data[,1] <- sapply(final_data$X,trans)
    final_data[,2] <- sapply(final_data$Y,trans)
    differ <- read.csv(paste0('Global/predict result/future/differ/',label[i],'-', SSP[n],'.csv'))
    differ[,1] <- sapply(differ$X,trans)
    differ[,2] <- sapply(differ$Y,trans)
    
    final_data <- merge(final_data, differ, by = c("X","Y"), all = TRUE) 
    final_data <- na.omit(final_data)
    final_data$DT <- final_data$DT + final_data$differ*3*0.2913/(2*0.2913+0.7951)
    final_data$NT <- final_data$NT + final_data$differ*3*0.7951/(2*0.2913+0.7951)
    final_data <- final_data[,1:7]
    
    data <- read.xlsx('Global/predict result/Length/Length-stress.xlsx',i)
    data[2:length(final_data[,1]),] <- data[1,]
    data$illumination <- final_data$illumination
    data$Humidity <- final_data$Humidity
    data$DT <- final_data$DT
    data$NT <- final_data$NT
    
    ###model
    map_data <- final_data[,1:2]
    for (random in 1:200){
      set.seed(random)
      rf <- randomForest(label~. , data =train_data, 
                         ntree=500,mtry=20,
                         proximity = F,
                         importance = F)
      predict_value <- as.data.frame(predict(rf, data))
      map_data <- cbind(map_data, predict_value)
      print(random)
    }
    print(n)
    colnames(map_data) <- c('X','Y', seq(1, 200, 1))
    ###cal uncertainty
    sample_mean <- as.data.frame(apply(map_data[,3:202], 1, mean))
    sample_sd <- as.data.frame(apply(map_data[,3:202], 1, sd))
    un <- abs(sample_sd/sample_mean)*100
    output <- cbind(final_data[,1:2], sample_mean, un)
    colnames(output) <- c('X', 'Y', 'value','Un')
    writeData(output_wb, sheet = n, output)
  }
  saveWorkbook(output_wb, paste0("Global/predict result/future/Length/", label[i],"-200run.xlsx"), overwrite = TRUE)
}

###Uptake
setwd("D:/master/project ing/plant-NP/model0118")
label <- c('bean', 'wheat', 'maize')
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')

train_data <- read.xlsx('R rf for plant/plant_data.xlsx', 10)

for (i in 1:3){
  output_wb <- createWorkbook()
  for (o in 1:4){
    addWorksheet(output_wb,sheetName = SSP[o])
  }
  for (n in 1:4){
    final_data <- read.csv(paste0('Global/crop climate data/',label[i],' climate data','.csv'))
    final_data[,1] <- sapply(final_data$X,trans)
    final_data[,2] <- sapply(final_data$Y,trans)
    differ <- read.csv(paste0('Global/predict result/future/differ/',label[i],'-', SSP[n],'.csv'))
    differ[,1] <- sapply(differ$X,trans)
    differ[,2] <- sapply(differ$Y,trans)
    final_data <- merge(final_data, differ, by = c("X","Y"), all = TRUE) 
    final_data <- na.omit(final_data)
    final_data$DT <- final_data$DT + final_data$differ*3*0.2913/(2*0.2913+0.7951)
    final_data$NT <- final_data$NT + final_data$differ*3*0.7951/(2*0.2913+0.7951)
    final_data <- final_data[,1:7]
    
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
    ###model
    map_data <- final_data[,1:2]
    for (random in 1:200){
      set.seed(random)
      rf <- randomForest(label~. , data =train_data, 
                         ntree=500,mtry=20,
                         proximity = F,
                         importance = F)
      predict_value <- as.data.frame(predict(rf, data))
      map_data <- cbind(map_data, predict_value)
      print(random)
    }
    print(n)
    colnames(map_data) <- c('X','Y', seq(1, 200, 1))
    ###cal uncertainty
    sample_mean <- as.data.frame(apply(map_data[,3:202], 1, mean))
    sample_sd <- as.data.frame(apply(map_data[,3:202], 1, sd))
    un <- abs(sample_sd/sample_mean)*100
    output <- cbind(final_data[,1:2], sample_mean, un)
    colnames(output) <- c('X', 'Y', 'value','Un')
    writeData(output_wb, sheet = n, output)
  }
  saveWorkbook(output_wb, paste0("Global/predict result/future/Content/", label[i],"-200run.xlsx"), overwrite = TRUE)
}

###Oxidative
setwd("D:/master/project ing/plant-NP/model0118")
model <- c(4,5,6,7)
label <- c('bean', 'wheat', 'maize')
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')
label_index <- c(excel_sheets('Global/predict result/Oxidative/Oxidative-stress.xlsx'))

for (i in 1:4){
  train_data <- read.xlsx('R rf for plant/plant_data.xlsx', model[i])
  data_crop <- read.xlsx('Global/predict result/Oxidative/Oxidative-stress.xlsx',i)
  
  for (n in 1:3){
    output_wb <- createWorkbook()
    for (o in 1:4){
      addWorksheet(output_wb,sheetName = SSP[o])
    }
    for (k in 1:4){
      final_data <- read.csv(paste0('Global/crop climate data/',label[n],' climate data','.csv'))
      final_data[,1] <- sapply(final_data$X,trans)
      final_data[,2] <- sapply(final_data$Y,trans)
      differ <- read.csv(paste0('Global/predict result/future/differ/',label[i],'-', SSP[n],'.csv'))
      differ[,1] <- sapply(differ$X,trans)
      differ[,2] <- sapply(differ$Y,trans)
      final_data <- merge(final_data, differ, by = c("X","Y"), all = TRUE) 
      final_data <- na.omit(final_data)
      final_data$DT <- final_data$DT + final_data$differ*3*0.2913/(2*0.2913+0.7951)
      final_data$NT <- final_data$NT + final_data$differ*3*0.7951/(2*0.2913+0.7951)
      final_data <- final_data[,1:7]
      
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
      ###model
      map_data <- final_data[,1:2]
      for (random in 1:200){
        set.seed(random)
        rf <- randomForest(label~. , data =train_data, 
                           ntree=500,mtry=20,
                           proximity = F,
                           importance = F)
        predict_value <- as.data.frame(predict(rf, data))
        map_data <- cbind(map_data, predict_value)
        print(random)
      }
      print(k)
      colnames(map_data) <- c('X','Y', seq(1, 200, 1))
      ###cal uncertainty
      sample_mean <- as.data.frame(apply(map_data[,3:202], 1, mean))
      sample_sd <- as.data.frame(apply(map_data[,3:202], 1, sd))
      un <- abs(sample_sd/sample_mean)*100
      output <- cbind(final_data[,1:2], sample_mean, un)
      colnames(output) <- c('X', 'Y', 'value','Un')
      writeData(output_wb, sheet = k, output)
    }
    saveWorkbook(output_wb, paste0('Global/predict result/future/Oxidative/', 
                                   label[n], '-', label_index[i], '-', SSP[k], '-200run.xlsx'), overwrite = TRUE)
    print(n)
  }
  print(i)
}

####################################world map plot##############################
#################mean value##############
trans<-function(x){
  if (x>0) {
    z<-floor(x)
  } else {
    z<-ceiling(x)
  }
  y<-z+(round(((x-z)*24-1)/2)*2+1)/24
  y<-round(y,4)
  return(y)
}
###adjust land-ocean value
final<-function(x){
  if (x == 999) {
    z <- x
  } else if (x < 999) {
    z <- x-999
  }else if (x>999 & x<9999) {
    z <- x-999
  } else if (x == 9999){
    z <- 9999
  } else {
    z <- x-9999
  }
  y=z
  return(y)
}
###adjust land-ocean value (special)
final_special <- function(x){
  if (x == 999999) {
    z <- x
  }else if (x>999999 & x<9999999) {
    z <- x-999999
  } else if (x == 9999999){
    z <- 9999999
  } else {
    z <- x-9999999
  }
  y=z
  return(y)
}
###region trans
region_trans<-function(x){
  if (x == 1) {
    z <- 'Asia'
  } else if (x == 2) {
    z <- 'Europe'
  } else if (x == 3) {
    z <- 'Africa'
  } else if (x == 4){
    z <- 'Africa'
  } else if (x == 7){
    z <- 'Latin America'
  } else if (x == 8){
    z <- 'Australia'
  } else {
    z <- 'North America'
  }
  y=z
  return(y)
}
change_percent<-function(x){
  if (x>0) {
    z <- x/(1-x)
  } else {
    z <- -x
  }
  y <- z
  return(y)
}

##############base map#################
setwd("D:/master/project ing/plant-NP/model0118")
base_map <- raster('Global/crop map/base map.tif')
base_frame <- as.data.frame(base_map, xy=T)
base_frame[is.na(base_frame)] <-  9999
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame$value[base_frame$value < 9999] <- 999
base_frame <- data.frame(matrix(as.numeric(unlist(base_frame)), ncol=3, byrow=F))
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame[,1] <- sapply(base_frame$X,trans)
base_frame[,2] <- sapply(base_frame$Y,trans)

###Length
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c(excel_sheets('Length/Length-stress.xlsx'))
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')
#bean & wheat
for (i in 1:3){
  for (n in 1:4){
    frame <- read.xlsx(paste0('future/Length/', label[i],'-200run.xlsx'), n)
    frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=4, byrow=F))
    colnames(frame) <- c('X', 'Y', 'value', 'Un')
    frame[,1] <- sapply(frame$X,trans)
    frame[,2] <- sapply(frame$Y,trans)
    map <- merge(base_frame, frame, by = c("X","Y"),  all = TRUE) 
    map[is.na(map)] <-  0
    map[,6] <- map[,3] + map[,4]
    map[,7] <- map[,3] + map[,5]
    map[,6] <- sapply(map$V6,final)
    map[,7] <- sapply(map$V7,final)
    map_value <- as.data.frame(cbind(map$X, map$Y, map$V6))
    map_un <- as.data.frame(cbind(map$X, map$Y, map$V7))
    colnames(map_value) <- c('Longitude', 'Latitude', 'value')
    colnames(map_un) <- c('Longitude', 'Latitude', 'Un')
    write.csv(map_value, paste0('future/Length/',label[i], '/', SSP[n],'-Length value map.csv'), row.names = FALSE)
    write.csv(map_un, paste0('future/Length/',label[i], '/', SSP[n],'-Length un map.csv'), row.names = FALSE)
    print(n)
  }
  print(i)
}
#maize (special)
setwd("D:/master/project ing/plant-NP/model0118")
base_map <- raster('Global/crop map/base map.tif')
base_frame <- as.data.frame(base_map, xy=T)
base_frame[is.na(base_frame)] <-  9999999
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame$value[base_frame$value < 9999999] <- 999999
base_frame <- data.frame(matrix(as.numeric(unlist(base_frame)), ncol=3, byrow=F))
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame[,1] <- sapply(base_frame$X,trans)
base_frame[,2] <- sapply(base_frame$Y,trans)
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
for (n in 1:4){
  frame <- read.xlsx(paste0('future/Length/', label[3],'-200run.xlsx'), n)
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=4, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value', 'Un')
  frame[,1] <- sapply(frame$X,trans)
  frame[,2] <- sapply(frame$Y,trans)
  map <- merge(base_frame, frame, by = c("X","Y"),  all = TRUE) 
  map[is.na(map)] <-  0
  map[,6] <- map[,3] + map[,5]
  map[,6] <- sapply(map$V6,final_special)
  map_un <- as.data.frame(cbind(map$X, map$Y, map$V6))
  colnames(map_un) <- c('Longitude', 'Latitude', 'Un')
  write.csv(map_un, paste0('future/Length/',label[3], '/', SSP[n],'-Length un map.csv'), row.names = FALSE)
  print(n)
}


###uptake
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c(excel_sheets('Content/Content-stress.xlsx'))
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')
for (i in 1:3){
  for (n in 1:4){
    frame <- read.xlsx(paste0('future/Content/', label[i],'-200run.xlsx'), n)
    frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=4, byrow=F))
    colnames(frame) <- c('X', 'Y', 'value', 'Un')
    frame[,1] <- sapply(frame$X,trans)
    frame[,2] <- sapply(frame$Y,trans)
    map <- merge(base_frame, frame, by = c("X","Y"),  all = TRUE) 
    map[is.na(map)] <-  0
    map[,6] <- map[,3] + map[,4]
    map[,7] <- map[,3] + map[,5]
    map[,6] <- sapply(map$V6,final)
    map[,7] <- sapply(map$V7,final)
    map_value <- as.data.frame(cbind(map$X, map$Y, map$V6))
    map_un <- as.data.frame(cbind(map$X, map$Y, map$V7))
    colnames(map_value) <- c('Longitude', 'Latitude', 'value')
    colnames(map_un) <- c('Longitude', 'Latitude', 'Un')
    write.csv(map_value, paste0('future/Content/',label[i], '/', SSP[n],'-Content value map.csv'), row.names = FALSE)
    write.csv(map_un, paste0('future/Content/',label[i], '/', SSP[n],'-Content un map.csv'), row.names = FALSE)
    print(n)
  }
  print(i)
}


###Oxidative
setwd("D:/master/project ing/plant-NP/model0118")
base_map <- raster('Global/crop map/base map.tif')
base_frame <- as.data.frame(base_map, xy=T)
base_frame[is.na(base_frame)] <-  9999999
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame$value[base_frame$value < 9999999] <- 999999
base_frame <- data.frame(matrix(as.numeric(unlist(base_frame)), ncol=3, byrow=F))
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame[,1] <- sapply(base_frame$X,trans)
base_frame[,2] <- sapply(base_frame$Y,trans)
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c('bean', 'wheat', 'maize')
SSP <- c('ssp1', 'ssp2', 'ssp3', 'ssp5')
for (i in 1:3){
  for (n in 1:4){
    frame_1 <- read.xlsx(paste0('future/Oxidative/', label[i], '-APX-200run.xlsx'), n)
    frame_2 <- read.xlsx(paste0('future/Oxidative/', label[i], '-H2O2-200run.xlsx'), n)
    frame_3 <- read.xlsx(paste0('future/Oxidative/', label[i], '-MDA-200run.xlsx'), n)
    frame_4 <- read.xlsx(paste0('future/Oxidative/', label[i], '-SOD-200run.xlsx'), n)
    Un_combine <- cbind(frame_1$Un, frame_2$Un, frame_3$Un, frame_4$Un)
    Un_mean <- as.data.frame(apply(Un_combine, 1, mean))
    frame <- as.data.frame(cbind(frame_1$X, frame_1$Y, Un_mean))
    
    frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
    colnames(frame) <- c('X', 'Y', 'Un')
    frame[,1] <- sapply(frame$X,trans)
    frame[,2] <- sapply(frame$Y,trans)
    map <- merge(base_frame, frame, by = c("X","Y"),  all = TRUE) 
    map[is.na(map)] <-  0
    map[,5] <- map[,3] + map[,4]
    map[,5] <- sapply(map$V5,final_special)
    map_un <- as.data.frame(cbind(map$X, map$Y, map$V5))
    colnames(map_un) <- c('Longitude', 'Latitude', 'Un')
    write.csv(map_un, paste0('future/Oxidative/',label[i], '-', SSP[n],'-Oxidative un map.csv'), row.names = FALSE)
    print(n)
  }
  print(i)
}

#value-20 class (4 index→1 index)
#combine
len <- c(407314, 436394, 531126)
label_index <- c('APX', 'H2O2', 'MDA', 'SOD')
for (n in 1:3){
  wb <- createWorkbook()
  for (m in 1:4){
    addWorksheet(wb,sheetName = SSP[m])
  }
  for (S in 1:4){
    indicator <- data.frame()
    indicator[1:len[n],1] <- 0
    for (i in 1:4){
      data <- read.xlsx(paste0('future/Oxidative/',label[n], '-',label_index[i], '-200run.xlsx'), S)
      indicator <- cbind(indicator, data[,3])
    }
    indicator <- indicator[,-1]
    colnames(indicator) <- label_index
    writeData(wb, sheet = S, indicator)
  }
  saveWorkbook(wb, paste0('future/Oxidative/indicator-',label[n], '.xlsx'), overwrite = TRUE)
}

for (i in 1:3){
  for (o in 1:4){
    data<-read.xlsx(paste0('future/Oxidative/indicator-', label[i],'.xlsx'), o)
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
    coor_data <- read.xlsx(paste0('future/Oxidative/', label[i], '-','APX-200run.xlsx'), 1)
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
    
    print(i)
    write.csv(set, paste0('future/Oxidative/',label[i], '/', label[i], '-', SSP[o], '-Oxidative-class-20.csv'), 
              row.names = FALSE)
  }
}

#class map
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c('bean', 'wheat', 'maize')
for (i in 1:3){
  for (n in 1:4){
    frame <- read.csv(paste0('future/Oxidative/',label[i], '-', SSP[n],'-Oxidative-class-20.csv'))
    frame <- frame[,-3]
    frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
    colnames(frame) <- c('X', 'Y', 'class')
    frame[,1] <- sapply(frame$X,trans)
    frame[,2] <- sapply(frame$Y,trans)
    
    map <- merge(base_frame, frame, by = c("X","Y"),  all = TRUE) 
    map[is.na(map)] <-  0
    map[,5] <- map[,3] + map[,4]
    map[,5] <- sapply(map$V5,final)
    map <- map[,-3]
    map <- map[,-3]
    colnames(map) <- c('Longitude', 'Latitude', 'class')
    write.csv(map, paste0('future/Oxidative/',label[i], '/', label[i], '-', SSP[n],'-Oxidative class 20 map.csv'), 
              row.names = FALSE)
    print(n)
  }
  print(i)
}










setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
i = 1
frame <- read.xlsx('future/Length/wheat-200run.xlsx',i)

max(frame$value)
min(frame$value)

max(frame$Un)
min(frame$Un)


frame <- read.xlsx('uncertainty/Length/Length-200run.xlsx', 2)
max(frame$value)
min(frame$value)

max(frame$Un)
min(frame$Un)

















































