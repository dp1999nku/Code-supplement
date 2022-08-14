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
###Fe2O3-Length
setwd("D:/master/project ing/plant-NP/model0118")
label <- c(excel_sheets('Global/predict result/Length/Length-stress.xlsx'))
train_data <- read.xlsx('R rf for plant/plant_data.xlsx', 1)

output_wb <- createWorkbook()
for (i in 1:3){
  addWorksheet(output_wb,sheetName = label[i])
}
for (i in 1:3){
  final_data <- read.csv(paste0('Global/crop climate data/',label[i],' climate data','.csv'))
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
  print(i)
  colnames(map_data) <- c('X','Y', seq(1, 200, 1))
  ###cal uncertainty
  sample_mean <- as.data.frame(apply(map_data[,3:202], 1, mean))
  sample_sd <- as.data.frame(apply(map_data[,3:202], 1, sd))
  un <- abs(sample_sd/sample_mean)*100
  output <- cbind(final_data[,1:2], sample_mean, un)
  colnames(output) <- c('X', 'Y', 'value','Un')
  writeData(output_wb, sheet = i, output)
}
saveWorkbook(output_wb, "Global/predict result/uncertainty/Length/Length-200run.xlsx", overwrite = TRUE)



###ZnO-Content
setwd("D:/master/project ing/plant-NP/model0118")
label <- c(excel_sheets('Global/predict result/Content/Content-stress.xlsx'))
train_data <- read.xlsx('R rf for plant/plant_data.xlsx', 10)

output_wb <- createWorkbook()
for (i in 1:3){
  addWorksheet(output_wb,sheetName = label[i])
}
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
  print(i)
  colnames(map_data) <- c('X','Y', seq(1, 200, 1))
  ###cal uncertainty
  sample_mean <- as.data.frame(apply(map_data[,3:202], 1, mean))
  sample_sd <- as.data.frame(apply(map_data[,3:202], 1, sd))
  un <- abs(sample_sd/sample_mean)*100
  output <- cbind(final_data[,1:2], sample_mean, un)
  colnames(output) <- c('X', 'Y', 'value','Un')
  writeData(output_wb, sheet = i, output)
}
saveWorkbook(output_wb, "Global/predict result/uncertainty/Content/Content-200run.xlsx", overwrite = TRUE)


###ZnO-Oxidative stress
setwd("D:/master/project ing/plant-NP/model0118")
model <- c(4,5,6,7)
label <- c('bean', 'wheat', 'maize')
label_index <- c(excel_sheets('Global/predict result/Oxidative/Oxidative-stress.xlsx'))

for (i in 1:4){
  output_wb <- createWorkbook()
  for (o in 1:3){
    addWorksheet(output_wb,sheetName = label[o])
  }
  train_data <- read.xlsx('R rf for plant/plant_data.xlsx', model[i])

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
    print(i)
    colnames(map_data) <- c('X','Y', seq(1, 200, 1))
    ###cal uncertainty
    sample_mean <- as.data.frame(apply(map_data[,3:202], 1, mean))
    sample_sd <- as.data.frame(apply(map_data[,3:202], 1, sd))
    un <- abs(sample_sd/sample_mean)*100
    output <- cbind(final_data[,1:2], sample_mean, un)
    colnames(output) <- c('X', 'Y', 'value','Un')
    writeData(output_wb, sheet = n, output)
  }
  saveWorkbook(output_wb, paste0('Global/predict result/uncertainty/Oxidative/',label_index[i], '-200run.xlsx'), overwrite = TRUE)
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
for (i in 1:3){
  frame <- read.xlsx('uncertainty/Length/Length-200run.xlsx',i)
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
  write.csv(map_value, paste0('uncertainty/Length/',label[i],'-Length value map.csv'), row.names = FALSE)
  write.csv(map_un, paste0('uncertainty/Length/',label[i],'-Length un map.csv'), row.names = FALSE)
  print(i)
}
###special for maize uncertainty
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
label <- c(excel_sheets('Length/Length-stress.xlsx'))
frame <- read.xlsx('uncertainty/Length/Length-200run.xlsx',3)
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
write.csv(map_un, paste0('uncertainty/Length/',label[3],'-Length un map.csv'), row.names = FALSE)




###uptake
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c(excel_sheets('Content/Content-stress.xlsx'))
for (i in 1:3){
  frame <- read.xlsx('uncertainty/Content/Content-200run.xlsx',i)
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
  write.csv(map_value, paste0('uncertainty/Content/',label[i],'-Content value map.csv'), row.names = FALSE)
  write.csv(map_un, paste0('uncertainty/Content/',label[i],'-Content un map.csv'), row.names = FALSE)
  print(i)
}


###Oxidative
#Un
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
for (i in 1:3){
  frame_1 <- read.xlsx('uncertainty/Oxidative/APX-200run.xlsx',i)
  frame_2 <- read.xlsx('uncertainty/Oxidative/H2O2-200run.xlsx',i)
  frame_3 <- read.xlsx('uncertainty/Oxidative/MDA-200run.xlsx',i)
  frame_4 <- read.xlsx('uncertainty/Oxidative/SOD-200run.xlsx',i)
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
  write.csv(map_un, paste0('uncertainty/Oxidative/',label[i],'-Oxidative un map.csv'), row.names = FALSE)
  print(i)
}
#value-20 class (4 index→1 index)
#combine
wb <- createWorkbook()
for (m in 1:3){
  addWorksheet(wb,sheetName = label[m])
}
len <- c(412572, 440706, 537906)
label_index <- c('APX', 'H2O2', 'MDA', 'SOD')
for (n in 1:3){
  indicator <- data.frame()
  indicator[1:len[n],1] <- 0
  for (i in 1:4){
    data <- read.xlsx(paste0('uncertainty/Oxidative/', label_index[i], '-200run.xlsx'), n)
    indicator <- cbind(indicator, data[,3])
  }
  indicator <- indicator[,-1]
  colnames(indicator) <- label_index
  writeData(wb, sheet = n, indicator)
}
saveWorkbook(wb, 'uncertainty/Oxidative/indicator.xlsx', overwrite = TRUE)
for (i in 1:3){
  data<-read.xlsx('uncertainty/Oxidative/indicator.xlsx', i)
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
  coor_data <- read.xlsx(paste0('uncertainty/Oxidative/APX-200run.xlsx'), i)
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
  write.csv(set, paste0('uncertainty/Oxidative/',label[i],'-Oxidative-class-20.csv'), 
            row.names = FALSE)
}

#class map
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c('bean', 'wheat', 'maize')
for (i in 1:3){
  frame <- read.csv(paste0('uncertainty/Oxidative/',label[i],'-Oxidative-class-20.csv'))
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
  write.csv(map, paste0('uncertainty/Oxidative/',label[i],'-Oxidative class 20 map.csv'), row.names = FALSE)
}



######################################################continent classification
setwd("D:/master/project ing/plant-NP/model0118")
region_map <- raster('Global/crop map/world region.tif')
region_frame <- as.data.frame(region_map, xy=T)
colnames(region_frame) <- c('X', 'Y', 'Region')
region_frame <- na.omit(region_frame)
region_frame[,1] <- sapply(region_frame$X,trans)
region_frame[,2] <- sapply(region_frame$Y,trans)

region_frame <- subset(region_frame, region_frame$Region == 1|
                         region_frame$Region == 2|
                         region_frame$Region == 3|
                         region_frame$Region == 4|
                         region_frame$Region == 7|
                         region_frame$Region == 8|
                         region_frame$Region == 9)
region_frame[,3] <- sapply(region_frame$Region, region_trans)
region_label <- c('Asia', 'Europe', 'Africa', 'Latin America', 'Australia', 'North America')
plant_label <- c('bean', 'maize', 'wheat')
###oxidative stress region histogram plot
for (n in 1:3){
  frame <- read.csv(paste0('Global/predict result/uncertainty/Oxidative/', plant_label[n], '-Oxidative class 20 map.csv'))
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value')
  frame <- subset(frame, frame$value < 100)
  map <- merge(region_frame, frame, by = c("X","Y"), , all = TRUE)
  result <- data.frame()
  for (i in 1:6){
    continent_all <- subset(map, map$Region == region_label[i]&map$value > -100)
    continent_risk1 <- subset(map, map$Region == region_label[i]&map$value>14)
    continent_risk2 <- subset(map, map$Region == region_label[i]&map$value>9)
    num <- length(continent_all[,1])
    num_1 <- length(continent_risk1[,1])
    num_2 <- length(continent_risk2[,1])
    precent_1 <- length(continent_risk1[,1])/length(continent_all[,1])
    precent_2 <- length(continent_risk2[,1])/length(continent_all[,1])
    result <- rbind(result,cbind(region_label[i], num, num_1, num_2,
                                 precent_1, precent_2))
  }
  colnames(result) <- c('Region', 'sum', 'sum risk1', 'sum risk2',
                        'pre risk1', 'pre risk2')
  write.csv(result, paste0('Global/predict result/uncertainty/Oxidative/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/uncertainty/Oxidative/region stat/',plant_label[i],'.csv'))
  plot_data[,5] <- sprintf("%0.3f", plot_data[,5])
  plot_data[,5] <- as.numeric(plot_data[,5])*100
  ggplot(data=plot_data, aes(reorder(Region, pre.risk1), pre.risk1)) +
    geom_bar(stat='identity', width = 0.8, color = 'black', 
             size = 0.25, fill = '#717731',alpha = 1) + 
    geom_text(aes(label = pre.risk1), show.legend = F) +
    labs(x="continent",y="value") +
    coord_flip() + theme_bw()
  ggsave(paste0('Global/predict result/uncertainty/Oxidative/region stat/',
                plant_label[i],'-risk1','.pdf'), width=8,height=5)
}


###uptake stress region histogram plot
for (n in 1:3){
  frame <- read.xlsx('Global/predict result/uncertainty/Content/Content-200run.xlsx', n)
  frame <- frame[,1:3]
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value')
  frame[,1] <- sapply(frame$X,trans)
  frame[,2] <- sapply(frame$Y,trans)
  map <- merge(region_frame, frame, by = c("X","Y"), , all = TRUE)
  result <- data.frame()
  for (i in 1:6){
    continent_one <- subset(map, map$Region == region_label[i]&map$value > -100)
    aver <- mean(continent_one$value)
    sd_v <- sd(continent_one$value)
    result <- rbind(result,cbind(region_label[i], aver, sd_v))
  }
  colnames(result) <- c('Region', 'aver', 'sd_v')
  write.csv(result, paste0('Global/predict result/uncertainty/Content/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/uncertainty/Content/region stat/',plant_label[i],'.csv'))
  ggplot(plot_data, aes(x=Region, y=aver)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=aver-sd_v, ymax=aver+sd_v),size=0.75,width=0.08,position=position_dodge(0.45))+
    labs(x="continent",y="value")
  theme_bw()
  ggsave(paste0('Global/predict result/uncertainty/Content/region stat/',
                plant_label[i],'-bar','.pdf'), width=8,height=5)
}


###length stress region histogram plot
for (n in 1:3){
  frame <- read.xlsx('Global/predict result/uncertainty/Length/Length-200run.xlsx', n)
  frame <- frame[,1:3]
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value')
  frame[,1] <- sapply(frame$X,trans)
  frame[,2] <- sapply(frame$Y,trans)
  map <- merge(region_frame, frame, by = c("X","Y"), , all = TRUE)
  result <- data.frame()
  for (i in 1:6){
    continent_one <- subset(map, map$Region == region_label[i]&map$value > -100)
    aver <- mean(continent_one$value)
    sd_v <- sd(continent_one$value)
    result <- rbind(result,cbind(region_label[i], aver, sd_v))
  }
  colnames(result) <- c('Region', 'aver', 'sd_v')
  write.csv(result, paste0('Global/predict result/uncertainty/Length/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/uncertainty/Length/region stat/',plant_label[i],'.csv'))
  ggplot(plot_data, aes(x=Region, y=aver)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=aver-sd_v, ymax=aver+sd_v),size=0.75,width=0.08,position=position_dodge(0.45))+
    labs(x="continent",y="value")
  theme_bw()
  ggsave(paste0('Global/predict result/uncertainty/Length/region stat/',
                plant_label[i],'-bar','.pdf'), width=8,height=5)
}




setwd("D:/master/project ing/plant-NP/model0118")
base_map <- raster('Global/crop map/base map.tif')
base_frame <- as.data.frame(base_map, xy=T)
country_frame <- na.omit(base_frame)

China  <- country_frame[country_frame$base_map == '44', ]
India  <- country_frame[country_frame$base_map == '100', ]















