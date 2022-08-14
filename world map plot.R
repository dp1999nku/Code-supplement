############################world map plot
library(ggplot2)
library(plyr)
library(maptools)
library(raster)
setwd("D:/master/project ing/plant-NP/model0118/Global")
###match coor
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

################################################################################
##############base map#################
base_map <- raster('crop map/base map.tif')
base_frame <- as.data.frame(base_map, xy=T)
base_frame[is.na(base_frame)] <-  9999
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame$value[base_frame$value < 9999] <- 999
base_frame <- data.frame(matrix(as.numeric(unlist(base_frame)), ncol=3, byrow=F))
colnames(base_frame) <- c('X', 'Y', 'value')
base_frame[,1] <- sapply(base_frame$X,trans)
base_frame[,2] <- sapply(base_frame$Y,trans)

################################################################################
###########################ZnO-Content map######################################
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c(excel_sheets('Content/Content-stress.xlsx'))
for (i in 1:3){
  frame <- read.csv(paste0('Content/',label[i],'-Fe2O3-Content.csv'))
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value')
  frame[,1] <- sapply(frame$X,trans)
  frame[,2] <- sapply(frame$Y,trans)
  
  map <- merge(base_frame, frame, by = c("X","Y"), , all = TRUE) 
  map[is.na(map)] <-  0
  map[,5] <- map[,3] + map[,4]
  map[,5] <- sapply(map$V5,final)
  map <- map[,-3]
  map <- map[,-3]
  colnames(map) <- c('Longitude', 'Latitude', 'value')
  write.csv(map, paste0('Content/',label[i],'-Fe2O3-Content map.csv'), row.names = FALSE)
}

###########################Fe2O3-Length map#####################################
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c(excel_sheets('Length/Length-stress.xlsx'))
for (i in 1:3){
  frame <- read.csv(paste0('Length/',label[i],'-Fe2O3-Length.csv'))
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value')
  frame[,1] <- sapply(frame$X,trans)
  frame[,2] <- sapply(frame$Y,trans)
  
  map <- merge(base_frame, frame, by = c("X","Y"), all = TRUE) 
  map[is.na(map)] <-  0
  map[,5] <- map[,3] + map[,4]
  map[,5] <- sapply(map$V5,final)
  map <- map[,-3]
  map <- map[,-3]
  colnames(map) <- c('Longitude', 'Latitude', 'value')
  write.csv(map, paste0('Length/',label[i],'-Fe2O3-Length map.csv'), row.names = FALSE)
}

###########################ZnO-Oxidative map#####################################
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c('bean', 'wheat', 'maize')
for (i in 1:3){
  frame <- read.csv(paste0('Oxidative/',label[i],'-ZnO-Oxidative.csv'))
  frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
  colnames(frame) <- c('X', 'Y', 'value')
  frame[,1] <- sapply(frame$X,trans)
  frame[,2] <- sapply(frame$Y,trans)
  
  map <- merge(base_frame, frame, by = c("X","Y"), , all = TRUE) 
  map[is.na(map)] <-  0
  map[,5] <- map[,3] + map[,4]
  map[,5] <- sapply(map$V5,final)
  map <- map[,-3]
  map <- map[,-3]
  colnames(map) <- c('Longitude', 'Latitude', 'value')
  write.csv(map, paste0('Oxidative/',label[i],'-ZnO-Oxidative map.csv'), row.names = FALSE)
}

###class
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c('bean', 'wheat', 'maize')
for (i in 1:3){
  frame <- read.csv(paste0('Oxidative/',label[i],'-ZnO-Oxidative-class-20.csv'))
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
  write.csv(map, paste0('Oxidative/',label[i],'-ZnO-Oxidative map class 20.csv'), row.names = FALSE)
}

###########################Fe2O3-Chl map#####################################
setwd("D:/master/project ing/plant-NP/model0118/Global/predict result")
label <- c(excel_sheets('Chl/Chl-stress.xlsx'))
label_crop <- c('bean', 'wheat', 'maize')
for (i in 1:2){
  for (n in 1:3){
    frame <- read.xlsx(paste0('Chl/',label[i],'-Fe2O3-Chl.xlsx'), n)
    frame <- data.frame(matrix(as.numeric(unlist(frame)), ncol=3, byrow=F))
    colnames(frame) <- c('X', 'Y', 'value')
    frame[,1] <- sapply(frame$X,trans)
    frame[,2] <- sapply(frame$Y,trans)
    
    map <- merge(base_frame, frame, by = c("X","Y"), , all = TRUE) 
    map[is.na(map)] <-  0
    map[,5] <- map[,3] + map[,4]
    map[,5] <- sapply(map$V5,final)
    map <- map[,-3]
    map <- map[,-3]
    colnames(map) <- c('Longitude', 'Latitude', 'value')
    write.csv(map, paste0('Chl/',label_crop[n],'-Fe2O3-', label[i], ' map.csv'), row.names = FALSE)
    print(n)
  }
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
  frame <- read.csv(paste0('Global/predict result/Oxidative/', plant_label[n], '-ZnO-Oxidative map class 20.csv'))
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
  write.csv(result, paste0('Global/predict result/Oxidative/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/Oxidative/region stat/',plant_label[i],'.csv'))
  plot_data[,6] <- sprintf("%0.3f", plot_data[,6])
  plot_data[,6] <- as.numeric(plot_data[,6])*100
  ggplot(data=plot_data, aes(reorder(Region, pre.risk2), pre.risk2)) +
    geom_bar(stat='identity', width = 0.8, color = 'black', 
             size = 0.25, fill = '#717731',alpha = 1) + 
    geom_text(aes(label = pre.risk2), show.legend = F) +
    labs(x="continent",y="value") +
    coord_flip() + theme_bw()
  ggsave(paste0('Global/predict result/Oxidative/region stat/',
                plant_label[i],'-risk2','.pdf'), width=8,height=5)
}


###uptake stress region histogram plot
for (n in 1:3){
  frame <- read.csv(paste0('Global/predict result/Content/', plant_label[n], '-ZnO-Content.csv'))
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
  write.csv(result, paste0('Global/predict result/Content/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/Content/region stat/',plant_label[i],'.csv'))
  ggplot(plot_data, aes(x=Region, y=aver)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=aver-sd_v, ymax=aver+sd_v),size=0.75,width=0.08,position=position_dodge(0.45))+
    labs(x="continent",y="value")
    theme_bw()
  ggsave(paste0('Global/predict result/Content/region stat/',
                plant_label[i],'-bar','.pdf'), width=8,height=5)
}


###length stress region histogram plot
for (n in 1:3){
  frame <- read.csv(paste0('Global/predict result/Length/', plant_label[n], '-Fe2O3-Length.csv'))
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
  write.csv(result, paste0('Global/predict result/Length/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/Length/region stat/',plant_label[i],'.csv'))
  ggplot(plot_data, aes(x=Region, y=aver)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=aver-sd_v, ymax=aver+sd_v),size=0.75,width=0.08,position=position_dodge(0.45))+
    labs(x="continent",y="value")
  theme_bw()
  ggsave(paste0('Global/predict result/Length/region stat/',
                plant_label[i],'-bar','.pdf'), width=8,height=5)
}


###Chla stress region histogram plot
for (n in 1:3){
  frame <- read.xlsx('Global/predict result/Chl/Chla-Fe2O3-Chl.xlsx', n)
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
  write.csv(result, paste0('Global/predict result/Chl/region stat/',
                           plant_label[n],'.csv'), row.names = FALSE)
}

for (i in 1:3){
  plot_data <- read.csv(paste0('Global/predict result/Chl/region stat/',plant_label[i],'.csv'))
  ggplot(plot_data, aes(x=Region, y=aver)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=aver-sd_v, ymax=aver+sd_v),size=0.75,width=0.08,position=position_dodge(0.45))+
    labs(x="continent",y="value") +
    theme_bw()
  ggsave(paste0('Global/predict result/Chl/region stat/',
                plant_label[i],'-bar','.pdf'), width=8,height=5)
}









######################################################Latitude classification
library(ggalt)
plant_label <- c('bean', 'maize', 'wheat')
###oxidative stress
result <- data.frame()
for (i in 1:3){
  data <- read.csv(paste0('Raster/',plant_label[i],' pca abs analysis.csv'))
  data_Lat <- unique(data[,2])
  result_one <- data.frame()
  for (n in 1:length(data_Lat)){
    set <- subset(data, data$Y == data_Lat[n])
    average <- mean(set$value)
    result_one <-  rbind(result_one, cbind(data_Lat[n], average))
  }
  result_one[,3] <- plant_label[i]
  result <- rbind(result, result_one)
  print(i)
}
colnames(result) <- c('Latitude', 'value', 'plant')

ggplot(result, aes(x=Latitude, y=value, group=plant, color=plant)) + xlim(-90,90)+
  geom_xspline(spline_shape = -0.4,size=1) + coord_flip() + theme_bw()
ggsave('Raster/oxidative stress/region stat/Latitude classification.pdf', width=5,height=8)
dev.off()


###uptake stress
result <- data.frame()
for (i in 1:3){
  data <- read.csv(paste0('Global/predict result/Content/', plant_label[i], '-ZnO-Content.csv'))
  data_Lat <- unique(data[,2])
  result_one <- data.frame()
  for (n in 1:length(data_Lat)){
    set <- subset(data, data$Y == data_Lat[n])
    average <- mean(set$value)
    result_one <-  rbind(result_one, cbind(data_Lat[n], average))
  }
  result_one[,3] <- plant_label[i]
  result <- rbind(result, result_one)
  print(i)
}
colnames(result) <- c('Latitude', 'value', 'plant')

ggplot(result, aes(x=Latitude, y=value, group=plant, color=plant)) + xlim(-90,90)+
  geom_xspline(spline_shape = -0.4,size=0.1) + coord_flip() + theme_bw()
ggsave('Global/predict result/Content/region stat/Latitude classification.pdf', width=8,height=8)
dev.off()


###length stress
result <- data.frame()
for (i in 1:3){
  data <- read.csv(paste0('Raster/Length stress/Length_', plant_label[i], '.csv'))
  data_Lat <- unique(data[,2])
  result_one <- data.frame()
  for (n in 1:length(data_Lat)){
    set <- subset(data, data$Y == data_Lat[n])
    average <- mean(set$value)
    result_one <-  rbind(result_one, cbind(data_Lat[n], average))
  }
  result_one[,3] <- plant_label[i]
  result <- rbind(result, result_one)
  print(i)
}
colnames(result) <- c('Latitude', 'value', 'plant')

ggplot(result, aes(x=Latitude, y=value, group=plant, color=plant)) + xlim(-90,90)+
  geom_xspline(spline_shape = -0.4,size=1) + coord_flip() + theme_bw()
ggsave('Raster/uptake stress/region stat/Latitude classification.pdf', width=8,height=8)
dev.off()







setwd("D:/master/project ing/plant-NP/model0118/Global/predict result/Chl")
data <- read.xlsx('Chla-Fe2O3-Chl.xlsx', 3)
max(data[,3])
min(data[,3])
















