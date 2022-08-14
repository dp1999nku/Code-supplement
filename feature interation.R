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
library(ggepi)
library(dplyr)
library(caret)
library(igraph)
library(ggtern)
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


#####DW
data <- data.frame()
for (i in 1:5){
  one <- read.csv(paste0('3interaction/DW_result/50/',i,'.csv'))
  data <- rbind(data, one)
}
data <- data[,-1]
data[,1] <-data[,1]/10
data[,2] <- (data[,2]-7)/100
data[,3] <- (data[,3]-22)/10
colnames(data) <- c('TC','Duration', 'DT', 'value')
#data$value <- round(data$value, 4)
#bk <- c(seq(-0.12,-0.0001,by=0.0001),seq(0, 0, 0), seq(0.0001, 0.1,by=0.0001))

#ggtern(data, aes(TC, Duration, DT,color = value))+
#  geom_point(size=0.5) + 
  #scale_color_steps(low = "blue", high = "red",
  #                  breaks = c(-Inf, 0, Inf)) +
  #scale_color_stepsn(colors = c(colorRampPalette(colors = c("#f9e422","white"))(1200),
  #                             colorRampPalette(colors = c("white","white"))(1),
  #                             colorRampPalette(colors = c("white","#44045a"))(1000))) +
  #scale_color_manual(values = c(colorRampPalette(colors = c("#f9e422","white"))(1200),
  #                             colorRampPalette(colors = c("white","white"))(1),
  #                             colorRampPalette(colors = c("white","#44045a"))(1000))) +
  #scale_color_steps2(low = "#00d538", mid = 'white', high = "#ff5e24") +
  #scale_fill_gradient(low='#00d538', high='#ff5e24') +
#  scale_color_gradient2(low = muted('#594B98'), mid = '#F9E9A1', high = muted('#CC414C'), midpoint = 0) +
#  theme_rgbw()
data_a <- subset(data,value<=-0.12)
data_b <- subset(data,value>-0.12&value<=-0.1)
data_c <- subset(data,value>-0.1&value<=-0.08)
data_d <- subset(data,value>-0.08&value<=-0.06)
data_e <- subset(data,value >-0.06&value <= -0.04)
data_f <- subset(data,value>-0.04&value<=-0.02)
data_g <- subset(data,value>-0.02&value<=0)
data_h <- subset(data,value> 0 &value<=0.02)
data_i <- subset(data,value>0.02&value<=0.04)
data_j <- subset(data,value > 0.04&value <= 0.06)
data_k <- subset(data,value>0.06&value<=0.08)

data_a[,5] <- 'A'
data_b[,5] <- 'B'
data_c[,5] <- 'C'
data_d[,5] <- 'D'
data_e[,5] <- 'E'
data_f[,5] <- 'F'
data_g[,5] <- 'G'
data_h[,5] <- 'H'
data_i[,5] <- 'I'
data_j[,5] <- 'J'
data_k[,5] <- 'K'

data_plot <- rbind(data_a,data_b,data_c,data_d,data_e,data_f,data_g,data_h,data_i,data_j,data_k)
colnames(data_plot) <- c('TC','Duration', 'DT', 'value', 'Type')

cols <- c("A"="#4575B5","B"="#6E8FB8","C"="#99AEBD","D"="#C0CCBE",
          "E"="#E9EDBE","F"="#FFE9AD","G"="#FAB984","H"="#F28D61",
          "I"="#E66043","J"="#D62F27","K"="#990F26")

ggtern(data_plot, aes(TC, Duration, DT,color = Type))+
  geom_point(size=0.3, alpha=1, shape=15) + 
  scale_color_manual(values = cols) +
  theme_rgbw()



#####MDA
setwd("C:/Users/admin/Desktop/plant-NP/model0118/R rf for plant")
data <- read.xlsx('3interaction/MDA-TC-Duration-Age.xlsx',1)
model_data <- read.xlsx('3interaction/plant_data.xlsx', 6)
rf <- randomForest(label~. , data = model_data, 
                   ntree=500,mtry=14,
                   proximity = T,
                   importance = T)
frame_all <- data.frame()
for (i in seq(0,2,0.2)){
  data$TC <- i
  for (k in seq(2,102,2)){
    data$Duration <- k
    for (r in seq(0,50,1)){
      data$Age <- r
      if (r <= 0){
        data$TOSeeds=1
        data$TOLeaf=0
        data$TORoot=0
      }else{
        data$TOSeeds=0
        data$TORoot=1
        data$TOLeaf=0
      }
      if (r <= 0){
        data$GSGermination=1
        data$GSSeedling=0
        data$GSVegetative=0
      }else if(r<=15&r>=1){
        data$GSGermination=0
        data$GSSeedling=1
        data$GSVegetative=0
      }else{
        data$GSGermination=0
        data$GSSeedling=0
        data$GSVegetative=1
      }
      p <- predict(rf, data)
      frame <- cbind(i,k,r,p)
      frame_all <- rbind(frame_all, frame)
    }
    print(k)
  }
  print(i)
}
write.csv(frame_all, '3interaction/MDA_result/1.csv')


data <- data.frame()
for (i in 1:5){
  one <- read.csv(paste0('3interaction/MDA_result/',i,'.csv'))
  data <- rbind(data, one)
}
data <- data[,-1]
data[,1] <-data[,1]/10
data[,2] <- (data[,2]-2)/100
data[,3] <- (data[,3])/50
colnames(data) <- c('TC','Duration', 'Age', 'value')

data_a <- subset(data,value<=0)
data_b <- subset(data,value>0&value<=0.05)
data_c <- subset(data,value>0.05&value<=0.1)
data_d <- subset(data,value>0.1&value<=0.15)
data_e <- subset(data,value>0.15&value<=0.2)
data_f <- subset(data,value>0.2&value<=0.25)
data_g <- subset(data,value>0.25&value<=0.3)
data_h <- subset(data,value>0.3&value<=0.35)
data_i <- subset(data,value>0.35&value<=0.4)
data_j <- subset(data,value>0.4&value<=0.45)

data_a[,5] <- 'A'
data_b[,5] <- 'B'
data_c[,5] <- 'C'
data_d[,5] <- 'D'
data_e[,5] <- 'E'
data_f[,5] <- 'F'
data_g[,5] <- 'G'
data_h[,5] <- 'H'
data_i[,5] <- 'I'
data_j[,5] <- 'J'

data_plot <- rbind(data_a,data_b,data_c,data_d,data_e,data_f,data_g,data_h,
                   data_i,data_j)
colnames(data_plot) <- c('TC','Duration', 'Age', 'value', 'Type')

cols <- c("A"="#4575B5",
          "B"="#6E8FB8","C"="#99AEBD","D"="#C0CCBE",
          "E"="#E9EDBE","F"="#FFE9AD","G"="#FAB984",
          "H"="#F28D61","I"="#E66043","J"="#D62F27")

ggtern(data_plot, aes(TC, Duration, Age,color = Type))+
  geom_point(size=0.3, alpha=1, shape=15) + 
  scale_color_manual(values = cols) +
  theme_rgbw()




###Content
setwd("C:/Users/admin/Desktop/plant-NP/model0118/R rf for plant")
data <- read.xlsx('3interaction/Content-TC-DT-illumination.xlsx',1)
model_data <- read.xlsx('plant_data.xlsx', 10)
rf <- randomForest(label~. , data = model_data, 
                   ntree=500,mtry=14,
                   proximity = T,
                   importance = T)

frame_all <- data.frame()
for (i in seq(0,10,1)){
  data$TC <- i
  for (k in seq(22,32,0.2)){
    data$DT <- k
    for (r in seq(150,400,5)){
      data$illumination <- r
      p <- predict(rf, data)
      frame <- cbind(i,k,r,p)
      frame_all <- rbind(frame_all, frame)
    }
    print(k)
  }
  print(i)
}
write.csv(frame_all, '3interaction/Content_result/1.csv')

data <- data.frame()
for (i in 1:5){
  one <- read.csv(paste0('3interaction/Content_result/',i,'.csv'))
  data <- rbind(data, one)
}
data <- data[,-1]
data[,1] <-data[,1]/50
data[,2] <- (data[,2]-22)/10
data[,3] <- (data[,3]-150)/250
colnames(data) <- c('TC','DT', 'illumination', 'value')

data_a <- subset(data,value>0&value<=0.05)
#data_b <- subset(data,value>0.05&value<=0.1)
#data_c <- subset(data,value>0.1&value<=0.15)
#data_d <- subset(data,value>0.15&value<=0.2)
data_e <- subset(data,value>0.2&value<=0.25)
data_f <- subset(data,value>0.25&value<=0.3)
data_g <- subset(data,value>0.3&value<=0.35)
data_h <- subset(data,value>0.35&value<=0.4)
data_i <- subset(data,value>0.4&value<=0.45)
data_j <- subset(data,value>0.45&value<=0.5)
data_k <- subset(data,value>0.5&value<=0.55)

data_a[,5] <- 'A'
#data_b[,5] <- 'B'
#data_c[,5] <- 'C'
#data_d[,5] <- 'D'
data_e[,5] <- 'E'
data_f[,5] <- 'F'
data_g[,5] <- 'G'
data_h[,5] <- 'H'
data_i[,5] <- 'I'
data_j[,5] <- 'J'
data_k[,5] <- 'K'

data_plot <- rbind(data_a,data_e,data_f,data_g,data_h,data_i,data_j,data_k)
colnames(data_plot) <- c('TC','DT', 'illumination', 'value', 'Type')

cols <- c("A"="#4575B5",
          "B"="#6E8FB8","C"="#99AEBD","D"="#C0CCBE",
          "E"="#99AEBD","F"="#C0CCBE","G"="#E9EDBE",
          "H"="#FFE9AD","I"="#F28D61","J"="#E66043","K"="#D62F27")

ggtern(data_plot, aes(TC, DT, illumination,color = Type))+
  geom_point(size=0.3, alpha=1, shape=15) + 
  scale_color_manual(values = cols) +
  theme_rgbw()


###Chlb
setwd("C:/Users/admin/Desktop/plant-NP/model0118/R rf for plant")
data <- read.xlsx('3interaction/Chlb-TC-Duration-NT.xlsx',1)
model_data <- read.xlsx('plant_data.xlsx', 9)
rf <- randomForest(label~. , data = model_data, 
                   ntree=500,mtry=14,
                   proximity = T,
                   importance = T)

frame_all <- data.frame()
for (i in seq(0,10,1)){
  data$TC <- i
  for (k in seq(7,107,2)){
    data$Duration <- k
    for (r in seq(4,29,0.5)){
      data$DT <- r
      p <- predict(rf, data)
      frame <- cbind(i,k,r,p)
      frame_all <- rbind(frame_all, frame)
    }
    print(k)
  }
  print(i)
}
write.csv(frame_all, '3interaction/Chlb_result/1.csv')


data <- data.frame()
for (i in 1:5){
  one <- read.csv(paste0('3interaction/Chlb_result/',i,'.csv'))
  data <- rbind(data, one)
}
data <- data[,-1]
data[,1] <-data[,1]/50
data[,2] <- (data[,2]-7)/100
data[,3] <- (data[,3]-4)/25
colnames(data) <- c('TC','Duration', 'DT', 'value')

data_a <- subset(data,value <= 0)
data_b <- subset(data,value>0&value<=0.02)
data_c <- subset(data,value>0.02&value<=0.04)
data_d <- subset(data,value>0.04&value<=0.06)
data_e <- subset(data,value>0.06&value<=0.08)
data_f <- subset(data,value>0.08&value<=0.1)
data_g <- subset(data,value>0.1&value<=0.12)
data_h <- subset(data,value>0.12&value<=0.14)
data_i <- subset(data,value>0.14&value<=0.16)
data_j <- subset(data,value>0.16)

data_a[,5] <- 'A'
data_b[,5] <- 'B'
data_c[,5] <- 'C'
data_d[,5] <- 'D'
data_e[,5] <- 'E'
data_f[,5] <- 'F'
data_g[,5] <- 'G'
data_h[,5] <- 'H'
data_i[,5] <- 'I'
data_j[,5] <- 'J'

data_plot <- rbind(data_a,data_b,data_c,data_d,data_e,data_f,data_g,data_h,
                   data_i,data_j)
colnames(data_plot) <- c('TC','Duration', 'NT', 'value', 'Type')

cols <- c("A"="#4575B5",
          "B"="#6E8FB8","C"="#99AEBD","D"="#C0CCBE",
          "E"="#E9EDBE","F"="#FFE9AD","G"="#FAB984",
          "H"="#F28D61","I"="#E66043","J"="#D62F27")

ggtern(data_plot, aes(TC, Duration, DT,color = Type))+
  geom_point(size=0.3, alpha=1, shape=15) + 
  scale_color_manual(values = cols) +
  theme_rgbw()


















