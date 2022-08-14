################################################################
#######################similar network##########################
################################################################
library(igraph)
library(randomForest)
library(xlsx)
library(readxl)
library(openxlsx)
library(writexl)

###calculate RF proximity matrix
setwd("C:/Users/admin/Desktop/plant-NP/model0118/network")
label <- c(excel_sheets('plant_data.xlsx'))
set.seed(12345)
prox_wb <- createWorkbook()
for (i in 1:13){
  addWorksheet(prox_wb,sheetName = label[i])
}

for (i in 1:13){
  rf.data <- read.xlsx('plant_data.xlsx', i)
  rf <- randomForest(label~. , data = rf.data, 
                     ntree=500,mtry=14,
                     proximity = T,
                     importance = F)
  prox <- rf$proximity
  writeData(prox_wb, sheet = i, prox)
  print(i)
}
saveWorkbook(prox_wb, "prox.xlsx", overwrite = TRUE)
######NPs type
#NP = c('Ag', 'Cu', 'Fe', 'Zn', 'nZVI', 'CuO', 'ZnO', 'MgO', 'Fe2O3', 'Fe3O4',
#        'TiO2', 'CeO2', 'SiO2', 'Al2O3', 'Graphene', 'MWCNTs', 'PS')

#color = c('#ffb6b9', '#5BE7C4', '#7A57D1', '#EAFFD0', '#F43E71', '#40afb1',
'#78fee0', '#6B78B4', '#f67504', '#6abe83', '#e29933', '#f3ee6b', 
#          '#7BCED7', '#a68572', '#2E94B9','#f6d6a2', '#a696c8')
TYPE_COLOR <- matrix(c('Ag', 'Cu', 'Fe', 'Zn', 'nZVI', 'CuO', 'ZnO', 'MgO', 'Fe2O3', 'Fe3O4',
                       'TiO2', 'CeO2', 'SiO2', 'Al2O3', 'Graphene', 'MWCNTs', 'PS',
                       '#ffb6b9', '#5BE7C4', '#7A57D1', '#EAFFD0', '#F43E71', '#40afb1',
                       '#78fee0', '#6B78B4', '#f67504', '#6abe83', '#e29933', '#f3ee6b', 
                       '#7BCED7', '#a68572', '#2E94B9','#f6d6a2', '#a696c8'),ncol=2,byrow=F)
mul <- c(5, 5, 3, 4, 5, 5, 5, 3, 4, 3, 3, 3,3)

for (i in 1:5){
  n <- i
  m <- mul[i]
  rf.data <- read.xlsx('plant_data.xlsx', n)
  data <- as.matrix(read.xlsx('prox.xlsx', n))
  diag(data) <- 0
  data[data < m*mean(data)] <- 0
  data[data >= m*mean(data)] <- 1
  net<-graph.adjacency(adjmatrix=data,mode="undirected")
  graph.density(net)
  
  col.data <- c(rf.data$NPs)
  for (i in 1:length(col.data)) {
    if (col.data[i]%in%TYPE_COLOR[,1]) {
      col.data[i]=TYPE_COLOR[which(TYPE_COLOR[,1]==col.data[i]),2]
    }
  }
  col.data <- as.data.frame(col.data)
  colnames(col.data) = 'col'
  l_color = c(unique(rf.data$NPs))
  for (i in 1:length(l_color)) {
    if (l_color[i]%in%TYPE_COLOR[,1]) {
      l_color[i]=TYPE_COLOR[which(TYPE_COLOR[,1]==l_color[i]),2]
    }
  }
  
  plot(net,layout= layout_with_kk,
       vertex.size=6,
       vertex.color=as.character(col.data$col),
       vertex.label="",
       vertex.label.size=0.5,
       vertex.label.cex=1,
       vertex.label.dist=0,
       vertex.label.color="black")
  legend("topleft",c(unique(rf.data$NPs)),
         pch=16, cex=1,
         col=l_color)
  ###自动保存函数有误
}
dev.off()


######Shape
#Shape = c('Dot', 'Sphere', 'Rod', 'Tube', 'Sheet', 'Irregular')

#color = c('#f3ee6b', '#6abe83', '#ffb6b9', '#40afb1', '#f43e71', '#78fee0')
Shape_COLOR <- matrix(c('Dot', 'Sphere', 'Rod', 'Tube', 'Sheet', 'Irregular',
                        '#f3ee6b', '#6abe83', '#ffb6b9', '#40afb1', '#f43e71',
                        '#78fee0'),ncol=2,byrow=F)
#num <- c(1,3,6,13,17)###MDA, length, DW, content, TF
mul <- c(5, 5, 3, 4, 5, 5, 5, 3, 4, 3, 3, 3,3)

for (i in 1:5){
  n <- i
  m <- mul[i]
  rf.data <- read.xlsx('plant_data.xlsx', n)
  data <- as.matrix(read.xlsx('prox.xlsx', n))
  diag(data) <- 0
  data[data < m*mean(data)] <- 0
  data[data >= m*mean(data)] <- 1
  net<-graph.adjacency(adjmatrix=data,mode="undirected")
  graph.density(net)
  
  col.data <- c(rf.data$Shape)
  for (i in 1:length(col.data)) {
    if (col.data[i]%in%Shape_COLOR[,1]) {
      col.data[i]=Shape_COLOR[which(Shape_COLOR[,1]==col.data[i]),2]
    }
  }
  col.data <- as.data.frame(col.data)
  colnames(col.data) = 'col'
  l_color = c(unique(rf.data$Shape))
  for (i in 1:length(l_color)) {
    if (l_color[i]%in%Shape_COLOR[,1]) {
      l_color[i]=Shape_COLOR[which(Shape_COLOR[,1]==l_color[i]),2]
    }
  }
  
  plot(net,layout= layout_with_kk,
       vertex.size=6,
       vertex.color=as.character(col.data$col),
       vertex.label="",
       vertex.label.size=0.5,
       vertex.label.cex=1,
       vertex.label.dist=0,
       vertex.label.color="black")
  legend("topleft",c(unique(rf.data$Shape)),
         pch=16, cex=1,
         col=l_color)
  ###自动保存函数有误
}
dev.off()



