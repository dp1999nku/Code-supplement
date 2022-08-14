#####################################################################
#####                    permutation test                       #####
#####################################################################
setwd("D:/master/project ing/plant-NP/model0118/R rf for plant")
q2plot<-list()

for (i in 1:13){
  permutation<-read.xlsx('permutation.xlsx',i)
  lm.model<-lm(permutation$q2~permutation$r2)
  coef<-lm.model$coefficients
  q2plot[[label[i]]]<-local({
    coef=coef
    permutation=permutation
    ggplot(data=permutation,aes(r2,q2))+
      geom_point(size=2.5,color='#ca6321')+
      geom_abline(intercept=coef[1],slope=coef[2],size=1,color='black')+
      scale_x_continuous(limits=c(0,1))+
      scale_y_continuous(limits=c(-2,1))+
      labs(title=label[i])+
      coord_fixed(ratio=2/3)+
      theme_bw()+
      theme(axis.line=element_line(color='black'),
            axis.ticks.length=unit(0.3,'line'))+
      xlab(NULL)+
      ylab(NULL)+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      annotate('text', x=0.2,y=1,label=paste0('intercept: ',round(coef[1],2)))
  })
}

q2p<-q2plot[[1]]
for (i in 2:13){
  q2p<-q2p+q2plot[[i]]
}
q2p<-q2p+plot_layout(ncol=5)
print(q2p)
ggsave(filename = 'plot/permutation test.pdf',width=15,height=13.5)
