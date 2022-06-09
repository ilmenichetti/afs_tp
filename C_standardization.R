

#reading the file

library(readxl)
library(RColorBrewer)

dataset<-read_excel("afs_data_tp_github.xlsx")
colnames(dataset)

unique(dataset$First_Author)

AFS_palette<-brewer.pal(8, "Dark2")

dataset$AFS_classification<-as.factor(dataset$AFS_classification)

plot(dataset$AFS_Stock_t_ha/dataset$Depth_cm, dataset$Reference_depth, ylim=c(150,0), 
     col=AFS_palette[dataset$AFS_classification], 
     pch=as.numeric(dataset$AFS_classification), xlim=c(0,4))

legend("bottomright", levels(dataset$AFS_classification), col=AFS_palette, pch=c(1:8), bty="n")

text(dataset$AFS_Stock_t_ha, dataset$Reference_depth+5, cex=0.6, dataset$ID)

