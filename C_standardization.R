

#reading the file

library(readxl) #to read the excel file
library(RColorBrewer) #to get some pre-made color palettes (just a bit quicker than selecting them manually)
library(dfoptim) #this package has some good optimization functions
library(hydroGOF) #loading some cost functions for the optimizer

#PACKAGES TO WORK WITH TIME SERIES
library(TSdist) #TS clustering algorithm
library(dtwclust) #TS clustering algorithm
library(zoo) #dealing with irregular time series
library(dplyr)
library(MASS) #for the kernel density estimation

dataset<-read_excel("afs_data_tp_github.xlsx")
dataset$Final_depth<-as.numeric(dataset$Final_depth)

dataset_stocks_delta<-dataset$AFS_Stock_t_ha - dataset$Control_Stock_ton_ha
dataset_stocks_delta_ratio<-100*dataset_stocks_delta/dataset$Control_Stock_ton_ha

dataset<-cbind(dataset, dataset_stocks_delta, dataset_stocks_delta_ratio)

colnames(dataset)
colnames(dataset)[52:53]<-c("C_stocks_delta_t_ha", "C_stocks_delta_procent")






#starting some plotting to get an idea of the dataset
AFS_palette<-brewer.pal(8, "Dark2")

dataset$AFS_classification<-as.factor(dataset$AFS_classification)

plot(dataset$AFS_Stock_t_ha/dataset$Depth_cm, dataset$Reference_depth, ylim=c(150,0), 
     col=AFS_palette[dataset$AFS_classification], 
     pch=as.numeric(dataset$AFS_classification), xlim=c(0,4))

legend("bottomright", levels(dataset$AFS_classification), col=AFS_palette, pch=c(1:8), bty="n")

#text(dataset$AFS_Stock_t_ha/dataset$Depth_cm, dataset$Reference_depth+5, cex=0.6, dataset$ID)

plot(dataset$C_stocks_delta_procent, dataset$Reference_depth, ylim=c(150,0), 
     col=AFS_palette[dataset$AFS_classification], 
     pch=as.numeric(dataset$AFS_classification))



###### Depth SOC distribution

#defining the unique ID of each profile
profile_ID_interactions<-interaction(as.factor(dataset$First_Author), as.factor(dataset$AFS_description), as.factor(dataset$AFS_age_yrs))
profile_ID<-as.factor(as.numeric(as.factor(as.numeric(profile_ID_interactions))))

profile_ID_interactions[11]
profile_ID[11]
dataset_profiles[profile_ID==114,]

#rebuilding the dataset with the profile_ID, excluding the ones without it
dataset_profiles<-cbind(profile_ID, dataset)
dataset_profiles_full<-dataset_profiles

dataset_profiles_NA<-cbind(dataset_profiles, is.na(dataset_profiles$profile_ID))
dataset_profiles<-dataset_profiles[!is.na(dataset_profiles$profile_ID),]


# TODO: there are quite some NAs because the AFS_description has NAs. We exclude 217 rows of 1186. Most seems reasonable to exclude, only one layer, but I am not sure about all of them. Thiago, could you check?
dataset$First_Author[is.na(profile_ID)]
dataset$AFS_description[is.na(profile_ID)]
dataset_NA_profiles<-dataset[is.na(profile_ID),]

dataset_profiles[dataset_profiles$profile_ID==3,]

#excluding the lines with only one row
duplicated<-(duplicated(dataset_profiles$profile_ID)|duplicated(dataset_profiles$profile_ID, fromLast=TRUE))
dataset_profiles_cleaned<-dataset_profiles[duplicated,]

na<-is.na(dataset_profiles_cleaned$C_stocks_delta_t_ha) | is.na(dataset_profiles_cleaned$Initial_depth)
dataset_profiles_cleaned<-dataset_profiles_cleaned[!na,]



# a very messy loop to try to indentify where there are repeated depths
profiles<-unique(dataset_profiles_cleaned$profile_ID)
repeated_depth<-c()
counter=0
for(i in 1:length(profiles)){
  subset<-dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==profiles[i],]
    duplicated_depth<-(duplicated(subset$Final_depth)|duplicated(subset$Final_depth, fromLast=TRUE))
  for(j in 1:length(duplicated_depth)){
    counter=counter+1
    repeated_depth[counter]<-duplicated_depth[j]
    }
  }

dataset_profiles_repeated_depth<-dataset_profiles_cleaned[repeated_depth,]

dim(dataset_profiles_full)
dim(dataset_profiles_cleaned)
discarded<-dataset_profiles_full$profile_ID %in% dataset_profiles_cleaned$profile_ID

write.csv(cbind(!discarded, dataset_profiles_full), "dataset_profiles_cleaned.csv")

#plotting the profiles
plot(dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$C_stocks_delta_t_ha/
       dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$C_stocks_delta_t_ha[1],
     dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$Initial_depth, type = "l", ylim=c(150,0), xlim=c(-15,50), xlab = "C stocks (t ha)", ylab="Depth (cm)")
for(i in 1:length(unique(dataset_profiles_cleaned$profile_ID))){
  lines(dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$C_stocks_delta_t_ha/
          dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$C_stocks_delta_t_ha[1],
       dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$Initial_depth, type = "l")
  
  }

#plotting the profiles
plot(dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$C_stocks_delta_t_ha/
       dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$C_stocks_delta_t_ha[1],
     dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$Initial_depth,  ylim=c(150,0), xlim=c(-10,10), xlab = "C stocks (proportion of topsoil)", ylab="Depth (cm)", pch=NA)
for(i in 1:length(unique(dataset_profiles_cleaned$profile_ID))){
  points(dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$C_stocks_delta_t_ha/
           dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$C_stocks_delta_t_ha[1],
        dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$Initial_depth,
        pch=as.numeric(dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$AFS_classification),
        col=AFS_palette[as.numeric(dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[i],]$AFS_classification)])
}

legend("topright", levels(dataset$AFS_classification), col=AFS_palette, pch=c(1:8), bty="n")


#########  Time series clustering with dynamic time warping (no idea what it means but it seems to work)

options(warn=1)
warning_list<-c()
warning_index=1

TS_list<-list() #here we create an empty list to store all the zoo (irregular time series) objects we will deal with (the profiles)
for(i in 1:length(profiles)){
#for(i in 1:10){
  SOC<-dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==profiles[i],]$C_stocks_delta_t_ha
  depth<-dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==profiles[i],]$Initial_depth
  TS_list[[i]]<-zoo(SOC, depth)
  warning<-tryCatch(zoo(SOC, depth), warning=function(w) w)
  if(!is.numeric(warning)){warning_list[warning_index]=i
                warning_index=warning_index+1}
}



dataset_profiles_cleaned_noerrors<-dataset_profiles_cleaned[!dataset_profiles_cleaned$profile_ID %in% profiles[warning_list],]
dataset_profiles_cleaned_errors<-dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID %in% profiles[warning_list],]

write.csv(dataset_profiles_cleaned_errors, "dataset_profiles_cleaned_errors.csv")

set.seed(55)
profiles<-unique(dataset_profiles_cleaned_noerrors$profile_ID)
TS_list<-list() #here we create an empty list to store all the zoo (irregular time series) objects we will deal with (the profiles)
TS_profiles<-c() #here we create an empty list to store all the zoo (irregular time series) objects we will deal with (the profiles)
for(i in 1:length(profiles)){
  #for(i in 1:10){
  SOC<-dataset_profiles_cleaned_noerrors[dataset_profiles_cleaned_noerrors$profile_ID==profiles[i],]$C_stocks_delta_t_ha
  depth<-dataset_profiles_cleaned_noerrors[dataset_profiles_cleaned_noerrors$profile_ID==profiles[i],]$Initial_depth
  TS_list[[i]]<-zoo(SOC, depth)
  warning<-tryCatch(zoo(SOC, depth), warning=function(w) w)
  if(!is.numeric(warning)){warning_list[warning_index]=i
  warning_index=warning_index+1}
  TS_profiles[i]=dataset_profiles_cleaned_noerrors[dataset_profiles_cleaned_noerrors$profile_ID==profiles[i],]$profile_ID[1]
}


str(rf)

chisq_list<-c()
for(q in 2:8)
{
#clustering with DBW
clusters<-tsclust(TS_list, k=q)
str(clusters)
profile_clusters<-clusters@cluster
unique_profile_clusters<-unique(profile_clusters)

#hierarchical clustering approach
# library("dbscan")
# clusters@distmat
# cl <- hdbscan(clusters@distmat, minPts = 4)
# profile_clusters<-cl$cluster+1
# unique_profile_clusters<-unique(profile_clusters)
# 
profile_palettes<-brewer.pal(8, "Dark2")

dataset_profiles_cleaned_noerrors_profiles<-dataset_profiles_cleaned_noerrors
dataset_profiles_cleaned_noerrors_profiles[ , 'clusters'] <- NA

densities<-list()

#plotting the profiles
png("profile_clustering.png", height=1800, width =3000, res=300)
par(mfrow=c(2,4))
for(j in 1:length(unique_profile_clusters)){
  which_profiles<-TS_profiles[profile_clusters==j]
  subset_plotting<-dataset_profiles_cleaned_noerrors[dataset_profiles_cleaned_noerrors$profile_ID %in% which_profiles,]
  dataset_profiles_cleaned_noerrors_profiles[dataset_profiles_cleaned_noerrors$profile_ID %in% which_profiles,]$clusters=j
 
  densities[[j]]<-kde2d(x=subset_plotting$C_stocks_delta_t_ha, y=subset_plotting$Initial_depth, n = 150, lims=c(-55,80,
                                                                                                               0,150))
   
  plot(dataset_profiles_cleaned_noerrors[dataset_profiles_cleaned_noerrors$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$C_stocks_delta_t_ha,
       dataset_profiles_cleaned_noerrors[dataset_profiles_cleaned$profile_ID==unique(dataset_profiles_cleaned$profile_ID)[1],]$Initial_depth, type = "l", ylim=c(150,0), xlim=c(-55,80), xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)", main=paste("Class", j), col=NA)
    for(i in 1:length(unique(dataset_profiles_cleaned_noerrors$profile_ID))){
      lines(subset_plotting[subset_plotting$profile_ID==unique(subset_plotting$profile_ID)[i],]$C_stocks_delta_t_ha,
            subset_plotting[subset_plotting$profile_ID==unique(subset_plotting$profile_ID)[i],]$Initial_depth, type = "l", col=profile_palettes[j])
            }

  }
dev.off()

library(compare)
chisq<-chisq.test(dataset_profiles_cleaned_noerrors$AFS_classification, as.factor(dataset_profiles_cleaned_noerrors_profiles$clusters))
chisq_list[q] <-chisq$p.value

}

barplot(chisq_list)


library(viridis)
png("profile_clustering_density1.png", height=2500, width =4000, res=300)
filled.contour(densities[[1]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 1), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density2.png", height=2500, width =4000, res=300)
filled.contour(densities[[2]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 2), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density3.png", height=2500, width =4000, res=300)
filled.contour(densities[[3]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 3), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density4.png", height=2500, width =4000, res=300)
filled.contour(densities[[4]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 4), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density5.png", height=2500, width =4000, res=300)
filled.contour(densities[[5]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 5), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density6.png", height=2500, width =4000, res=300)
filled.contour(densities[[6]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 6), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density7.png", height=2500, width =4000, res=300)
filled.contour(densities[[7]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 7), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()
png("profile_clustering_density8.png", height=2500, width =4000, res=300)
filled.contour(densities[[8]],xlab = expression(paste(Delta, "SOC (t h", a^-1, ")" )), ylab="Depth (cm)",  main=paste("Class", 8), ylim=c(150,0),xlim=c(-55,80), color.palette = plasma)
dev.off()


range(dataset_profiles_cleaned_noerrors$C_stocks_delta_t_ha)

write.csv(dataset_profiles_cleaned_noerrors_profiles, "dataset_profiles_cleaned_noerrors_profiles.csv")


library(plyr) 
DF=data.frame(dataset_profiles_cleaned_noerrors$AFS_classification, as.factor(dataset_profiles_cleaned_noerrors_profiles$clusters))
str(DF)
names(DF)<-c("AFS_class", "cluster")
DF_count<-count(DF, vars=c("AFS_class","cluster")) 

# library
library(ggplot2)

# Stacked + percent
barplot_proportion_cluster<-ggplot(DF_count, aes(fill=cluster, y=freq, x=AFS_class )) + 
  geom_bar(position="fill", stat="identity")+ ylab("Frequency")+ xlab("")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ scale_fill_manual(values=profile_palettes)
png("barplot_proportion_cluster.png", height=1900, width=2000, res=300)
barplot_proportion_cluster
dev.off()

######### Interpolation with random forests


dataset_profiles_cleaned_noerrors_rf<-dataset_profiles_cleaned_noerrors[,c("Longitude", "Latitude", "Climate_Köppen",       
                                                                           "Soil_type_WRB/FAO", "Initial_depth", "AFS_classification","C_stocks_delta_t_ha")]
dataset_profiles_cleaned_noerrors_rf<-na.omit(dataset_profiles_cleaned_noerrors_rf)
colnames(dataset_profiles_cleaned_noerrors_rf)[3:4]<-c("Climate_Koppen", "Soil_type")

library(randomForest)
library(caret)
library(e1071)

#fitting some models, rf and linear
rf1 <- randomForest(C_stocks_delta_t_ha~.,
                    data=dataset_profiles_cleaned_noerrors_rf,
                    trControl = fitControl,
                    ntree = 500,
                    metric = "Accuracy")
lm1 <- lm(dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha~dataset_profiles_cleaned_noerrors_rf$Latitude+
            dataset_profiles_cleaned_noerrors_rf$Climate_Koppen+
            dataset_profiles_cleaned_noerrors_rf$Longitude+
            dataset_profiles_cleaned_noerrors_rf$Soil_type+
            dataset_profiles_cleaned_noerrors_rf$Initial_depth+
            dataset_profiles_cleaned_noerrors_rf$AFS_classification)

#trying metatrain with caret
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf2 <-  train(C_stocks_delta_t_ha~.,
              data=dataset_profiles_cleaned_noerrors_rf,
              method = "rf",
              trControl = trControl)




png("depth_models_comparison.png", height=1200, width = 3400, res=300)
par(mfrow=c(1,3))
plot(predict(lm1), dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha, xlim=c(-10,10),  ylim=c(-15,15), main="Linear", xlab=expression(paste(Delta, "SOC (t h", a^-1, ") predicted" )), ylab=expression(paste(Delta, "SOC (t h", a^-1, ") measured" )))
text(7,-7,paste("R2=", round(summary(lm(predict(lm1) ~ dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha))$r.squared,3)))
plot(rf1$predicted, dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha, xlim=c(-10,10),  ylim=c(-15,15), main="RF", xlab=expression(paste(Delta, "SOC (t h", a^-1, ") predicted" )), ylab=expression(paste(Delta, "SOC (t h", a^-1, ") measured" )))
text(7,-7,paste("R2=", round(summary(lm(rf1$predicted ~ dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha))$r.squared,3)))
plot(predict(rf2), dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha, xlim=c(-10,10),  ylim=c(-15,15), main="RF Caret train", xlab=expression(paste(Delta, "SOC (t h", a^-1, ") predicted" )), ylab=expression(paste(Delta, "SOC (t h", a^-1, ") measured" )))
text(7,-7,paste("R2=", round(summary(lm(predict(rf2) ~ dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha))$r.squared,3)))
dev.off()

png("depth_model.png", height=1800, width = 2000, res=300)
plot(predict(rf2), dataset_profiles_cleaned_noerrors_rf$C_stocks_delta_t_ha, xlim=c(-10,10),  ylim=c(-15,15), main="RF Caret train", 
     xlab=expression(paste(Delta, "SOC (t h", a^-1, ") predicted" )), ylab=expression(paste(Delta, "SOC (t h", a^-1, ") measured" )),
     pch=as.numeric(as.factor(dataset_profiles_cleaned_noerrors_rf$AFS_classification)),
     col=AFS_palette[as.numeric(as.factor(dataset_profiles_cleaned_noerrors_rf$AFS_classification))])
legend("topright", levels(dataset_profiles_cleaned_noerrors_rf$AFS_classification), col=AFS_palette, pch=c(1:8), bty="n")
dev.off()


png("depth_model_importance.png", height=1800, width = 2400, res=300)
par(mar=c(12,4,2,2))
barplot(i_scores[rev(order(i_scores[,2])),][1:20,2], names.arg = i_scores[1:20,1], las=2)
dev.off()
