

#reading the file

library(readxl) #to read the excel file
library(RColorBrewer) #to get some pre-made color palettes (just a bit quicker than selecting them manually)
library(dfoptim) #this package has some good optimization functions
library(hydroGOF) #loading some cost functions for the optimizer

#PACKAGES TO WORK WITH TIME SERIES
library(TSdist) #TS clustering algorithm
library(dtwclust) #TS clustering algorithm
library(zoo) #doealing with irregular time series

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
dataset_profiles<-dataset_profiles[!is.na(dataset_profiles$profile_ID),]


# TODO: there are quite some NAs because the AFS_description has NAs. We exclude 217 rows of 1186. Most seems reasonable to exclude, only one layer, but I am not sure about all of them. Thiago, could you check?
dataset$First_Author[is.na(profile_ID)]
dataset$AFS_description[is.na(profile_ID)]
dataset_NA_profiles<-dataset[is.na(profile_ID),]

dataset_profiles[dataset_profiles$profile_ID==3,]

#excluding the lines with only one row
duplicated<-(duplicated(dataset_profiles$profile_ID)|duplicated(dataset_profiles$profile_ID, fromLast=TRUE))
dataset_profiles_cleaned<-dataset_profiles[duplicated,]


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


#Time series clustering with dinamic time warping (no idea what it means but it seems to work)

TS_list<-list() #here we create an empty list to store all the zoo (irregular time series) objects we will deal with (the profiles)
#for(i in 1:length(levels(dataset_profiles_cleaned$profile_ID))){
for(i in 1:10){
    SOC<-dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==profiles[i],]$C_stocks_delta_t_ha
  depth<-dataset_profiles_cleaned[dataset_profiles_cleaned$profile_ID==profiles[i],]$Initial_depth
  TS_list[[i]]<-zoo(SOC, depth)
}

TSDistances(TS_list[[1]], TS_list[[2]], distance="euclidean")

clusters<-tsclust(TS_list, k=3)
str(clusters)

clusters@cluster


