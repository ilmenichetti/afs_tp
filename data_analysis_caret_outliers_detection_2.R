
library(readxl)
library(randomForest)
library(paletteer)
library(extraDistr) #for discrete uniform distribution, for resampling
library(caret)
#library(hydroGOF)


# Test tree density prediction
# Move to cross validation
# Agroforsestry system journal: https://www.springer.com/journal/10457


#remi had DMS converted to decimal degrees


set.seed(1789) #Bastille! Anyway setting the RNG so to get consistent results at every new run



thiago_data<-read_excel("afs_data_tp_breve.xlsx")

thiago_data$AFS_classification<-as.factor(thiago_data$AFS_classification)
thiago_data$Climate_Köppen<-as.factor(thiago_data$Climate_Köppen)
thiago_data$Previous_land_use<-as.factor(thiago_data$Previous_land_use)






#######OUTLIERS DETECTION (LOF)

outliers_data=thiago_data[,c("AFS_Stock_t_ha","Control_Stock_ton_ha", "Latitude", "Longitude")]


#mahalanobis distance repeated by classes
library(mvoutlier)
names(thiago_data)
outlier_mahalanobis<-c()
factor<-levels(thiago_data$Previous_land_use)
for(i in 1:length(factor)){
outliers_subset<-thiago_data[thiago_data$Previous_land_use==factor[i],c("AFS_Stock_t_ha","Control_Stock_ton_ha")]
outliers_subset_mahalanobis<-arw(outliers_subset, apply(outliers_subset,2,mean), cov(outliers_subset), alpha=0.025)
outlier_mahalanobis<-c(outlier_mahalanobis, outliers_subset_mahalanobis$w)
}
thiago_data_filtered<-thiago_data[as.logical(unlist(outlier_mahalanobis)),]
dim(thiago_data_filtered)
thiago_data_filtered$Control_Stock_ton_ha_normalized<-thiago_data_filtered$Control_Stock_ton_ha*SOC_Z_normalization(thiago_data_filtered$Reference_depth)
thiago_data_filtered$AFS_Stock_ton_ha_normalized<-thiago_data_filtered$AFS_Stock_t_ha*SOC_Z_normalization(thiago_data_filtered$Reference_depth)
thiago_data_filtered$Delta_Stock_ton_ha_normalized<-thiago_data_filtered$AFS_Stock_ton_ha_normalized-thiago_data_filtered$Control_Stock_ton_ha_normalized
thiago_data_filtered$Delta_Stock_ton_ha<-thiago_data_filtered$AFS_Stock_t_ha-thiago_data_filtered$Control_Stock_ton_ha



thiago_data_filtered<-thiago_data_filtered[!thiago_data_filtered$Delta_Stock_ton_ha>150,]

boxplot(thiago_data$Control_Stock_ton_ha ~ thiago_data$AFS_classification, las=2)
boxplot(thiago_data_filtered$Control_Stock_ton_ha ~ thiago_data_filtered$AFS_classification, las=2)

plot(thiago_data_filtered$Latitude, thiago_data_filtered$Control_Stock_ton_ha_normalized)

#######INTEGRATION WITH SOILGRIDS




# Load the required libraries
library(maps)

png("./Manuscript/data_points.png", height = 1000, width = 2000, res=300)
map(database = "world", myborder=0)
# marking points on map
points(y = thiago_data_filtered$Latitude, x = thiago_data_filtered$Longitude, bg = "steelblue", pch=21, cex=0.6)
dev.off()

write.csv(data.frame(latitude = thiago_data_filtered$Latitude, longitude = thiago_data_filtered$Longitude, authoryear = thiago_data_filtered$Author_year), "coordinates.csv")


library(soilDB)
dim(thiago_data_filtered)

soilgrids_matrix<-mat.or.vec(dim(thiago_data_filtered)[1], 4)
colnames(soilgrids_matrix)<-c("clay_soilgrids", "sand_soilgrids", "ph_soilgrids", "N_soilgrids")

for(i in 1:dim(thiago_data_filtered)[1]){
start_time <- Sys.time()
  
coord <- data.frame(id  = c(thiago_data_filtered[i,]$Site_name), 
                          lat = c(thiago_data_filtered[i,]$Latitude), 
                          lon = c(thiago_data_filtered[i,]$Longitude), 
                          stringsAsFactors = FALSE)
Soilgrids_extract<-fetchSoilGrids(coord, verbose = T,  progress = T)

clay<-mean(Soilgrids_extract@horizons$claymean[1:3])
sand<-mean(Soilgrids_extract@horizons$sandmean[1:3])
ph<-mean(Soilgrids_extract@horizons$phh2omean[1:3])
nitrogen<-mean(Soilgrids_extract@horizons$nitrogenmean[1:3])

soilgrids_matrix[i,]<-c(clay, sand, ph, nitrogen)
end_time <- Sys.time()
time_run<-end_time - start_time
}

dim(soilgrids_matrix)



NAs_list<-data.frame(authoryear= thiago_data_filtered$Author_year[is.na(soilgrids_matrix[,1])], 
                     latitude= thiago_data_filtered$Latitude[is.na(soilgrids_matrix[,1])],
                      longitude= thiago_data_filtered$Longitude[is.na(soilgrids_matrix[,1])])
write.csv(NAs_list, file = "NAs.csv")

NAs_list<-as.data.frame((NAs_list))
points(y = NAs_list$V2, x = NAs_list$V3, bg = "red", pch=21, cex=0.6)




dim(thiago_data_filtered)


thiago_data_filtered_soilgrids<-cbind(thiago_data_filtered, soilgrids_matrix)
dim(thiago_data_filtered_soilgrids)

thiago_data_filtered_soilgrids<-thiago_data_filtered_soilgrids[!is.na(soilgrids_matrix[,1]),]
dim(thiago_data_filtered_soilgrids)



################ Optimization with CARET

## validation dataset, one approach is to select them within clusters

names(thiago_data_filtered_soilgrids)

library(cluster);library(Ecdat);library(compareGroups)
analysis_dataset<-thiago_data_filtered_soilgrids[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm", "clay_soilgrids", "sand_soilgrids",  "ph_soilgrids", "N_soilgrids" )]
names(analysis_dataset)

analysis_dataset$Region<-as.factor(analysis_dataset$Region)

disMat<-daisy(analysis_dataset, metric = "gower")
mixedClusters<-kmeans(disMat, centers=8)
analysis_dataset<-cbind(analysis_dataset, mixedClusters$cluster)
boxplot(analysis_dataset$Delta_Stock_ton_ha ~ analysis_dataset$`mixedClusters$cluster`)

sites_sample_cluster<-c()
for (i in 1:8){
  which_sub<-as.factor(mixedClusters$cluster)==i
  subsites<-levels(as.factor(thiago_data_filtered_soilgrids[which_sub,]$Site_name))
  subsites_sample<-sample(subsites, length(subsites)*0.1)
  sites_sample_cluster<-c(sites_sample_cluster, subsites_sample)
  }
which_row<- thiago_data_filtered_soilgrids$Site_name %in% sites_sample_cluster

# # sample 10 sites to keep for validation at random
# sites<-levels(as.factor(thiago_data_filtered_soilgrids$Site_name))
# sites_sample<-sample(sites, length(sites)*0.1)
# which_row<- thiago_data_filtered_soilgrids$Site_name %in% sites_sample

thiago_data_filtered_soilgrids_subset_training<-thiago_data_filtered_soilgrids[!which_row,]
thiago_data_filtered_soilgrids_subset_validation<-thiago_data_filtered_soilgrids[which_row,]

dim(thiago_data_filtered_soilgrids_subset_training)
dim(thiago_data_filtered_soilgrids_subset_validation)
dim(thiago_data_filtered_soilgrids)
dim(thiago_data_filtered_soilgrids_subset_training)[1]+dim(thiago_data_filtered_soilgrids_subset_validation)[1]

# now we have one validation and one training dataset
names(thiago_data_filtered_soilgrids_subset_training)


##CARET SETUP
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
tunegrid <- expand.grid(.mtry = (13:17)) 

## Predicting delta stocks
model1_stocks <- train(Delta_Stock_ton_ha~., 
                data=thiago_data_filtered_soilgrids_subset_training[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")], 
                method='rf', 
                importance = TRUE,
                tuneGrid=tunegrid, 
                trControl=control)
print(model1_stocks)
lm_validation1_stocks<-lm(predict(model1_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$Delta_Stock_ton_ha)
summary(lm_validation1_stocks) 

model1.1_stocks <- train(Delta_Stock_ton_ha~., 
                       data=thiago_data_filtered_soilgrids_subset_training[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm", "clay_soilgrids", "sand_soilgrids",  "ph_soilgrids", "N_soilgrids" )], 
                       method='rf', 
                       importance = TRUE,
                       tuneGrid=tunegrid, 
                       trControl=control)
print(model1.1_stocks)
lm_validation1.1_stocks<-lm(predict(model1.1_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$Delta_Stock_ton_ha)
summary(lm_validation1.1_stocks) 


### Predicting AFS stocks

model1_AFS_stocks <- train(AFS_Stock_t_ha~., 
                       data=thiago_data_filtered_soilgrids_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")], 
                       method='rf', 
                       importance = TRUE,
                       tuneGrid=tunegrid, 
                       trControl=control)
print(model1_AFS_stocks)
lm_validation1_AFS_stocks<-lm(predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
summary(lm_validation1_AFS_stocks) 


# adding all the SOilrids variables
model1.1_AFS_stocks <- train(AFS_Stock_t_ha~., 
                           data=thiago_data_filtered_soilgrids_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm", "clay_soilgrids", "sand_soilgrids",  "ph_soilgrids", "N_soilgrids" )], 
                           method='rf', 
                           importance = TRUE,
                           tuneGrid=tunegrid, 
                           trControl=control)
print(model1.1_AFS_stocks)
lm_validation1.1_AFS_stocks<-lm(predict(model1.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
summary(lm_validation1.1_AFS_stocks) 

# adding only clay from SOilGrids
model1.1_AFS_stocks_onlyclay <- train(AFS_Stock_t_ha~., 
                             data=thiago_data_filtered_soilgrids_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm", "clay_soilgrids")], 
                             method='rf', 
                             importance = TRUE,
                             tuneGrid=tunegrid, 
                             trControl=control)
print(model1.1_AFS_stocks_onlyclay)
lm_validation1.1_AFS_stocks_onlyclay<-lm(predict(model1.1_AFS_stocks_onlyclay, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
summary(lm_validation1.1_AFS_stocks_onlyclay) 




#### REmoving previous land use

model0_AFS_stocks <- train(AFS_Stock_t_ha~., 
                           data=thiago_data_filtered_soilgrids_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")], 
                           method='rf', 
                           importance = TRUE,
                           tuneGrid=tunegrid, 
                           trControl=control)
print(model0_AFS_stocks)
lm_validation0_AFS_stocks<-lm(predict(model0_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
summary(lm_validation0_AFS_stocks) 

#### REmoving previous land use but adding all the SOilGrids variables

model0.1_AFS_stocks <- train(AFS_Stock_t_ha~., 
                           data=thiago_data_filtered_soilgrids_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm", "clay_soilgrids", "sand_soilgrids",  "ph_soilgrids", "N_soilgrids" )], 
                           method='rf', 
                           importance = TRUE,
                           tuneGrid=tunegrid, 
                           trControl=control)
print(model0.1_AFS_stocks)
lm_validation0.1_AFS_stocks<-lm(predict(model0.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
summary(lm_validation0.1_AFS_stocks) 

model0.0_AFS_stocks <- train(AFS_Stock_t_ha~., 
                           data=thiago_data_filtered_soilgrids_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "AFS_age_yrs", "Reference_depth", "Depth_cm")], 
                           method='rf', 
                           importance = TRUE,
                           tuneGrid=tunegrid, 
                           trControl=control)
print(model0.0_AFS_stocks)
lm_validation0.0_AFS_stocks<-lm(predict(model0.0_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
summary(lm_validation0.0_AFS_stocks) 


### Building a table with the results



models_types<-c("Stocks difference, without SoilGrids data",
                "Stocks difference, with SoilGrids data",
                "Stocks after AFS, without SoilGrids data",
                "Stocks after AFS, with SoilGrids data",
                "Stocks after AFS, with only clay from SoilGrids data",
                "Stocks after AFS, no previous land use but with SoilGrids data",
                "Stocks after AFS, no previous land use")


models_results<-c(summary(lm_validation1_stocks)$r.squared,
                  summary(lm_validation1.1_stocks)$r.squared,
                  summary(lm_validation1_AFS_stocks)$r.squared,
                  summary(lm_validation1.1_AFS_stocks)$r.squared,
                  summary(lm_validation1.1_AFS_stocks_onlyclay)$r.squared,
                  summary(lm_validation0.1_AFS_stocks)$r.squared,
                  summary(lm_validation0.0_AFS_stocks)$r.squared)


validations<-data.frame(models_types, models_results)
colnames(validations)<- c("Model type", "R^2")
write.csv(validations, file = "validation_results.csv", row.names = FALSE)
model_description<-data.frame(models_types, c( "Model 1", "Model 1.1", "Model 2", "Model 2.1", "Model 3.1", "Model 0", "Model 0.1"))
names(model_description)<-c("Model description", "Model name")
write.csv(model_description, file = "models.csv", row.names = FALSE)


paste(models_types[1])



png("./Manuscript/1.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model1_stocks, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()

png("./Manuscript/1.1.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model1.1_stocks, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()

png("./Manuscript/1_AFS.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model1_AFS_stocks, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()

png("./Manuscript/1.1_AFS.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model1.1_AFS_stocks, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()


png("./Manuscript/1.1_AFS_onlyclay.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model1.1_AFS_stocks_onlyclay, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()

png("./Manuscript/0AFS.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model0_AFS_stocks, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()

png("./Manuscript/0.1_AFS.png")
par(mar=c(2,13,2,2))
importance<-as.matrix(varImp(model0.1_AFS_stocks, scale = FALSE)$importance)
importance_plot<-importance[order(importance, decreasing = TRUE),][1:10]
barplot(rev(importance_plot), horiz=TRUE, las=2)
dev.off()


#plotting the C stocks model with depth
dim(thiago_data_filtered_soilgrids)

range_training<-range(c(thiago_data_filtered_soilgrids_subset_training$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_soilgrids_subset_training)))
plot(thiago_data_filtered_soilgrids_subset_training$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_soilgrids_subset_training), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_training$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_training$AFS_classification)), main="Training", ylim=range_training, xlim=range_training)
lm_training<-lm(predict(model1_stocks, thiago_data_filtered_soilgrids_subset_training) ~ thiago_data_filtered_soilgrids_subset_training$Delta_Stock_ton_ha)
abline(lm_training, lty=2)
text(range_training[2]*0.8, range_training[1]+10, paste("R2=",round(summary(lm_training)$r.squared,2)))

range_validation<-range(c(thiago_data_filtered_soilgrids_subset_validation$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_soilgrids_subset_validation)))
plot(thiago_data_filtered_soilgrids_subset_validation$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_soilgrids_subset_validation), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), main="Validation", ylim=range_validation, xlim=range_validation)
lm_validation<-lm(predict(model1_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$Delta_Stock_ton_ha)
abline(lm_validation, lty=2)
text(range_validation[2]*0.8, range_validation[1]+10, paste("R2=",round(summary(lm_validation)$r.squared,2)))

#legend("topright", levels(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)))


png("./Manuscript/model_evaliation.png", height=3000, width = 3000, res=300)
par(mfrow=c(2,2))
range_training<-range(c(thiago_data_filtered_soilgrids_subset_training$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_training)))
plot(thiago_data_filtered_soilgrids_subset_training$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_training), ylab="AFS C stocks, predicted", xlab="AFS C stocks, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_training$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_training$AFS_classification)), main="Training, without SoilGrids", ylim=range_training, xlim=range_training)
lm_training_AFS<-lm(predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_training) ~ thiago_data_filtered_soilgrids_subset_training$AFS_Stock_t_ha)
abline(lm_training_AFS, lty=2)
text(range_training[2]*0.8, range_training[1]+10, paste("R2=",round(summary(lm_training_AFS)$r.squared,3)))

range_validation<-range(c(thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation)))
plot(thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation), ylab="AFS C stocks, predicted", xlab="AFS C stocks, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), main="Validation, without SoilGrids", ylim=range_validation, xlim=range_validation)
lm_validation<-lm(predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
abline(lm_validation, lty=2)
text(range_validation[2]*0.8, range_validation[1]+10, paste("R2=",round(summary(lm_validation)$r.squared,3)))
legend("topright", levels(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), pch=1:8, col=1:8, bty="n")


range_training<-range(c(thiago_data_filtered_soilgrids_subset_training$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_soilgrids_subset_training)))
plot(thiago_data_filtered_soilgrids_subset_training$AFS_Stock_t_ha, predict(model1.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_training), ylab="AFS C stocks, predicted", xlab="AFS C stocks, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_training$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_training$AFS_classification)), main="Training, with SoilGrids", ylim=range_training, xlim=range_training)
lm_training_AFS<-lm(predict(model1.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_training) ~ thiago_data_filtered_soilgrids_subset_training$AFS_Stock_t_ha)
abline(lm_training_AFS, lty=2)
text(range_training[2]*0.8, range_training[1]+10, paste("R2=",round(summary(lm_training_AFS)$r.squared,3)))


range_validation<-range(c(thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha, predict(model1.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation)))
plot(thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha, predict(model1.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation), ylab="AFS C stocks, predicted", xlab="AFS C stocks, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), main="Validation, with SoilGrids", ylim=range_validation, xlim=range_validation)
lm_validation<-lm(predict(model1.1_AFS_stocks, thiago_data_filtered_soilgrids_subset_validation) ~ thiago_data_filtered_soilgrids_subset_validation$AFS_Stock_t_ha)
abline(lm_validation, lty=2)
text(range_validation[2]*0.8, range_validation[1]+10, paste("R2=",round(summary(lm_validation)$r.squared,3)))
legend("topright", levels(as.factor(thiago_data_filtered_soilgrids_subset_validation$AFS_classification)), pch=1:8, col=1:8, bty="n")

dev.off()


varImp(model1_AFS_stocks, scale = FALSE)


table_variables<-data.frame(
  c("AFS management","Climate (Köppen)", "Previous land use", "AFS duration", "Depth", "Region", "layer thickness", "clay (SoilGrids)", "sand (SoilGrids)",  "pH (SoilGrids)", "N (SoilGrids)" ),
  c("Man", "Clim", "L", "Dur", "Depth", "Reg", "Thick", "Clay", "Sand", "pH", "N"),
  c("X", "X", "X", "X", "X", "X", "X", " ", " ", " ", " "),
  c("X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X"),
  c("X", "X", "X", "X", "X", "X", "X", " ", " ", " ", " "),
  c("X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X"),
  c("X", "X", "X", "X", "X", "X", "X", "X", " ", " ", " "),
  c("X", "X", " ", "X", "X", "X", "X", " ", " ", " ", " "),
  c("X", "X", " ", "X", "X", "X", "X", "X", "X", "X", "X"))

colnames(table_variables)=c("Variable", "Abbreviation", "Model 1", "Model 1.1", "Model 2", "Model 2.1", "Model 3.1", "Model 0", "Model 0.1")
write.csv(table_variables, "table_variables.csv", row.names = FALSE)
