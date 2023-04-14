
library(readxl)
library(randomForest)
library(paletteer)
library(extraDistr) #for discrete uniform distribution, for resampling
library(caret)
library(hydroGOF)




set.seed(1789) #Bastille! Anyway setting the RNG so to get consistent results at every new run

##CARET SETUP
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
metric <- "RMSE"


thiago_data<-read_excel("afs_data_tp_breve.xlsx")

thiago_data$AFS_classification<-as.factor(thiago_data$AFS_classification)
thiago_data$Climate_Köppen<-as.factor(thiago_data$Climate_Köppen)
thiago_data$Previous_land_use<-as.factor(thiago_data$Previous_land_use)


thiago_data$texture_flag

sum(is.na(thiago_data$Clay))
sum(is.na(thiago_data$pH))

sum(is.na(thiago_data$Clay) | is.na(thiago_data$pH))



# thiago_data<-thiago_data[!is.na(thiago_data$`Soil_type_WRB/FAO`),]




# normalizing data for depth
#function to return SOC in kg per m3 at a certain depth, from:
#Mishra, U., Lal, R., Slater, B., Calhoun, F., Liu, D., & Van Meirvenne, M. (2009). Predicting Soil Organic Carbon Stock Using Profile Depth Distribution Functions and Ordinary Kriging. Soil Science Society of America Journal, 73(2), 614–621. https://doi.org/10.2136/sssaj2007.0410
SOC_Z<-function(x){
  0.1126*exp(-0.1698*x)
}


plot(SOC_Z(seq(1:60)), seq(1:60), type="l", ylim=c(60, 0))

#the normalization is the ratio between the integral of the function at 20cm and the integral at the specified depth
SOC_Z_normalization<-function(Z){
  norm_ratio<-c()
  for(i in 1:length(Z)){
    norm_ratio[i]<-  integrate(SOC_Z, lower=0, upper=20)$value/integrate(SOC_Z, lower=0, upper=Z[i])$value
  }
  return(norm_ratio)
}



#######OUTLIERS DETECTION (LOF)

outliers_data=thiago_data[,c("AFS_Stock_t_ha","Control_Stock_ton_ha", "Latitude", "Longitude")]


#LOF repeated by classes
library(DMwR2)
names(thiago_data)
outlier_LOF<-c()
factor<-levels(thiago_data$Previous_land_use)
for(i in 1:length(factor)){
  outliers_subset<-thiago_data[thiago_data$Previous_land_use==factor[i],c("AFS_Stock_t_ha","Control_Stock_ton_ha")]
  outlier.scores <- lofactor(outliers_data, k=10)
  outliers <- order(outlier.scores, decreasing=T)[1:round(dim(outliers_subset)[1]*0.25)]
  
  outlier_LOF<-c(outlier_LOF, rownames(outliers_subset) %in% outliers)
}
thiago_data_filtered_LOF<-thiago_data[!outlier_LOF,]
dim(thiago_data_filtered_LOF)


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

# 
# outlier_mahalanobis<-c()
# factor<-levels(thiago_data$Previous_land_use)
# for(i in 1:length(factor)){
#   outliers_subset<-thiago_data[thiago_data$Previous_land_use==factor[i],c("AFS_Stock_t_ha","Control_Stock_ton_ha")]
#   # Calculate Mahalanobis with predictor variables
#   m_dist <- mahalanobis(outliers_subset, colMeans(outliers_subset), cov(outliers_subset))
#   outliers_subset_mahalanobis<-m_dist < quantile(m_dist, 0.9)
#   outlier_mahalanobis<-c(outlier_mahalanobis, outliers_subset_mahalanobis)
# }
# thiago_data_filtered<-thiago_data[as.logical(unlist(outlier_mahalanobis)),]
# dim(thiago_data_filtered)
# 
# 
# 
# names(thiago_data)
# outliers_quantiles<-c()
# factor<-levels(thiago_data$Previous_land_use)
# for(i in 1:length(factor)){
#   outliers_subset<-thiago_data[thiago_data$Previous_land_use==factor[i],c("AFS_Stock_t_ha","Control_Stock_ton_ha")]
#   outliers_subset_thresholds<-quantile(outliers_subset$Control_Stock_ton_ha, c(0.1, 0.9))
#   outliers_quantiles_subset<-outliers_subset$AFS_Stock_t_ha<outliers_subset_thresholds[1] | outliers_subset$AFS_Stock_t_ha>outliers_subset_thresholds[2]
#   outliers_quantiles<-c(outliers_quantiles, outliers_quantiles_subset)
# }
# 
# 
# thiago_data_filtered<-thiago_data[!outliers_quantiles,]
# dim(thiago_data_filtered)
# 
# 
# 
thiago_data_filtered$Control_Stock_ton_ha_normalized<-thiago_data_filtered$Control_Stock_ton_ha*SOC_Z_normalization(thiago_data_filtered$Reference_depth)
thiago_data_filtered$AFS_Stock_ton_ha_normalized<-thiago_data_filtered$AFS_Stock_t_ha*SOC_Z_normalization(thiago_data_filtered$Reference_depth)
thiago_data_filtered$Delta_Stock_ton_ha_normalized<-thiago_data_filtered$AFS_Stock_ton_ha_normalized-thiago_data_filtered$Control_Stock_ton_ha_normalized
thiago_data_filtered$Delta_Stock_ton_ha<-thiago_data_filtered$AFS_Stock_t_ha-thiago_data_filtered$Control_Stock_ton_ha

thiago_data_filtered_LOF$Control_Stock_ton_ha_normalized<-thiago_data_filtered_LOF$Control_Stock_ton_ha*SOC_Z_normalization(thiago_data_filtered_LOF$Reference_depth)
thiago_data_filtered_LOF$AFS_Stock_ton_ha_normalized<-thiago_data_filtered_LOF$AFS_Stock_t_ha*SOC_Z_normalization(thiago_data_filtered_LOF$Reference_depth)
thiago_data_filtered_LOF$Delta_Stock_ton_ha_normalized<-thiago_data_filtered_LOF$AFS_Stock_ton_ha_normalized-thiago_data_filtered_LOF$Control_Stock_ton_ha_normalized
thiago_data_filtered_LOF$Delta_Stock_ton_ha<-thiago_data_filtered_LOF$AFS_Stock_t_ha-thiago_data_filtered_LOF$Control_Stock_ton_ha




thiago_data_filtered<-thiago_data_filtered[!thiago_data_filtered$Delta_Stock_ton_ha>150,]
thiago_data_filtered_LOF<-thiago_data_filtered_LOF[!thiago_data_filtered_LOF$Delta_Stock_ton_ha>150,]

boxplot(thiago_data$Control_Stock_ton_ha ~ thiago_data$AFS_classification, las=2)
boxplot(thiago_data_filtered$Control_Stock_ton_ha ~ thiago_data_filtered$AFS_classification, las=2)

boxplot(thiago_data_filtered$Delta_Stock_ton_ha ~ thiago_data_filtered$AFS_classification, las=2)
boxplot(thiago_data_filtered_LOF$Delta_Stock_ton_ha ~ thiago_data_filtered_LOF$AFS_classification, las=2)


png("Normalization_check.png", height = 3000, width = 1000, res=300)
par(mfrow=c(2,1))
rbPal <- colorRampPalette(c('red','blue'))
plot(thiago_data_filtered$Control_Stock_ton_ha, thiago_data_filtered$Control_Stock_ton_ha_normalized, col=rbPal(10)[as.numeric(cut((thiago_data_filtered$Control_Stock_ton_ha- thiago_data_filtered$Control_Stock_ton_ha_normalized),breaks = 10))], pch=16)
plot(thiago_data_filtered$AFS_Stock_t_ha, thiago_data_filtered$AFS_Stock_ton_ha_normalized, col=rbPal(10)[as.numeric(cut((thiago_data_filtered$AFS_Stock_t_ha- thiago_data_filtered$AFS_Stock_ton_ha_normalized),breaks = 10))], pch=16)
dev.off()


boxplot(thiago_data_filtered$Delta_Stock_ton_ha_normalized ~ thiago_data_filtered$AFS_classification, las=2)

boxplot(thiago_data$Control_Stock_ton_ha ~ thiago_data$AFS_classification)



plot(thiago_data_filtered$Latitude, thiago_data_filtered$Control_Stock_ton_ha_normalized)




################ Optimization with CARET

## validation dataset, one approach is to select them within clusters
library(cluster);library(Ecdat);library(compareGroups)
analysis_dataset<-thiago_data_filtered[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")]
names(analysis_dataset)

analysis_dataset$Region<-as.factor(analysis_dataset$Region)

disMat<-daisy(analysis_dataset, metric = "gower")
mixedClusters<-kmeans(disMat, centers=8)
analysis_dataset<-cbind(analysis_dataset, mixedClusters$cluster)
boxplot(analysis_dataset$Delta_Stock_ton_ha ~ analysis_dataset$`mixedClusters$cluster`)

sites_sample_cluster<-c()
for (i in 1:8){
  which_sub<-as.factor(mixedClusters$cluster)==i
  subsites<-levels(as.factor(thiago_data_filtered[which_sub,]$Site_name))
  subsites_sample<-sample(subsites, length(subsites)*0.1)
  sites_sample_cluster<-c(sites_sample_cluster, subsites_sample)
  }
which_row<- thiago_data_filtered$Site_name %in% sites_sample_cluster

# # sample 10 sites to keep for validation at random
# sites<-levels(as.factor(thiago_data_filtered$Site_name))
# sites_sample<-sample(sites, length(sites)*0.1)
# which_row<- thiago_data_filtered$Site_name %in% sites_sample

thiago_data_filtered_subset_training<-thiago_data_filtered[!which_row,]
thiago_data_filtered_subset_validation<-thiago_data_filtered[which_row,]

dim(thiago_data_filtered_subset_training)
dim(thiago_data_filtered_subset_validation)
dim(thiago_data_filtered)
dim(thiago_data_filtered_subset_training)[1]+dim(thiago_data_filtered_subset_validation)[1]

# now we have one validation and one training dataset

names(thiago_data_filtered_subset_training)



tunegrid <- expand.grid(.mtry = (12:16)) 
 # training_control <- trainControl(method='LOOCV',
 #                         number=5)#,
 #                         #repeats=3)

#  model1 <- train(Delta_Stock_ton_ha_normalized~.,
#                  data=thiago_data_filtered_subset_training[,c("Delta_Stock_ton_ha_normalized","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs")],
#                  method='rf',
#                  importance = TRUE,
#                  tuneGrid=tunegrid,
#                  trControl=control)
#  print(model1)
# lm_validation1<-lm(predict(model1, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$Delta_Stock_ton_ha)
# summary(lm_validation1) 
names(thiago_data_filtered_subset_training)

model1_stocks <- train(Delta_Stock_ton_ha~., 
                data=thiago_data_filtered_subset_training[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")], 
                method='rf', 
                importance = TRUE,
                tuneGrid=tunegrid, 
                trControl=control)
print(model1_stocks)
lm_validation1_stocks<-lm(predict(model1_stocks, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$Delta_Stock_ton_ha)
summary(lm_validation1_stocks) 

model1_AFS_stocks <- train(AFS_Stock_t_ha~., 
                       data=thiago_data_filtered_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")], 
                       method='rf', 
                       importance = TRUE,
                       tuneGrid=tunegrid, 
                       trControl=control)
print(model1_AFS_stocks)
lm_validation1_AFS_stocks<-lm(predict(model1_AFS_stocks, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$AFS_Stock_t_ha)
summary(lm_validation1_AFS_stocks) 


model0_AFS_stocks <- train(AFS_Stock_t_ha~., 
                           data=thiago_data_filtered_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "AFS_age_yrs", "Reference_depth", "Region", "Depth_cm")], 
                           method='rf', 
                           importance = TRUE,
                           tuneGrid=tunegrid, 
                           trControl=control)
lm_validation0_AFS_stocks<-lm(predict(model0_AFS_stocks, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$AFS_Stock_t_ha)
summary(lm_validation0_AFS_stocks) 

model0.0_AFS_stocks <- train(AFS_Stock_t_ha~., 
                           data=thiago_data_filtered_subset_training[,c("AFS_Stock_t_ha","AFS_classification","Climate_Köppen", "AFS_age_yrs", "Reference_depth", "Depth_cm")], 
                           method='rf', 
                           importance = TRUE,
                           tuneGrid=tunegrid, 
                           trControl=control)
lm_validation0.0_AFS_stocks<-lm(predict(model0.0_AFS_stocks, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$AFS_Stock_t_ha)
summary(lm_validation0.0_AFS_stocks) 



# model2_stocks <- train(Delta_Stock_ton_ha~., 
#                        data=thiago_data_filtered_subset_training[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Depth_cm")], 
#                        method='rf', 
#                        importance = TRUE,
#                        tuneGrid=tunegrid, 
#                        trControl=control)
# model2_rmse<-rmse(predict(model2_stocks, thiago_data_filtered_subset_validation), thiago_data_filtered_subset_validation$Delta_Stock_ton_ha)
# model2_rmse



varImp(model0_AFS_stocks, scale = FALSE)



#plotting the C stocks model with depth
dim(thiago_data_filtered)

range_training<-range(c(thiago_data_filtered_subset_training$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_subset_training)))
plot(thiago_data_filtered_subset_training$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_subset_training), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_training$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_training$AFS_classification)), main="Training", ylim=range_training, xlim=range_training)
lm_training<-lm(predict(model1_stocks, thiago_data_filtered_subset_training) ~ thiago_data_filtered_subset_training$Delta_Stock_ton_ha)
abline(lm_training, lty=2)
text(range_training[2]*0.8, range_training[1]+10, paste("R2=",round(summary(lm_training)$r.squared,2)))

range_validation<-range(c(thiago_data_filtered_subset_validation$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_subset_validation)))
plot(thiago_data_filtered_subset_validation$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_subset_validation), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), main="Validation", ylim=range_validation, xlim=range_validation)
lm_validation<-lm(predict(model1_stocks, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$Delta_Stock_ton_ha)
abline(lm_validation, lty=2)
text(range_validation[2]*0.8, range_validation[1]+10, paste("R2=",round(summary(lm_validation)$r.squared,2)))

#legend("topright", levels(as.factor(thiago_data_filtered_subset_validation$AFS_classification)))




range_training<-range(c(thiago_data_filtered_subset_training$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_subset_training)))
plot(thiago_data_filtered_subset_training$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_subset_training), ylab="AFS C stocks, predicted", xlab="AFS C stocks, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_training$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_training$AFS_classification)), main="Training", ylim=range_training, xlim=range_training)
lm_training_AFS<-lm(predict(model1_AFS_stocks, thiago_data_filtered_subset_training) ~ thiago_data_filtered_subset_training$AFS_Stock_t_ha)
abline(lm_training_AFS, lty=2)
text(range_training[2]*0.8, range_training[1]+10, paste("R2=",round(summary(lm_training_AFS)$r.squared,2)))

range_validation<-range(c(thiago_data_filtered_subset_validation$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_subset_validation)))
plot(thiago_data_filtered_subset_validation$AFS_Stock_t_ha, predict(model1_AFS_stocks, thiago_data_filtered_subset_validation), ylab="AFS C stocks, predicted", xlab="AFS C stocks, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), main="Validation", ylim=range_validation, xlim=range_validation)
lm_validation<-lm(predict(model1_AFS_stocks, thiago_data_filtered_subset_validation) ~ thiago_data_filtered_subset_validation$AFS_Stock_t_ha)
abline(lm_validation, lty=2)
text(range_validation[2]*0.8, range_validation[1]+10, paste("R2=",round(summary(lm_validation)$r.squared,2)))
legend("topright", levels(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), pch=1:8, col=1:8, bty="n")



varImp(model1_AFS_stocks, scale = FALSE)
