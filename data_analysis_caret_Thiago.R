
library(readxl)
library(randomForest)
library(paletteer)
library(extraDistr) #for discrete uniform distribution, for resampling
library(caret)
library(hydroGOF)




set.seed(1407) #Bastille! Anyway setting the RNG so to get consistent results at every new run

##CARET SETUP
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
metric <- "RMSE"
grid <- expand.grid(mtry = seq(2,4,8)) #grid of values to try for optimization


thiago_data<-read_excel("afs_data_tp_breve.xlsx")

thiago_data$AFS_classification<-as.factor(thiago_data$AFS_classification)
thiago_data$Climate_Köppen<-as.factor(thiago_data$Climate_Köppen)
thiago_data$Previous_land_use<-as.factor(thiago_data$Previous_land_use)

unique(thiago_data$Control_type)
unique(thiago_data$Previous_land_use)


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


quantiles_filter<-quantile(thiago_data$Control_Stock_ton_ha, c(0.05, 0.95))
filter_vec<-thiago_data$Control_Stock_ton_ha<quantiles_filter[1]  | thiago_data$Control_Stock_ton_ha>quantiles_filter[2]

which.max(thiago_data$AFS_Stock_t_ha)


quantiles_filter_AFS<-quantile(thiago_data$AFS_Stock_t_ha, c(0.05, 0.95))
filter_vec_AFS<-thiago_data$AFS_Stock_t_ha<quantiles_filter_AFS[1]  | thiago_data$AFS_Stock_t_ha>quantiles_filter_AFS[2]
thiago_data_filtered<-thiago_data[!filter_vec | !filter_vec_AFS,]


thiago_data_filtered$Control_Stock_ton_ha_normalized<-thiago_data_filtered$Control_Stock_ton_ha*SOC_Z_normalization(thiago_data_filtered$Reference_depth)
thiago_data_filtered$AFS_Stock_ton_ha_normalized<-thiago_data_filtered$AFS_Stock_t_ha*SOC_Z_normalization(thiago_data_filtered$Reference_depth)
thiago_data_filtered$Delta_Stock_ton_ha_normalized<-thiago_data_filtered$AFS_Stock_ton_ha_normalized-thiago_data_filtered$Control_Stock_ton_ha_normalized
thiago_data_filtered$Delta_Stock_ton_ha<-thiago_data_filtered$AFS_Stock_t_ha-thiago_data_filtered$Control_Stock_ton_ha

thiago_data_filtered<-thiago_data_filtered[-1009,]

png("Normalization_check.png", height = 3000, width = 1000, res=300)
par(mfrow=c(2,1))
rbPal <- colorRampPalette(c('red','blue'))
plot(thiago_data_filtered$Control_Stock_ton_ha, thiago_data_filtered$Control_Stock_ton_ha_normalized, col=rbPal(10)[as.numeric(cut((thiago_data_filtered$Control_Stock_ton_ha- thiago_data_filtered$Control_Stock_ton_ha_normalized),breaks = 10))], pch=16)
plot(thiago_data_filtered$AFS_Stock_t_ha, thiago_data_filtered$AFS_Stock_ton_ha_normalized, col=rbPal(10)[as.numeric(cut((thiago_data_filtered$AFS_Stock_t_ha- thiago_data_filtered$AFS_Stock_ton_ha_normalized),breaks = 10))], pch=16)
dev.off()


boxplot(thiago_data_filtered$Delta_Stock_ton_ha_normalized ~ thiago_data_filtered$AFS_classification)

boxplot(thiago_data$Control_Stock_ton_ha ~ thiago_data$AFS_classification)



plot(thiago_data_filtered$Latitude, thiago_data_filtered$Control_Stock_ton_ha_normalized)


################ Normalizing by fitting the depth distribution function on each profile



################ Optimization with CARET

## validation dataset

# sample 10 sites to keep for validation
sites<-levels(as.factor(thiago_data_filtered$Site_name))
sites_sample<-sample(sites, 20)

which_row<- thiago_data_filtered$Site_name %in% sites_sample
thiago_data_filtered_subset_training<-thiago_data_filtered[!which_row,]
thiago_data_filtered_subset_validation<-thiago_data_filtered[which_row,]

dim(thiago_data_filtered_subset_training)
dim(thiago_data_filtered_subset_validation)
dim(thiago_data_filtered)
dim(thiago_data_filtered_subset_training)[1]+dim(thiago_data_filtered_subset_validation)[1]

# now we have one validation and one training dataset

names(thiago_data_filtered_subset_training)



tunegrid <- expand.grid(.mtry = (12:16)) 
# control <- trainControl(method='LOOCV', 
#                         number=5)#, 
#                         #repeats=3)

model1 <- train(Delta_Stock_ton_ha_normalized~., 
                data=thiago_data_filtered_subset_training[,c("Delta_Stock_ton_ha_normalized","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs")], 
                method='rf', 
                importance = TRUE,
                tuneGrid=tunegrid, 
                trControl=control)
print(model1)
rmse(predict(model1, thiago_data_filtered_subset_validation), thiago_data_filtered_subset_validation$Delta_Stock_ton_ha_normalized)


model1_stocks <- train(Delta_Stock_ton_ha~., 
                data=thiago_data_filtered_subset_training[,c("Delta_Stock_ton_ha","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs", "Reference_depth", "Region")], 
                method='rf', 
                importance = TRUE,
                tuneGrid=tunegrid, 
                trControl=control)
print(model1_stocks)
rmse(predict(model1_stocks, thiago_data_filtered_subset_validation), thiago_data_filtered_subset_validation$Delta_Stock_ton_ha)

varImp(model1_stocks, scale = FALSE)


#finding some outliers
error1<-thiago_data_filtered_subset_training$Delta_Stock_ton_ha_normalized - predict(model1, thiago_data_filtered_subset_training)
plot(density(error1))
upper_error_limit<-quantile(abs(error1), 0.95)
which_outliers<-which(error1 > upper_error_limit)

plot(thiago_data_filtered_subset_training$Delta_Stock_ton_ha_normalized, predict(model1, thiago_data_filtered_subset_training), ylab="C stocks delta, predicted", xlab="C stocks delta, measured")
points(thiago_data_filtered_subset_training$Delta_Stock_ton_ha_normalized[which_outliers], predict(model1, thiago_data_filtered_subset_training)[which_outliers], col="red")
#text(thiago_data_filtered_subset_training$Delta_Stock_ton_ha_normalized[which_outliers], predict(model1, thiago_data_filtered_subset_training)[which_outliers]+5, thiago_data_filtered_subset_training$Author_year[which_outliers] , col="red", cex=0.5)
thiago_data_filtered_subset_training$Author_year[which_outliers]


plot(thiago_data_filtered_subset_validation$Delta_Stock_ton_ha_normalized, predict(model1, thiago_data_filtered_subset_validation), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)))
legend("bottomright", as.character(unique(as.factor(thiago_data_filtered_subset_validation$AFS_classification))),
       pch=unique(as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification))))



#plotting the C stocks model with depth

plot(thiago_data_filtered_subset_training$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_subset_training), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_training$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_training$AFS_classification),ylim=c(-20,60), xlim=c(-20,60)))

plot(thiago_data_filtered_subset_validation$Delta_Stock_ton_ha, predict(model1_stocks, thiago_data_filtered_subset_validation), ylab="C stocks delta, predicted", xlab="C stocks delta, measured", 
     pch=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification)), 
     col=as.numeric(as.factor(thiago_data_filtered_subset_validation$AFS_classification),ylim=c(-20,60), xlim=c(-20,60)))


#legend("topright", levels(as.factor(thiago_data_filtered_subset_validation$AFS_classification)))


