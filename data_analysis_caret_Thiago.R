
library(readxl)
library(randomForest)
library(paletteer)
library(extraDistr) #for discrete uniform distribution, for resampling
library(caret)

#TODO: CROSS VALIDATION



set.seed(1407) #Bastille! Anyway setting the RNG so to get consistent results at every new run

##CARET SETUP
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
metric <- "RMSE"
grid <- expand.grid(mtry = seq(2,4,8)) #grid of values to try for optimization


thiago_data<-read_excel("afs_data_tp_breve.xlsx")


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


thiago_data$Control_Stock_ton_ha_normalized<-thiago_data$Control_Stock_ton_ha*SOC_Z_normalization(thiago_data$Depth_cm)
thiago_data$AFS_Stock_ton_ha_normalized<-thiago_data$AFS_Stock_t_ha*SOC_Z_normalization(thiago_data$Depth_cm)
thiago_data$Delta_Stock_ton_ha_normalized<-thiago_data$AFS_Stock_ton_ha_normalized-thiago_data$Control_Stock_ton_ha_normalized


png("Normalization_check.png", height = 3000, width = 1000, res=300)
par(mfrow=c(2,1))
rbPal <- colorRampPalette(c('red','blue'))
plot(thiago_data$Control_Stock_ton_ha, thiago_data$Control_Stock_ton_ha_normalized, col=rbPal(10)[as.numeric(cut((thiago_data$Control_Stock_ton_ha- thiago_data$Control_Stock_ton_ha_normalized),breaks = 10))], pch=16)
plot(thiago_data$AFS_Stock_t_ha, thiago_data$AFS_Stock_ton_ha_normalized, col=rbPal(10)[as.numeric(cut((thiago_data$AFS_Stock_t_ha- thiago_data$AFS_Stock_ton_ha_normalized),breaks = 10))], pch=16)
dev.off()




boxplot(thiago_data$Delta_Stock_ton_ha_normalized ~ thiago_data$AFS_classification)


## trying some simple models, linear

# only with AFS classification
lm1<-lm(thiago_data$Delta_Stock_ton_ha_normalized ~ thiago_data$AFS_classification)
summary(lm1)

#adding climate
lm2<-lm(thiago_data$Delta_Stock_ton_ha_normalized ~ thiago_data$AFS_classification*thiago_data$Climate_Köppen)
summary(lm2)

#adding alsol previous land use
lm3<-lm(thiago_data$Delta_Stock_ton_ha_normalized ~ thiago_data$AFS_classification*thiago_data$Climate_Köppen*thiago_data$Previous_land_use)
summary(lm3)

# adding the age
lm4<-lm(thiago_data$Delta_Stock_ton_ha_normalized ~ thiago_data$AFS_classification*thiago_data$Climate_Köppen*thiago_data$Previous_land_use*thiago_data$AFS_age_yrs)
summary(lm4)




################ Optimization with CARET, model 1 and 2

## validation dataset

# sample 10 sites to keep for validation
sites<-levels(as.factor(thiago_data$Site_name))
sites_sample<-sample(sites, 10)

which_row<- thiago_data$Site_name %in% sites_sample
thiago_data_subset_training<-thiago_data[!which_row,]
thiago_data_subset_validation<-thiago_data[which_row,]

dim(thiago_data_subset_training)
dim(thiago_data_subset_validation)
dim(thiago_data)
dim(thiago_data_subset_training)[1]+dim(thiago_data_subset_validation)[1]

# now we have one validation and one training dataset

names(thiago_data_subset_training)

thiago_data_subset_training[,"Climate_Köppen"] 
lm4<-lm(thiago_data$Delta_Stock_ton_ha_normalized ~ thiago_data$AFS_classification*thiago_data$Climate_Köppen*thiago_data$Previous_land_use*thiago_data$AFS_age_yrs)

model1 <- train(Delta_Stock_ton_ha_normalized~., 
                data=thiago_data_subset_training[,c("Delta_Stock_ton_ha_normalized","AFS_classification","Climate_Köppen", "Previous_land_use", "AFS_age_yrs")], 
                method='rf', 
                importance = TRUE,
                metric='RMSE',
                tuneGrid=grid, 
                trControl=control)
print(model1)


predict(model1, thiago_data_subset_validation)

#finding some outliers
error1<-thiago_data_subset_training$Delta_Stock_ton_ha_normalized - predict(model1, thiago_data_subset_training)
plot(density(error1))
upper_error_limit<-quantile(abs(error1), 0.95)
which_outliers<-which(error1 > upper_error_limit)

plot(thiago_data_subset_training$Delta_Stock_ton_ha_normalized, predict(model1, thiago_data_subset_training), ylab="C stocks delta, predicted", xlab="C stocks delta, measured")
points(thiago_data_subset_training$Delta_Stock_ton_ha_normalized[which_outliers], predict(model1, thiago_data_subset_training)[which_outliers], col="red")
#text(thiago_data_subset_training$Delta_Stock_ton_ha_normalized[which_outliers], predict(model1, thiago_data_subset_training)[which_outliers]+5, thiago_data_subset_training$Author_year[which_outliers] , col="red", cex=0.5)
thiago_data_subset_training$Author_year[which_outliers]


plot(thiago_data_subset_validation$Delta_Stock_ton_ha_normalized, predict(model1, thiago_data_subset_validation))


