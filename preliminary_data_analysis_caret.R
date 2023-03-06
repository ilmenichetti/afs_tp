
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




#Number randomely variable selected is mtry
which_to_remove1<-which(names(cardinael_data_subset_training)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training)=="SOC_Storage_rate_t_ha_yr_normalized" )

model1 <- train(SOC_Storage_rate_t_ha_yr_normalized~., 
                data=cardinael_data_subset_training[,-which_to_remove1], 
                method='rf', 
                importance = TRUE,
                metric='RMSE',
                tuneGrid=grid, 
                trControl=control)
print(model1)

model2 <- train(AFS_Stock_t_ha_normalized~., 
                data=cardinael_data_subset_training[,-which_to_remove2], 
                method='rf', 
                importance = TRUE,
                metric='RMSE', 
                tuneGrid=grid, 
                trControl=control)
print(model2)
      

plot(cardinael_data_subset_training$SOC_Storage_rate_t_ha_yr_normalized,predict(model1))


png("importance_model1.png", height=2600, width = 2600, res = 300)
importance_vec1<-t(as.data.frame(varImp(model1)$importance))
importance_vec1_names<-colnames(importance_vec1)
importance_vec1<-importance_vec1[order(as.numeric(importance_vec1))]
importance_vec1_names<-importance_vec1_names[order(as.numeric(importance_vec1))]
par(mar=c(18,5,2,2))
barplot(importance_vec1, las=2, main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance_vec1)*1.1), names.arg = importance_vec1_names)
dev.off()

png("importance_model2.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance_vec2<-t(as.data.frame(varImp(model2)$importance))
importance_vec2_names<-colnames(importance_vec2)
importance_vec2<-importance_vec2[order(as.numeric(importance_vec2))]
importance_vec2_names<-importance_vec2_names[order(as.numeric(importance_vec2))]
par(mar=c(18,5,2,2))
barplot(importance_vec2, las=2, main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance_vec2)*1.1), names.arg = importance_vec2_names)
dev.off()


#subsetting the dataset for escluding previous SOC
which_exclude_2<-which(names(cardinael_data_subset_training) %in% c("Control_Stock_t_ha_normalized"))
cardinael_data_subset_training_2<-cardinael_data_subset_training[,-which_exclude_2]
colnames(cardinael_data_subset_training_2)

#Random Forests analysis
which_to_remove1<-which(names(cardinael_data_subset_training_2)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training_2)=="SOC_Storage_rate_t_ha_yr_normalized" )

model1.2 <- train(SOC_Storage_rate_t_ha_yr_normalized~., 
                data=cardinael_data_subset_training_2[,-which_to_remove1], 
                method='rf', 
                importance = TRUE,
                metric='RMSE',
                tuneGrid=grid, 
                trControl=control)
print(model1.2)

model2.2 <- train(AFS_Stock_t_ha_normalized~., 
                data=cardinael_data_subset_training_2[,-which_to_remove2], 
                method='rf', 
                importance = TRUE,
                metric='RMSE', 
                tuneGrid=grid, 
                trControl=control)
print(model2.2)


png("importance_model1.2.png", height=2600, width = 2600, res = 300)
importance_vec1.2<-t(as.data.frame(varImp(model1.2)$importance))
importance_vec1.2_names<-colnames(importance_vec1.2)
importance_vec1.2<-importance_vec1.2[order(as.numeric(importance_vec1.2))]
importance_vec1.2_names<-importance_vec1.2_names[order(as.numeric(importance_vec1.2))]
par(mar=c(18,5,2,2))
barplot(importance_vec1.2, las=2, main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance_vec1.2)*1.1), names.arg = importance_vec1.2_names)
dev.off()

png("importance_model2.2.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance_vec2.2<-t(as.data.frame(varImp(model2.2)$importance))
importance_vec2.2_names<-colnames(importance_vec2.2)
importance_vec2.2<-importance_vec2.2[order(as.numeric(importance_vec2.2))]
importance_vec2.2_names<-importance_vec1_names[order(as.numeric(importance_vec2.2))]
par(mar=c(18,5,2,2))
barplot(importance_vec2.2, las=2, main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance_vec2.2)*1.1), names.arg = importance_vec2.2_names)
dev.off()




#subsetting the dataset for escluding age
which_exclude_3<-which(names(cardinael_data_subset_training_2) %in% c("Age_yrs"))
cardinael_data_subset_training_3<-cardinael_data_subset_training_2[,-which_exclude_3]
names(cardinael_data_subset_training_3)

#Random Forests analysis
which_to_remove1<-which(names(cardinael_data_subset_training_3)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training_3)=="SOC_Storage_rate_t_ha_yr_normalized" )


model1.3 <- train(SOC_Storage_rate_t_ha_yr_normalized~., 
                  data=cardinael_data_subset_training_3[,-which_to_remove1], 
                  method='rf', 
                  importance = TRUE,
                  metric='RMSE',
                  tuneGrid=grid, 
                  trControl=control)
print(model1.3)

model2.3 <- train(AFS_Stock_t_ha_normalized~., 
                  data=cardinael_data_subset_training_3[,-which_to_remove2], 
                  method='rf', 
                  importance = TRUE,
                  metric='RMSE', 
                  tuneGrid=grid, 
                  trControl=control)
print(model2.3)


png("importance_model1.3.png", height=2600, width = 2600, res = 300)
importance_vec1.3<-t(as.data.frame(varImp(model1.3)$importance))
importance_vec1.3_names<-colnames(importance_vec1.3)
importance_vec1.3<-importance_vec1.3[order(as.numeric(importance_vec1.3))]
importance_vec1.3_names<-importance_vec1.3_names[order(as.numeric(importance_vec1.3))]
par(mar=c(18,5,2,2))
barplot(importance_vec1.3, las=2, main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance_vec1.3)*1.1), names.arg = importance_vec1.3_names)
dev.off()

png("importance_model2.3.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance_vec2.3<-t(as.data.frame(varImp(model2.3)$importance))
importance_vec2.3_names<-colnames(importance_vec2.3)
importance_vec2.3<-importance_vec2.3[order(as.numeric(importance_vec2.3))]
importance_vec2.3_names<-importance_vec2.3_names[order(as.numeric(importance_vec2.3))]
par(mar=c(18,5,2,2))
barplot(importance_vec2.3, las=2, main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance_vec2.3)*1.1), names.arg = importance_vec2.3_names)
dev.off()






##### PREDICTIVE MODEL
which_exclude_4<-which(names(cardinael_data_subset_training) %in% c("Control_Stock_t_ha_normalized", "Latitude", "Longitude",  "Mean_annual_rainfall", "Mean_annual_temperature", "Age_yrs" ))
cardinael_data_subset_training_4<-cardinael_data_subset_training[,-which_exclude_4]
colnames(cardinael_data_subset_training_4)

#Random Forests analysis
which_to_remove1.4<-which(names(cardinael_data_subset_training_4)=="AFS_Stock_t_ha_normalized" )
which_to_remove2.4<-which(names(cardinael_data_subset_training_4)=="SOC_Storage_rate_t_ha_yr_normalized" )

model1.4 <- train(SOC_Storage_rate_t_ha_yr_normalized~., 
                  data=cardinael_data_subset_training_4[,-which_to_remove1.4], 
                  method='rf', 
                  importance = TRUE,
                  metric='RMSE',
                  tuneGrid=grid, 
                  trControl=control)


model2.4 <- train(AFS_Stock_t_ha_normalized~., 
                  data=cardinael_data_subset_training_4[,-which_to_remove2.4], 
                  method='rf', 
                  importance = TRUE,
                  metric='RMSE', 
                  tuneGrid=grid, 
                  trControl=control)
print(model2.4)


png("importance_model1.4.png", height=2600, width = 2600, res = 300)
importance_vec1.4<-t(as.data.frame(varImp(model1.4)$importance))
importance_vec1.4_names<-colnames(importance_vec1.4)
importance_vec1.4<-importance_vec1.4[order(as.numeric(importance_vec1.4))]
importance_vec1.4_names<-importance_vec1.4_names[order(as.numeric(importance_vec1.4))]
par(mar=c(18,5,2,2))
barplot(importance_vec1.4, las=2, main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance_vec1.4)*1.1), names.arg = importance_vec1.4_names)
dev.off()

png("importance_model2.4.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance_vec2.4<-t(as.data.frame(varImp(model2.4)$importance))
importance_vec2.4_names<-colnames(importance_vec2.4)
importance_vec2.4<-importance_vec2.4[order(as.numeric(importance_vec2.4))]
importance_vec2.4_names<-importance_vec2.4_names[order(as.numeric(importance_vec2.4))]
par(mar=c(18,5,2,2))
barplot(importance_vec2.4, las=2, main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance_vec2.4)*1.1), names.arg = importance_vec2.4_names)
dev.off()



table_RMSE<-mat.or.vec(4,2)

table_RMSE[1,1]<-as.numeric(model1$results["Rsquared"])
table_RMSE[1,2]<-as.numeric(model2$results["Rsquared"])

table_RMSE[2,1]<-as.numeric(model1.2$results["Rsquared"])
table_RMSE[2,2]<-as.numeric(model2.2$results["Rsquared"])

table_RMSE[3,1]<-as.numeric(model1.3$results["Rsquared"])
table_RMSE[3,2]<-as.numeric(model2.3$results["Rsquared"])

table_RMSE[4,1]<-as.numeric(model1.4$results["Rsquared"])
table_RMSE[4,2]<-as.numeric(model2.4$results["Rsquared"])

colnames(table_RMSE)<-c("SOC sequestration", "SOC stocks")
rownames(table_RMSE)<-c("Full", "No previous stocks", "No age", "Minimal")


write.csv(table_RMSE, file="RMSE.csv")

#saving the workspace
save.image("workspace.RData")
save.image("./Model_interface/workspace.RData")



  
# validation on 10% of the sites
png("Validation.png", width = 2000, height = 4000, res=350)
  
  rbPal_temps <- c('cornsilk3','cadetblue3', 'cyan4', 'mediumorchid3', "darkorange", "darkkhaki", "goldenrod", "seagreen1", "thistle2")
  par(mfrow=c(4,2))
  
  #model 1 and 2
  range=range(c(predict(model1, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
  plot(predict(model1, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 1, predicting SOC storage rates", ylim=range, xlim=range)
  text(predict(model1, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized-0.1, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  range=range(c(predict(model2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized))
  plot(predict(model2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2, predicting SOC stocks", ylim=range, xlim=range)
  text(predict(model2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  
  #model 1.2 and 2.2
  range=range(c(predict(model1.2, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
  plot(predict(model1.2, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 1.2, predicting SOC storage rates", ylim=range, xlim=range)
  text(predict(model1.2, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized-0.1, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  range=range(c(predict(model2.2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized))
  plot(predict(model2.2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2.2, predicting SOC stocks", ylim=range, xlim=range)
  text(predict(model2.2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  #model 1.3 and 2.3
  range=range(c(predict(model1.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
  plot(predict(model1.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 1.3, predicting SOC storage rates", ylim=range, xlim=range)
  text(predict(model1.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized-0.1, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  range=range(c(predict(model2.3, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized))
  plot(predict(model2.3, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2.3, predicting SOC stocks", ylim=range, xlim=range)
  text(predict(model2.3, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  #model 1.4 and 2.4
  range=range(c(predict(model1.4, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
  plot(predict(model1.4, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 1.4 (predictive), predicting SOC storage rates", ylim=range, xlim=range)
  text(predict(model1.4, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized-0.1, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  range=range(c(predict(model2.4, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized))
  plot(predict(model2.4, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2.4 (predictive), predicting SOC stocks", ylim=range, xlim=range)
  text(predict(model2.4, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
  legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
  abline(0, 1, lty=2) 
  
  dev.off()
  
  
  


#TODO: minimal version of the model


#TODO: fuction to fit an exponential and smooth the weird prediction from the RF
#TODO: rearrange the predictions into a function with function(years in the future)

###Testing predictions
which_line<-13
years=50
line<-cardinael_data_subset_training[,-which_to_remove1][which_line,]


# function for predicting model 2 future
model2_pred<-function(line, years){
  predictions<-c()
  predictions[1]<-predict(model1, line)
  for(i in 2:years){
    line_pred<-line
    line_pred$Age_yrs<-line_pred$Age_yrs+i
    predictions[i]<-predict(model2, line_pred)
  }
  return(predictions)
}



model1_pred(line, years)

CSS=input/k
Ct=Css+(C0-CSS)*exp(-k*timestep)


#png("test_predictions.png", height=2500, width = 1500, res=300)
#par(mfrow=c(2,1))
plot(seq(1:50), model2_pred(line, years =50), type="l", xlab="Years in the future", ylab="C stocks")
#dev.off()







