
library(readxl)
library(randomForest)
library(paletteer)
library(matchingR) #for reprow
library(extraDistr) #for discrete uniform distribution, for resampling

set.seed(1407) #Bastille! Anyway setting the RNG so to get consistent results at every new run

cardinael_data<-read_excel("./Data/Cardinael_et_al_2018_ERL_Database_AFS_SOC.xlsx")

plot(cardinael_data$Longitude,cardinael_data$Latitude) # map of afs locations

# normalizing data for depth
#function to return SOC in kg per m3, from:
#Mishra, U., Lal, R., Slater, B., Calhoun, F., Liu, D., & Van Meirvenne, M. (2009). Predicting Soil Organic Carbon Stock Using Profile Depth Distribution Functions and Ordinary Kriging. Soil Science Society of America Journal, 73(2), 614–621. https://doi.org/10.2136/sssaj2007.0410
SOC_Z<-function(x){
  0.1126*exp(-0.1698*x)
}

#the normalization is the ratio between the integral of the function at 20cm and the integral at the specified depth
SOC_Z_normalization<-function(Z){
  norm_ratio<-c()
  for(i in 1:length(Z)){
    norm_ratio[i]<-  integrate(SOC_Z, lower=0, upper=20)$value/integrate(SOC_Z, lower=0, upper=Z[i])$value
  }
  return(norm_ratio)
}

#adding a column with the normalized C values
cardinael_data$Control_Stock_t_ha_normalized<-cardinael_data$Control_Stock_t_ha*SOC_Z_normalization(cardinael_data$Depth_cm)
cardinael_data$AFS_Stock_t_ha_normalized<-cardinael_data$AFS_Stock_t_ha*SOC_Z_normalization(cardinael_data$Depth_cm)
cardinael_data$SOC_Storage_rate_t_ha_yr_normalized<-cardinael_data$SOC_Storage_rate_t_ha_yr*SOC_Z_normalization(cardinael_data$Depth_cm)

png("Normalization_check.png", height = 3000, width = 1000, res=300)
par(mfrow=c(3,1))
rbPal <- colorRampPalette(c('red','blue'))
plot(cardinael_data$Control_Stock_t_ha, cardinael_data$Control_Stock_t_ha_normalized, col=rbPal(10)[as.numeric(cut((cardinael_data$Control_Stock_t_ha- cardinael_data$Control_Stock_t_ha_normalized),breaks = 10))], pch=16)
plot(cardinael_data$AFS_Stock_t_ha, cardinael_data$AFS_Stock_t_ha_normalized, col=rbPal(10)[as.numeric(cut((cardinael_data$AFS_Stock_t_ha- cardinael_data$AFS_Stock_t_ha_normalized),breaks = 10))], pch=16)
plot(cardinael_data$SOC_Storage_rate_t_ha_yr, cardinael_data$SOC_Storage_rate_t_ha_yr_normalized, col=rbPal(10)[as.numeric(cut((cardinael_data$SOC_Storage_rate_t_ha_yr- cardinael_data$SOC_Storage_rate_t_ha_yr_normalized),breaks = 10))], pch=16)
dev.off()


#counting how many NAS we have in different fields
NAs<-c()
for (i in 1:dim(cardinael_data)[2]){
  NAs[i]<-length(which(is.na(cardinael_data[,i])))
}
order<-order(NAs)

png("NAs_in_data.png", height=2000, width = 4000, res = 300)
par(mar=c(18,5,2,2))
barplot(NAs[order], names.arg = names(cardinael_data)[order], las=2, ylab="number of NAs", ylim=c(0, max(NAs)*1.1))
box()
dev.off()



# remove ones where CONTROL!=PRevious land use
cardinael_data_filtered<-cardinael_data[cardinael_data$Control_type==cardinael_data$Previous_land_use,]
cardinael_data_filtered$IPCC_Climate<-as.factor(cardinael_data_filtered$IPCC_Climate)

colnames(cardinael_data_filtered)
#subsetting the dataset for reducing nas
selected_names<-names(cardinael_data_filtered)[order][c(1:33)]
#selected_names<-selected_names[c(-5,-6,-10,-11,-13,-15, -17,-20, -22, -23, -24, -25, -26,-30,-32 )]
which_exclude<-which(selected_names %in% c("ID_publication", "Reference","Country" ,"Control_type", "Experimental_design" , "Depth_cm","SOC_Storage_rate_t_ha_yr"  ,
                                           "Tree_species"  ,"Control_Stock_t_ha", "AFS_Stock_t_ha","Response_Ratio",	"IPCC_based_rate", "Crop_species","Tree_density" ,
                                           "Climate_Köppen",	"FAO_Climate",	"CLIMATE", "Site_me"))
selected_names<-selected_names[-which_exclude]
cardinael_data_subset<-na.omit(cardinael_data_filtered[,selected_names])


#split the dataset in testing and training (5% for training)
which_testing<-rdunif(round(0.1*dim(cardinael_data_subset)[1]), 0, dim(cardinael_data_subset)[1])

cardinael_data_subset_testing<-cardinael_data_subset[which_testing,]
cardinael_data_subset_training<-cardinael_data_subset[-which_testing,]

#Random Forests analysis
which_to_remove1<-which(names(cardinael_data_subset_training)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training)=="SOC_Storage_rate_t_ha_yr_normalized" )
model1<-randomForest(SOC_Storage_rate_t_ha_yr_normalized ~ ., data=cardinael_data_subset_training[,-which_to_remove1]) #removing AFS Stocks
model2<-randomForest(AFS_Stock_t_ha_normalized ~ ., data=cardinael_data_subset_training[,-which_to_remove2]) #removing SOC storage rate





png("importance_model1.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model1<-importance(model1)[order(importance(model1))]
names(importance.model1)<-rownames(importance(model1))[order(importance(model1))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model1))
barplot(importance.model1, beside=T, names.arg = rownames(importance.model1), las=2,  col = colors[rank(importance.model1)],
        main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance.model1)*1.1))
text(5, max(importance.model1), paste("% Variance =", round(tail(model1$rsq,1),2)))
box()
dev.off()

png("importance_model2.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model2<-importance(model2)[order(importance(model2))]
names(importance.model2)<-rownames(importance(model2))[order(importance(model2))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model2))  
barplot(importance.model2, beside=T, names.arg = rownames(importance.model2), las=2, col = colors[rank(importance.model2)],
        main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance.model2)*1.1))
text(5, max(importance.model2), paste("% Variance =", round(tail(model2$rsq,1),2)))
box()
dev.off()



#subsetting the dataset for escluding previous SOC
which_exclude_2<-which(names(cardinael_data_subset_training) %in% c("Control_Stock_t_ha_normalized"))
cardinael_data_subset_training_2<-cardinael_data_subset_training[,-which_exclude_2]
colnames(cardinael_data_subset_training_2)

#Random Forests analysis
which_to_remove1<-which(names(cardinael_data_subset_training_2)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training_2)=="SOC_Storage_rate_t_ha_yr_normalized" )
model1.2<-randomForest(SOC_Storage_rate_t_ha_yr_normalized ~ ., data=cardinael_data_subset_training_2[,-which_to_remove1]) #removing AFS stocks
model2.2<-randomForest(AFS_Stock_t_ha_normalized ~ ., data=cardinael_data_subset_training_2[,-which_to_remove2])

png("importance_model1.2.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model1.2<-importance(model1.2)[order(importance(model1.2))]
names(importance.model1.2)<-rownames(importance(model1.2))[order(importance(model1.2))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model1.2))
barplot(importance.model1.2, beside=T, names.arg = rownames(importance.model1.2), las=2,  col = colors[rank(importance.model1.2)],
        main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance.model1.2)*1.1))
text(5, max(importance.model1.2), paste("% Variance =", round(tail(model1.2$rsq,1),2)))
box()
dev.off()

png("importance_model2.2.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model2.2<-importance(model2.2)[order(importance(model2.2))]
names(importance.model2.2)<-rownames(importance(model2.2))[order(importance(model2.2))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model2.2))  
barplot(importance.model2.2, beside=T, names.arg = rownames(importance.model2.2), las=2, col = colors[rank(importance.model2.2)],
        main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance.model2.2)*1.1))
text(5, max(importance.model2.2), paste("% Variance =", round(tail(model2.2$rsq,1),2)))
box()
dev.off()




#subsetting the dataset for escluding age
which_exclude_3<-which(names(cardinael_data_subset_training_2) %in% c("Age_yrs"))
cardinael_data_subset_training_3<-cardinael_data_subset_training_2[,-which_exclude_3]
names(cardinael_data_subset_training_3)

#Random Forests analysis
which_to_remove1<-which(names(cardinael_data_subset_training_3)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training_3)=="SOC_Storage_rate_t_ha_yr_normalized" )
model1.3<-randomForest(SOC_Storage_rate_t_ha_yr_normalized ~ ., data=cardinael_data_subset_training_3[,-which_to_remove1])
model2.3<-randomForest(AFS_Stock_t_ha_normalized ~ ., data=cardinael_data_subset_training_3[,-which_to_remove2])

png("importance_model1.3.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model1.3<-importance(model1.3)[order(importance(model1.3))]
names(importance.model1.3)<-rownames(importance(model1.3))[order(importance(model1.3))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model1.3))
barplot(importance.model1.3, beside=T, names.arg = rownames(importance.model1.3), las=2,  col = colors[rank(importance.model1.3)],
        main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance.model1.3)*1.1))
text(5, max(importance.model1.3), paste("% Variance =", round(tail(model1.3$rsq,1),2)))
box()
dev.off()


png("importance_model2.3.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model2.3<-importance(model2.3)[order(importance(model2.3))]
names(importance.model2.3)<-rownames(importance(model2.3))[order(importance(model2.3))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model2.3))
barplot(importance.model2.3, beside=T, names.arg = rownames(importance.model2.3), las=2,  col = colors[rank(importance.model2.3)],
        main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance.model2.3)*1.1))
text(5, max(importance.model2.3), paste("% Variance =", round(tail(model2.3$rsq,1),2)))
box()
dev.off()



#saving the workspace
save.image("workspace.RData")
save.image("./Model_interface/workspace.RData")




# validation on 10% of the sites
png("Validation.png", width = 2000, height = 3000, res=350)

rbPal_temps <- c('cornsilk3','cadetblue3', 'cyan4', 'mediumorchid3', "darkorange", "darkkhaki", "goldenrod", "seagreen1", "thistle2")
par(mfrow=c(3,2))

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

range=range(c(predict(model2.2, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
plot(predict(model2.2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2.2, predicting SOC OC stocks", ylim=range, xlim=range)
text(predict(model2.2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
abline(0, 1, lty=2) 

#model 1.3 and 2.3
range=range(c(predict(model1.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
plot(predict(model1.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 1.3, predicting SOC storage rates", ylim=range, xlim=range)
text(predict(model1.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized-0.1, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
abline(0, 1, lty=2) 

range=range(c(predict(model2.3, cardinael_data_subset_testing), cardinael_data_subset_testing$SOC_Storage_rate_t_ha_yr_normalized))
plot(predict(model2.3, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2.3, predicting SOC OC stocks", ylim=range, xlim=range)
text(predict(model2.3, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
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





################ Optimization with CARET
library(caret)

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "RMSE"

model1<-randomForest(SOC_Storage_rate_t_ha_yr_normalized ~ ., data=cardinael_data_subset_training[,-which_to_remove1]) #removing AFS Stocks
model2<-randomForest(AFS_Stock_t_ha_normalized ~ ., data=cardinael_data_subset_training[,-which_to_remove2]) #removing SOC storage rate


#Number randomely variable selected is mtry
which_to_remove1<-which(names(cardinael_data_subset_training)=="AFS_Stock_t_ha_normalized" )
which_to_remove2<-which(names(cardinael_data_subset_training)=="SOC_Storage_rate_t_ha_yr_normalized" )

mtry <- sqrt(ncol(cardinael_data_subset_training[,-which_to_remove1]))
tunegrid <- expand.grid(.mtry=mtry)
model1_caret <- train(SOC_Storage_rate_t_ha_yr_normalized~., 
                    data=cardinael_data_subset_training[,-which_to_remove1], 
                    method='rf', 
                    metric='RMSE', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(model1_caret)

mtry <- sqrt(ncol(cardinael_data_subset_training[,-which_to_remove2]))
tunegrid <- expand.grid(.mtry=mtry)
model2_caret <- train(AFS_Stock_t_ha_normalized~., 
                      data=cardinael_data_subset_training[,-which_to_remove2], 
                      method='rf', 
                      metric='RMSE', 
                      tuneGrid=tunegrid, 
                      trControl=control)
print(model2_caret)





png("importance_model1.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model1<-importance(model1)[order(importance(model1))]
names(importance.model1)<-rownames(importance(model1))[order(importance(model1))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model1))
barplot(importance.model1, beside=T, names.arg = rownames(importance.model1), las=2,  col = colors[rank(importance.model1)],
        main="Predicting SOC_Storage_rate_t_ha_yr", ylim=c(0, max(importance.model1)*1.1))
text(5, max(importance.model1), paste("% Variance =", round(tail(model1$rsq,1),2)))
box()
dev.off()

png("importance_model2.png", height=2600, width = 2600, res = 300)
par(mar=c(18,5,2,2))
importance.model2<-importance(model2)[order(importance(model2))]
names(importance.model2)<-rownames(importance(model2))[order(importance(model2))]
colors <- paletteer_c("ggthemes::Red-Gold", n = length(importance.model2))  
barplot(importance.model2, beside=T, names.arg = rownames(importance.model2), las=2, col = colors[rank(importance.model2)],
        main="Predicting AFS_Stock_t_ha", ylim=c(0, max(importance.model2)*1.1))
text(5, max(importance.model2), paste("% Variance =", round(tail(model2$rsq,1),2)))
box()
dev.off()




range=range(c(predict(model2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized))
plot(predict(model2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2, predicting SOC stocks", ylim=range, xlim=range)
text(predict(model2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
abline(0, 1, lty=2) 

range=range(c(predict(mode2, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized))
plot(predict(rf_default, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized, xlab="Predicted", ylab="Measured", pch=16, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)], main="model 2, predicting SOC stocks", ylim=range, xlim=range)
text(predict(rf_default, cardinael_data_subset_testing), cardinael_data_subset_testing$AFS_Stock_t_ha_normalized-2, cardinael_data_subset_testing$Agroforestry_classification, cex=0.7, col=rbPal_temps[as.numeric(cardinael_data_subset_testing$IPCC_Climate)])
legend("topleft", levels(cardinael_data_subset_testing$IPCC_Climate)[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], col=rbPal_temps[unique(as.numeric(cardinael_data_subset_testing$IPCC_Climate))], pch=16, bty="n", cex=0.7, pt.cex=1)
abline(0, 1, lty=2) 


