library(ggplot2)
library(corrplot)
library(car) #VIF
library(ISLR) # Split data into train and test 
library(gbm) #for the Gradiant Boosting Machine 
library(dplyr)# for removing columns in dataset; select function
library(pROC) #how we determine AUC
#library(rgdal)
library (sp)

data <- read.csv("subset.csv") #Read the 30% subset csv file into R and it is named "data"

attach(data)
head(data)
names(data)

categorical_vars <- c("HSG_GROUP", "Landuse_Regroup")
data[categorical_vars] <- lapply(data[categorical_vars], factor)

attach(data)
head(data)
names(data)

columns_to_normalize <- c(3, 4, 5, 6, 7, 8)
data[, columns_to_normalize] <- scale(data[, columns_to_normalize])

print(data)




#Check Variance Inflation Factors (VIF) by first creating a simple linear regression model
model_simpler <- lm(Flood_Status ~ Slope + TWI  + SPI + Drainage_Density + Distance_ST_RI + Elevation + HSG_GROUP + Landuse_Regroup, data = data)
vif(model_simpler)#Determine Variance Inflation Factor
sqrt(vif(model_simpler)) > 2 #Test if the VIF value is too high (False is a pass)



#Split dataset into 70% Training, 30% Testing data
smp_siz = floor(0.70*nrow(data))
set.seed(123)
train_ind = sample(seq_len(nrow(data)),size = smp_siz)
train =data[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=data[-train_ind,] # creates the test dataset excluding the row numbers mentioned in train_ind


#######   Gradiant Boosting Machine code  #########

#source of code: https://github.com/blairscriven/Flood_Susceptibility_R/blob/main/Red_River_Flood_GBM.R

#FIRST PART: test for the optimal ntrees by creating training and testing data without Flood binary
# and then run an iterative fit GBM model. This model and optimal # of trees will be used for prediction

flooded = train$Flood_Status
train_gbm = select(train, -flooded)#remove the flood binary column
test_gbm = select(test, -flooded)#remove the flood binary column
end_trn = nrow(train_gbm)

#combine the two datasets into one to make sure n=both train and test data have similar
#variable manipulations
all = rbind(train_gbm, test_gbm)
end = nrow(all)
ntrees = 2000

all = select(all, TWI, SPI, Drainage_Density, Distance_ST_RI, Elevation, Slope, HSG_GROUP, Landuse_Regroup)

Gbm_model = gbm.fit(x = all[1: end_trn,], y = flooded, distribution = "bernoulli", n.trees = ntrees, shrinkage = 0.05,
                    interaction.depth = 3, n.minobsinnode = 20, nTrain = round(end_trn * 0.08), verbose = TRUE)

summary(Gbm_model)
gbm.perf(Gbm_model) # determines the best number of trees for the prediction model



TestPrediction = predict(object = Gbm_model, newdata = all[(end_trn+1):end,], 
                         n.trees = gbm.perf(Gbm_model, plot.it = FALSE), type = "response")

# Define a prediction score of >=0.6 as a flood (1), below 0.6 is a non-flood (0) 
GBM_Flood = ifelse(TestPrediction>=0.6 , 1 , 0)


head(test$Flood_Status, n = 20)
head(GBM_Flood, n = 20)

#Classification Accuracy
1 - sum(abs(test$Flood_Status - GBM_Flood)) / length(GBM_Flood)


#AUC disgnostic
par(pty = "s")
roc(test$Flood_Status, TestPrediction, plot = TRUE, legacy.axes = TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage")



#######  Creating Maps  code  #########

#insert the full dataset
XY_fix <- read.csv("Full_Data.csv")
#XY_fix = select(XY_fix, -HSG_GROUP, -Landuse_Regroup)

categorical_vars <- c("HSG_GROUP", "Landuse_Regroup")
XY_fix[categorical_vars] <- lapply(XY_fix[categorical_vars], factor)

columns_to_normalize2 <- c(3, 4, 5, 6, 7, 8)
XY_fix[, columns_to_normalize2] <- scale(XY_fix[, columns_to_normalize2])


fulldata <- read.csv("Full_Data_no_XY.csv") #need a .csv file with just the explanatory vairbles, no xy
#fulldata = select(fulldata, -HSG_GROUP, -Landuse_Regroup)

categorical_vars <- c("HSG_GROUP", "Landuse_Regroup")
fulldata[categorical_vars] <- lapply(fulldata[categorical_vars], factor)

columns_to_normalize3 <- c(3, 4, 5, 6, 7, 8)
fulldata[, columns_to_normalize3] <- scale(fulldata[, columns_to_normalize3])

attach(fulldata)
head(fulldata)

fulldata = select(fulldata, TWI, SPI, Drainage_Density, Distance_ST_RI, Elevation, Slope, HSG_GROUP, Landuse_Regroup)

GBM_pred = predict(object = Gbm_model, newdata = fulldata, 
                   n.trees = gbm.perf(Gbm_model, plot.it = FALSE), type = "response")



xyz_GBM_bind <- rbind(XY_fix$X_field, XY_fix$Y_field, GBM_pred)#create xyz data
xyz_GBM <- data.frame(t(xyz_GBM_bind)) #switches the rows and column (so that columns are x,y,z)
colnames(xyz_GBM) <- c("x", "y", "Flood_model")
remove(xyz_GBM_bind) #Not needed anymore so let's just delete it


#install.packages("sf") #to retrieve or set the coordinate reference system (CRS) of a spatial object in R
#install.packages("raster") (install at this stage)

library(sf)
library(raster) #this masks out the 'select' funcion from dplyr, load when you get to this stage
GBM_e <- extent(xyz_GBM[,(1:2)])

abc = "+proj=lcc +lat_1=38.3 +lat_2=39.45 
                    +lat_0=37.66666666666666 +lon_0=-77 
                    +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 
                    +to_meter=0.3048006096012192 +no_defs" 

GBM_r <- raster(GBM_e, ncol=1000, nrow=1000, crs = abc) #create raster with projection and rows/columns
crs(GBM_r)
GBM_r_new <- rasterize(xyz_GBM[,1:2], GBM_r, xyz_GBM[,3], fun=mean)
writeRaster(GBM_r_new, 'PRED1new_full_year_out_September2023.tif', options= c('TFW=YES'), overwrite=TRUE)


