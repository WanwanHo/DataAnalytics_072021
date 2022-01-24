# Title: MultipleRegression_pipeline

# Last update: 2021.11

# File: MultipleRegression_pipeline_Ho_111221.R
# Project name: Product Sales Prediction


###############
# Project Notes
###############
# Use existing product attributes dataset to train the modele
# predict variable "Volume" in of four different product types in new product attribute

# Assignment "<-" short-cut:
#   OSX [Alt]+[-] (next to "+" sign)
#   Win [Alt]+[-]

# Clear console: CTRL + L

###############
# Housekeeping
###############

# Clear objects if necessary
#rm(list = ls())

# get working directory
getwd()

# set working directory
setwd("/home/shirokami/Wan-Yun's/RStudio/C3T3")
dir()

################
# Load packages
################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
install.packages("doParallel")  # for Win parallel processing (see below) 
install.packages("gbm")
install.packages("xgboost")
#install.packages("questionr", dep = TRUE)
#install.packages('htmltools')

library(ggplot2)
library(lattice)
library(caret)
library(corrplot)
library(readr)
library(mlbench)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)             # for Win
library(gbm)
library(xgboost)

#####################
# Parallel processing
#####################

# NOTE: Be sure to use the correct package for your operating system. 

#--- for Win ---#

detectCores()          # detect number of cores
cl <- makeCluster(8)   # select number of cores
registerDoParallel(cl) # register cluster
getDoParWorkers()      # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
stopCluster(cl)
on.exit(stopCluster(cl))

##############
# Import data 
##############

##-- Load Train/Existing data (Dataset 1) --##

PdExistOOB <- read.csv("existingproductattributes2017.csv", stringsAsFactors = FALSE)

##--- Load Predict/New data (Dataset 2) [dv = NA, 0, or Blank] ---##

PdNewOOB <- read.csv("newproductattributes2017.csv", stringsAsFactors = FALSE)

###############
# Evaluate data
################

##--- Dataset 1 ---##

str(PdExistOOB)  
#'data.frame':	80 obs. of  18 variables:
#  $ ProductType          : chr  "PC" "PC" "PC" "Laptop" ...
#$ ProductNum           : int  101 102 103 104 105 106 107 108 109 110 ...
#$ Price                : num  949 2250 399 410 1080 ...
#$ x5StarReviews        : int  3 2 3 49 58 83 11 33 16 10 ...
#$ x4StarReviews        : int  3 1 0 19 31 30 3 19 9 1 ...
#$ x3StarReviews        : int  2 0 0 8 11 10 0 12 2 1 ...
#$ x2StarReviews        : int  0 0 0 3 7 9 0 5 0 0 ...
#$ x1StarReviews        : int  0 0 0 9 36 40 1 9 2 0 ...
#$ PositiveServiceReview: int  2 1 1 7 7 12 3 5 2 2 ...
#$ NegativeServiceReview: int  0 0 0 8 20 5 0 3 1 0 ...
#$ Recommendproduct     : num  0.9 0.9 0.9 0.8 0.7 0.3 0.9 0.7 0.8 0.9 ...
#$ BestSellersRank      : int  1967 4806 12076 109 268 64 NA 2 NA 18 ...
#$ ShippingWeight       : num  25.8 50 17.4 5.7 7 1.6 7.3 12 1.8 0.75 ...
#$ ProductDepth         : num  23.9 35 10.5 15 12.9 ...
#$ ProductWidth         : num  6.62 31.75 8.3 9.9 0.3 ...
#$ ProductHeight        : num  16.9 19 10.2 1.3 8.9 ...
#$ ProfitMargin         : num  0.15 0.25 0.08 0.08 0.09 0.05 0.05 0.05 0.05 0.05 ...
#$ Volume               : int  12 8 12 196 232 332 44 132 64 40 ...

# view first/last obs/rows
head(PdExistOOB)
tail(PdExistOOB)

# check for missing values 
anyNA(PdExistOOB)
which(is.na(PdExistOOB))
apply(is.na(PdExistOOB), 2, which)

# check for duplicates
anyDuplicated(PdExistOOB)

##--- Dataset 2 ---##

str(PdNewOOB) 
#'data.frame':	24 obs. of  18 variables:
#  $ ProductType          : chr  "PC" "PC" "Laptop" "Laptop" ...
#$ ProductNum           : int  171 172 173 175 176 178 180 181 183 186 ...
#$ Price                : num  699 860 1199 1199 1999 ...
#$ x5StarReviews        : int  96 51 74 7 1 19 312 23 3 296 ...
#$ x4StarReviews        : int  26 11 10 2 1 8 112 18 4 66 ...
#$ x3StarReviews        : int  14 10 3 1 1 4 28 7 0 30 ...
#$ x2StarReviews        : int  14 10 3 1 3 1 31 22 1 21 ...
#$ x1StarReviews        : int  25 21 11 1 0 10 47 18 0 36 ...
#$ PositiveServiceReview: int  12 7 11 2 0 2 28 5 1 28 ...
#$ NegativeServiceReview: int  3 5 5 1 1 4 16 16 0 9 ...
#$ Recommendproduct     : num  0.7 0.6 0.8 0.6 0.3 0.6 0.7 0.4 0.7 0.8 ...
#$ BestSellersRank      : int  2498 490 111 4446 2820 4140 2699 1704 5128 34 ...
#$ ShippingWeight       : num  19.9 27 6.6 13 11.6 5.8 4.6 4.8 4.3 3 ...
#$ ProductDepth         : num  20.63 21.89 8.94 16.3 16.81 ...
#$ ProductWidth         : num  19.2 27 12.8 10.8 10.9 ...
#$ ProductHeight        : num  8.39 9.13 0.68 1.4 0.88 1.2 0.95 1.5 0.97 0.37 ...
#$ ProfitMargin         : num  0.25 0.2 0.1 0.15 0.23 0.08 0.09 0.11 0.09 0.1 ...
#$ Volume               : int  0 0 0 0 0 0 0 0 0 0 ...

# view first/last obs/rows
head(PdNewOOB)
tail(PdNewOOB)

# check for missing values 
anyNA(PdNewOOB)
# check for duplicates
anyDuplicated(PdNewOOB)

#############
# Preprocess
#############

##--- Dataset 1 ---##

# rename a column
names(PdExistOOB) <- c("ProductType","ProductNum","Price","5Star","4Star","3Star","2Star","1Star","Positive","Negative","Recommend","BestSellerRank","Weight","Depth","Width","Height","ProfitMargin","Volume") 
str(PdExistOOB) 
#'data.frame':	80 obs. of  18 variables:
#  $ ProductType   : chr  "PC" "PC" "PC" "Laptop" ...
#$ ProductNum    : int  101 102 103 104 105 106 107 108 109 110 ...
#$ Price         : num  949 2250 399 410 1080 ...
#$ 5Star         : int  3 2 3 49 58 83 11 33 16 10 ...
#$ 4Star         : int  3 1 0 19 31 30 3 19 9 1 ...
#$ 3Star         : int  2 0 0 8 11 10 0 12 2 1 ...
#$ 2Star         : int  0 0 0 3 7 9 0 5 0 0 ...
#$ 1Star         : int  0 0 0 9 36 40 1 9 2 0 ...
#$ Positive      : int  2 1 1 7 7 12 3 5 2 2 ...
#$ Negative      : int  0 0 0 8 20 5 0 3 1 0 ...
#$ Recommend     : num  0.9 0.9 0.9 0.8 0.7 0.3 0.9 0.7 0.8 0.9 ...
#$ BestSellerRank: int  1967 4806 12076 109 268 64 NA 2 NA 18 ...
#$ Weight        : num  25.8 50 17.4 5.7 7 1.6 7.3 12 1.8 0.75 ...
#$ Depth         : num  23.9 35 10.5 15 12.9 ...
#$ Width         : num  6.62 31.75 8.3 9.9 0.3 ...
#$ Height        : num  16.9 19 10.2 1.3 8.9 ...
#$ ProfitMargin  : num  0.15 0.25 0.08 0.08 0.09 0.05 0.05 0.05 0.05 0.05 ...
#$ Volume        : int  12 8 12 196 232 332 44 132 64 40 ...

# change data types
#DataFrame$Variable <- as.numeric(DataFrameB$Variable)
#str(DataFrame)

# dummify the data
PdExist_dummy <- dummyVars(" ~ .", data = PdExistOOB)
PdExist_dummy <- data.frame(predict(PdExist_dummy, newdata = PdExistOOB))
str(PdExist_dummy)
#$ ProductTypeAccessories     : num  0 0 0 0 0 1 1 1 1 1 ...
#$ ProductTypeDisplay         : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypeExtendedWarranty: num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypeGameConsole     : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypeLaptop          : num  0 0 0 1 1 0 0 0 0 0 ...
#$ ProductTypeNetbook         : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypePC              : num  1 1 1 0 0 0 0 0 0 0 ...
#$ ProductTypePrinter         : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypePrinterSupplies : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypeSmartphone      : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypeSoftware        : num  0 0 0 0 0 0 0 0 0 0 ...
#$ ProductTypeTablet          : num  0 0 0 0 0 0 0 0 0 0 ...

# handle missing values 
#PdExist_dummy <- na.omit(PdExist_dummy)
PdExist_dummy <- na.exclude(PdExist_dummy)
str(PdExist_dummy)
anyNA(PdExist_dummy)
#DataFrame$Var[is.na(DataFrame$Var)] <- mean(DataFrame$Var,na.rm = TRUE)

##--- Dataset 2 ---##

# rename a column
names(PdNewOOB) <- c("ProductType","ProductNum","Price","5Star","4Star","3Star","2Star","1Star","Positive","Negative","Recommend","BestSellerRank","Weight","Depth","Width","Height","ProfitMargin","Volume") 
str(PdNewOOB) 

# change data types
#DataFrame$Variable <- as.numeric(DataFrameB$Variable)
#str(DataFrame)

# dummify the data
PdNew_dummy <- dummyVars(" ~ .", data = PdNewOOB)
PdNew_dummy <- data.frame(predict(PdNew_dummy, newdata = PdNewOOB))
str(PdNew_dummy)

# handle missing values 
#PdNew_dummy <- na.omit(PdNew_dummy)
PdNew_dummy <- na.exclude(PdNew_dummy)
str(PdNew_dummy)
anyNA(PdNew_dummy)
#DataFrame$Var[is.na(DataFrame$Var)] <- mean(DataFrame$Var,na.rm = TRUE)

#####################
# EDA/Visualizations
#####################

# statistics
summary(PdExist_dummy)

# plots
hist(PdExist_dummy$Recommend)
plot(PdExist_dummy$Volume, PdExist_dummy$Positive)
qqnorm(PdExist_dummy$Volume) 

#######################
# Correlation analysis
#######################

# good for num/int data 
# calculate correlation matrix for all vars
corrAll <- cor(PdExist_dummy)

# view the correlation matrix
corrAll

# plot correlation matrix
corrplot(corrAll, method = "circle")
corrplot(corrAll, method = "circle", order = "hclust") # sorts based on level of collinearity

# find IVs that are highly corrected (ideally >0.90)
corrIV <- cor(PdExist_dummy[,1:17])
corrIV

# create object with indexes of highly corr features
corrIVhigh <- findCorrelation(corrIV, cutoff=0.9)   
corrIVhigh   # print indexes of highly correlated attributes

# get var name of high corr IV
colnames(PdExist_dummy[corrIVhigh]) # "X.4Star"
colnames(PdExist_dummy[c(16)]) # "X.4Star" 
colnames(PdExist_dummy[16]) # "X.4Star" 

# remove highly correlated features
PdExist_corr <- PdExist_dummy[-c(16)] #code to remove highly corr features
str(PdExist_corr)  # X.4Star is removed

############
# caret RFE 
############

# lmFuncs - linear model
# rfFuncs - random forests
# treebagFuncs - bagged trees

#register the sequential backend to "unregister" a foreach backend
registerDoSEQ()

## ---- linear model ---- ##

# define refControl using a linear model selection function (regression only)
LMcontrol <- rfeControl(functions=lmFuncs, method="cv", number=10)

# run the RFE algorithm
set.seed(7)
rfeLM <- rfe(PdExist_dummy[,1:28], PdExist_dummy[,29], sizes=c(1:28), rfeControl=LMcontrol)
rfeLM
#  Variables      RMSE Rsquared       MAE    RMSESD RsquaredSD     MAESD Selected
#          1 1.117e-13        1 9.073e-14 1.229e-13          0 1.016e-13        *
#          2 1.240e-13        1 9.180e-14 1.128e-13          0 9.042e-14         
#          3 1.520e-13        1 1.008e-13 1.196e-13          0 8.574e-14         
#          4 2.857e-13        1 1.819e-13 3.982e-13          0 2.255e-13         
#          5 2.930e-13        1 1.864e-13 3.981e-13          0 2.243e-13         
#          6 3.079e-13        1 1.878e-13 3.999e-13          0 2.249e-13         
#          7 3.091e-13        1 1.906e-13 3.990e-13          0 2.242e-13         
#          8 3.083e-13        1 2.019e-13 3.912e-13          0 2.227e-13         
#          9 3.203e-13        1 2.136e-13 3.924e-13          0 2.239e-13         
#         10 3.189e-13        1 2.100e-13 3.919e-13          0 2.233e-13         
#         ....
#         27 1.228e-12        1 7.926e-13 1.280e-12          0 5.934e-13         
#The top 1 variables (out of 1):
#  X.5Star.

# plot the results
plot(rfeLM, type=c("g", "o"))  

# show predictors used
predictors(rfeLM) # "X.5Star."

# show var importance
varImp(rfeLM)
# Note results
#        Overall
#X.5Star.     4

## ---- random forests ---- ##

# define the control using a random forest selection function (regression or classification)
RFcontrol <- rfeControl(functions=rfFuncs, method="cv", number=10, repeats=1)

# run the RFE algorithm
set.seed(7)
rfeRF <- rfe(PdExist_dummy[,1:28], PdExist_dummy[,29], sizes=c(1:28), rfeControl=RFcontrol)
rfeRF 
#Variables  RMSE Rsquared   MAE RMSESD RsquaredSD MAESD Selected
#        1 298.1   0.9956 121.9  693.2   0.007626 271.2        *
#        2 416.3   0.9773 187.2  807.8   0.030996 328.7         
#        3 508.8   0.9672 223.4  791.5   0.032845 321.7         
#        4 658.9   0.9479 291.8  847.5   0.040380 349.4         
#        5 731.6   0.9258 333.5  826.8   0.064006 348.9         
#        6 702.3   0.9425 311.8  854.3   0.048371 355.9         
#        7 721.1   0.9333 324.2  862.2   0.065295 362.7         
#        8 758.0   0.9200 343.7  909.7   0.081980 385.8         
#        9 702.9   0.9402 316.5  871.9   0.057041 367.0         
#       10 738.2   0.9303 333.8  894.1   0.064837 374.1       
        .....
#       28 711.2   0.9347 322.7  858.3   0.053938 356.4         
#The top 1 variables (out of 1):
#  X.5Star.

# plot the results
plot(rfeRF, type=c("g", "o"))

# show predictors used
predictors(rfeRF) # "X.5Star."  

# Variable Importance  
varImp(rfeRF)

# Note results.
#         Overall
#X.5Star. 13.50853

## ----  bagged trees ---- ##

# define the control using a bagged trees selection function 
TBcontrol <- rfeControl(functions=treebagFuncs, method="cv", number=10, repeats=1)

# run the RFE algorithm
set.seed(7)
rfeTB <- rfe(PdExist_dummy[,1:28], PdExist_dummy[,29], sizes=c(1:28), rfeControl=TBcontrol)
rfeTB 
# Variables  RMSE Rsquared   MAE RMSESD RsquaredSD MAESD Selected
#         1 835.6   0.8954 403.0 1035.5    0.12311 398.1         
#         2 814.9   0.9095 385.5 1010.2    0.10409 388.1         
#         3 799.7   0.9171 389.3 1025.8    0.09974 392.9         
#         4 865.2   0.9077 406.6 1021.8    0.09451 390.2         
#         5 859.1   0.8993 415.1 1017.7    0.13518 394.7         
#         6 811.6   0.9176 400.5  936.4    0.06855 367.4         
#         7 828.6   0.8931 394.0 1016.6    0.12072 384.0         
#         8 776.3   0.9164 379.2  972.4    0.07489 371.3         
#         9 908.6   0.8861 437.1  956.0    0.07531 381.1         
#        10 831.7   0.9049 398.8 1029.0    0.11246 400.9         
#        11 829.6   0.8964 394.7 1038.8    0.09706 402.7         
#        12 787.8   0.9295 388.9  988.2    0.08346 380.2         
#        13 787.8   0.9264 382.1 1007.1    0.09639 387.5         
#        14 807.9   0.9173 381.8 1041.3    0.09811 397.4         
#        15 809.3   0.9224 391.5  998.2    0.08389 384.6         
#        16 789.0   0.9105 384.1 1028.6    0.11430 391.9         
#        17 795.1   0.9145 386.4 1047.6    0.10616 392.2         
#        18 830.8   0.9050 411.7  998.3    0.08555 388.7         
#        19 792.6   0.9107 385.6 1008.9    0.08857 389.1         
#        20 807.1   0.9017 382.3 1014.3    0.10607 389.9         
#        21 830.4   0.9089 401.8 1041.2    0.09688 395.8         
#        22 754.3   0.9187 369.7 1056.4    0.12278 407.3        *
#        23 819.1   0.9051 389.3 1020.7    0.08464 383.4         
#        24 775.4   0.9123 382.2 1007.1    0.10313 378.4         
#        25 847.8   0.8921 410.8  995.9    0.10919 396.7         
#        26 811.3   0.8973 385.3 1031.2    0.11905 387.1         
#        27 856.8   0.9100 432.1 1031.8    0.11997 392.0         
#        28 845.7   0.8916 422.1 1015.0    0.12660 399.3         
#The top 5 variables (out of 22):
#  X.5Star., Positive, X.4Star., X.1Star., X.3Star.

# plot the results
plot(rfeTB, type=c("g", "o"))

# show predictors used
predictors(rfeTB) 
#[1] "X.5Star."                    "Positive"                    "X.4Star."                    "X.1Star."                   
#[5] "X.3Star."                    "X.2Star."                    "Negative"                    "BestSellerRank"             
#[9] "Recommend"                   "Width"                       "ProfitMargin"                "ProductTypeExtendedWarranty"
#[13] "Weight"                      "ProductNum"                  "Depth"                       "Price"                      
#[17] "Height"                      "ProductTypeAccessories"      "ProductTypeDisplay"          "ProductTypeGameConsole"     
#[21] "ProductTypeLaptop"           "ProductTypeNetbook"     

# Variable Importance  
varImp(rfeTB)

# Note results.
#                            Overall
#X.5Star.                    1.379177385
#Positive                    1.069651297
#X.4Star.                    1.036357124
#X.1Star.                    0.771315076
#X.3Star.                    0.600261175
#X.2Star.                    0.518222317
#Negative                    0.288543272
#BestSellerRank              0.067863770
#Recommend                   0.047107369
#Width                       0.028203501
#ProfitMargin                0.026055800
#Weight                      0.017056033
#ProductTypeExtendedWarranty 0.015393828
#Depth                       0.012947075
#ProductNum                  0.012590334
#Height                      0.008727418
#Price                       0.008648313

##--- create ds with features using varImp from top model ---##

# create ds with predictors from varImp
PdExist_rfeTB <- PdExist_dummy[,predictors(rfeTB)]
str(PdExist_rfeTB)

# add dv
PdExist_rfeTB$Volume <- PdExist_dummy$Volume

# confirm new ds
str(PdExist_rfeTB)

##################
# Train/test sets
##################

# PdExist_dummy
# define an 75%/25% train/test split of the dataset
set.seed(123) 
inTraining <- createDataPartition(PdExist_dummy$Volume, p=0.75, list=FALSE)
dummyTrain <- PdExist_dummy[inTraining,]   
dummyTest <- PdExist_dummy[-inTraining,]  

# verify number of obs 
nrow(dummyTrain) # 50
nrow(dummyTest)  # 15

# PdExist_corr (dataset without highly corr features)
set.seed(123) 
inTraining_corr <- createDataPartition(PdExist_corr$Volume, p=0.75, list=FALSE)
corrTrain <- PdExist_corr[inTraining_corr,]   
corrTest <- PdExist_corr[-inTraining_corr,] 
nrow(corrTrain) # 50
nrow(corrTest)  # 15

# PdExist_rfeTB (dataset with features using varImp from top model)
set.seed(123)
inTraining_rfe <- createDataPartition(PdExist_rfeTB$Volume, p=0.75, list=FALSE)
rfeTrain <- PdExist_rfeTB[inTraining_rfe,]   
rfeTest <- PdExist_rfeTB[-inTraining_rfe,] 
nrow(rfeTrain) # 50
nrow(rfeTest)  # 15

################
# Train control
################

# 5 fold cross validation
fitControl <- trainControl(method="repeatedcv", number=5, repeats=1) 

###############
# Train models
###############

modelLookup('lm')
#  model parameter     label forReg forClass probModel
#1    lm intercept intercept   TRUE    FALSE     FALSE
modelLookup('rf')
#  model parameter                         label forReg forClass probModel
#1    rf      mtry #Randomly Selected Predictors   TRUE     TRUE      TRUE
modelLookup('svmLinear')
#      model parameter label forReg forClass probModel
#1 svmLinear         C  Cost   TRUE     TRUE      TRUE
modelLookup('xgbTree')
#    model        parameter                          label forReg forClass probModel
#1 xgbTree          nrounds          # Boosting Iterations   TRUE     TRUE      TRUE
#2 xgbTree        max_depth                 Max Tree Depth   TRUE     TRUE      TRUE
#3 xgbTree              eta                      Shrinkage   TRUE     TRUE      TRUE
#4 xgbTree            gamma         Minimum Loss Reduction   TRUE     TRUE      TRUE
#5 xgbTree colsample_bytree     Subsample Ratio of Columns   TRUE     TRUE      TRUE
#6 xgbTree min_child_weight Minimum Sum of Instance Weight   TRUE     TRUE      TRUE
#7 xgbTree        subsample           Subsample Percentage   TRUE     TRUE      TRUE

## ------- random forests ------- ##

# default
set.seed(123)
dummyRFfit <- train(Volume~., data=dummyTrain, method="rf", importance=T, trControl=fitControl)
dummyRFfit

# manual grid
# dataframe for manual tuning of mtry
#rfGrid <- expand.grid(mtry=c(4,5,6))  
#set.seed(123)
#dummyRFfit <- train(Volume~.,data=dummyTrain, method="rf",importance=T, trControl=fitControl, tuneGrid=rfGrid)
#dummyRFfit

####################################################################
#Random Forest 
#
#50 samples
#28 predictors
#
#No pre-processing
#Resampling: Cross-Validated (5 fold, repeated 1 times) 
#Summary of sample sizes: 41, 40, 39, 40, 40 
#Resampling results across tuning parameters:
#  
#  mtry  RMSE       Rsquared   MAE     
#2    1097.8794  0.7647353  544.8234
#15     989.0788  0.8610598  376.1737
#28     996.3648  0.8499210  368.9118
#
#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was mtry = 15.
####################################################################

plot(dummyRFfit)
varImp(dummyRFfit)
#                           Overall
#X.5Star.                     100.00
#Positive                      88.64
#X.4Star.                      65.00
#X.1Star.                      57.67
#X.2Star.                      47.90
#X.3Star.                      40.35
#ProductTypeExtendedWarranty   33.86
#ProductTypeLaptop             22.37
#Price                         22.10
#ProductTypeAccessories        21.08
#ProductTypeDisplay            21.08
#ProductNum                    20.78
#Depth                         20.60
#ProductTypeSmartphone         20.27
#ProductTypeNetbook            19.89
#ProductTypePrinter            19.54
#ProductTypeSoftware           19.28
#Negative                      18.82
#Width                         17.44
#ProfitMargin                  16.81

## ------- linear model ------- ##

# LM train/fit
set.seed(123)
dummyLMfit <- train(Volume~., data=dummyTrain, method="lm", trControl=fitControl)
dummyLMfit
###################################################################
#Linear Regression 
#
#50 samples
#28 predictors
#
#No pre-processing
#Resampling: Cross-Validated (5 fold, repeated 1 times) 
#Summary of sample sizes: 41, 40, 39, 40, 40 
#Resampling results:
#  
#  RMSE          Rsquared  MAE         
#3.192664e-12  1         1.287143e-12
#
#Tuning parameter 'intercept' was held constant at a value of TRUE
###################################################################

varImp(dummyLMfit)
#                            Overall
#X.5Star.                    1.000e+02
#ProductTypeAccessories      5.451e-14
#ProductNum                  4.159e-14
#ProductTypeGameConsole      3.957e-14
#ProductTypeDisplay          3.638e-14
#....
#Warning message:
#In summary.lm(object) : essentially perfect fit: summary may be unreliable


## ------- Support Vector Machine (SVMLinear) ------- ##

# LM train/fit
set.seed(123)
dummySVMfit <- train(Volume~., data=dummyTrain, method="svmLinear", trControl=fitControl)
dummySVMfit
###################################################################
#Support Vector Machines with Linear Kernel 
#
#50 samples
#28 predictors
#
#No pre-processing
#Resampling: Cross-Validated (5 fold, repeated 1 times) 
#Summary of sample sizes: 39, 41, 40, 39, 41 
#Resampling results:
#  
#  RMSE      Rsquared  MAE     
#453.7197  0.884558  214.1205
#
#Tuning parameter 'C' was held constant at a value of 1
###################################################################

varImp(dummySVMfit)
#                      Overall
#X.5Star.               100.000
#X.4Star.                91.812
#Positive                84.477
#X.3Star.                58.605
#X.2Star.                54.084
#Negative                31.261
#ProductTypeGameConsole  22.877
#ProductNum              16.984
#Width                   12.138
#Depth                    8.945
#Height                   8.444
#Price                    7.356
#ProfitMargin             6.029
#Weight                   5.927
#BestSellerRank           5.809
#ProductTypeAccessories   2.430
#ProductTypePC            1.570
#Recommend                1.379
#ProductTypePrinter       1.290
#ProductTypeLaptop        1.061

## ------- Gradient Boosting ------- ##

# LM train/fit
set.seed(123)
str(dummyTrain)
dummyGBfit <- train(Volume~., data=dummyTrain, method="xgbTree", trControl=fitControl)
#warnings()
#getModelInfo('gbm')[['gbm']]$fit
dummyGBfit
###################################################################
#eXtreme Gradient Boosting 
#
#50 samples
#28 predictors
#
#No pre-processing
#Resampling: Cross-Validated (5 fold, repeated 1 times) 
#Summary of sample sizes: 39, 41, 41, 39, 40 
#Resampling results across tuning parameters:
#  
#  eta  max_depth  colsample_bytree  subsample  nrounds  RMSE       Rsquared   MAE     
#  0.3  1          0.6               0.50        50       839.6662  0.8662376  373.7451
#  0.3  1          0.6               0.50       100       801.6406  0.8814140  361.7709
#  0.3  1          0.6               0.50       150       799.0789  0.8771864  361.7332
#  0.3  1          0.6               0.75        50       664.9946  0.9350129  259.0091
#  0.3  1          0.6               0.75       100       669.1843  0.9341225  261.0907
#.
#.
#.
#
#Tuning parameter 'gamma' was held constant at a value of 0
#Tuning
#parameter 'min_child_weight' was held constant at a value of 1
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were nrounds = 50, max_depth = 2, eta = 0.4, gamma =
#  0, colsample_bytree = 0.8, min_child_weight = 1 and subsample = 0.75.
###################################################################

varImp(dummyGBfit)
#                       Overall
#X.5Star.               1.000e+02
#X.4Star.               7.703e+01
#X.1Star.               1.158e+01
#X.3Star.               2.537e+00
#ProductNum             7.140e-01
#X.2Star.               3.824e-01
#ProductTypeAccessories 2.449e-01
#ProductTypeGameConsole 2.119e-01
#Recommend              9.997e-02
#Weight                 7.632e-02
#BestSellerRank         6.583e-02
#ProductTypeSoftware    3.215e-02
#Width                  2.982e-02
#Positive               2.950e-02
#Negative               1.944e-02
#Price                  1.853e-02
#Height                 1.453e-02
#ProductTypeLaptop      5.225e-03
#Depth                  3.201e-03
#ProductTypeTablet      1.482e-03

##################
# Model selection
##################

#-- dummyTrain --# 
# Use resamples function to compare model performances
FitComp <- resamples(list(rf=dummyRFfit, svmLinear=dummySVMfit, xgbTree=dummyGBfit))

# output summary metrics for tuned models 
summary(FitComp)
############################################################################
#Call:
#  summary.resamples(object = FitComp)
#
#Models: rf, svmLinear, xgbTree 
#Number of resamples: 5 
#
#MAE 
#                 Min.     1st Qu.    Median     Mean  3rd Qu.     Max. NA's
#rf        75.14816000 150.0743111 283.80913 376.1737 429.4900 942.3467    0
#svmLinear  0.09646404   0.1416431 151.78846 214.1205 322.3076 596.2682    0
#xgbTree   29.98437444  82.0056569  85.67462 235.8542 122.5238 859.0824    0
#
#RMSE 
#                 Min.     1st Qu.   Median     Mean   3rd Qu.     Max. NA's
#rf        108.9348684 254.1352330 717.0958 989.0788 1131.3855 2733.843    0
#svmLinear   0.1216601   0.1854551 221.9110 453.7197  572.1856 1474.195    0
#xgbTree    34.8002978 116.1740190 142.2096 654.9664  234.5771 2747.071    0
#
#Rsquared 
#                Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf        0.7457715 0.7984011 0.8761115 0.8610598 0.9172709 0.9677440    0
#svmLinear 0.6683976 0.8729452 0.8814473 0.8845580 1.0000000 1.0000000    0
#xgbTree   0.6993975 0.9720502 0.9879915 0.9313151 0.9982170 0.9989193    0
###############################################################################

############################
# Predict testSet/validation
############################

# predict with SVM
svmPred1 <- predict(dummySVMfit, dummyTest)
rfPred1 <- predict(dummyRFfit, dummyTest)
gbPred1 <- predict(dummyGBfit, dummyTest)

# performace measurment
postResample(svmPred1, dummyTest$Volume)
#       RMSE    Rsquared         MAE 
#365.5822410   0.9013156 285.7031443 

postResample(rfPred1, dummyTest$Volume)
#      RMSE    Rsquared         MAE 
#170.9811713   0.9287063  89.2645778 

postResample(gbPred1, dummyTest$Volume)
#       RMSE    Rsquared         MAE 
#208.3635127   0.8954733 123.3704985

# plot predicted verses actual
plot(svmPred1, dummyTest$Volume)
plot(rfPred1, dummyTest$Volume)

###############################
# Predict new data (Dataset 2)
###############################

svmPred2 <- predict(dummySVMfit, PdNew_dummy)
head(svmPred2)
#1           2           3           4           5           6 
#55.025934 -319.948512    7.396812  -10.519562   80.978306  -66.617962 

rfPred2 <- predict(dummyRFfit, PdNew_dummy)
head(rfPred2)
#1         2         3         4         5         6 
#478.42240 294.55760 264.03907  29.54480  25.03027  88.80800 

gbPred2 <- predict(dummyGBfit, PdNew_dummy)
head(gbPred2)
#[1] 750.44714 575.21161 301.98965  50.44928  43.58993  85.88957

# combine predicted result with the Incomplete dataset
PdNewOOB$Volume <- rfPred2

# write the results to csv file
write.csv(PdNewOOB, "rfPred2.csv", row.names = F)

# Merge two dataset
DataTotal <- rbind(PdExistOOB, PdNewOOB)
str(DataTotal)
write.csv(DataTotal, "DataTotal.csv", row.names = F)
