# Title: caret-pipeline-classification-wy

# Last update: 2021.11

# File: SurveyData_pipeline_Ho_110821.R
# Project name: Brand Preference Prediction


###############
# Project Notes
###############
# Use CompleteResponse dataset to train the models and predict variable "brand" in SurveyIncomplete dataset

# Assignment "<-" short-cut:
#   Win [Alt]+[-]
# Clear console: CTRL + L

###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()

# set working directory
setwd("C:/Users/kukul/OneDrive/0. Data Analytics Course/R Studio/C3T2")
dir()

################
# Load packages
################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
install.packages("doParallel")  # for Win parallel processing (see below) 
install.packages("C50", repos="http://R-Forge.R-project.org")
install.packages("foo", dependencies = TRUE)

library(caret)
library(corrplot)
library(readr)
library(mlbench)
library(doParallel)             # for Win
library('C50')   

#####################
# Parallel processing
#####################
# NOTE: Be sure to use the correct package for your operating system. 

#--- for Win ---#
detectCores()          # detect number of cores
cl <- makeCluster(2)   # select number of cores
registerDoParallel(cl) # register cluster
getDoParWorkers()      # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
stopCluster(cl)
on.exit(stopCluster(cl))

##############
# Import data 
##############

##-- Load Train/Existing data (Dataset 1) --##
SDCompOOB <- read.csv("CompleteResponses.csv", stringsAsFactors = FALSE)
str(SDCompOOB)

##--- Load Predict/New data (Dataset 2) [dv = NA, 0, or Blank] ---##
SDIncompOOB <- read.csv("SurveyIncomplete.csv", stringsAsFactors = FALSE)
str(SDIncompOOB)

##--- Load preprocessed datasets that have been saved ---##

#read.csv("CompleteResponses.csv", stringsAsFactors = FALSE) 
#read.csv("SurveyIncomplete.csv", stringsAsFactors = FALSE) 

###############
# Save datasets
###############

# can save all datasets to csv format, or 
# can save the ds with the best performance (after all modeling)
write.csv(SDCompOOB, "SDCompOOB.csv", row.names = F)
write.csv(SDIncompOOB, "SDIncompOOB.csv", row.names = F)
dir()

################
# Evaluate data
################

##--- Dataset 1 ---##
str(SDCompOOB)   
#'data.frame':	9898 obs. of  7 variables:
#  $ salary : num  119807 106880 78021 63690 50874 ...
#$ age    : int  45 63 23 51 20 56 24 62 29 41 ...
#$ elevel : int  0 1 0 3 3 3 4 3 4 1 ...
#$ car    : int  14 11 15 6 14 14 8 3 17 5 ...
#$ zipcode: int  4 6 2 5 4 3 5 0 0 4 ...
#$ credit : num  442038 45007 48795 40889 352951 ...
#$ brand  : int  0 1 0 1 0 1 1 1 0 1 ...

# view first/last obs/rows
head(SDCompOOB)
tail(SDCompOOB)

# check for missing values 
anyNA(SDCompOOB)

# check for duplicates
anyDuplicated((SDCompOOB))

##--- Dataset 2 ---##
str(SDIncompOOB)
#'data.frame':	5000 obs. of  7 variables:
#  $ salary : num  150000 82524 115647 141443 149211 ...
#$ age    : int  76 51 34 22 56 26 64 50 26 46 ...
#$ elevel : int  1 1 0 3 0 4 3 3 2 3 ...
#$ car    : int  3 8 10 18 5 12 1 9 3 18 ...
#$ zipcode: int  3 3 2 2 3 1 2 0 4 6 ...
#$ credit : num  377980 141658 360980 282736 215667 ...
#$ brand  : int  1 0 1 1 1 1 1 1 1 0 ...

# view first/last obs/rows
head(SDIncompOOB)
tail(SDIncompOOB)
# check for missing values 
anyNA(SDIncompOOB)
# check for duplicates
anyDuplicated((SDIncompOOB))

#############
# Preprocess
#############

##--- Dataset 1 ---##

# remove any features
#SDCompOOB$X <- NULL   # remove X feature
#str(SDCompOOB) # confirm removed features

# rename a column
names(SDCompOOB) <- c("Salary","Age","Education","Car","Zip","Credit","Brand") 
str(SDCompOOB) # confirm removed features

# change data types
SDCompOOB$Age <- as.numeric(SDCompOOB$Age)
SDCompOOB[,'Brand']<-factor(SDCompOOB[,'Brand'])
str(SDCompOOB)

# handle missing values (if applicable) 
na.omit(SDCompOOB$Brand)
na.exclude(SDCompOOB$Brand)        
SDCompOOB$Brand[is.na(wyCompOOB$Brand)] <- mean(wyCompOOB$Brand,na.rm = TRUE)

##--- Dataset 2 ---##

# remove any features
#SDIncompOOB$X <- NULL   # remove X feature
#str(SDIncompOOB) # confirm removed features

# rename a column
names(SDIncompOOB) <- c("Salary","Age","Education","Car","Zip","Credit","Brand") 
str(SDIncompOOB) # confirm removed features

# change data types
SDIncompOOB$Age <- as.numeric(SDIncompOOB$Age)
SDIncompOOB[,'Brand']<-factor(SDIncompOOB[,'Brand'])
str(SDIncompOOB)

# handle missing values (if applicable) 
na.omit(SDIncompOOB$Brand)  #remove any rows with NA values in specific column or whole dataset 
na.exclude(SDIncompOOB$Brand)  #does not use the missing values, but maintains their position for the residuals and fitted values.       
SDIncompOOB$Brand[is.na(SDIncompOOB$Brand)] <- mean(SDIncompOOB$Brand, na.rm = TRUE) # "na.rm = TRUE" means remove NA values from the calculation
str(SDIncompOOB)

#####################
# EDA/Visualizations
#####################

# statistics
summary(SDCompOOB)
#Salary            Age          Education          Car             Zip            Credit           Brand       
#Min.   : 20000   Min.   :20.00   Min.   :0.000   Min.   : 1.00   Min.   :0.000   Min.   :     0   Min.   :0.0000  
#1st Qu.: 52082   1st Qu.:35.00   1st Qu.:1.000   1st Qu.: 6.00   1st Qu.:2.000   1st Qu.:120807   1st Qu.:0.0000  
#Median : 84950   Median :50.00   Median :2.000   Median :11.00   Median :4.000   Median :250607   Median :1.0000  
#Mean   : 84871   Mean   :49.78   Mean   :1.983   Mean   :10.52   Mean   :4.041   Mean   :249176   Mean   :0.6217  
#3rd Qu.:117162   3rd Qu.:65.00   3rd Qu.:3.000   3rd Qu.:15.75   3rd Qu.:6.000   3rd Qu.:374640   3rd Qu.:1.0000  
#Max.   :150000   Max.   :80.00   Max.   :4.000   Max.   :20.00   Max.   :8.000   Max.   :500000   Max.   :1.0000  

# plots
hist(SDCompOOB$Salary)
hist(SDCompOOB$Age)
hist(SDCompOOB$Education)
hist(SDCompOOB$Car)
hist(SDCompOOB$Credit)
plot(SDCompOOB$Salary, SDCompOOB$Brand)
qqnorm(SDCompOOB$Salary) # Be familiar with this plot, but don't spend a lot of time on it

################
# Sampling
################

# create 10% sample 
set.seed(1) # set random seed
SDCompOOB10p <- SDCompOOB[sample(1:nrow(SDCompOOB), round(nrow(SDCompOOB)*.1),replace=FALSE),]
nrow(SDCompOOB10p)
head(SDCompOOB10p) # ensure randomness

# 1k sample
set.seed(1) # set random seed
SDCompOOB1k <- SDCompOOB[sample(1:nrow(SDCompOOB), 1000, replace=FALSE),]
nrow(SDCompOOB1k) # ensure number of obs
head(SDCompOOB1k) # ensure randomness

#######################
# Feature selection
#######################

#######################
# Correlation analysis
#######################

# good for num/int data 

# calculate correlation matrix for all vars
corrAll <- cor(SDCompOOB1k[,1:7])
# view the correlation matrix
corrAll
# plot correlation matrix
corrplot(corrAll, method = "circle")
corrplot(corrAll, order = "hclust") # sorts based on level of collinearity
# find IVs that are highly corrected (ideally >0.90)
corrIV <- cor(SDCompOOB1k[,1:6])
corrIV
# create object with indexes of highly corr features
corrIVhigh <- findCorrelation(corrIV, cutoff=0.8)   
# print indexes of highly correlated attributes
corrIVhigh
# get var name of high corr IV
colnames(SDCompOOB1k[corrIVhigh]) # NA
colnames(SDCompOOB1k[c(0)]) # NA
colnames(SDCompOOB1k[0]) # NA
# remove highly correlated features
corr_SD <- SDCompOOB1k[-c(0)] #code to remove highly corr features
str(corr_SD)

##########################################
# caret RFE(recursive feature elimination) 
##########################################

# lmFuncs - linear model
# rfFuncs - random forests
# nbFuncs - naive Bayes
# treebagFuncs - bagged trees
# caret's train function - caretFuncs

#register the sequential backend to "unregister" a foreach backend
registerDoSEQ()

## ---- random forests ---- ##

# define the control using a random forest selection function (regression or classification)
RFcontrol <- rfeControl(functions=rfFuncs, method="cv", number=10, repeats=1)

# run the RFE algorithm
set.seed(7)
rfeRF <- rfe(SDCompOOB1k[,1:6], SDCompOOB1k[,7], sizes=c(1:6), rfeControl=RFcontrol)
rfeRF 
#Outer resampling method: Cross-Validated (10 fold) 
#Resampling performance over subset size:
#  Variables Accuracy  Kappa AccuracySD KappaSD Selected
#          1    0.648 0.2501    0.05978 0.13195         
#          2    0.906 0.8013    0.03534 0.07551         
#          3    0.908 0.8045    0.03155 0.06798         
#          4    0.917 0.8252    0.03433 0.07126        *
#          5    0.912 0.8144    0.03490 0.07335         
#          6    0.885 0.7560    0.03951 0.08683         
#The top 4 variables (out of 4):
#  Salary, Age, Education, Car

# plot the results
plot(rfeRF, type=c("g", "o"))

# show predictors used
predictors(rfeRF) #"Salary" "Age" "Education" "Car"  

# Variable Importance  
varImp(rfeRF)

# Note results.
#             Overall
#Salary    75.7055989
#Age       48.1128552
#Education  1.4392232
#Zip        0.6221157
#Car        0.6210173
#Credit    -0.6434650

## ----  caret's train function ---- ##

# define the control using a bagged trees selection function 
CAcontrol <- rfeControl(functions=caretFuncs, method="cv", number=5, repeats=1)

# run the RFE algorithm
set.seed(7)
rfeCA <- rfe(SDCompOOB1k[,1:6], SDCompOOB1k[,7], sizes=c(1:6), rfeControl=CAcontrol)
rfeCA 
#Variables Accuracy  Kappa AccuracySD KappaSD Selected
#        1    0.640 0.2350    0.05743 0.12350         
#        2    0.908 0.8061    0.01816 0.03704         
#        3    0.903 0.7949    0.02059 0.04383         
#        4    0.906 0.8015    0.01750 0.03621         
#        5    0.906 0.8018    0.02201 0.04591         
#        6    0.908 0.8057    0.01874 0.03889        *
#  The top 5 variables (out of 6):
#  Salary, Age, Credit, Car, Zip

# plot the results
plot(rfeCA, type=c("g", "o"))

# show predictors used
predictors(rfeCA) # "Salary" "Age" "Credit" "Car" "Zip" "Education"   

#Variable Importance  
varImp(rfeCA)
# Note results.
#             Overall
#Salary    191.591150
#Age       130.464564
#Credit     23.488801
#Car        13.227191
#Zip        10.539802
#Education   7.975161

##--- create ds with features using varImp from top model ---##

# create ds with predictors from varImp
rfeRF_SD <- SDCompOOB1k[,predictors(rfeRF)]
str(rfeRF_SD)

# add dv
rfeRF_SD$Brand <- SDCompOOB1k$Brand

# confirm new ds
str(rfeRF_SD)

##############################
# Feature engineering
##############################

# code goes here

##################
# Train/test sets
##################

# SDCompOOB
#define an 75%/25% train/test split of the dataset
set.seed(123) 
inTraining <- createDataPartition(SDCompOOB1k$Brand, p=0.75, list=FALSE)
oobTrain <- SDCompOOB1k[inTraining,]   
oobTest <- SDCompOOB1k[-inTraining,]  
# verify number of obs 
nrow(oobTrain) # 751
nrow(oobTest)  # 249

# corr_wy (dataset without highly corr features)
#set.seed(123) 
#inTraining_corr <- createDataPartition(corr_SD$Brand, p=0.75, list=FALSE)
#oobTrain_corr <- corr_SD[inTraining_corr,]   
#oobTest_corr <- corr_SD[-inTraining_corr,] 
#nrow(oobTrain_corr)
#nrow(oobTest_corr)  

# rfeRF_wy (dataset with features using varImp from top model)
set.seed(123)
inTraining_rfe <- createDataPartition(rfeRF_SD$Brand, p=0.75, list=FALSE)
oobTrain_rfe <- rfeRF_SD[inTraining_rfe,]   
oobTest_rfe <- rfeRF_SD[-inTraining_rfe,] 
nrow(oobTrain_rfe) # 751
nrow(oobTest_rfe)  # 249

################
# Train control
################

# 10 fold cross validation
fitControl <- trainControl(method="repeatedcv", number=10, repeats=1) 

###############
# Train models
###############

?modelLookup()
modelLookup('rf')
# model parameter                         label forReg forClass probModel
#    rf      mtry #Randomly Selected Predictors   TRUE     TRUE      TRUE

modelLookup('C5.0')
#  model parameter                 label forReg forClass probModel
#1  C5.0    trials # Boosting Iterations  FALSE     TRUE      TRUE
#2  C5.0     model            Model Type  FALSE     TRUE      TRUE
#3  C5.0    winnow                Winnow  FALSE     TRUE      TRUE

## ------- random forests ------- ##

# default
set.seed(123)
oobRFfit <- train(Brand~., data=oobTrain, method="rf", importance=T, trControl=fitControl)
oobRFfit
########################################################################
#Random Forest 
#751 samples
#6 predictor
#2 classes: '0', '1' 
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 677, 676, 676, 676, 675, 676, ... 
#Resampling results across tuning parameters:
#  mtry  Accuracy   Kappa    
#  2     0.8680749  0.7178707
#  4     0.9028174  0.7945484
#  6     0.8974832  0.7835109
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 4.
##########################################################################

# manual grid
# dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(2,4,6))  
set.seed(123)
oobRFfit <- train(Brand~.,data=oobTrain,method="rf",
                  importance=T,
                  trControl=fitControl,
                  tuneGrid=rfGrid)
oobRFfit
########################################################################
#Random Forest 
#751 samples
#6 predictor
#2 classes: '0', '1' 
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 675, 676, 675, 676, 676, 677, ... 
#Resampling results across tuning parameters:
#  mtry  Accuracy   Kappa    
#  4     0.8907824  0.7702186
#  5     0.9001342  0.7890348
#  6     0.8987477  0.7868037
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 5.
#######################################################################

plot(oobRFfit)
varImp(oobRFfit)
#          Overall
#Salary       100.000
#Age           68.113
#Credit         1.911
#Zip            1.595
#Education      1.042
#Car            0.000

## ------- C5.0 ------- ##

# default
set.seed(123)
oobC5fit <- train(Brand~., data=oobTrain, method="C5.0", importance=T, trControl=fitControl)

# manual grid
# dataframe for manual tuning of mtry
C5Grid <- expand.grid(.winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model=c("rules","tree"))  
set.seed(123)
oobC5fit <- train(Brand~.,data=oobTrain,method="C5.0",
                  importance=T,
                  trControl=fitControl,
                  tuneGrid=C5Grid)
oobC5fit
##################################################################################
#C5.0 
#
#751 samples
#6 predictor
#2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (5 fold, repeated 1 times) 
#Summary of sample sizes: 601, 601, 600, 602, 600 
#Resampling results across tuning parameters:
#  
#  model  winnow  trials  Accuracy   Kappa    
#  rules  FALSE    1      0.8268229  0.6374561
#  rules  FALSE    5      0.8082448  0.6080574
#  rules  FALSE   10      0.8228143  0.6419049
#  rules  FALSE   15      0.8228321  0.6424793
#  rules  FALSE   20      0.8228321  0.6424793
#  rules   TRUE    1      0.7972660  0.5525099
#  rules   TRUE    5      0.8028132  0.6044667
#  rules   TRUE   10      0.8041377  0.6063740
#  rules   TRUE   15      0.8028132  0.6038410
#  rules   TRUE   20      0.8028132  0.6038410
#  tree   FALSE    1      0.8268141  0.6362355
#  tree   FALSE    5      0.8174096  0.6304591
#  tree   FALSE   10      0.8241210  0.6442530
#  tree   FALSE   15      0.8160673  0.6267373
#  tree   FALSE   20      0.8160673  0.6267373
#  tree    TRUE    1      0.7985905  0.5546693
#  tree    TRUE    5      0.8134092  0.6285253
#  tree    TRUE   10      0.8134092  0.6285253
#  tree    TRUE   15      0.8134092  0.6285253
#  tree    TRUE   20      0.8134092  0.6285253
#
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were trials = 1, model = rules and winnow = FALSE.
#####################################################################################

plot(oobC5fit)
varImp(oobC5fit)
#          Overall
#Salary     100.00
#Age         98.94
#Education   69.43
#Zip         18.43
#Car          0.00
#Credit       0.00

##################
# Model selection
##################

#-- SDCompOOB --# 
# Use resamples function to compare model performances
oobFitComp1k <- resamples(list(rf=oobRFfit, C5.0=oobC5fit))

# output summary metrics for tuned models 
summary(oobFitComp1k)
#####################################################################
#Models: rf, C5.0 
#Number of resamples: 5 
#
#Accuracy 
#     Min.  1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf   0.88 0.885906 0.9006623 0.9054196 0.9205298 0.9400000    0
#C5.0 0.76 0.800000 0.8456376 0.8268229 0.8543046 0.8741722    0
#
#Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf   0.7470489 0.7608798 0.7907235 0.8004086 0.8320356 0.8713551    0
#C5.0 0.5319813 0.5446266 0.6721515 0.6374561 0.7036046 0.7349164    0
######################################################################

#-- rfe_SD --#
rfGrid <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
oobRFfit_rfe <- train(Brand~.,data=oobTrain_rfe,method="rf",
                  importance=T,
                  trControl=fitControl,
                  tuneGrid=rfGrid)

C5Grid <- expand.grid(.winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model=c("rules","tree"))  
set.seed(123)
oobC5fit_rfe <- train(Brand~.,data=oobTrain_rfe,method="C5.0",
                  importance=T,
                  trControl=fitControl,
                  tuneGrid=C5Grid)
oobFitComp1k <- resamples(list(rf=oobRFfit_rfe, C5.0=oobC5fit_rfe))
summary(oobFitComp1k)
######################################################################
#Models: rf, C5.0 
#Number of resamples: 5 
#
#Accuracy 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf   0.8543046 0.8724832 0.9066667 0.8921479 0.9072848 0.9200000    0
#C5.0 0.7666667 0.8000000 0.8724832 0.8427969 0.8874172 0.8874172    0
#
#Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#rf   0.7036046 0.7254921 0.8019242 0.7725891 0.8040415 0.8278830    0
#C5.0 0.5446266 0.5519713 0.7236161 0.6718457 0.7673344 0.7716802    0
#######################################################################

##--- Save/load top performing model ---##

# save top performing model after validation
saveRDS(oobRFfit, "oobRFfit.rds")  

# load and name model
rfFit1 <- readRDS("oobRFfit.rds")

############################
# Predict testSet/validation
############################

# predict with RF
rfPred1 <- predict(rfFit1, oobTest)

# performace measurment
postResample(rfPred1, oobTest$Brand)
#Accuracy     Kappa 
#0.9317269 0.8550392 

# plot predicted verses actual
plot(rfPred1, oobTest$Brand)

###############################
# Predict new data (Dataset 2)
###############################

rfPred2 <- predict(rfFit1, SDIncompOOB)
summary(rfPred2)
head(rfPred2)

# combine predicted result with the Incomplete dataset
SDIncompOOB$Brand <- rfPred2

# change data types
SDIncompOOB$Brand <- as.integer(SDIncompOOB$Brand)
str(SDIncompOOB)

# change the key numbers to brand names  
SDIncompOOB$Brand[SDIncompOOB$Brand == 1] <- "Acer"
SDIncompOOB$Brand[SDIncompOOB$Brand == 2] <- "Sony"
str(SDIncompOOB)

# write the results to csv file
write.csv(SDIncompOOB, "rfPred2.csv", row.names = F)

# Merge two dataset
SDCompOOB$Brand <- as.integer(SDCompOOB$Brand)
SDCompOOB$Brand[SDCompOOB$Brand == 1] <- "Acer"
SDCompOOB$Brand[SDCompOOB$Brand == 2] <- "Sony"
SurveyDataTotal <- rbind(SDCompOOB, SDIncompOOB)
str(SurveyDataTotal)
write.csv(SurveyDataTotal, "SurveyDataTotal.csv", row.names = F)
