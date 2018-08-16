# blah blah blah
#-----------------------------
# 1. Loading packages
#-----------------------------
library(tidyverse)
library(caret) #Classification and Regression Training
library(skimr) #Compact and Flexible Summaries of Data
library(RANN) #Fast Nearest Neighbor Search
library(mice) #Multivariate Imputation by Chained Equations
library(VIM) #Visualization and Imputation of Missing Values
library(randomForest)
library(here)

#I think we don't actually need these
#library(pROC)
#library(pscl)
#

#-----------------------------
# 2. Random descriptive analyses
#-----------------------------

f <- here::here("online-science-motivation.csv")
online_science_motivation <- read_csv(f)

online_science_motivation %>%
    count(enrollment_reason)

#-----------------------------
# 3. Pre-process and impute missing data - PROBLEMS here, deleting missing data listwise for now to get RFs done, see 3Temp
#-----------------------------
set.seed(2019)

#str(online_science_motivation)

#Trying imputation with mice - step 1 - looking at missing data pattern
md.pattern(online_science_motivation)

#Use VIM package to visualize what is missing in our dataset 
aggr_plot <- aggr(online_science_motivation, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

#*******************************************************************************************
#ORIGINAL imputation
    #trying normal imputation
    #it works
    #BUT
    #centers/imputes things that we don't want, e.g. student ID and outcome variables
    #our goal is to only impute predictors
#*******************************************************************************************
#SECOND TRY imputation (this doesn't work)
    #don't want to impute: student_ID, section, outcomes
    #DO want to impute: pre and post motivation
#impute <- 
 #   online_science_motivation %>%
 #   select(pre_int, pre_uv, pre_percomp, pre_tv, post_int, post_uv, post_tv, post_percomp) %>%
 #   preProcess(online_science_motivation, method=c("center","scale","knnImpute")) ##using pre-process function in caret to create a dataset to be imputed 
  
#dat.imputed <- predict(impute, online_science_motivation) ##impute

#compare imputed vs. non-imputed data
#   skim(online_science_motivation)
#   skim(dat.imputed)

#This might be helpful for troubleshooting
    #   https://stackoverflow.com/questions/14023423/how-to-preprocess-features-when-some-of-them-are-factors

#-----------------------------
# 3 TEMP WE WILL CHANGE THIS.  Placeholder until we figure out multiple imputation
#-----------------------------

data <- na.omit(online_science_motivation)
    #this takes us down from 662 observations of 17 variables to 91 observations of 17 variables :(

skim(data)

#-----------------------------
# 4.  Data splitting: Creating training and testing dataset
#-----------------------------
set.seed(2019)
trainIndex <- createDataPartition(data$final_grade,
                                  p = .8, list = FALSE)

data_train <- data[ trainIndex,] #rows defined by train index
data_test <- data[-trainIndex,] #give me everyting in data EXCEPT the ones indicated by train index
dim(data_train)
dim(data_test)

#Check whether distributions of variables look good across datasets

skim(data_train)
skim(data_test)
#-----------------------------
# 5.  Random forest Predicting Final Grade
#-----------------------------
colnames(data_test)
#Outcome of interest = final grade
#Inputs = pre and post motivation

#Training
set.seed(2019)
RF_FinalGrade <-randomForest(formula = final_grade ~  pre_int + pre_uv + pre_percomp + pre_tv
                             post_int + post_uv + post_percomp + post_tv,
                             data = data_train,
                             method = "regression")

#Generate Predicted classes using the model object

set.seed(2019)
FinalGrade_prediction <- predict(object = RF_FinalGrade,   # model object 
                             newdata = data_test)  # train dataset
#this above returns a matrix
#NOW - correspond this matrix to the info we have

plot(density(FinalGrade_prediction))    #check out the predictions that are generated to see what they look like

#Take test data frames and cbind new prediction matrix
FinalGrade_data <- cbind(data_test, FinalGrade_prediction)

##How does this model peform?

#Create a function to calculate the (predicted - actual: RESIDUAL)
#take the absolute value
Emily_residuals<- function(pred, obs){
    diff <- pred-obs
    return(abs(diff))
}

#Call my newly created function
#first arg = predicted
#second arg = observed

Residuals_FinalGrade <- Emily_residuals(FinalGrade_data$FinalGrade_prediction , FinalGrade_data$final_grade)


#plot the errors in a density plot
FinalGrade_resid_plot <- plot(density(Residuals_FinalGrade), 
                          main = "Absolute Value of Out-of-Sample\n Residuals for Final Grade Model",
                          xlab = "Residuals",
                          ylab = "Density")