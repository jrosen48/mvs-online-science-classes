#Updated 8.23.18
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
#####MISSING DATA STEPS
    #Note on August 23 - based on our meeting, we decided NOT to impute Pre-data and also not to use it as a predictor
    #so, whatever we do for the imputation, we will not impute Pre-data

set.seed(2019)

#Trying imputation with mice - step 1 - looking at missing data pattern
md.pattern(online_science_motivation)

#Use VIM package to visualize what is missing in our dataset 
aggr_plot <- aggr(online_science_motivation, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/


#####CREATING FINAL DATASET FOR ANALYSES STEPS
#Select only data that are complete on pre-motivation and final grade
    #EMILY WILL WORK ON THIS TOMORROW 8.24.18
##data <- 

#Eww delete this code it is gross but I am just testing to see if I can get the random forest to run    
data <- na.omit(online_science_motivation)
#this takes us down from 662 observations of 17 variables to 91 observations of 17 variables :(

skim(data)

#-----------------------------
# 4.  Data splitting: Creating training and testing dataset
#-----------------------------
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
#Inputs = ONLY post motivation - considering our August 16 decision that the "pre" data is icky

RF_FinalGrade <-randomForest(formula = final_grade ~ post_int + post_uv + post_percomp + post_tv,
                             data = data_train,
                             method = "regression")

#Generate Predicted classes using the model object
FinalGrade_prediction <- predict(object = RF_FinalGrade,   # model object 
                             newdata = data_test)  # train dataset
#this above returns a matrix
#NOW - correspond this matrix to the info we have

plot(density(FinalGrade_prediction)) #check out the predictions that are generated to see what they look like

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



###Old stuff we are not doing anymore

#-----------------------------
# 3 TEMP WE WILL CHANGE THIS.  Placeholder until we figure out multiple imputation
#-----------------------------

#data <- na.omit(online_science_motivation)
#this takes us down from 662 observations of 17 variables to 91 observations of 17 variables :(


#str(online_science_motivation)