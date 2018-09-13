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

#Create a function to calculate the (predicted - actual: RESIDUAL)
#take the absolute value
Emily_residuals<- function(pred, obs){
    diff <- pred-obs
    return(abs(diff))
}

#-----------------------------
# 2. Random descriptive analyses
#-----------------------------

f <- here::here("online-science-motivation-w-disc.csv")
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
data <- online_science_motivation %>% 
        select(pre_int, pre_uv,  pre_percomp, time_spent,course_ID, final_grade, subject, enrollment_reason, semester, enrollment_status)
   
data <- na.omit(data)

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

data_test <- data_test %>%
    mutate_if(is.character, as.factor)

data_train <- data_train %>%
    mutate_if(is.character, as.factor)

lte <- levels(data_test$course_ID) # 25 levels
ltr <- levels(data_train$course_ID) # 36 levels

levels_to_add <- ltr[!(ltr %in% lte)]

levels(data_test$course_ID) <- c(levels(data_test$course_ID), levels_to_add)
# 
# RF_FinalGrade <-randomForest(formula = final_grade ~ pre_int + pre_uv + pre_percomp + time_spent +
#                                  enrollment_reason + subject + course_ID,
#                              data = data_train,
#                              method = "regression", 
#                              importance = TRUE)

RF_FinalGrade <-randomForest(formula = final_grade ~ pre_int + pre_uv + pre_percomp + time_spent +
                                 enrollment_reason + course_ID,
                             data = data_train,
                             method = "regression", 
                             importance = TRUE)

RF_FinalGrade

#Generate Predicted classes using the model object
FinalGrade_prediction <- predict(object = RF_FinalGrade,   # model object 
                             newdata = data_test)  # train dataset

FinalGrade_prediction <- as.data.frame(FinalGrade_prediction)

d <- data.frame(data_test, FinalGrade_prediction)

p <- d %>% 
    as_tibble() %>% 
    rename(pred_final_grade = FinalGrade_prediction) %>% 
    mutate(abs_diff = Emily_residuals(final_grade, pred_final_grade),
           diff = final_grade - pred_final_grade)

p %>% summarize_all(funs(mean)) %>% select (pred_final_grade, abs_diff, diff)

p %>% 
    select(final_grade, pred_final_grade) %>% 
    gather(key, val, final_grade:pred_final_grade) %>% 
    ggplot(aes(x = val, fill = key, color = key)) +
    geom_density(alpha = .4)

p %>% 
    select(final_grade, pred_final_grade, time_spent) %>% 
    ggplot(aes(x = final_grade, y = pred_final_grade, color = time_spent)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    ylim(0, 100) +
    xlim(0, 100) +
    scale_color_viridis_c()
    
#this above returns a matrix
#NOW - correspond this matrix to the info we have

plot(density(FinalGrade_prediction)) #check out the predictions that are generated to see what they look like

#Take test data frames and cbind new prediction matrix
FinalGrade_data <- cbind(data_test, FinalGrade_prediction)

##How does this model peform?


#Call my newly created function
#first arg = predicted
#second arg = observed

Residuals_FinalGrade <- Emily_residuals(FinalGrade_data$FinalGrade_prediction , FinalGrade_data$final_grade)

Residuals_FinalGrade %>% 
    as_data_frame() %>% 
    ggplot(aes(x = value)) +
    geom_density()

#plot the errors in a density plot
FinalGrade_resid_plot <- plot(density(Residuals_FinalGrade), 
                          main = "Absolute Value of Out-of-Sample\n Residuals for Final Grade Model",
                          xlab = "Residuals",
                          ylab = "Density")

FinalGrade_resid_plot

#VARIABLE IMPORTANCE PLOT
varImpPlot(RF_FinalGrade)

varImp(RF_FinalGrade) #this gives us the actual values for the variable importance MSE
