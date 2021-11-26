## Required libraries
library("gbm")
library("xgboost")
library("ggplot2")

## Pre-processed Data
load("case1_testPreproces.R")
dim(data)
str(data)
names(data)

# Model 6 Decision Tree Structures ----------
## Model 6-1(train) GB Regression---------


gbm_1 <- gbm(formula = Apps ~ + .,
             distribution = "gaussian", 
             data = train,
             n.trees = 800, #the total number of trees to fit
             interaction.depth = 1, #1: stump, the maximum depth of each tree 
             shrinkage = 0.01, #learning rate
             cv.folds = 5, #Number of cross-validation folds to perform
             n.cores = NULL, #will use all cores by default
             verbose = FALSE)

## get MSE and compute RMSE
min(gbm_1$cv.error)         #MSE
sqrt(min(gbm_1$cv.error))   #RMSE

## plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm_1, method = "cv")
which(gbm_1$cv.error == min(gbm_1$cv.error))
## returns the estimated optimal number of iterations



## Model 6-2(train) GB Regression----
set.seed(123)
gbm_2 <- gbm(formula =  Apps ~ + .,
             distribution = "gaussian",
             data = train,
             n.trees = 800,
             interaction.depth = 3,
             shrinkage = 0.01,
             cv.folds = 5,
             n.cores = NULL, #will use all cores by default
             verbose = FALSE)  
## get MSE and compute RMSE
min(gbm_2$cv.error)         #MSE
sqrt(min(gbm_2$cv.error))
## plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm_2, method = "cv")
which(gbm_2$cv.error == min(gbm_2$cv.error))


## Model 6-3 (train) GB Regression----
## Tuning
## Create hyper-parameter grid
par_grid <- expand.grid(shrinkage = c(0.01, 0.15, 0.1, 0.3),  #learning rate
                        interaction_depth = c(1,2 ,3, 5), #the maximum depth of each tree
                        n_minobsinnode = c(5, 10, 15, 20),  #the minimum number of observations in the terminal nodes of the trees
                        bag_fraction = c(0.5, 0.7, 0.8,0.9) #stochastic gradient :bag.fraction < 1
)
View(par_grid)
nrow(par_grid)

## Grid search (train/validation approach)
for(i in 1:nrow(par_grid)) {
  set.seed(123)
  #train model
  gbm_tune <- gbm(formula =  Apps ~ + .,
                  distribution = "gaussian",
                  data = train,
                  n.trees = 5000,
                  interaction.depth = par_grid$interaction_depth[i],
                  shrinkage = par_grid$shrinkage[i],
                  n.minobsinnode = par_grid$n_minobsinnode[i],
                  bag.fraction = par_grid$bag_fraction[i],
                  train.fraction = 0.8,
                  #cv.folds = 5,
                  n.cores = NULL, #will use all cores by default
                  verbose = FALSE)  
  #add min training error and trees to grid
  par_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  par_grid$min_RMSE[i]      <- sqrt(min(gbm_tune$valid.error))
}

head(par_grid)
View(par_grid)


## Model 6-4 (train) GB Regression----
## Modify hyper-parameter grid


par_grid2 <- expand.grid(shrinkage = c(0.01, 0.15, 0.1, 0.3),  #learning rate
                        interaction_depth = c(2,3), #the maximum depth of each tree
                        n_minobsinnode = c(5) , #the minimum number of observations in the terminal nodes of the trees
                        bag_fraction = c(0.5, 0.6,0.7, 0.8)
                        )
nrow(par_grid2)
## Grid search (train/validation approach)
for(i in 1:nrow(par_grid2)) {
  set.seed(123)
  #train model
  gbm_tune <- gbm(formula = Apps ~ + .,
                  distribution = "gaussian",
                  data = train2,
                  n.trees = 5000,
                  interaction.depth = par_grid2$interaction_depth[i],
                  shrinkage = par_grid2$shrinkage[i],
                  n.minobsinnode = par_grid2$n_minobsinnode[i],
                  bag.fraction = par_grid2$bag_fraction[i],
                  train.fraction = 0.8,
                  cv.folds = 0,
                  n.cores = NULL, #will use all cores by default
                  verbose = FALSE)  
  #add min training error and trees to grid
  par_grid2$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  par_grid2$min_RMSE[i]    <- sqrt(min(gbm_tune$valid.error))
}

head(par_grid2)
View(par_grid2)

## Model 6-5 Final Model-----
gbmFinal <- gbm(formula = Apps  ~ . ,
             distribution = "gaussian",
             data = train2,
             n.trees = 100,
             interaction.depth = 2,
             shrinkage = 0.03,
             n.minobsinnode = 5,
             bag.fraction = 0.8,
             train.fraction = 1,
             cv.folds = 0,
             n.cores = NULL, #will use all cores by default
)  

summary(gbmFinal)

## Test the Model
## Model 6-5: gbmFinal
## Prediction
pred_gbm <- predict(gbmFinal, n.trees = 100, newdata = test)
pred_gbm
## Absolute error mean, median, sd, max, min
abs_err_gbm <- abs(pred_gbm - test$Apps)
models_comp <- rbind(models_comp, "GBReg" = c(mean(abs_err_gbm),
                                              median(abs_err_gbm),
                                              sd(abs_err_gbm),
                                              IQR(abs_err_gbm),
                                              range(abs_err_gbm)))
View(models_comp)
## Actual vs. Predicted
plot(test$Apps, pred_gbm, main = 'GBReg',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)



# Model 7: XGBoost Regression------------------------
x <- model.matrix(Apps ~ . , data = train2)[, -1] #remove intercept
y <- train2$Apps

set.seed(123)
xgb_1 <- xgboost(data = x, 
                 label = y,
                 eta = 0.1,                       #learning rate
                 lambda = 0,                      #regularization term
                 max_depth = 8,                   #tree depth 
                 nround = 1000,                   #max number of boosting iterations
                 subsample = 0.65,                #percent of training data to sample for each tree
                 objective = "reg:squarederror",  #for regression models
                 verbose = 0                      #silent
) 

## train RMSE
xgb_1$evaluation_log
#plot error vs number trees
ggplot(xgb_1$evaluation_log) +
  geom_line(aes(iter, train_rmse), color = "red") 


## Tuning(Train/validation using xgboost)
## Train and validation sets
set.seed(1234)
train_cases <- sample(1:nrow(train2), nrow(train2) * 0.8)
## Train data set
train_xgboost <- train[train_cases,]
dim(train_xgboost)
## Model Matrix
xtrain <- model.matrix(Apps ~ . , data = train_xgboost)[, -1] #remove intercept
ytrain <- train_xgboost$Apps
## Validation data set
validation_xgboost  <- train[- train_cases,]
dim(validation_xgboost)
xvalidation <- model.matrix(Apps ~ . , data = validation_xgboost)[, -1] #remove intercept
yvalidation <- validation_xgboost$Apps

## Create hyper-parameter grid
par_grid <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                        lambda = c(0, 1, 2, 5),
                        max_depth = c(1, 3, 5, 7),
                        subsample = c(0.65, 0.8, 1), 
                        colsample_bytree = c(0.8, 0.9, 1))
View(par_grid)
dim(par_grid)


#Grid search 
for(i in 1:nrow(par_grid)) {
  set.seed(123)
  
  #train model
  xgb_tune <- xgboost(data =  xtrain,
                      label = ytrain,
                      eta = par_grid$eta[i],
                      max_depth = par_grid$max_depth[i],
                      subsample = par_grid$subsample[i],
                      colsample_bytree = par_grid$colsample_bytree[i],
                      nrounds = 1000,
                      objective = "reg:squarederror",  #for regression models
                      verbose = 0,                     #silent,
                      early_stopping_rounds = 10       #stop if no improvement for 10 consecutive trees
  )
  
  #prediction on validation data set
  pred_xgb_validation <- predict(xgb_tune, xvalidation)
  rmse <- sqrt(mean((yvalidation - pred_xgb_validation) ^ 2))
  
  #add validation error
  par_grid$RMSE[i]  <- rmse
}
View(par_grid)

## Model 7-1 Final Model------
set.seed(123)
xgb_2 <- xgboost(data = x, 
                 label = y,
                 eta = 0.1,     #learning rate
                 max_depth = 3,  #tree depth 
                 lambda = 0,
                 nround = 1000,
                 colsample_bytree = 0.9,
                 subsample = 0.65,                #percent of training data to sample for each tree
                 objective = "reg:squarederror",  #for regression models
                 verbose = 0,                      #silent
                 early_stopping_rounds = 10
)

## Test the Model
## Model: xgb_2
x_test   <- model.matrix(Apps ~ . , data = test)[, -1]#remove intercept
pred_xgb <- predict(xgb_2, x_test)
pred_xgb

## Absolute error mean, median, sd, max, min
abs_err_xgb <- abs(pred_xgb - test$Apps)
models_comp <- rbind(models_comp, "XGBReg" = c(mean(abs_err_xgb),
                                               median(abs_err_xgb),
                                               sd(abs_err_xgb),
                                               IQR(abs_err_xgb),
                                               range(abs_err_xgb)))
View(models_comp)
## Actual vs. Predicted
plot(test$Apps, pred_xgb, main = 'XGBReg',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

#Save the results--------------------------------
save(data, dataRaw, train, train2, test, models_comp, file = "case2_firstAttempts.R")

# Assignment------------------------------------
#Try to predict the number of applications received
#  using the other variables in the
#  college.csv data set.

# (a) Do a thorough exploratory analysis on data set
# (b) Split the data set into a training set and a test set.
# (c) Fit a linear model using least squares on the training set, and
#     report the test error obtained.
#     (c-1) select predictors based on t-test results
#     (c-2) select predictors based on step-wise method and k-fold cross validation
# (d) Fit a ridge regression model on the training set, with ?? chosen
#     by cross-validation. Report the test error obtained.
# (e) Fit a lasso model on the training set, with ?? chosen by cross-validation.
#     Report the test error obtained.
# (f) Fit a regression tree to the training set. Report the test error obtained.
# (g) Use the bagging approach in order to analyze this data. Report the test error obtained.
# (h) Use random forests to analyze the data set. Report the test error obtained.
# (j) Use GB and XGB Regression to analyze the data set. Report the test error obtained.
# (k) Compare the test errors across the all models. Which one is better?
###End of Code###--------------------------------