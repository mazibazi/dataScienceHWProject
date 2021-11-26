## Pre-processed Data
load("case2_firstAttempts.R")
dim(data)
str(data)
names(data)
View(data)
library("xgboost")
library("ggplot2")

#Scatter Plot
par(mar = c(2, 2, 2, 2))
par(mfrow = c(3, 3))  # 4 rows and 4 columns
for (i in c(3:18)) {
  plot(data[, i], data$Apps, xlab = "", main = paste("Apps vs.", names(data)[i]))
}
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 4.5, 4.5))

###
#Accept  + Enroll + F.Undergrad + Top10perc + 
#  Room.Board  + Grad.Rate + Top25perc + Outstate + 
#  Expend + Private + perc.alumni + Terminal + PhD + 
#  P.Undergrad 
###


## Check for poly line approach -----
m1_2 <-lm(Apps ~ Enroll, data = train2)  #  R-squared:  0.7593
summary(m1_2)
plot(m1_2)

m1_2M <-lm(Apps ~ Enroll * Enroll, data = train2) # R-squared:  0.8857
summary(m1_2M)
plot(m1_2M)

## Poly line Does not work


## Check Random tree and T test difference -----
mTtest <-lm(Apps  ~ Private+ Accept +Top10perc  + Enroll + F.Undergrad + P.Undergrad 
            +Outstate + Room.Board + PhD  + Expend + Grad.Rate, data = train2) # R-squared:  0.931
summary(mTtest)
plot(mTtest)


mRandomT <-lm(Apps ~ PhD + Terminal + Outstate + Room.Board + P.Undergrad + 
                perc.alumni + Private +Expend + Top25perc + Grad.Rate + Top10perc +
                F.Undergrad + Enroll + Accept, data = train2) # R-squared:  0.9323
summary(mRandomT)
plot(mRandomT)

## They are so close to each other :D

# normalize all data----

## Box-Cox Transformation
box_results <- boxcox(Apps ~ ., data = train, lambda = seq(-5, 5, 0.1))               
box_results <- data.frame(box_results$x, box_results$y)            
# Create a data frame with the results
lambda <- box_results[which(box_results$box_results.y == max(box_results$box_results.y)), 1]
lambda  

train2$newApps <- ((train2$Apps) ^ lambda - 1) /lambda


# Apps ~ PhD + Terminal + Outstate + Room.Board + P.Undergrad



# Min-Max normalization -----
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

names(train2)

#apply Min-Max normalization to first four columns in iris dataset
train2$newApps <- as.data.frame(lapply(train2$Apps, min_max_norm))




# log ----
train2$newApps <-  log(train2$Apps)




A = train2$newApps
qqnorm(A, main = paste("QQ plot of", pch = 20))
qqline(A, col = "red")


hist(A, xlab = "",  probability = T, breaks = 15, 
     main = paste("Histogram of", ))
lines(density(A), col = "red")





mAllRemoved <-lm(Apps ~., data = train2) # R-squared: 0.9325
summary(mAllRemoved)

train2$newApps


#Model 8- Bagging with Log------
bagging_NormalLog <- randomForest(newApps ~ .-Apps , mtry = ncol(train2) - 3, ntree = 500, data = train2)
bagging_NormalLog


## Predict 
pred_rf  <- predict(bagging_NormalLog, test)
pred_rf
pred_rf  <- exp(pred_rf)

## Absolute error mean, median, sd, max, min
abs_err_rf <- abs(pred_rf - test$Apps)
models_comp <- rbind(models_comp, "newBaggingNormal" = c(mean(abs_err_rf),
                                                      median(abs_err_rf),
                                                      sd(abs_err_rf),
                                                      IQR(abs_err_rf),
                                                      range(abs_err_rf)))
View(models_comp)

## Actual vs. Predicted

plot(test$Apps, pred_rf, main = 'newBaggingNormal',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)




#Model 8- XGBoost Regression with Log------

x <- model.matrix(newApps ~ . - Apps, data = train2)[, -1] #remove intercept
y <- train2$newApps

set.seed(123)
xgb_1Log <- xgboost(data = x, 
                 label = y,
                 eta = 0.1,                       #learning rate
                 lambda = 0,                      #regularization term
                 max_depth = 8,                   #tree depth 
                 nround = 1000,                   #max number of boosting iterations
                 subsample = 0.65,                #percent of training data to sample for each tree
                 objective = "reg:squarederror",  #for regression models
                 verbose = 0                      #silent
) 

#train RMSE
xgb_1Log$evaluation_log
#plot error vs number trees
ggplot(xgb_1Log$evaluation_log) +
  geom_line(aes(iter, train_rmse), color = "red") 

#Tuning(Train/validation using xgboost)
#Train and validation sets
set.seed(1234)
train_cases <- sample(1:nrow(train2), nrow(train2) * 0.8)
#Train data set
train_xgboost <- train2[train_cases,]
dim(train_xgboost)
#Model Matrix
xtrain <- model.matrix(newApps ~ . - Apps, data = train_xgboost)[, -1] #remove intercept
ytrain <- train_xgboost$newApps
#Validation data set
validation_xgboost  <- train2[- train_cases,]
dim(validation_xgboost)
xvalidation <- model.matrix(newApps ~ . - Apps, data = validation_xgboost)[, -1] #remove intercept
yvalidation <- validation_xgboost$newApps

#Create hyper-parameter grid
par_gridLog <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                        lambda = c(0, 1, 2, 5),
                        max_depth = c(1, 3, 5, 7),
                        subsample = c(0.65, 0.8, 1), 
                        colsample_bytree = c(0.8, 0.9, 1))
View(par_gridLog)
dim(par_gridLog)

#Grid search 
for(i in 1:nrow(par_gridLog)) {
  set.seed(123)
  
  #train model
  xgb_tune <- xgboost(data =  xtrain,
                      label = ytrain,
                      eta = par_gridLog$eta[i],
                      max_depth = par_gridLog$max_depth[i],
                      subsample = par_gridLog$subsample[i],
                      colsample_bytree = par_gridLog$colsample_bytree[i],
                      nrounds = 1000,
                      objective = "reg:squarederror",  #for regression models
                      verbose = 0,                     #silent,
                      early_stopping_rounds = 10       #stop if no improvement for 10 consecutive trees
  )
  
  #prediction on validation data set
  pred_xgb_validation <- predict(xgb_tune, xvalidation)
  rmse <- sqrt(mean((yvalidation - pred_xgb_validation) ^ 2))
  
  #add validation error
  par_gridLog$RMSE[i]  <- rmse
}

save(par_gridLog, file = "par_grid_xgboost.R")
View(par_gridLog)

## Final Model
set.seed(123)
xgb_1LogFinal <- xgboost(data = x, 
                 label = y,
                 eta = 0.05,     #learning rate
                 max_depth = 1,  #tree depth 
                 lambda = 0,
                 nround = 1000,
                 colsample_bytree = 1,
                 subsample = 0.65,                #percent of training data to sample for each tree
                 objective = "reg:squarederror",  #for regression models
                 verbose = 0,                      #silent
                 early_stopping_rounds = 10
)

## Test the Model
#Model: xgb Log Final 
x_test   <- model.matrix(newApps ~ . - Apps, data = test)[, -1]#remove intercept
pred_xgb <- predict(xgb_1LogFinal, x_test)
pred_xgb
pred_xgb <- exp(pred_xgb)
pred_xgb

#Absolute error mean, median, sd, max, min-------
abs_err_xgb <- abs(pred_xgb - test$Apps)
models_comp <- rbind(models_comp, "XGBRegLog" = c(mean(abs_err_xgb),
                                               median(abs_err_xgb),
                                               sd(abs_err_xgb),
                                               IQR(abs_err_xgb),
                                               range(abs_err_xgb)))
View(models_comp)
#Actual vs. Predicted
plot(test$Apps, pred_xgb, main = 'XGBRegLog',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#Save the results--------------------------------
save(data, dataRaw, train, train2, test, models_comp, file = "final.R")




















