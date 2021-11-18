setwd("D:/PhD/Data Science/dataScienceHWProject/college")
getwd()

library("moments")  #Moments, skewness, kurtosis and related tests 
library("MASS")     #Box-Cox Transformations for Linear Models
library("leaps")    #Regression Subset Selection
library("corrplot") #Visualization of Correlation Matrix
library("car")
library("corrplot")
library("glmnet")
library("randomForest")
library("leaps")
library("rpart")          #Classification and Regression Trees 
library("rpart.plot") 
dataRaw<- read.csv("college.csv", header = TRUE)

## Data Description and Reprocessing ------

# Private     -- A factor with levels No and Yes indicating private or public university
# Apps        -- Number of applications received
# Accept      -- Number of applications accepted
# Enroll      -- Number of new students enrolled
# Top10perc   -- Pct. new students from top 10% of H.S. class
# Top25perc   -- Pct. new students from top 25% of H.S. class
# F.Undergrad -- Number of fulltime undergraduates
# Outstate    -- Out-of-state tuition
# Room.Board  -- Room and board costs
# Books       -- Estimated book costs
# Personal    -- Estimated personal spending
# PhD         -- Pct. of faculty with Ph.D.'s
# Terminal    -- Pct. of faculty with terminal degree
# S.F.Ratio   -- Student/faculty ratio
# perc.alumni -- Pct. alumni who donate
# Expend      -- Instructional expenditure per student
# Grad.Rate   -- Graduation rate

colnames(dataRaw)
class(dataRaw)
dim(dataRaw)
head(dataRaw)
tail(dataRaw, 2)
str(dataRaw)
summary(dataRaw)


#Remove College.Name
unique(dataRaw$College.Name)
length(unique(dataRaw$College.Name))
data <- dataRaw[,-1]


##Uni variate Profiling
unique(data$Private)
length(unique(data$Private))


#customer_type
summary(data$Grad.Rate)
summary(data$Apps)


# Categorical data
data$Private <- factor(data$Private)
summary(data)


# Data Visualization -------
#distribution
par(mfrow = c(3, 3))  # 4 rows and 4 columns
#Continuous variables distribution
for (i in 3:18) {
  hist(data[, i], xlab = "",  probability = T, breaks = 15, 
       main = paste("Histogram of", names(data)[i]))
  lines(density(data[,i]), col = "red")
}

for (i in 3:18) {
  qqnorm(data[,i], main = paste("QQ plot of", names(data)[i]), pch = 20)
  qqline(data[,i], col = "red")
}


#Scatter Plot
par(mfrow = c(2, 2))  # 2 rows and 2 columns
for (i in 3:18) {
  plot(data[,i], data$Apps, xlab = "", main = paste("Apps vs.", names(data)[i]))
}

par(mfrow = c(1,1))


boxplot(data$Apps, main = "Number of applications received")

# categorical
table(data$Private)

#Identify Outliers  
tukey_ul <- quantile(data$Apps, probs = 0.75) + 1.5 * IQR(data$Apps)
tukey_ul 
sum(data$Apps > tukey_ul)
sum(data$Apps > tukey_ul)/nrow(data) * 100
#  9 % of total data are Outliers 

#Correlation Analysis
cor_table <- round(cor(data[, c(2: 18)]), 2)
View(cor_table)
corrplot(cor_table)

#Scatter Plot
par(mar = c(2, 2, 2, 2))
par(mfrow = c(3, 3))  # 4 rows and 4 columns
for (i in c(3:18)) {
  plot(data[, i], data$Apps, xlab = "", main = paste("Apps vs.", names(data)[i]))
}
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 4.5, 4.5))

tapply(data$Apps, data$Private, mean)
#Categorical variables    
table(data$Private)     # we want to check Categorical data are not distributed 

#Divide Data set into Train and Test --------------
#set.seed(1234)
train_cases <- sample(1:nrow(data), nrow(data) * 0.8)  # we do not have enough data to study 80-20
train <- data[train_cases,]
test  <- data[- train_cases,]

dim(train)
summary(train)
dim(test)
summary(test)



for (i in 2:18){
  print(colnames(train[i]))
  print("train")
  print(summary(train[, i]))
  print("test")
  print(summary(test[, i]))
  print("----------------------")
}




# First Linear Regression without changes --------------
# I develop some single regression, to obtain a general knowledge on the database
## Model 1 single -------------------------------------

m1_0 <-lm(Apps ~ Private  , data = train)
summary(m1_0)
plot(m1_0)

m1_1 <-lm(Apps ~ Accept, data = train) # R-squared:  0.898
summary(m1_1)
plot(m1_1)


m1_2 <-lm(Apps ~ Enroll, data = train)  #  R-squared:  0.7051
summary(m1_2)
plot(m1_2)

m1_3 <-lm(Apps ~ F.Undergrad, data = train) # R-squared:  0.6527
summary(m1_3)
plot(m3)

m1_4 <-lm(Apps ~ Top25perc, data = train) # R-squared:  0.1108
summary(m1_4)
plot(m1_4)

m1_5 <-lm(Apps ~ Top10perc, data = train)
summary(m1_5)
plot(m1_5)

m1_6 <-lm(Apps ~ Room.Board, data = train)
summary(m1_6)
plot(m1_6)


m1_7 <-lm(Apps ~ PhD, data = train)
summary(m1_7)
plot(m1_7)


m1_8 <-lm(Apps ~ Grad.Rate, data = train)
summary(m1_8)
plot(m1_8)

# Model 2 All data ----
## Model 2-1 All in model ------------------------------------------

mAll <-lm(Apps ~., data = train) # R-squared:  0.9367
summary(mAll)
plot(mAll)

car :: vif(mAll)
# F.Undergrad and Enroll have multidisciplinary 
# I think row number 484 has an issue
train[484,]
dataRaw[484,]
summary(train)
train[71,]
train[251,]
train[694,]
# https://www.usnews.com/best-colleges/rutgers-new-brunswick-6964
# has lower Accept, Top10perc, Top25perc, F.Undergrad, perc.alumni, Grad.Rate
# has higher Enroll,  P.Undergrad, Outstate, Room.Board, Books , Personal , PhD , Terminal, S.F.Ratio
# mean Expend 
train2 <- train[- which(rownames(train) %in% c(484, 251, 71, 694)), ]

mAllRemoved <-lm(Apps ~., data = train2) # R-squared: 0.9457
summary(mAllRemoved)
plot(mAllRemoved)

## Test the Model
## Model: mAll and mAllRemoved
## Prediction
pred_mAll <- predict(mAllRemoved, test)

#Absolute error mean, median, sd, max, min
abs_err_lm <- abs(pred_mAll - test$Apps)

#Create a dataframe to save prediction results on test data set
models_comp <- data.frame("Mean of AbsErrors"   = mean(abs_err_lm),
                          "Median of AbsErrors" = median(abs_err_lm),
                          "SD of AbsErrors"  = sd(abs_err_lm),
                          "IQR of AbsErrors" = IQR(abs_err_lm),
                          "Min of AbsErrors" = min(abs_err_lm),
                          "Max of AbsErrors" = max(abs_err_lm), 
                          row.names = 'LM_AllFeature')
View(models_comp)

#Actual vs. Predicted
plot(test$Apps, pred_mAll, main = 'LM_AllFeature',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2) 

## Model 2-2 t-test in model ------------------------------------------
summary(mAllRemoved)


mTtest <-lm(Apps  ~ Private+ Accept +Top10perc  + Enroll + F.Undergrad + P.Undergrad 
   +Outstate + Room.Board + PhD  + Expend + Grad.Rate, data = train2) # R-squared:  0.9452
summary(mTtest)
plot(mTtest)


## Test the Model 
## Model: t-test 
## Prediction
pred_mTtest <- predict(mTtest, test)
#Absolute error mean, median, sd, max, min
abs_err_lm <- abs(pred_mTtest - test$Apps)

#Create a dataframe to save prediction results on test data set
models_comp <- rbind(models_comp, 'LM_Ttest' = c(mean(abs_err_lm),
                                                       median(abs_err_lm),
                                                       sd(abs_err_lm),
                                                       IQR(abs_err_lm),
                                                       range(abs_err_lm)))
View(models_comp)

#Actual vs. Predicted
plot(test$Apps, pred_mTtest, main = 'LM_Ttest',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2) 


## Model 2-3 data manipulate and model all in -------------------------------

## Box-Cox Transformation
box_results <- boxcox(Apps ~ ., data = train, lambda = seq(-5, 5, 0.1))               
box_results <- data.frame(box_results$x, box_results$y)            
# Create a data frame with the results
lambda <- box_results[which(box_results$box_results.y == max(box_results$box_results.y)), 1]
lambda  

#New transformation

train2$newApps <- ((train2$Apps) ^ lambda - 1) /lambda
mTransformAll <-lm(newApps ~ Private+ Accept +Top10perc  + Enroll + F.Undergrad + P.Undergrad 
                   +Outstate + Room.Board + PhD  + Expend + Grad.Rate, data = train2) # R-squared:  0.9266
summary(mTransformAll)
plot(mTransformAll) # 
car :: vif(mTransformAll)  # F.Undergrad and Enroll

train2 <- subset (train2, select = -newApps) 
# the method does not work, and I prefer to work with normal,
#                                      and it does not have any improvements 
# But still we have some issues with Multidisciplinary, ... 

# Model 3 Using the Best Subset Selection Methods ----------

dim(train)
dim(train2)
bestsub_1 <- regsubsets(Apps ~ . , nvmax = 17, data = train, method = "exhaustive")
summary(bestsub_1)
summary(bestsub_1)$rsq

bestsub_2 <- regsubsets(Apps ~ . , nvmax = 17, data = train2, method = "exhaustive")
summary(bestsub_2)
summary(bestsub_2)$rsq

#Plot Adjusted R-squared  --> 14
plot(summary(bestsub_2)$adjr2,
     type = "b",
     xlab = "# of Variables", 
     ylab = "AdjR2", 
     xaxt = 'n',
     xlim = c(1, 18)); grid()
axis(1, at = 1: 18, labels = 1: 18)

points(which.max(summary(bestsub_2)$adjr2), 
       summary(bestsub_2)$adjr2[which.max(summary(bestsub_2)$adjr2)],
       col = "red", cex = 2, pch = 20)

#Plot Cp --> 11
plot(summary(bestsub_2)$cp,
     type = "b",
     xlab = "# of Variables", 
     ylab = "Cp", 
     xaxt = 'n',
     xlim = c(1, 19)); grid()
axis(1, at = 1: 19, labels = 1: 19)
points(which.min(summary(bestsub_2)$cp), 
       summary(bestsub_2)$cp[which.min(summary(bestsub_2)$cp)],
       col = "red", cex = 2, pch = 20)

#Plot BIC --> 9
plot(summary(bestsub_2)$bic,
     type = "b",
     xlab = "# of Variables", 
     ylab = "BIC", 
     xaxt = 'n',
     xlim = c(1, 19)); grid()
axis(1, at = 1: 19, labels = 1: 19)
points(which.min(summary(bestsub_2)$bic), 
       summary(bestsub_2)$bic[which.min(summary(bestsub_2)$bic)],
       col = "red", cex = 2, pch = 20)

#Coefficients of the best model
coef(bestsub_2, 9) 
bestsub2_BIC <- lm(Apps  ~Private + Accept + Enroll + Top10perc + F.Undergrad +
                     Outstate + Room.Board + PhD + Expend , data = train2)
summary(bestsub2_BIC) # R-squared:  0.9443


coef(bestsub_2, 11) 
bestsub2_Cp <- lm(Apps  ~ Accept +Top10perc + Private + Enroll +
                    Outstate + Room.Board + PhD  + Expend + Grad.Rate +
                  F.Undergrad + P.Undergrad, data = train2)
summary(bestsub2_Cp) # R-squared:  0.9452


coef(bestsub_2, 14) 
bestsub2_Adj <- lm(Apps  ~ Accept + Top10perc + Top25perc+ Private + Enroll +
                    Outstate + Room.Board + PhD  + Expend + Grad.Rate +
                    F.Undergrad + P.Undergrad + S.F.Ratio + perc.alumni, data = train2)
summary(bestsub2_Adj) # R-squared:  0.9456

# BIC results are better a little with 9 predictor it works, so in the following
#             Asqadj and BIC will be added

## Test the Model bestsub2_BIC 
#Prediction
pred_bestsubAdj  <- predict(bestsub2_Adj, test)    #we predict the log salary so we should use exp to find the real amount of salary
pred_bestsubAdj 
#Absolute error mean, median, sd, max, min
abs_err_bestsub <- abs(pred_bestsubAdj  - test$Apps )
abs_err_lm <- abs(pred_mTtest - test$Apps)
models_comp <- rbind(models_comp, 'BestSubset_RAdj' = c(mean(abs_err_bestsub),
                                                         median(abs_err_bestsub),
                                                         sd(abs_err_bestsub),
                                                         IQR(abs_err_bestsub),
                                                         range(abs_err_bestsub)))
View(models_comp)

#Actual vs. Predicted
plot(test$Apps, pred_bestsubAdj, main = 'BestSubset_RAdj',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

## model 3-2 Forward Selection ----

fwd_1 <- regsubsets(Apps ~ . , nvmax = 17, data = train2, method = "forward")
summary(fwd_1)
summary(fwd_1)$rsq


which.max(summary(fwd_1)$adjr2)
which.min(summary(fwd_1)$cp)
which.min(summary(fwd_1)$bic)

summary(fwd_1)$adjr2[which.max(summary(fwd_1)$adjr2)]
summary(fwd_1)$cp[which.min(summary(fwd_1)$cp)]
summary(fwd_1)$bic[which.min(summary(fwd_1)$bic)]


fwd_bic <- lm(Apps ~ Private + Accept + Enroll + Top10perc + F.Undergrad
                      + Outstate + Room.Board + Expend  + PhD 
                        , data = train2) # R-squared:  0.9443

summary(fwd_bic)
##  We have already made this one, the result of forward and best subset are eqaul 

## model 3-3 backward Selection ----
bwd_1 <- regsubsets(Apps ~ . , nvmax = 17, data = train2, method = "backward")
summary(bwd_1)
summary(bwd_1)$rsq

which.max(summary(bwd_1)$adjr2)
which.min(summary(bwd_1)$cp)
which.min(summary(bwd_1)$bic)

##  Adj
coef(bestsub_2, 13)
coef(fwd_1, 13)
coef(bwd_1, 13)

## Bic
coef(bestsub_2, 9)
coef(fwd_1, 9)
coef(bwd_1, 9)


## Results of best sub, forward and backward are same here. 

## model 3-4 using Cross validation for find  ----
k <- 10
set.seed(123)
folds <- sample(1: k, nrow(train), rep = TRUE)
cv_errors <- matrix(NA, k, 17, dimnames = list(NULL , paste(1: 17)))
cv_errors
#Create prediction function for reg subsets()
#Prediction function  (object is Regression syntax)
predict_regsubsets <- function(object, newdata, id) {
  reg_formula <- as.formula(object$call[[2]])
  mat    <- model.matrix(reg_formula, newdata)
  coef_i <- coef(object, id = id)
  mat[, names(coef_i)] %*% coef_i
}
#K-fold Cross Validation
set.seed(1234)
#K-fold Cross Validation (For best subset ==> exhaustive)
for(i in 1: 17){
  for(j in 1: k){
    best_fit <- regsubsets(Apps ~ . , data = train2[folds != j,], nvmax = 17, method = "exhaustive")
    pred <- predict_regsubsets(best_fit, newdata = train[folds == j,], id = i)
    cv_errors[j, i] <- mean((train$Apps[folds == j] - pred) ^ 2)
  }
}

View(cv_errors)
mean_cv_erros <- apply(cv_errors, 2, mean)
mean_cv_erros 
plot(mean_cv_erros, type = "b")
which.min(mean_cv_erros)

coef(bestsub_2, 14)
bestsub_cv <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc +
                   F.Undergrad + P.Undergrad + Outstate + Room.Board + PhD +
                   S.F.Ratio + perc.alumni + Expend + Grad.Rate, data = train2)
summary(bestsub_cv) #  R-squared:  0.9456

## Test the Model-
## Model: bestsub_cv
## Prediction
pred_bestsub_cv <- predict(bestsub_cv, test)

##Absolute error mean, median, sd, max, min
abs_err_bestsub_cv <- abs(pred_bestsub_cv - test$Apps)
models_comp <- rbind(models_comp, 'BestSubset_CV' = c(mean(abs_err_bestsub_cv),
                                                      median(abs_err_bestsub_cv),
                                                      sd(abs_err_bestsub_cv),
                                                      IQR(abs_err_bestsub_cv),
                                                      range(abs_err_bestsub_cv)))
View(models_comp)

## Actual vs. Predicted
plot(test$Salary, pred_bestsub_cv, main = 'BestSubset_CV',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)




# Model 4 Using Regularization -----
## Model 4-1 Ridge Regression -----
x <- model.matrix(Apps ~ . , data = train2)[, -1] #remove intercept
y <- train2$Apps
lambda_grid <- 10 ^ seq(5, -2, length = 100) # we can change more grid and each time focus more to find better
lambda_grid

## Apply Ridge Regression (alpha = 0)
ridgereg_1 <- glmnet(x, y, alpha = 0, lambda = lambda_grid, standardize = TRUE, intercept = TRUE)
dim(coef(ridgereg_1))  # 20 100 ---> 19 + 1 intercept 

## Plot Reg. Coefficients vs. Log Lambda
plot(ridgereg_1, xvar = "lambda")

## Retrieve Coefficients
ridgereg_1$lambda [50]
coef(ridgereg_1)[, 50]

## Cross validation to choose the best model between 100 models
set.seed(1234)
ridge_cv    <- cv.glmnet(x, y, alpha = 0, lambda = lambda_grid, nfolds = 10)
## The mean cross-validated error
ridge_cv$cvm
## Estimate of standard error of cv.
ridge_cv$cvsd

## value of lambda that gives minimum cvm
ridge_cv$lambda.min

## Coefficients of regression w/ best_lambda, Do not need to assign grid because we know the best lambda 
ridgereg_2 <- glmnet(x, y, alpha = 0, lambda = ridge_cv$lambda.min, standardize = TRUE, intercept = TRUE)
coef(ridgereg_2)


## Test the Model
## Model: ridgereg_2
## Prediction
## Create model matrix for test
x_test <- model.matrix(Apps ~ . , data = test)[, -1]#remove intercept
pred_ridgereg <- predict(ridgereg_2, s = ridge_cv$lambda.min, newx = x_test)
pred_ridgereg
#Absolute error mean, median, sd, max, min-------
abs_err_ridgereg <- abs(pred_ridgereg - test$Apps)
models_comp <- rbind(models_comp, "RidgeReg" = c(mean(abs_err_ridgereg),
                                                 median(abs_err_ridgereg),
                                                 sd(abs_err_ridgereg),
                                                 IQR(abs_err_ridgereg),
                                                 range(abs_err_ridgereg)))
View(models_comp)

#Actual vs. Predicted
plot(test$Apps, pred_ridgereg, main = 'RidgeReg',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")

abline(a = 0, b = 1, col = "red", lwd = 2)


##  Model 4-2 LASSO -----
lassoreg_1 <- glmnet(x, y, alpha = 1, lambda = lambda_grid, standardize = TRUE, intercept = TRUE)
dim(coef(lassoreg_1))

## Plot Reg. Coefficients vs. Log Lambda
plot(lassoreg_1, xvar = "lambda")

## Retrieve Coefficients
lassoreg_1$lambda [50]
coef(lassoreg_1)[, 20]


## Cross validation to choose the best model
lasso_cv    <- cv.glmnet(x, y, alpha = 1, lambda = lambda_grid, nfolds = 10)
## The mean cross-validated error
lasso_cv$cvm
## Estimate of standard error of cvm.
lasso_cv$cvsd

## value of lambda that gives minimum cvm
lasso_cv$lambda.min

## Coefficients of regression w/ best_lambda
lassoreg_2 <- glmnet(x, y, alpha = 1, lambda = lasso_cv$lambda.min, standardize = TRUE, intercept = TRUE)
coef(lassoreg_2)

x_test <- model.matrix(Apps ~ . , data = test)[, -1]#remove intercept
pred_lassoreg <- predict(lassoreg_2, s = lasso_cv$lambda.min, newx = x_test)
pred_lassoreg

abs_err_lassoreg <- abs(pred_lassoreg - test$Apps)
models_comp <- rbind(models_comp, "LASSOReg" = c(mean(abs_err_lassoreg),
                                                 median(abs_err_lassoreg),
                                                 sd(abs_err_lassoreg),
                                                 IQR(abs_err_lassoreg),
                                                 range(abs_err_lassoreg)))
View(models_comp)
length(coef(lassoreg_2))

#Actual vs. Predicted
plot(test$Apps, pred_lassoreg, main = 'LASSOReg',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)


# Model 5 Decision Tree Structures -----
## Model 5-1 Simple Decision tree----

tree_1 <- rpart(Apps ~ Private + Accept + Enroll + Top10perc + F.Undergrad
                + Outstate + Room.Board + Expend  + PhD, data = train, 
                control = list(cp = 0.01, maxdepth = 10, minbucket = 20))

## Plot the tree##
prp(tree_1)

## Decision Tree Structure
tree_1
## we see the Accept, and Top 10 per are playing an important role

## Model 5-2 Simple Decision tree----
tree_2 <- rpart(Apps ~ Private + Accept + Enroll + Top10perc + F.Undergrad
                + Outstate + Room.Board + Expend  + PhD, data = train, 
                control = list(cp = 0.001, maxdepth = 20, minbucket = 20))

## Plot the tree
prp(tree_2)
tree_2
## we see the Accept, Top 10 and F.Underg per are playing an important role

#Plot the tree, CP plays an important role and we have to measure it good. and find it the best one
plotcp(tree_2)
tree_2$cptable
tree_2$cptable[which.min(tree_2$cptable[,"xerror"])]
#Prune the tree
tree_2P <- prune.rpart(tree_2, 
                      cp = tree_2$cptable[which.min(tree_2$cptable[,"xerror"])])

#Plot the pruned tree
prp(tree_2P)

## Model 5-3  Decision tree on All----
## we see the Accept, and Top 10  are playing an important role

tree_3 <- rpart(Apps ~ ., data = train, 
                control = list(cp = 0.0001, maxdepth = 20, minbucket = 5))
## Plot the tree##
prp(tree_3)
tree_3


#Plot the tree, CP plays an important role and we have to measure it good. and find it the best one
plotcp(tree_3)


tree_3$cptable

tree_3$cptable[which.min(tree_2$cptable[,"xerror"])]

tree_3P <- prune.rpart(tree_2, 
                      cp = tree_2$cptable[which.min(tree_2$cptable[,"xerror"])])

#Plot the pruned tree
prp(tree_3P)

## Test the Model
## Prediction: tree_5
pred_tree  <- predict(tree_3P, test)

## Absolute error mean, median, sd, max, min
abs_err_tree <- abs(pred_tree - test$Apps)
models_comp  <- rbind(models_comp, "TreeReg" = c(mean(abs_err_tree),
                                                 median(abs_err_tree),
                                                 sd(abs_err_tree),
                                                 IQR(abs_err_tree),
                                                 range(abs_err_tree)))
View(models_comp)

## Actual vs. Predicted
plot(test$Apps, pred_tree, main = 'TreeReg',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)




## Model 5-4  Decision tree on All in eliminated data----
## we see the Accept, and Top 10  are playing an important role

tree_4 <- rpart(Apps ~ ., data = train2, 
                control = list(cp = 0.0001, maxdepth = 20, minbucket = 5))
## Plot the tree##
prp(tree_4)
tree_4
plotcp(tree_4)
tree_4$cptable
tree_4P <- prune.rpart(tree_4, 
                       cp = tree_4$cptable[which.min(tree_4$cptable[,"xerror"])])
prp(tree_4P)

## Test the Model
## Prediction: tree train2 
pred_tree2  <- predict(tree_4P, test)

## Absolute error mean, median, sd, max, min
abs_err_tree2 <- abs(pred_tree2 - test$Apps)
models_comp  <- rbind(models_comp, "TreeReg2" = c(mean(abs_err_tree2),
                                                 median(abs_err_tree2),
                                                 sd(abs_err_tree2),
                                                 IQR(abs_err_tree2),
                                                 range(abs_err_tree2)))
View(models_comp)

## Actual vs. Predicted
plot(test$Apps, pred_tree2, main = 'TreeReg2',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

## So in here I have found that, if I used train2 instated 
##                                   of train 1 model accuracy is decreased


## Model 5-5 Bagging
set.seed(1234)
bagging_1 <- randomForest(Apps ~ . , mtry = ncol(train) - 2, ntree = 500, data = train)
bagging_1

bagging_2 <- randomForest(Apps ~ . , mtry = ncol(train) - 2, ntree = 500, data = train2)
bagging_2

## in here the Accuracy increased 10% more in train2 

pred_bagging1  <- predict(bagging_1, test)
## Absolute error mean, median, sd, max, min
abs_err_bagging <- abs(pred_bagging1 - test$Apps)
models_comp <- rbind(models_comp, "Bagging1" = c(mean(abs_err_bagging),
                                                median(abs_err_bagging),
                                                sd(abs_err_bagging),
                                                IQR(abs_err_bagging),
                                                range(abs_err_bagging)))
View(models_comp)

## Actual vs. Predicted
plot(test$Apps, pred_bagging1, main = 'Bagging1',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)

pred_bagging2  <- predict(bagging_2, test)
## Absolute error mean, median, sd, max, min
abs_err_bagging2 <- abs(pred_bagging2 - test$Apps)
models_comp <- rbind(models_comp, "Bagging2" = c(mean(abs_err_bagging2),
                                                 median(abs_err_bagging2),
                                                 sd(abs_err_bagging2),
                                                 IQR(abs_err_bagging2),
                                                 range(abs_err_bagging2)))
View(models_comp)

## Actual vs. Predicted
plot(test$Apps, pred_bagging2, main = 'Bagging2',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)



## Model 5- random Forest
rf_1 <- randomForest(Apps ~ . , data = train, 
                     mtry = 6, ntree = 500, nodesize = 5, importance = TRUE)
## mtry	
#     for regression = p/3
rf_1
importance(rf_1)
varImpPlot(rf_1)


## recursive: whether variable importance is (re-)assessed at each step of variable reduction

rf_cv <- rfcv(train[, - c(2)], 
              train$Apps, 
              cv.fold = 10,
              step = 0.75,
              mtry = function(p) max(1, floor(sqrt(p))), 
              recursive = FALSE)
class(rf_cv)
str(rf_cv)
## Vector of number of variables used at each step
rf_cv$n.var
## Corresponding vector of MSEs at each step
rf_cv$error.cv
which.min(rf_cv$error.cv)

## Remove 4 variables based on Importance of Variables
sort(importance(rf_1)[,1])


## Regression formula
reg_formula <- as.formula(Apps ~ Accept  + Enroll + F.Undergrad + Top10perc + 
                            Room.Board  + Grad.Rate + Top25perc + Outstate + 
                            Expend + Private + perc.alumni + Terminal + PhD + 
                            P.Undergrad ) 

rf_2_1 <- randomForest(reg_formula, data = train, mtry = 3, ntree = 500, nodesize = 5)
rf_2_1

rf_2_2 <- randomForest(reg_formula, data = train2, mtry = 3, ntree = 500, nodesize = 5)
rf_2_2

reg_formula
class(reg_formula)
## mtry	
floor(sqrt(14)) 


## Test the Model
# Prediction: rf_2
pred_rf  <- predict(rf_2_1, test)
pred_rf
## Absolute error mean, median, sd, max, min
abs_err_rf <- abs(pred_rf - test$Apps)
models_comp <- rbind(models_comp, "RandomForest1" = c(mean(abs_err_rf),
                                                     median(abs_err_rf),
                                                     sd(abs_err_rf),
                                                     IQR(abs_err_rf),
                                                     range(abs_err_rf)))
View(models_comp)

## Actual vs. Predicted

plot(test$Apps, pred_rf, main = 'RandomForest',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)



## Test the Model
## Prediction: rf_2
pred_rf2  <- predict(rf_2_2, test)
pred_rf2
## Absolute error mean, median, sd, max, min
abs_err_rf <- abs(pred_rf2 - test$Apps)
models_comp <- rbind(models_comp, "RandomForest2" = c(mean(abs_err_rf),
                                                      median(abs_err_rf),
                                                      sd(abs_err_rf),
                                                      IQR(abs_err_rf),
                                                      range(abs_err_rf)))
View(models_comp)

## Actual vs. Predicted
plot(test$Apps, pred_rf, main = 'RandomForest2',
     xlim = c(0, 2000), ylim = c(0, 2000),
     xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)







