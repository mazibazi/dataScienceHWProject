setwd("D:\PhD\Data Science\dataScienceHWProject\college")
getwd()

library("moments")  #Moments, skewness, kurtosis and related tests 
library("MASS")     #Box-Cox Transformations for Linear Models
library("leaps")    #Regression Subset Selection
library("corrplot") #Visualization of Correlation Matrix

data<- read.csv("college.csv", header = TRUE)

## Data Description ------
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

colnames(data)
class(data)
dim(data)
head(data)
tail(data, 2)
View(head(data,20))
str(data)
summary(data)


#Remove College.Name
unique(data$College.Name)
length(unique(data$College.Name))
data <- data[,-1]


##Uni variate Profiling-----
unique(data$Private)
length(unique(data$Private))


#customer_type------------
summary(data$Grad.Rate)

for (i in 2:18){
  print(colnames(data[i]))
  print(summary(data[, i]))
}


# Categorical data
data$Private <- factor(data$Private)

summary(data)



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


