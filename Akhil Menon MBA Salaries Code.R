# Analysis of MBA SALARIES
# NAME: Akhil Menon
# EMAIL : menonakhil90@gmail.com
# COLLEGE : Sri Venkateswara College of Engineering

# Setting working directory to folder containing Dataset
setwd("C:/Users/menon/Desktop/New Stuff")

# Reading the dataset into a data frame
MBAS <- read.csv("MBA Starting Salaries Data.csv")

# Standard view of the dataset
View(MBAS)

# Creating a subset of only Placed MBA students
MBAP <- subset(MBAS, MBAS$salary > 1000)

# Summary Statistics of the dataset
summary(MBAP)

# Boxplot Visualizations of each variable of the dataset independently
boxplot(MBAP$age)
boxplot(MBAP$sex)
boxplot(MBAP$gmat_tot)
boxplot(MBAP$gmat_qpc)
boxplot(MBAP$gmat_vpc)
boxplot(MBAP$gmat_tpc)
boxplot(MBAP$s_avg)
boxplot(MBAP$f_avg)
boxplot(MBAP$quarter)
boxplot(MBAP$work_yrs)
boxplot(MBAP$frstlang)
boxplot(MBAP$salary)
boxplot(MBAP$satis)

# Scatter Plot Visualization
library(car)
scatterplot(MBAP$salary,MBAP$satis)
scatterplot(MBAP$salary, MBAP$work_yrs)


# Corrgram Visualization
library(corrgram)
corrgram(MBAP, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

corrgram(MBAP, order=NULL, lower.panel=panel.shade,upper.panel=NULL, text.panel=panel.txt)

# Variance- Covariance Matrix Creation
cor(MBAP[,-c(1)])  

# Contingency Tables with Salary as Y and First Language, Gender, GMAT Performance, MBA Average, Prior Work Experience as X respectively 
xtabs(~frstlang+salary, data = MBAP)
xtabs(~sex+salary, data = MBAP)
xtabs(~gmat_tot+salary, data = MBAP)
xtabs(~s_avg+salary, data =  MBAP)
xtabs(~f_avg+salary, data = MBAP)
xtabs(~work_yrs+salary, data = MBAP)

# t-tests of Salary as Y against First Language, Gender, GMAT Performance, MBA Average, Prior Work Experience as X.
t.test(MBAP$frstlang,MBAP$salary)  # p-value < 2.2e-16
t.test(MBAP$sex,MBAP$salary)       # p-value < 2.2e-16
t.test(MBAP$gmat_tot,MBAP$salary)  # p-value < 2.2e-16
t.test(MBAP$s_avg, MBAP$salary)    # p-value < 2.2e-16
t.test(MBAP$f_avg, MBAP$salary)    # p-value < 2.2e-16
t.test(MBAP$work_yrs, MBAP$salary) # p-value < 2.2e-16

# So in all cases since p < 2.2e-16, we reject Null Hyppothesis

# Segregation of Score based On gender
library(ggplot2)
ggplot(MBAP, aes(x = sex, fill = sex)) + geom_bar()

# CORRELATION OF GMAT TOTAL AND ITS COMPONENTS
ggplot(MBAP, aes(x=gmat_tot,y=gmat_qpc)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth()
ggplot(MBAP, aes(x=gmat_tot,y=gmat_vpc)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth()
ggplot(MBAP, aes(x=gmat_tot,y=gmat_tpc)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth()

# Creating test and training data frames
ratio = sample(1:nrow(MBAP), size = 0.25*nrow(MBAP))
Test = MBAP[ratio,] #Test dataset 25% of total
Training = MBAP[-ratio,] #Train dataset 75% of total
dim(Training)
dim(Test)

# Multi Variable Linear Regression Model
linear.mod<- lm(salary ~ age + work_yrs + quarter + s_avg + f_avg + gmat_tot, data = Training)
summary(linear.mod)

# p-value < 0.05, hence null hypothesis can be rejected.

# Implementing a Regression Tree Model utilising a Root mean square function

rms <- function(x,y)
{
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

library(rpart)
samp <- rpart(salary ~ age + work_yrs + quarter + s_avg + f_avg + gmat_tot, data = Training, method = "anova")
predict <- predict(samp, Test)
rmsa <- rms(predict, Test$salary)
rmsb <- round(rmsa, digits = 3)
rmsa

# Accuracy Prediction of the model
predict<- predict(linear.mod, Test)
P1 <- data.frame(cbind(actuals=Test$salary, predicteds=predict)) # make actuals_predicteds dataframe.
corracc <- cor(P1)
corracc
