# Introduction to R
# Copyright 2017 by Michael Grogan
# Tools -> Install Packages

# Set working directory to where csv file is located
setwd("/home/michael/Documents/Universal Data/1 Documents and Music/Documents/R/CSV")

# Read the data
mydata<- read.csv("/home/michael/Documents/Universal Data/1 Documents and Music/Documents/R/CSV/ols_stock.csv")
attach(mydata)

stock_return_scaled=(stock_return/marketcap)*100

df<-data.frame(stock_return,stock_return_scaled,dividend,earnings_ranking,debt_to_equity,marketcap)
attach(df)

# OLS regression - stock_return_scaled (dependent variable) and dividend + earnings_ranking + debt_to_equity (independent variables)
reg1 <- lm(stock_return ~ dividend + earnings_ranking + debt_to_equity)
summary(reg1)

reg2 <- lm(stock_return_scaled ~ dividend + earnings_ranking + debt_to_equity)
summary(reg2)

# Redefine variables 
Y <- cbind(stock_return)
X <- cbind(dividend, earnings_ranking, debt_to_equity)

#Install “car” library and regress independent variables
library(car)
reg1m <- lm(dividend ~ earnings_ranking + debt_to_equity)
summary(reg1m)
reg2m <- lm(earnings_ranking ~ dividend + debt_to_equity)
summary(reg2m)
reg3m <- lm(debt_to_equity ~ earnings_ranking + dividend)
summary(reg3m)

#Variance Inflation Factor – Test for multicollinearity
vif(reg1m)
vif(reg2m)
vif(reg3m)

#Breusch-Pagan Test for Heteroscedasticity
library(lmtest)
bptest(Y ~ X)

reg3 <- lm(stock_return_scaled ~ earnings_ranking + debt_to_equity)
summary(reg3)

bptest(stock_return_scaled ~ earnings_ranking + debt_to_equity)

#Variables: Making predictions using regression analysis
dividendvar=1
earningsvar=1
debtvar=0.3

totalN=as.numeric(reg2$coeff[1]+(reg2$coeff[2]*dividendvar)+(reg2$coeff[3]*earningsvar)+(reg2$coeff[4]*debtvar))
totalN