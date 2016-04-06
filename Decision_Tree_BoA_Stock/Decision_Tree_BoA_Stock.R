# Install the packages we need
# Quantmod lets us calculate the indicators
install.packages("quantmod")
library("quantmod")
# rpart gives us the decision tree we will use
install.packages("rpart") 
library("rpart")
# rpart.plot can create good looking diagrams for the decision trees
install.packages("rpart.plot")
library("rpart.plot")

# Retrieve the data we need
startDate = as.Date("2012-01-01")
endDate = as.Date("2014-01-01")
# Daily OHLCV of Bank of America’s stock from Yahoo Finance
getSymbols("BAC", src = "yahoo", from = startDate, to = endDate) 

# Calculate the indicators
RSI3<-RSI(Op(BAC), n= 3)
EMA5<-EMA(Op(BAC),n=5)
# Let’s explore the difference between the open price and our 5-period EMA
EMAcross<- Op(BAC)-EMA5
MACD<-MACD(Op(BAC),fast = 12, slow = 26, signal = 9)
# Use just the signal line to use as our indicator
MACDsignal<-MACD[,2]
SMI<-SMI(Op(BAC),n=13,slow=25,fast=2,signal=9) 
# Use just the oscillator to use as our indicator
SMI<-SMI[,1]

# Calculate the variable we are trying to predict
PriceChange<- Cl(BAC) - Op(BAC)
# Create a binary classification variable
Class<-ifelse(PriceChange>0,"UP","DOWN")

# Combine into one data set
DataSet<-data.frame(RSI3,EMAcross,MACDsignal,SMI,Class)
colnames(DataSet)<-c("RSI3","EMAcross","MACDsignal","Stochastic","Class") 
DataSet<-na.exclude(DataSet)

# Break into Training and Test Sets
Breakpoint <- (2/3)*nrow(DataSet)
TrainingSet<-DataSet[1:Breakpoint,]
TestSet<-DataSet[(Breakpoint+1):nrow(DataSet),]

# Build our decidion tree
set.seed(1)
DecisionTree<-rpart(Class~RSI3+EMAcross+MACDsignal+Stochastic,data=TrainingSet, cp=.001)

# Plot the results
prp(DecisionTree,type=2,extra=8)

# Prune the tree
printcp(DecisionTree)
plotcp(DecisionTree,upper="splits")
# Selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror)
PrunedDecisionTree<-prune(DecisionTree,cp=0.0170068)
prp(PrunedDecisionTree, type=2, extra=8)

# Test over the Test Set
table(predict(PrunedDecisionTree,TestSet,type="class"),TestSet[,5],dnn=list('predicted','actual'))