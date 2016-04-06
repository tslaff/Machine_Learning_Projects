# Install necessary packages
# Quantmod allows us to calculate the indicators
install.packages("quantmod")
library("quantmod")
# Lubridate makes it easier to work with the dates
install.packages("lubridate")
library("lubridate")
# e1071 gives us access to the Naïve Bayes classifier
install.packages("e1071")
library("e1071")


# Retrieve data from Yahoo Finance
# The beginning of the date range we want to look at
startDate = as.Date("2012-01-01")
# The end of the date range we want to look at
endDate = as.Date("2014-01-01") 
# Retrieving Apple’s daily OHLCV from Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = startDate, to = endDate) 

# Calculate the dependent variable we are trying to predict
# Find the difference between the close price and open price
PriceChange<- Cl(AAPL) - Op(AAPL)
#Convert to a binary classification. (In our data set, there are no bars with an exactly 0 price change so, for simplicity sake, we will not address bars that had the same open and close price.)
Class<-ifelse(PriceChange > 0,"UP","DOWN")

# Calculate the indicators as our independent variables
# Find the day of the week
DayofWeek<-wday(AAPL, label=TRUE)
# Calculate a 5-period EMA off the open price
EMA5<-EMA(Op(AAPL),n = 5)
# Then the 10-period EMA, also off the open price
EMA10<-EMA(Op(AAPL),n = 10)
# Positive values correspond to the 5-period EMA being above the 10-period EMA
EMACross <- EMA5 - EMA10
# Round the values to two decimal places
EMACross<-round(EMACross,2)

# Create our data set
DataSet<-data.frame(DayofWeek,EMACross, Class)
# We need to remove the instances where the 10-period moving average is still being calculated
DataSet<-na.exclude(DataSet)
# Find the breakpoint to split the data set
Breakpoint <- (2/3)*nrow(DataSet)
# We will use ⅔ of the data to train the model
TrainingSet<-DataSet[1:Breakpoint,]
# And ⅓ to test it on unseen data
TestSet<-DataSet[(Breakpoint+1):nrow(DataSet),] 

# Build our model
EMACrossModel<-naiveBayes(TrainingSet[,1:2],TrainingSet[,3])

# Test it over new data
table(predict(EMACrossModel,TestSet),TestSet[,3],dnn=list('predicted','actual'))



