# Install the packages we need
# Quantmod gives us the indicator calculations and market data
install.packages("quantmod")
library("quantmod")
# The neural network package we will use. There are many different ANN packages but this one has particularly nice plotting capabilities.
install.packages(“neuralnet”)
library(“neuralnet”)

# Set the start and end dates then retreive the data
startDate <- as.Date('2009-01-01')
endDate <- as.Date('2014-01-01')
getSymbols("^GSPC",src="yahoo",from=startDate,to=endDate)
# Calculate the indicators
RSI3 <- RSI(Op(GSPC),n=3)
EMA5 <- EMA(Op(GSPC),n=5)
EMAcross <- Op(GSPC)-EMA5
# Use just the MACD Signal Line
MACD <- MACD(Op(GSPC),fast = 12, slow = 26, signal = 9)
MACDsignal <- MACD[,2]
# Use the Bollinger Band %B to measure price relative to high-low range
BB <- BBands(Op(GSPC),n=20,sd=2)
BBp <- BB[,4]

# Calculate the numeric change in price
Price <- Cl(GSPC)-Op(GSPC)

# Create the data set
DataSet <- data.frame(RSI3,EMAcross,MACDsignal,BBp,Price)
DataSet <- na.exclude(DataSet)
colnames(DataSet) <-c ("RSI3","EMAcross","MACDsignal","BollingerB","Price")

# Normalize between 0 and 1
Normalized <- function(x) {(x-min(x))/(max(x)-min(x))}
NormalizedData <- as.data.frame(lapply(DataSet,Normalized))

# Create a training and test set
Breakpoint <- (2/3)*nrow(NormalizedData)
TrainingSet <- NormalizedData[1:Breakpoint,]
TestSet <- NormalizedData[(Breakpoint:1):nrow(NormalizedData) , ]

# Build our ANN with a learning rate of 0.001 and a backpropogation algorithm
set.seed(1)
nn1<-neuralnet(Price~RSI3+EMAcross+MACDsignal+BollingerB,data=TrainingSet, hidden=c(3,3), learningrate=.001,algorithm="backprop")

# Plot the ANN
plot(nn1)






