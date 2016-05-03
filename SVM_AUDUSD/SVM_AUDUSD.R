# A great quantitative trading resource
install.packages('quantmod')
library(quantmod)
# The library containing our SVM
install.packages('e1071')
library(e1071)
# The plotting tools we will use
install.packages('ggplot2')
library(ggplot2)
# Allows us to grab the data from Dropbox
install.packages("repmis")
library(repmis)

# Grab the data directly from my dropbox account
filename <-"AUDUSD.csv"
mykey <- "kk47ydcz36xik2i"
Data<-source_DropboxData(filename,key=mykey, sep=",", header=TRUE)

#The 3-period relative strength index calculated off the open
RSI3<-RSI(Op(Data),n=3)

#Our measure of trend: the difference between the open price and the 50-period simple moving average
SMA50<-SMA(Op(Data),n=50)
Trend<-Op(Data)-SMA50

#The variable we are looking to predict; the direction of the next bar
Price<-Cl(Data)-Op(Data)
Class<-ifelse(Price>0,"UP","DOWN")

#Create the data set and removing the points where our indicators are still being calculated
DataSet<-data.frame(RSI3,Trend,Class)
DataSet<-na.omit(DataSet)


# Break int training, test and validation sets
Breakpoint_Test = (0.6)*nrow(DataSet)
Breakpoint_Val = (0.8)*nrow(DataSet)
Training<-DataSet[1:Breakpoint_Test,]
Test<-DataSet[(Breakpoint_Test+1):Breakpoint_Val,]
Validation<-DataSet[(Breakpoint_Val+1):nrow(DataSet),]

set.seed(1)

# Build our support vector machine using a radial basis function as our kernel, the cost, or C, at 1, and the gamma function at ½, or 1 over the number of inputs we are using
SVM<-svm(Class~RSI3+Trend,data=Training, kernel="radial",cost=1,gamma=1/2)

#Run the algorithm once more over the training set to visualize the patterns it found
TrainingPredictions<-predict(SVM,Training,type="class")

# Create a data set with the predictions
TrainingData<-data.frame(Training,TrainingPredictions)

# Let's visualize the patterns it was able to find
ggplot(TrainingData,aes(x=Trend,y=RSI3))+stat_density2d(geom="contour",aes(color=TrainingPredictions))+labs(title="SVM RSI3 and Trend Predictions",x="Open - SMA50",y="RSI3",color="Training Predictions")

# Now we'll build our long and short rules based on the patterns it found
ShortRange1<-which(Test$RSI3 < 25 & Test$Trend > -.010 & Test$Trend < -.005)
ShortRange2<-which(Test$RSI3 > 70 & Test$Trend < -.005)
ShortRange3<-which(Test$RSI3 > 75 & Test$Trend > .015)
LongRange1<-which(Test$RSI3 < 25 & Test$Trend < -.02)
LongRange2<-which(Test$RSI3 > 50 & Test$RSI3 < 75 & Test$Trend > .005 & Test$Trend < .01)

# Find our short trades
ShortTrades<-Test[c(ShortRange1,ShortRange2,ShortRange3),]

# And the short accuracy
ShortCorrect<-((length(which(ShortTrades[,3]=="DOWN")))/nrow(ShortTrades))*100

# Our long trades
LongTrades<-Test[c(LongRange1,LongRange2), ]
LongCorrect<-((length(which(LongTrades[,3]=="UP")))/nrow(LongTrades))*100



