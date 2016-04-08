# Install the packages we need
# Quantmod gives us the indicators
install.packages("quantmod")
library(quantmod)
# e1071 has our Naive Bayes classifier
install.packages("e1071")
library(e1071)

#Set the date range we want to explore
startDate = as.Date("2009-01-01")
endDate = as.Date("2014-06-01")

#Grab our data
getSymbols("MSFT", src = "yahoo", from = startDate, to = endDate)

# Calculate the indicators
EMA5<-EMA(Op(MSFT),5)
RSI14<-RSI(Op(MSFT),14)
Volume<-lag(MSFT[,5],1)

# Create the variable we are looking to predict
PriceChange<- Cl(MSFT) - Op(MSFT)
Class<-ifelse(PriceChange>0,"UP","DOWN")

# Combine into one data set
BaselineDataSet<-data.frame(EMA5,RSI14,Volume)
BaselineDataSet<-round(BaselineDataSet,2)
BaselineDataSet<-data.frame(BaselineDataSet,Class)
BaselineDataSet<-na.exclude(BaselineDataSet)
colnames(BaselineDataSet)<-c("EMA5","RSI14","Volume","Class")

# Break into training, test, and validation sets
Breakpoint_Test = (0.6)*nrow(BaselineDataSet)
Breakpoint_Val = (0.8)*nrow(BaselineDataSet)
BaselineTrainingSet<-BaselineDataSet[1:Breakpoint_Test,]
BaselineTestSet<-BaselineDataSet[(Breakpoint_Test+1):Breakpoint_Val,]
BaselineValSet<-BaselineDataSet[(Breakpoint_Val+1):nrow(BaselineDataSet),]

# Build our model and view performance on the test set
BaselineNB<-naiveBayes(Class~EMA5+RSI14+Volume,data=BaselineTrainingSet)
table(predict(BaselineNB,BaselineTestSet),BaselineTestSet[,4],dnn=list('predicted','actual'))

# Now let's see if we can improve the performance by using features instead of just the indicators
EMA5Cross<-EMA5-Op(MSFT)
RSI14ROC3<-ROC(RSI14,3,type="discrete")
VolumeROC1<-ROC(Volume,1,type="discrete")

# Combine into a new data set
FeatureDataSet<-data.frame(EMA5Cross,RSI14ROC3,VolumeROC1)
FeatureDataSet<-round(FeatureDataSet,2)
FeatureDataSet<-data.frame(FeatureDataSet, Class)
FeatureDataSet<-na.exclude(FeatureDataSet)
colnames(FeatureDataSet)<-c("EMA5Cross","RSI14ROC3","VolumeROC1","Class")


# Build our new training, test, and validation sets
Feature_Breakpoint_Test = (0.6)*nrow(FeatureDataSet)
Feature_Breakpoint_Val = (0.8)*nrow(FeatureDataSet)
FeatureTrainingSet<-FeatureDataSet[1:Feature_Breakpoint_Test,]
FeatureTestSet<-FeatureDataSet[(Feature_Breakpoint_Test+1):Feature_Breakpoint_Val,]
FeatureValSet<-FeatureDataSet[(Feature_Breakpoint_Val+1):nrow(FeatureDataSet),]

# Build our model and see if performance on test set improved
FeatureNB<-naiveBayes(Class~EMA5Cross+RSI14ROC3+VolumeROC1,data=FeatureTrainingSet)
table(predict(FeatureNB,FeatureTestSet),FeatureTestSet[,4],dnn=list('predicted','actual'))
