# Install the packages we need
# rpart for the decision tree
install.packages("rpart")
library(rpart)
# foreach gives us the iterative function for the bagged decision
install.packages("foreach")
library(foreach)
# Quantmod gives us the indicators
install.packages("quantmod")
library(quantmod)


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

# Build our features
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

# Create our baseline decision tree
BaselineDecisionTree<-rpart(Class~EMA5Cross+RSI14ROC3+VolumeROC1,data=FeatureTrainingSet, cp=.001)
# See how well it does over the test set
table(predict(BaselineDecisionTree,FeatureTestSet,type="class"),FeatureTestSet[,4],dnn=list('predicted','actual'))

# This determines how we create subsets of the training set and how many models to include in the ensemble. Here we are building 1501 different models off a randomly sampled 1/10th of the data
length_divisor<-10
iterations<-1501

#This is our handmade bagging algorithm. Thanks to Vic Paruchuri for this.
BaggedDecisionTree<- foreach(m=1:iterations,.combine=cbind) %do% { training_positions <- sample(nrow(FeatureTrainingSet), size=floor((nrow(FeatureTrainingSet)/length_divisor)))
train_pos<-1:nrow(FeatureTrainingSet) %in% training_positions 
BaselineDecisionTree<-rpart(Class~EMA5Cross+RSI14ROC3+VolumeROC1,data=FeatureTrainingSet[train_pos,])
predict(BaselineDecisionTree,newdata=FeatureTestSet)
}

# Now we need to aggregate the predictions of our 1501 decision trees
CumulativePredictions<-apply(BaggedDecisionTree[,1:iterations],1,function(x){s<-(sum(x)/iterations)
round(s,0)})
#Return from a binary output to our classification labels
FinalPredictions<-ifelse(CumulativePredictions==1,"DOWN","UP")

# Compare the perforance 
table(FinalPredictions,FeatureTestSet[,4],dnn=list('predicted','actual'))





