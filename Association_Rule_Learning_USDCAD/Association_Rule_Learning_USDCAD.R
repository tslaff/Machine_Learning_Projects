# Install the packages we need
# Gives us access to the technical indicators we need
install.packages("quantmod")
library(quantmod)
# Includes the Naive Bayes classifier
install.packages("e1071")
library(e1071)
# The charting functions we will use
install.packages("ggplot2")
library(ggplot2)
# Allow us to download data directly from dropbox
install.packages("repmis")
library(repmis)

# Grab the data directly from my dropbox account
filename <-"USDCAD.csv"
mykey <- "xlrbbnm465jlf6z"
Data<-source_DropboxData(filename,key=mykey, sep=",", header=TRUE)

# Calculate our indicators
CCI20<-CCI(Data[,3:5],n=20)
RSI3<-RSI(Cl(Data),n=3)
#A 10-period Double Exponential Moving Average (DEMA), with standard parameters. And we will be looking at the difference between the price and the DEMA.
DEMA10<-DEMA(Cl(Data),n = 10, v = 1, wilder = FALSE)
DEMA10c<-Cl(Data) - DEMA10
#Convert into pips
DEMA10c<-DEMA10c/.0001

# Combine into one data set and line up indicators with the bar close to prevent data snooping
Indicators<-data.frame(RSI3,DEMA10c,CCI20)
Indicators<-round(Indicators,2)
Indicators<-Indicators[-nrow(Data),]

# Create the variable we're looking to predict
Price<-Cl(Data)-Op(Data)
Class<-ifelse(Price > 0 ,"UP","DOWN")
# Remove oldest data point to line up with indicators
Class<-Class[-1]
DataSet<-data.frame(Indicators,Class)
DataSet<-na.exclude(DataSet)

# Break int training, test and validation sets
Breakpoint_Test = (0.6)*nrow(DataSet)
Breakpoint_Val = (0.8)*nrow(DataSet)
Training<-DataSet[1:Breakpoint_Test,]
Test<-DataSet[(Breakpoint_Test+1):Breakpoint_Val,]
Validation<-DataSet[(Breakpoint_Val+1):nrow(DataSet),]

# Build our model on the training set
NB<-naiveBayes(Class ~ RSI3 + CCI20 + DEMA10c, data=Training)

# Test over test set
table(predict(NB,Test,type="class"),Test[,4],dnn=list('predicted','actual'))

# Now let's see where the algorithm went long and where it went short
# First build our data set of whether it predicted long or short over the training set
TrainingPredictions<-predict(NB,Training,type="class")
TrainingData<-data.frame(Training,TrainingPredictions)


# Build a data set of where the algorithm was correct or incorrect
TestPredictions<-predict(NB,Test,type="class")
TestCorrect<-ifelse(TestPredictions==Test[,4],"Correct","Incorrect")
TestData<-data.frame(Test,TestPredictions,TestCorrect)

# Build two sets of plots: one for predictions and one for accuracy
# For the predictions:
Training_colnames<-colnames(TrainingData)
Prediction_Plots<-list()
for(i in 1:(length(TrainingData)-2)){
  Prediction_Plots[[i]]<-ggplot(TrainingData,aes_string(x=Training_colnames[i],fill="TrainingPredictions"))+
  geom_histogram(binwidth=15,position="fill")+
  labs(title="Training Predictions", y= "Up/Down Ratio",fill="Predictions")+
  scale_fill_manual(values=c("#FF6737","#00B204"))
}  

# For the accuracy:
Test_colnames<-colnames(TestData)
Accuracy_Plots<-list()
for(i in 1:(length(TestData)-3)){
      bin <-(max(TestData[,i])-min(TestData[,i]))/20
      Accuracy_Plots[[i]]<-ggplot(TestData,aes_string(x=Test_colnames[i],fill="TestCorrect"))+
      geom_histogram(binwidth=bin,position="fill")+
      labs(title="Test Accuracy", y= "Correct/Incorrect Ratio",fill="Accuracy")+
      scale_fill_manual(values=c("#0066FF","#FF4747"))
} 

# We then manually build the rules by looking for areas where there was a majority of the prediciton direction and high accuracy
# Here are the long rules I built:
Long<-which(Validation$RSI3 < 30 & Validation$CCI > -290 & Validation$CCI < -100 & Validation$DEMA10c > -40 & Validation$DEMA10c < -20)
LongTrades<-Validation[Long,]
LongAcc<-ifelse(LongTrades[,4]=="UP",1,0)

# Finding the accuracy of our long rules
(sum(LongAcc)/length(LongAcc))*100

# Here are the short rules:
Short<-which(Validation$DEMA10c > 10 & Validation$DEMA10c < 40 & Validation$CCI > 185 & Validation$CCI < 325 & Validation$RSI3 > 50)
ShortTrades<-Validation[Short,]
ShortAcc<-ifelse(ShortTrades[,4]=="DOWN",1,0)

# Finding the accuracy of our short rules 
(sum(ShortAcc)/length(ShortAcc))*100

# Final statistics
Number_of_Trades <- length(LongAcc)+length(ShortAcc)
Accuracy <- (sum(ShortAcc)+sum(LongAcc))/(Number_of_Trades)*100




