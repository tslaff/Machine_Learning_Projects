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

# Grab the data from this dropbox link
Data<-USDCAD

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
DataSet<-na.exclued(DataSet)

# Break int training, test and validation sets
Breakpoint_Test = (0.6)*nrow(DataSet)
Breakpoint_Val = (0.8)*nrow(lineDataSet)
Training<-DataSet[1:Breakpoint_Test,]
Test<-DataSet[(Breakpoint_Test+1):Breakpoint_Val,]
Validation<-DataSet[(Breakpoint_Val+1):nrow(DataSet),]

# Build our model on the training set
NB<-naiveBayes(Class ~ RSI3 + CCI20 + DEMA10c, data=Training)

# Test over test set
table(predict(NB,Test,type="class"),Test[,4],dnn=list('predicted','actual'))

# Now let's see where the algorithm went long and where it went short
# First build our data set of its predictions, and whether it was correct or incorrect
TestPredictions<-predict(NB,Test,type="class")
TestCorrect<-ifelse(TestPredictions==Test[,4],"Correct","Incorrect")
TestData<-data.frame(Test,TestPredictions,TestCorrect)

# Build two sets of plots: one for predictions and one for accuracy
# For the predictions:
ggplot(TrainingData,aes(x=CCI20,fill=TrainingPredictions))+
  geom_histogram(binwidth=15,position="fill")+
  labs(title="Training Predictions: CCI", x = "20-Period CCI", y= "Up/Down Ratio",fill="Predictions")+
  scale_fill_manual(values=c("#FF6737","#00B204"))

# For the accuracy:
ggplot(TestData,aes(x=RSI3,fill=TestCorrect))+
  geom_histogram(binwidth=5,position="fill")+
  labs(title="Test Accuracy: RSI", x = "3-Period RSI", y= "Correct/Incorrect Ratio",fill="Accuracy")+
  scale_fill_manual(values=c("#0066FF","#FF4747"))

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




