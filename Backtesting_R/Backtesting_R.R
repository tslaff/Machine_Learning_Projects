library(quantmod)
library(repmis)
library(ggplot2)

# Retrieve the data from my dropbox account
filename_usdcad <-"USDCAD.csv"
mykey_usdcad <- "xlrbbnm465jlf6z"
Data<-source_DropboxData(filename_usdcad,key=mykey_usdcad, sep=",", header=TRUE)

# Convert into an xts 
Dates<-as.POSIXct(USDCAD_Data[,1],format = "%m/%d/%y %H:%M",tz='UTC')
USDCAD_xts<-as.xts(data.frame(USDCAD_Data[,2:5],row.names=Dates))

# Calculate our indicators
#A 20-period Commodity Channel Index calculated of the High/Low/Close of our data
CCI20<-CCI(USDCAD_xts[,2:4],n=20)
#A 3-period RSI calculated off the close
RSI3<-RSI(Cl(USDCAD_xts),n=3)
#A 10-period Double Exponential Moving Average (DEMA), with standard parameters. And we will be looking at the difference between the price and the DEMA.
DEMA10<-DEMA(Cl(USDCAD_xts),n = 10, v = 1, wilder = FALSE)
DEMA10c<-Cl(USDCAD_xts) - DEMA10
#Convert into pips
DEMA10c<-DEMA10c/.0001

# Combine indicators into one data set
Indicator_Data<-data.frame(RSI3,DEMA10c,CCI20)
# Remove the last indicator value to line up with our prices and remove look-ahead bias
Indicator_Data<-Indicator_Data[-nrow(Indicator_Data),]
# Remove first price value to line up with indicators
Market_Data<-USDCAD_xts[-1,]
# Create our final data set
Strategy_Data<-data.frame(Indicator_Data,Market_Data)
Strategy_Data<-na.omit(Strategy_Data)
colnames(Strategy_Data)<-c("RSI3","Price-DEMA","CCI20","Open","High","Low","Close")


# Let's isolate the validation set
Validation_Set<-Strategy_Data[(.8*nrow(Strategy_Data)):nrow(Strategy_Data),]

# Now we have the indicator valuse lined up with the open prices (indicators originally calculated off the close then shifted back one)
# Build our long rules
buy.signal<-ifelse(Validation_Set$RSI3 < 30 & Validation_Set$CCI20 > -290 & Validation_Set$CCI20 < -100 & Validation_Set$`Price-DEMA` > -40 & Validation_Set$`Price-DEMA` < -20,1,0)
# And our short rules
sell.signal<-ifelse(Validation_Set$RSI3 > 50 & Validation_Set$CCI20 > 185 & Validation_Set$CCI20 < 325 & Validation_Set$`Price-DEMA` > 10 & Validation_Set$`Price-DEMA` < 40, -1 ,0)

# Calculate our long returns and covert to dollars
long.returns<-ifelse(buy.signal==1,Validation_Set$Close-Validation_Set$Open,0)
long.returns<-long.returns/.0001
# Calculate our short returns and convert to dollars
short.returns<-ifelse(sell.signal==-1,Validation_Set$Open-Validation_Set$Close,0)
short.returns<-short.returns/.0001

# Find the long and short cumulative returns
long.cumulative.returns<-cumsum(long.returns)
short.cumulative.returns<-cumsum(short.returns)
# And our total returns
total.return<-long.cumulative.returns+short.cumulative.returns

# Combine into a single data frame to plot
Plot_Data<-data.frame(Strategy_Data,buy.signal,sell.signal,long.cumulative.returns,short.cumulative.returns, total.return)
# Create a column from our row names to use as the x-axis
Plot_Data$Datetime<-row.names(Plot_Data)

# Create a plot for the price
Price_Plot<-ggplot(Plot_Data,aes(x=Datetime,y=Open,group=1,color="Open Price"))+geom_line()+labs(title="USDCAD 4h",x="Date",y="Open",color=" ")

# Then plot the returns
Return_Data<-data.frame(Plot_Data$Datetime,Plot_Data$total.return,Plot_Data$long.cumulative.returns,Plot_Data$short.cumulative.returns)
colnames(Return_Data)<-c("Date","Total Return","Long Returns","Short Returns")
Return_Data.melted <- melt(Return_Data, id = "Date")
Return_Plot<-ggplot(data = Return_Data.melted, aes(x = Date, y = value, group = variable,color=variable)) +geom_line()+labs(title="Strategy Returns",x="Date",y="Return ($)",color=" ")

# View the final plots
grid.arrange(Price_Plot,Return_Plot,ncol=1,nrow=2)
