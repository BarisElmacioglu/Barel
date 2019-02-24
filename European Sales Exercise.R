
#################################################################
## Marketing analytics                                         ##
## Lecture 2 Exercise in Market Response Models				         ##
## European Sales Data Exercise                                ##
## Süleyman Baris Elamcioglu - 1805546                         ## 
#################################################################


# Get the working directory.
getwd()

# Read the Data files from a directory 
EuropeanSalesData<-read.csv("EuropeanSales.csv",header=T)

#Show types of columns  
sapply(EuropeanSalesData,typeof)

#Show attributes and summary of data
attributes(EuropeanSalesData)
head(EuropeanSalesData)
summary(EuropeanSalesData)

#Draw the graph
plot(EuropeanSalesData$GDPperHead, EuropeanSalesData$ComputerSales, ylab = "Sales", xlab = "GDP per Head")
plot(EuropeanSalesData$EducationSpending, EuropeanSalesData$ComputerSales, ylab = "Sales", xlab = "Edu Spending")
plot(EuropeanSalesData)

#Check the correlation between attributes
cor(EuropeanSalesData$GDPperHead, EuropeanSalesData$ComputerSales)
cor(EuropeanSalesData$EducationSpending, EuropeanSalesData$ComputerSales)
EuropeanSalesData2<-(EuropeanSalesData[-1])
EuropeanSalesData2
attributes(EuropeanSalesData2)
head(EuropeanSalesData2)
summary((EuropeanSalesData2))
cor(EuropeanSalesData2[,])

#Fit data to model
#Kitchen Sink
model1 <- lm(ComputerSales ~ Population + GDPperHead + UnemploymentRate + EducationSpending + SalesPerCapita, data = EuropeanSalesData2)
summary(model1)

model2 <- lm(ComputerSales ~ Population + GDPperHead + UnemploymentRate + SalesPerCapita, data = EuropeanSalesData2)
summary(model2)

model3 <- lm(ComputerSales ~ Population + GDPperHead + UnemploymentRate, data = EuropeanSalesData2)
summary(model3)

model4 <- lm(ComputerSales ~ Population + GDPperHead, data = EuropeanSalesData2)
summary(model4)

