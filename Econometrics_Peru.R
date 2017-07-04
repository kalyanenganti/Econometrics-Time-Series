#IMPORT LIBRARIES
library(ggplot2)
library(e1071)
library(Metrics)
library(caret)
library(glmnet)
library(plotmo) 
library(caret)
library(randomForest)
library(lars)
library(Metrics)
library(zoo)
library(vegan)
#clear env
rm(list=ls())

#read data in
setwd('/Users/kalyan/Desktop/Econometrics/')
data<-read.csv('Peru_Economic_Factors.csv',sep = ',',header = T)

#check the data for missing and invalid values
str(data)
summary(data)
colnames(data)
colSums(is.na(data))


#ANALYIZE THE DATA
Years<-data$Year
NAs<-rowSums(is.na(data))
FDI_Inflows<-data$FDI_Net_Inflows

#Aggregate NA's missing years wise
ggplot(data, aes(x= Years, y = NAs)) + geom_point(size = 1.5, color="blue") +xlab("Years")+ylab("Missing Values")

#Plot showing FDI_inflows over the years and the missing values for the years from 1960 to 1970
ggplot(data, aes(x= Years, y = FDI_Inflows)) + geom_line(size = 0.5, color="red") +xlab("Years")+ylab("FDI_Inflows")

#Plot showing FDI_inflows and its missing and negative values
ggplot(data, aes(x= Years, y = FDI_Inflows)) + geom_bar(stat = "identity") +xlab("Years")+ylab("FDI_Inflows")

#PRE-PROCCESSING
#Most of the FDI_data prior to 1970 is missing  and NA's 
#are high, subsetting data to include values from 1970 to 2015
data<-data[11:56,]

#make all zeroes NA's in the dataframe as they are not adding much value
data[data == 0] <- NA

#remove all columns with 1% or more missing data
data<-data[, -which(colMeans(is.na(data)) > 0.01)]

#remove columns with zero variance
fdi.data<-data[ , apply(data, 2, var) != 0.00]

#remove years and dependent variable
dep.variables<-fdi.data[,1:2]

#remove higlhly correlated variables
x.sparse = cor(fdi.data)
hc = findCorrelation(x.sparse, cutoff=0.99)
hc = sort(hc)
reduced_Data = fdi.data[,-c(hc)]
colnames(reduced_Data)
#remove derivatives of fdi_components as we are interested in truly independent variables
reduced_Data=reduced_Data[-c(223,224)]



#column bind indpt. variables and dept vars
fdi.data<-cbind(dep.variables,reduced_Data)

#Random Forest to get important values
set.seed(1234)
forest_model <- randomForest(FDI_Net_Inflows ~ ., data = fdi.data,mtry=75,ntree=300,do.trace=10)
print(forest_model)
plot(forest_model)
varImpPlot(forest_model)
round(importance(forest_model), 2)  
Imp_factors_rf <- data.frame(forest_model$importance)
Imp_factors_rf1 <- data.frame(row.names(Imp_factors_rf),Imp_factors_rf[,1])
colnames(Imp_factors_rf1) = c('Attributes','Importance')
Imp_factors_rf2 <- Imp_factors_rf1[order(Imp_factors_rf1$Importance , decreasing = TRUE),]
Imp_factors_rf2 <- Imp_factors_rf2[1:10,]
varImpPlot(forest_model)


#Lasso regression doesn't perform very well when n<<p.
#Using elastic instead
#ELASTIC NET FIT
# load data
x.m <- as.matrix(fdi.data[,-2])
y.m <- as.matrix(fdi.data[,2])
# fit model
cvlassofit<-cv.glmnet(x.m,y.m,alpha=0.5,nfolds = 3)
print(cvlassofit$glmnet.fit)

imp<-varImp(cvlassofit$glmnet.fit,scale=F,lambda = 12)
rownames(imp)[order(imp$Overall, decreasing=TRUE)]

par(mfrow=c(1,1))
plot(cvlassofit$glmnet.fit,xvar="lambda",label = TRUE)
coef(cvlassofit, s = "lambda.min") # to get the coefficients for the "optimal" lambda
coef(cvlassofit, s = "lambda.1se") # to get the coefficients for the one SE lambda
plot(cvlassofit,xvar="dev",label=TRUE)

# for plot_glmnet
plot_glmnet(cvlassofit$glmnet.fit)                             # default colors
plot_glmnet(cvlassofit$glmnet.fit, label=7)                    # label the 7 biggest final coefs

#SVM fit elbow-plot with least RMSE
stepVariables<-function(df)
{
 vardf=fdi.data[2]
 varstr=""
 varList=c()
 errorList=c()
for (i in 1 : ncol(df))
  {
  vardf=cbind(vardf,df[i])
  # varstr=paste(varstr,colnames(df[i]),sep="+")
  # varList[[i]]<-substr(varstr,2,nchar(varstr))
  svmfit<-svm(FDI_Net_Inflows~.,scale = TRUE,type="eps-regression",kernel="linear",vardf)
  predicted<-predict(svmfit,vardf)
  rmse<-rmse(predicted,vardf$FDI_Net_Inflows)
  errorList[[i]]<-rmse
}
 print(sprintf("rmse minimum value is %g at index value %g ",min(errorList),which.min(errorList)))
 variablesCount<-as.numeric(seq(1,length(errorList),1))
 errorList<-as.numeric(errorList)
 df1<-data.frame(v1=variablesCount,v2=errorList)
 plotRmseVsVars(df1)
}

#function to plot rmse and no.of variables required for least rmse
plotRmseVsVars<-function(df1){
  ggplot(df1, aes(x= df1[1], y = df1[2]))+ geom_line(size = 0.5, color="black") + xlab("No of Variables") + ylab("RMSE")
}

#remove years, FDI  from dataset x and add one column each iteration to svm
stepVariables(fdi.data[,3:ncol(fdi.data)])

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#decide final significant variables select them
finaldf<-subset(fdi.data,select=c('Year','FDI_Net_Inflows','Merchandise.exports..current.US..','External.debt.stocks..short.term..DOD..current.US..','Official.exchange.rate..LCU.per.US...period.average.','Interest.payments.on.external.debt..long.term..INT..current.US..','General.government.final.consumption.expenditure..current.US..'))

#finaldf=subset(fdi.data,select=c('Year','FDI_Net_Inflows','Interest.payments.on.external.debt..private.nonguaranteed..PNG...INT..current.US..','Merchandise.exports..current.US..','General.government.final.consumption.expenditure..current.US..','Int_Payments_Ext_debt'))
colnames(finaldf)<-c('Year','FDI_Net_Inflows','Total_Merchandise_Exports','Ext_debt_stocks','Exchange_Rate','Interest_on_ext_debt_lt_US','GFCE')

df.scaled <- as.data.frame(scale(finaldf[-1]))
finaldf<-cbind(finaldf$Year,df.scaled)
linearModel<-lm(FDI_Net_Inflows~.,finaldf[-1])
predicted<-predict(linearModel)
summary(linearModel)
print(rmse(predicted,finaldf$FDI_Net_Inflows))

acf(linearModel$residuals)  
library(vars)
finalts<-ts(finaldf)


finalvar<-VAR(finalts,lag.max = 10,type = "const",ic="AIC")
plot(finalvar)
summary(finalvar)
par(mfrow=c(1,1))
plot(linearModel)
pairs(finaldf[-1])



#plot all graphs on one plot
library(reshape2)
data <- melt(finaldf, id.vars="Year",variable.name = 'series')
ggplot(data, aes(x=Year,y=value)) + geom_line(aes(colour = series))

#GFCE VS FDI_Net_Inflows
ggplot(finaldf, aes(x= GFCE, y =FDI_Net_Inflows )) + geom_line(size = 1.5, color="blue") +xlab("GFCE")+ylab("FDI_Net_Inflows")

#Ext Debt Stocks
ggplot(finaldf, aes(x= FDI_Net_Inflows, y =Ext_debt_stocks )) + geom_point(size = 1.5, color="black") +xlab("FDI_Net_Inflows")+ylab("Ext_debt_stocks")

#Interest earned on external debt
ggplot(finaldf, aes(x= , y =FDI_Net_Inflows )) + geom_point(size = 1.5,color="black") +xlab("FDI_Net_Inflows")+ylab("Interest_on_ext_debt_lt_US")

#bar plot to see them side by side
ggplot(finaldf, aes(x= Total_Merchandise_Exports, y =FDI_Net_Inflows )) + geom_line(size = 1.5, color="red") +xlab("Total_Merchandise_Exports")+ylab("FDI_Net_Inflows")

#exchange rate vs fdi_net_inflows
ggplot(finaldf, aes(x= Exchange_Rate, y = FDI_Net_Inflows)) + geom_point(size = 1.5, color="green") +xlab("Exchange_Rate")+ylab("FDI_Net_Inflows")

#get the data for submission in a data frame
submit_list<- data.frame()
df<-read.zoo(finaldf)
lagDF <- function(a,b,c,d,e,z)
{
  df<-as.data.frame(na.omit(cbind(z[,1], lag(z[,2], -a), lag(z[,3], -b), lag(z[,4], -c),lag(z[,5], -d),lag(z[,6], -e))))
  colnames(df)<-colnames(z) 
  return(df)
}

iterateLag<-function(){
for(a in 0:5){
  for(b in 0:5){
    for(c in 0:5){
      for(d in 0:5){
        for(e in 0:5){
          lag_fit = lagDF(a,b,c,d,e,df)
          linearfit <-lm(lag_fit$FDI_Net_Inflows~.,lag_fit)
          predicted_values<-predict(linearfit)
          rmserror<-rmse(linearfit$residuals,predicted_values)
          row<-data.frame(a,b,c,d,e,summary(linearfit)$r.squared,summary(linearfit)$adj.r.squared,rmserror)
          submit_list<-rbind(submit_list, row)
        }
      }
    }
  }
}
write.csv(submit_list,file ='Summary_Variables_With_Lag.csv',row.names = F )
submit_list[which.min(submit_list$rmserror),]
}


#function to iterate the dataframe and lag the variables one at a time
#writes the list to disk and
#returns the row with the least rmse
iterateLag()

#read the data in
output<-read.csv("Summary_Variables_With_Lag.csv",header = T,sep=",")
rownames(output)

