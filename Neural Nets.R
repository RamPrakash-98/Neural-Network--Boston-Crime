# Importing the Libraries----
library(MASS)
library(neuralnet)
library(caTools)
library(ggplot2)

# Reading the dataset----
df<-Boston

# Normalising the data----

maxs<-apply(df,2,max)
mins<-apply(df,2,min)

scaled.data<-as.data.frame(scale(df,center = mins,scale = maxs-mins))

# Splitting the data----

split<-sample.split(scaled.data$medv,SplitRatio = 0.7)

train<-subset(scaled.data,split=T)
test<-subset(scaled.data,split=F)

# Neural Net----

n<-names(df)
n

f<-as.formula(paste("medv~",paste(n[!n %in% "medv"],collapse=" + ")))

nn<-neuralnet(f,data = train,hidden = c(5,3),linear.output = TRUE)
plot(nn)

# Predictions----

predicted.nn.values<-compute(nn,test)


predicted.nn.values


y.pred<-predicted.nn.values$net.result*(max(df$medv)-min(df$medv))+min(df$medv)

test.r<-(test$medv)*(max(df$medv)-min(df$medv))+min(df$medv)

error.df<-data.frame(test.r,y.pred)
# Mean Squared Error---- 
mse<-sum((test.r-y.pred)^2)/nrow(y.pred)
mse

#Plotting

pl<-ggplot(error.df,aes(x=test.r,y=y.pred))+geom_point()+stat_smooth()+ggtitle("Boston Crime Predictions")

pl

ggplot(error.df,aes(x=abs(test.r-y.pred)))+geom_bar()
