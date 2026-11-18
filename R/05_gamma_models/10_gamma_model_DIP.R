library(stats4)
library(dplyr)
library(pracma)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
train<-read.csv(file="./Intermediate_Data/DIP_train.csv", header=TRUE, sep=",")
test<-read.csv(file="./Intermediate_Data/DIP_test.csv", header=TRUE, sep=",")

test <- subset(test, test$cum_time > 0)
test$color<-ifelse(test$DEF_PRTY=="RED",1,0)
train <- subset(train, train$cum_time > 0)
train$color<-ifelse(train$DEF_PRTY=="RED",1,0)
train$startweight<-train$TOT_DFLT_MGT-train$weight
test$startweight<-test$TOT_DFLT_MGT-test$weight
train<-rbind(test,train)
#### calculating IQR
iqr<-summary(growth)
X<-as.numeric(iqr)
up<-X[5]+1.5*(X[5]-X[2])
down<-X[2]-1.5*(X[5]-X[2])

train<-rbind(train,test)
train<-subset(train,growth>0)
train<-subset(train,startweight!=TOT_DFLT_MGT)
attach(train)
a<-c(2.87,0.006,1.4)

liklihoodfunction<- function(a){
  beta=a[1]
  alpha=a[2]
  b=a[3]
  
  likeli<-0
  for (i in 1:nrow(train)){
    
    x<- sum((alpha*(TOT_DFLT_MGT[i]^b-startweight[i]^b)-1)* log(growth[i]))+sum((alpha*(TOT_DFLT_MGT[i]^b-startweight[i]^b))*log(beta))-sum(beta*growth[i])-sum(log(gamma(alpha*(TOT_DFLT_MGT[i]^b-startweight[i]^b))))
  
  likeli= x +likeli
  }
  
  tikhonov=0.1*beta^2+60000000*alpha^2+5*b^2
  return(-likeli+tikhonov)
}

result<-optim(a,liklihoodfunction,method="Nelder-Mead")
(AIC=-2*result$value+2*length(result$par))

(a<-result$par)
beta=result$par[1]
alpha=result$par[2]
b=result$par[3]
summary(result)

k=alpha*((train$TOT_DFLT_MGT^b)-train$startweight^b)/beta



library(ggplot2)
observation<-data.frame(growth=train$growth)
observation$Type<-'Observation'
prediction<-data.frame(growth=k)
prediction$Type<-'Predicted'

data<-rbind(prediction,observation)
ggplot(data,aes(growth,fill=Type))+geom_density(alpha=0.2)+
  xlab("Growth")+
  ylab("Density")+
  theme(text=element_text(size=12))+
  theme_bw()+
  ggsave(filename = "./Output/DIP_gamma_pdf.png")

ggplot(data,aes(growth,fill=Type))+geom_histogram(alpha=0.5, aes(y=..density..),position='identity')+
  xlab("Growth")+
  ylab("Density")+
  theme(text=element_text(size=12))+
  theme_bw()+
  ggsave(filename = "./Output/DIP_gamma_hist.png")




ks.test(prediction$growth,observation$growth, "pgamma", exact = FALSE)
(chisq.test(observation$growth,prediction$growth))
res<-observation$growth-prediction$growth
(msrq<-sqrt(mean(sum((res)^2))))

shape<-alpha*(TOT_DFLT_MGT^b-startweight^b)
(mean(shape))




real<-hist(train$growth,main="real", breaks=51)
predict<-hist(k,breaks=c(real$breaks),main="predict")
aa<-cumsum(real$counts)/sum(real$counts)
pp<-cumsum(predict$counts)/sum(predict$counts)
(D<-max(abs(pp-aa)))
critical<-0.16982
ifelse(D>critical,print("reject"),print("don't reject"))

png('output/DIP_gamma_cum.png')
plot(aa,type="l",ylab="CDF", xlab="X")
lines(pp,col="blue")
legend("topleft", legend=c("Practical", "Theorical"),
       col=c("black", "blue"), lty=1:1, cex=0.8)
dev.off()

DIP=a


# c(0.85,109000,2)
# 
# c(4,100000000,1) 90
