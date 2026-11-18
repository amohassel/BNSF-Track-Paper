library(rjags)
library(lubridate)
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90", "Intermediate_Data"))
defect=read.csv("DIP_train.csv")
defect=defect[,-c(1,2)]
defect$TSC_CD=ifelse(defect$TSC_CD=="C",1,0)
defect$TSC_CD=as.factor(defect$TSC_CD)
defect$TEST_DT=ymd(defect$TEST_DT)
defect$LINE_SEG_NBR=factor(defect$LINE_SEG_NBR,levels = c(1:4))
defect=subset(defect,defect$initial>0)
defect=subset(defect,defect$CLASS==5)


#define model

rail_model_1<-"model{
## likelihood for amplitude[i]
for ( i in 1:length(Y)){

Y[i]~dnorm(m[i],s^(-2))
m[i]=a+b*X[i]+c*Z[i]

}

#prior knowledge

a~dnorm(1,0.01^(-2))
b~dnorm(0.1,0.01^(-2))
c~dnorm(0.1,0.01^(-2))
s~dunif(0.1,0.2)

}"

rail_model_1_rjags<-jags.model(textConnection(rail_model_1), data=list(X=defect$weight, Z=defect$DEF_LGTH, Y=defect$growth), inits = list( .RNG.name="base::Wichmann-Hill", .RNG.seed=2018))
rail_model_1_sim<-coda.samples(model=rail_model_1_rjags, variable.names = c("a","b","c","s"), n.iter = 10000)
plot(rail_model_1_sim)
defect_chain<-data.frame(rail_model_1_sim[[1]], iter=1:10000)
k=summary(rail_model_1_sim)
defect$bayse<-k$statistics[1]+k$statistics[2]*defect$TOT_DFLT_MGT+k$statistics[3]*defect$DEF_LGTH

defect$bayse_res<-(defect$DEF_AMPLTD-defect$bayse)
(defect$bayse_mse<-mean((defect$DEF_AMPLTD-defect$bayse)^2))

ssr<-sum((defect$bayse-mean(defect$DEF_AMPLTD))^2)
sse<-sum((defect$bayse-defect$DEF_AMPLTD)^2)
(R<-1-(sse/(ssr+sse)))




#=====================================================================================================================================
#consider class
#define model
defect=read.csv("DIP_train.csv")
defect=defect[,-c(1,2)]
defect$TSC_CD=ifelse(defect$TSC_CD=="C",1,0)
defect$TSC_CD=as.factor(defect$TSC_CD)
defect$TEST_DT=ymd(defect$TEST_DT)
defect$LINE_SEG_NBR=factor(defect$LINE_SEG_NBR,levels = c(1:4))
defect=subset(defect,defect$initial>0)
defect=subset(defect,defect$CLASS>3)


rail_model_2<-"model{
## likelihood for amplitude[i]
for ( i in 1:length(Y)){

Y[i]~dnorm(m[i],s^(-2))
m[i]<-a+d[P[i]]+b*X[i]+c*Z[i]

}

#prior knowledge
d[4]~dnorm(-0.2,0.1^(-2))
d[5]<-0
a~dnorm(1,0.01^(-2))
b~dnorm(0.1,0.01^(-2))
c~dnorm(0.1,0.01^(-2))
s~dnorm(0.1,0.05^(-2))

}"

rail_model_2_rjags<-jags.model(textConnection(rail_model_2), data=list(X=defect$TOT_DFLT_MGT, P=defect$CLASS, Z=defect$DEF_LGTH, Y=defect$DEF_AMPLTD), inits = list( .RNG.name="base::Wichmann-Hill", .RNG.seed=200))
rail_model_2_sim<-coda.samples(model=rail_model_2_rjags, variable.names = c("a","b","c","s","d"), n.iter = 10000)
plot(rail_model_2_sim)
k=summary(rail_model_2_sim)
defect_chain<-data.frame(rail_model_2_sim[[1]], iter=1:10000)

#mse
defect$CLASS= ifelse(defect$CLASS==4,1,0)
defect$bayse<-k$statistics[1]+k$statistics[4]*defect$CLASS+k$statistics[2]*defect$TOT_DFLT_MGT+k$statistics[3]*defect$DEF_LGTH
defect$bayse_res<-(defect$DEF_AMPLTD-defect$bayse)
defect$bayse_se<-(defect$DEF_AMPLTD-defect$bayse)^2
(mse<-mean(defect$bayse_se))
sum(defect$bayse_se)
ssr<-sum((defect$bayse-mean(defect$DEF_AMPLTD))^2)
sse<-sum((defect$bayse-defect$DEF_AMPLTD)^2)
(R<-1-(sse/(ssr+sse)))
qqnorm(defect$bayse_res)
qqline(defect$bayse_res)


# test 
test=read.csv("DIP_test.csv")
test=subset(test,test$initial>0)
test=subset(test,test$CLASS>3)
test$CLASS= ifelse(test$CLASS==4,1,0)
test$bayse<-k$statistics[1]+k$statistics[4]*test$CLASS+k$statistics[2]*test$TOT_DFLT_MGT+k$statistics[3]*test$DEF_LGTH
test$bayse_res<-(test$DEF_AMPLTD-test$bayse)
test$bayse_mse<-(test$DEF_AMPLTD-test$bayse)^2
(mse<-mean(test$bayse_mse))


(sse<-sum((test$bayse-test$DEF_AMPLTD)^2))
ssr<-sum((test$bayse-mean(test$DEF_AMPLTD))^2)






# prepering test set
test=read.csv("DIP_test.csv")
test=test[,-c(1,2)]
test$TSC_CD=ifelse(test$TSC_CD=="C",1,0)
test$TSC_CD=as.factor(test$TSC_CD)
test$TEST_DT=ymd(test$TEST_DT)
test$LINE_SEG_NBR=factor(test$LINE_SEG_NBR,levels = c(1:4))
test=subset(test,test$initial>0)
test=subset(test,test$CLASS>3)
test$CLASS = factor(test$CLASS, levels = 4:5, c("Class 4", "Class 5"))
test$CLASS = relevel(test$CLASS, ref ="Class 5")

test$predict=predict(model.3,test)
test$mse<-(test$DEF_AMPLTD-test$predict)^2
(msetest<-mean(test$mse))
