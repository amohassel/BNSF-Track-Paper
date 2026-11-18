
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
train<-read.csv(file="./Intermediate_Data/DIP_train.csv", header=TRUE, sep=",")
test<-read.csv(file="./Intermediate_Data/DIP_test.csv", header=TRUE, sep=",")
train<-subset(train,train$TOT_DFLT_MGT>0)
frasdip<-c(3,2.75,2.25,1.75,1.5)
frassurface<-c(3,2.75,2.25,2,1.25)
frasxlevel<-c(3,2,1.75,1.25,1)

z=(log(train$DEF_AMPLTD)-log(train$initial))/train$time_interval
alpha=sum(z)/(length(z)-1)

var<-sum((z-alpha)^2)/(length(z)-2)

mu<-alpha+var/2

test<-subset(test,test$TOT_DFLT_MGT>0)

test$zz<-log(frasdip[test$CLASS])-log(test$initial)-(mu-(var/2))*test$time_interval/sqrt(var*test$time_interval)
test$prob<-pnorm(test$zz)



test$predict<-test$initial*exp((mu-var/2)*test$time_interval)+sqrt(var*test$time_interval)
