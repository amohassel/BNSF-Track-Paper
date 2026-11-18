
# 1 inspection
library(zipfR)
library(ggplot2)
A=0.875
l=1
ci=900
cp=20000
cupm=40000
cr=50000
beta=3.556
c=0.001
b=2.45
weight=80
lambda=0.0069*9.37


pf<-function (ther,a,n=1,x=TRUE){
  
  yl<-pgamma(ther,rate=beta,shape=n*c*(lambda*a)^b,lower.tail =x)  
  return(yl)
}
 

cost1<- function(a){
 
  avgcost= (cp+ci*(A/((c*(lambda*a)^b)/beta))+(cr-cp)*(1-pf(l-A,a)))/(((A/((c*(lambda*a)^b)/beta))+1)*a)
  
  return(avgcost)
}



num<-seq(70,200,10)
costini1<-data.frame(inspection=num,Cost=unlist(lapply(num,cost1)))


ggplot(costini1,aes(x=factor(inspection),y=Cost, col=Cost))+
  geom_point()+
  theme_bw()+
  labs(title="Xlevel Class 5",x="Annual Inspection Frequency",y="Average Total Cost")
rn<-which.min(costini1$Cost)
costini1[rn,]

## approach 2


cost2<- function(a){
  
  avgcost= round(weight/a)*ci+ cp* ((pf(l,a,n=round(weight/a))-pf(A,a,n=round(weight/a)))/pf(A,a,n=(round(weight/a))-1))+ cr* (pf(l,a,n=round(weight/a),x=FALSE)/pf(A,a,n=(round(weight/a))-1))
  
  return(avgcost)
}


num<-seq(1,360,10)
costini2<-data.frame(inspection=num,Cost=unlist(lapply(num,cost2)))


ggplot(costini2,aes(x=factor(inspection),y=Cost, col=Cost))+
  geom_point()+
  theme_bw()+
  labs(title="Xlevel Class 5",x="Annual Inspection Frequency",y="Average Total Cost")
rn<-which.min(costini2$Cost)
costini2[rn,]

## approach 3

n_failure<- function (lim,a){
  np=0
for (i in 1:100000) {
  
  np_temp=pf(lim,a,weight,n=i)-pf(lim,a,weight,n=i+1)
  np=np=np_temp*i+np
  return(np)
  
}
}
cost3<- function(a){
 
  
  avgcost= (round(weight/a)*ci+ cp*n_failure(A,a) + cr*n_failure(l,a))
  
  return(avgcost)
}

num<-seq(1,40000,1)
costini3<-data.frame(inspection=num,Cost=unlist(lapply(num,cost3)))


ggplot(costini3,aes(x=factor(inspection),y=Cost, col=Cost))+
  geom_point()+
  theme_bw()+
  labs(title="Xlevel Class 5",x="Annual Inspection Frequency",y="Average Total Cost")
rn<-which.min(costini3$Cost)
costini3[rn,]

## conv gamma
a=80
n=100
library(cubature) 
f <- function(x) { dgamma(x[1],rate=beta,shape=(n-1)*c*(lambda*a)^b) *  dgamma(x[2]-x[1],rate=beta,shape=c*(lambda*a)^b)  }
adaptIntegrate(f, lowerLimit = c(A,0), upperLimit = c(Inf, A))



### double integral
integralf<-function(day,n){

InnerFunc <- function(x, y)  { dgamma(x,rate=beta,shape=(n-1)*c*(lambda*day)^b) *  dgamma(y-x,rate=beta,shape=c*(lambda*day)^b)  }
InnerIntegral <- function(y) { sapply(y, function(z) { integrate(InnerFunc, 0, A, y=z)$value }) }
k=integrate(InnerIntegral, A, Inf)

return(k$value)
}

np= function(ins){
  sum=0
  k=ceiling(365*15/ins)
  for (i in 2:k) {
    sum_temp=i* integralf(ins,i)
    sum=sum_temp+sum
  }
  return(sum)
  
}
# num=c(2:200)
# p=lapply(num, integralf,a=200)
# weighted.mean(num, p)
cost4<- function(inp){
  
  
  avgcost= (np(inp)*ci+ cp+(cr-cp)*pgamma(l-A,rate=beta,shape=c*(lambda*inp)^b))/(np(inp)*inp)
  
  return(avgcost)
}

num<-seq(70,200,10)
costini4<-data.frame(inspection=num,Cost=unlist(lapply(num,cost4)))


ggplot(costini4,aes(x=factor(inspection),y=Cost, col=Cost))+
  geom_point()+
  theme_bw()+
  labs(title="Xlevel Class 5",x="Annual Inspection Frequency",y="Average Total Cost")
rn<-which.min(costini4$Cost)
costini4[rn,]


glm(y~x) %>%
tidy()

