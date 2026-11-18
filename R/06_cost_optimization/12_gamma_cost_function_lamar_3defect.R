
# 1 inspection
library(zipfR)
library(ggplot2)
library(tictoc)
###########################################Costs###############################

ci=900
cp=20000
cr=50000
##################################################################################
### distribution parameter and failure thresholds##################################
beta=c(3.556,4.57,7.39)
c=c(0.001,0.002,0.002)
b=c(2.45,2.09,2.16)
f5=c(1.5,1.25,1)
yel5=c(1.25,1,7/8)
f4=c(1.75,2,1.25)
yel4=c(1.5,1.75,1.125)
f3=c(2.25,2.25,1.75)
yel3=c(1.75,2,1.625)
f=f4
yel=yel4
info=as.data.frame(cbind(beta,c,b,f,yel))
###############################################################################################

###################################################################################

### #########################double integral#######################################
integralf<-function(beta, c, b, A,weight,n){
  
  InnerFunc <- function(x, y)  { dgamma(x,rate=beta,shape=(n-1)*c*(weight)^b) *  dgamma(y-x,rate=beta,shape=c*(weight)^b)  }
  InnerIntegral <- function(y) { sapply(y, function(z) { integrate(InnerFunc, 0, A, y=z)$value }) }
  k=integrate(InnerIntegral, A, Inf)
  
  return(k$value)
}




setfun<- function(a,b,c){
  
  ans=a+b+c-(a*b)-(b*c)-(a*c)+(a*b*c)
  return(ans)
}




np= function(k){
  sum=0
  for (i in 1:10000) {
    a=integralf(info$beta[1],info$c[1],info$b[1],info$yel[1],k,i)
    b=integralf(info$beta[2],info$c[2],info$b[2],info$yel[2],k,i)
    c=integralf(info$beta[3],info$c[3],info$b[3],info$yel[3],k,i)
    sum_temp=i*setfun(a,b,c)
    sum=sum_temp+sum
  }
  return(sum)
  
}
# num=c(2:200)
# p=lapply(num, integralf,a=200)
# weighted.mean(num, p)
cost4<- function(k=5){

  d=pgamma(info$f[1]-info$yel[1],rate=info$beta[1],shape=info$c[1]*(k)^info$b[1])
  e=pgamma(info$f[2]-info$yel[2],rate=info$beta[2],shape=info$c[2]*(k)^info$b[2])
  f=pgamma(info$f[3]-info$yel[3],rate=info$beta[3],shape=info$c[3]*(k)^info$b[3])
  n=np(k)
  avgcost= (n*ci+ cp+(cr-cp)*(1-(d*e*f)))/(n*k)
  
  return(avgcost)
}
tic()
num<-seq(5,50,1)
costini4<-data.frame(inspection=num,Cost=unlist(lapply(num,cost4)))
toc()

ggplot(costini4,aes(x=inspection,y=Cost, col=Cost))+
  geom_line(size=1)+
  theme_bw()+
  labs(title="Xlevel Class 5",x="Annual Inspection Frequency",y="Average Total Cost")
rn<-which.min(costini4$Cost)
costini4[rn,]
write.csv(costini4,"cost4.csv")

d=read.csv("cost4.csv")
library(tidyr)
d=d[-1]
d=gather(d,Class, Cost,-("inspection"))
colnames(d)[1]<-"MGT"
d$Class=ifelse(d$Class=="Class.4",4,5)
d$Class=as.factor(d$Class)
ggplot(d,aes(x=MGT,y=Cost, fill=Class, group=Class, color=Class))+
  geom_line(size=1)+
  theme_bw()+
  theme(axis.text=element_text(size=12), axis.title = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12))+
  geom_vline(xintercept =c(11,10), linetype="dashed", color=c("red","blue"))+
  geom_hline(yintercept =c(118.98,132.97), linetype="dashed", color=c("red","blue"))+
  xlim(5,25)+
  ylim(80,300)+
  labs(x="MGT",y="Average cost of maintenance per usage unit ($)")+
  ggsave("inspection_interval_current.png")
