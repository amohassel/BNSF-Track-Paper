
# 1 inspection
library(zipfR)
library(ggplot2)
###########################################Costs###############################

ci=900
cp=20000
cupm=40000
cr=50000
##################################################################################
### distribution parameter and failure thresholds##################################
beta=c(3.556,4.57,7.39)
c=c(0.001,0.002,0.002)
b=c(2.45,2.09,2.16)
f=c(1.5,1.25,1)
yel=c(1.25,1,7/8)
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
    a=integralf(info$beta[1],info$c[1],info$b[1],info$f[1],k,i)
    b=integralf(info$beta[2],info$c[2],info$b[2],info$f[2],k,i)
    c=integralf(info$beta[3],info$c[3],info$b[3],info$f[3],k,i)
    sum_temp= setfun(a,b,c)
    sum=sum_temp+sum
  }
  return(sum)
  
}
# num=c(2:200)
# p=lapply(num, integralf,a=200)
# weighted.mean(num, p)
cost4<- function(k=5){
  
a=pgamma(info$f[1]-info$yel[1],rate=info$beta[1],shape=info$c[1]*(k)^info$b[1])
b=pgamma(info$f[2]-info$yel[2],rate=info$beta[2],shape=info$c[2]*(k)^info$b[2])
c=pgamma(info$f[3]-info$yel[3],rate=info$beta[3],shape=info$c[3]*(k)^info$b[3])
  
  avgcost= (np(inp)*ci+ cp+(cr-cp)*setfun(a,b,c)/(np(inp)*k))
  
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

