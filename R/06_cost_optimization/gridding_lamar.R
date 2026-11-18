# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90", "Output"))
# 1 inspection
library(zipfR)
library(tictoc)
library(foreach)
library(doParallel)
###########################################Costs###############################

ci=900
cp=20000
cr=50000
##################################################################################
### distribution parameter and failure thresholds##################################
beta=c(3.556,4.57,7.39)
c=c(0.001,0.002,0.002)
b=c(2.45,2.09,2.16)
f=c(1.5,1.25,1)
#modify class to f for consideration
f4=c(1.75,2,1.25)

f3=c(2.25,2.25,1.75)

info=as.data.frame(cbind(beta,c,b,f))
###############################################################################################
ld=1
ls=1
lx=seq(0.8,1,0.1)
num<-25
gr=expand.grid(num,ld,ls,lx)
colnames(gr)<-c("k","ld","ls","lx")

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




np= function(k,ld,ls,lx){
  sum=0
  for (i in 1:10000) {
    a=integralf(info$beta[1],info$c[1],info$b[1],ld,k,i)
    b=integralf(info$beta[2],info$c[2],info$b[2],ls,k,i)
    c=integralf(info$beta[3],info$c[3],info$b[3],lx,k,i)
    sum_temp= i*setfun(a,b,c)
    sum=sum_temp+sum
  }
  return(sum)
  
}

cost4<- function(k,ld,ls,lx){

  
  d=pgamma(info$f[1]-ld,rate=info$beta[1],shape=info$c[1]*(k)^info$b[1])
  e=pgamma(info$f[2]-ls,rate=info$beta[2],shape=info$c[2]*(k)^info$b[2])
  f=pgamma(info$f[3]-lx,rate=info$beta[3],shape=info$c[3]*(k)^info$b[3])
  n=np(k,ld,ls,lx)
  avgcost= (n*ci+ cp+(cr-cp)*(1-setfun(d,e,f)))/(n*k)
  
  return(avgcost)
}
####################################################
cores=detectCores()
cl <- makeCluster(cores[1])
registerDoParallel(cl)
time<-system.time(
  finalMatrix <- foreach(i=1:nrow(gr),.combine=rbind) %dopar% {
    tempMatrix = cost4(gr$k[i],gr$ld[i],gr$ls[i],gr$lx[i])
    tempMatrix=unlist(append(gr[i,],tempMatrix))
  }
)
stopCluster(cl)
costini4<-as.data.frame(finalMatrix)
write.csv(costini4,"costini5.csv")
##################################################################
f<-function(k,ld,ls,lx){
  
  d=pgamma(info$f[1]-ld,rate=info$beta[1],shape=info$c[1]*(k)^info$b[1])
  e=pgamma(info$f[2]-ls,rate=info$beta[2],shape=info$c[2]*(k)^info$b[2])
  f=pgamma(info$f[3]-lx,rate=info$beta[3],shape=info$c[3]*(k)^info$b[3])
  
  k=1-setfun(d,e,f)
return(k)}

