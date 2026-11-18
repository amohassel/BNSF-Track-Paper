library(stats4)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(pracma)
library(BaTFLED3D)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
x<-c(1:200)


hazard<- function(x=100,beta=1.215,weight=20,defectlength=15,initial_amp=1,class=0){
 
  a0=18.484
  a1=-0.051
  a2=-0.019
  a3=-9.54  
  a4=2.289
  

  haz<-( beta/exp(a0+a1*weight+a2*defectlength+a3*initial_amp+a4*class))*(x/exp(a0+a1*weight+a2*defectlength+a3*initial_amp+a4*class))^(beta-1)
  
  return(haz)
}


y<-cbind(day=x,hazard(x,initial_amp=1,class=0),initial_amp=1,class=0)
y1<-cbind(day=x,hazard(x,initial_amp=1.1,class=0),initial_amp=1.1,class=0)
y2<-cbind(day=x,hazard(x,initial_amp=1.2,class=0),initial_amp=1.2,class=0)
y3<-cbind(day=x,hazard(x,initial_amp=1.3,class=0),initial_amp=1.3,class=0)
y4<-cbind(day=x,hazard(x,initial_amp=1.4,class=0),initial_amp=1.4,class=0)

y5<-cbind(day=x,hazard(x,initial_amp=1,class=1),initial_amp=1,class=1)
y6<-cbind(day=x,hazard(x,initial_amp=1.1,class=1),initial_amp=1.1,class=1)
y7<-cbind(day=x,hazard(x,initial_amp=1.2,class=1),initial_amp=1.2,class=1)
y8<-cbind(day=x,hazard(x,initial_amp=1.3,class=1),initial_amp=1.3,class=1)
y9<-cbind(day=x,hazard(x,initial_amp=1.4,class=1),initial_amp=1.4,class=1)

mat<-as.data.frame(rbind(y,y1,y2,y3,y4,y5,y6,y7,y8,y9))

mat$initial_amp<-as.factor(mat$initial_amp)
mat$class<-ifelse(mat$class==0,5,4)
mat$class = factor(mat$class, levels = 4:5, c("Class 4", "Class 5"))
colnames(mat)[3]<-"Initial Amplitude"

ggplot(mat,aes(x=day,y=V2,color=`Initial Amplitude`))+
  geom_line(size=0.8)+
  facet_grid(~class)+
  theme_bw()+
   theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Time(day)",y="Hazard rate")+
  ggsave("./Output/hazard initial amplitude analysis.png", width = 8, height = 6)
  
x<-c()

x<-c(1:100)
w<-cbind(day=x,hazard(x,weight=0.1*x,class=0),Daily=0.1,class=0)
w1<-cbind(day=x,hazard(x,weight=0.2*x,class=0),Daily=0.2,class=0)
w2<-cbind(day=x,hazard(x,weight=0.3*x,class=0),Daily=0.3,class=0)
w3<-cbind(day=x,hazard(x,weight=0.4*x,class=0),Daily=0.4,class=0)


w4<-cbind(day=x,hazard(x,weight=0.1*x,class=1),Daily=0.1,class=1)
w5<-cbind(day=x,hazard(x,weight=0.2*x,class=1),Daily=0.2,class=1)
w6<-cbind(day=x,hazard(x,weight=0.3*x,class=1),Daily=0.3,class=1)
w7<-cbind(day=x,hazard(x,weight=0.4*x,class=1),Daily=0.4,class=1)

matw<-as.data.frame(rbind(w,w1,w2,w3,w4,w5,w6,w7))
matw$Daily<-as.factor(matw$Daily)
matw$class<-ifelse(matw$class==0,5,4)
matw$class = factor(matw$class, levels = 4:5, c("Class 4", "Class 5"))
colnames(matw)[3]<-"Daily tonnage"

ggplot(matw,aes(x=day,y=V2,color=`Daily tonnage`))+
  geom_line(size=1.2)+
  facet_grid(~class)+
  theme_bw()+
  labs(x="Time(Day)",y="Hazard rate")+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
 ggsave("./Output/hazard weight analysis.png", width = 8, height = 6)


#################################pDF CDF
n=600
Weight=seq(25,35,5)
data<-data.frame(day=vector(mode = "numeric",length = length(weight)*n))
data$day=rep(c(1:n),length(weight))
data$weight=rep(weight,each=n)
data$defectlength=20
data$initial_amp=1.25
data$class=0
attach(data)
data$scale<-exp(a0+a1*data$weight+a2*data$defectlength+a3*data$initial_amp+a4*data$class)
data$weight<-as.factor(data$weight)

data$cdf<-(pweibull(data$day,scale=data$scale ,shape=1.25))
data$pdf<-(qweibull(data$day,scale=data$scale ,shape=1.25))

ggplot(data,aes(x=day,y=cdf, color=weight))+
  geom_line(size=1)+
  theme_bw()+
  labs(x="Time(Day)",y="Cumulative Distribution Function")+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  ggsave("./Output/cdfweibull.png",width=8, height = 6)

