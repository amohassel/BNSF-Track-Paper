# investigate inspection data

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stargazer)

# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(data_dir)
# Read CSV into R
inspection<- read.csv(file="./Input/Inspection_Run_Header_Modified.csv", header=TRUE, sep=",")
defectdip<- read.csv(file="./Intermediate_Data/dip_with_tonnage__without_removing.csv", header=TRUE, sep=",")
defectsurf<- read.csv(file="./Intermediate_Data/surface_with_tonnage__without_removing.csv", header=TRUE, sep=",")
defectx<- read.csv(file="./Intermediate_Data/xlevel_with_tonnage__without_removing.csv", header=TRUE, sep=",")
defectx$TSC_CD<-as.factor(defectx$TSC_CD)
defect<-rbind(defectdip,defectsurf)
defect<-rbind(defect,defectx)
tonnage<- read.csv(file="./Input/TONNAGE_SAMPLE_DATA _Header_Modified.csv", header=TRUE, sep=",")
# tonnage$day<-01
# tonnage=unite(tonnage,date,YEAR,MONTH,day,sep="-")
# tonnage$date=ymd(tonnage$date)
#tonnage$date=tonnage$date+30
inspection<-inspection[,-1]
inspection$TEST_DT<-dmy(inspection$TEST_DT)
inspection$month<-month(inspection$TEST_DT)
inspection$year<-year(inspection$TEST_DT)
inspection$quarter<-quarter(inspection$TEST_DT)

defect$TEST_DT<-ymd(defect$TEST_DT)
defect$month<-month(defect$TEST_DT)
defect$year<-year(defect$TEST_DT)
defect$quarter<-quarter(defect$TEST_DT)
tonnage$quarter<-quarter(tonnage$MONTH)

summary(inspection)
## number of inspections in each segment and line in each year

line1_inspection=inspection %>%
  filter(LINE_SEG_NBR==1) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())


line2_inspection=inspection %>%
  filter(LINE_SEG_NBR==2) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())

line3_inspection=inspection %>%
  filter(LINE_SEG_NBR==3) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())

line4_inspection=inspection %>%
  filter(LINE_SEG_NBR==4) %>%
  group_by(TRACK_SDTK_NBR, year) %>%
  summarize(number_of_inspection=n())

totali<-bind_rows(line1=line1_inspection,line2=line2_inspection,line3=line3_inspection,line4=line4_inspection,.id="name")

# total passing weight from each segment and line in each year

weightline1<-tonnage %>%
  filter(LINE_SEG_NBR==1) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

weightline2<-tonnage %>%
  filter(LINE_SEG_NBR==2) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

weightline3<-tonnage %>%
  filter(LINE_SEG_NBR==3) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

weightline4<-tonnage %>%
  filter(LINE_SEG_NBR==4) %>%
  group_by(TRACK_SDTK_NBR,YEAR) %>%
  summarize(weight=sum(TOT_DFLT_MGT))

totalw<-bind_rows(line1=weightline1,line2=weightline2,line3=weightline3,line4=weightline4,.id="name")
colnames(totalw)[3]<-"year"

## defect type and iteration

defecetline1<-defect %>%
  filter(LINE_SEG_NBR==1) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())

defecetline2<-defect %>%
  filter(LINE_SEG_NBR==2) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())

defecetline3<-defect %>%
  filter(LINE_SEG_NBR==3) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())

defecetline4<-defect %>%
  filter(LINE_SEG_NBR==4) %>%
  group_by(TRACK_SDTK_NBR,DEF_PRTY,year) %>%
  summarize(number_of_defect=n())
## joinig tables togather
totald=bind_rows(line1=defecetline1,line2=defecetline2,line3=defecetline3,line4=defecetline4,.id="name")
total1=left_join(totald,totalw,by=c("name","TRACK_SDTK_NBR","year"))
total<-left_join(total1, totali,by=c("name","TRACK_SDTK_NBR","year"))
total=total[complete.cases(total),]
total<-arrange(total,name,TRACK_SDTK_NBR,year)
total<-as.data.frame(total)

ggplot(total,aes(x=weight,y=number_of_inspection))+
  geom_point()+
  facet_grid(.~name+TRACK_SDTK_NBR)

ggplot(total,aes(x=TRACK_SDTK_NBR,y=number_of_inspection,size=weight))+
  geom_point()+
  facet_grid(.~factor(name)+year)

model<-lm(number_of_inspection~weight,data=total)
summary(model)

write.csv(total, file="./Intermediate_Data/total_inspection.csv")

stargazer(total, type = "text", title="Descriptive statistics", digits=1, out="./Output/inspection_stat.tex")
stargazer(total, type = "latex", title="Descriptive statistics", digits=1, out="./Output/inspection_stat.tex")


#################inspection of every one mile

# find the number of inspection in every defect
for(i in 1:nrow(defect)){
  temp<-(which(inspection$MILEPOST_START<=defect$MILEPOST[i] & inspection$MILEPOST_END>=defect$MILEPOST[i] & inspection$LINE_SEG_NBR==defect$LINE_SEG_NBR[i] & inspection$TRACK_SDTK_NBR==defect$TRACK_SDTK_NBR[i] & inspection$year==defect$year[i] & inspection$month==defect$month[i]))
  defect$inspection[i]<-length(unique(inspection$TEST_DT[temp]))
}



write.csv(defect, file="./Intermediate_Data/defcet_inspection_output_05_01.csv")

# linear model for inspection
model.ins<-lm(inspection~(weight),data=defect)
summary(model.ins)  
stargazer(model.ins, dep.var.labels=c("Inspection") 
          
          , title="Inspection Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/inspection_result01.tex")
################################################################################################################################
#Run This Part##########################################################

ab<-read.csv(file="./Intermediate_Data/defcet_inspection_output_05_01.csv", header=TRUE, sep=",")
model.ins<-lm(inspection~(weight),data=ab)
summary(model.ins) 
#combine year and quarter
ab$time<-paste(ab$year,ab$quarter,sep="_")
ab$time<-yq(ab$time)
#remove zero from data frame 
ab<-subset(ab,ab$weight>0)
ggplot(ab,aes(x= weight,y=inspection,col=factor(LINE_SEG_NBR)))+
  geom_jitter()+
  facet_grid(~year)+
  geom_smooth(method=lm)+
  ggsave("./Output/ton_ins_segment01.png")

ggplot(ab,aes(x=weight,y=inspection))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Tonnage",y="Number of inspection")+
  ggsave("./Output/ton_ins01.png")

ggplot(ab,aes(x=time,y=inspection,size=weight))+
  geom_jitter(alpha=0.2)+
  facet_grid(~LINE_SEG_NBR)+
  ggsave("./Output/time_ins_ton01.png")


data<-ab%>%
  group_by(LINE_SEG_NBR,time)%>%
  summarize(avg_ins=mean(inspection), avg_ton=mean(weight),avg_yel=mean(DEF_PRTY=="YEL"),avg_red=mean(DEF_PRTY=="RED"))

ggplot(data,aes(x=time,y=avg_ton,size=avg_ins))+
  geom_jitter()+
  facet_grid(.~LINE_SEG_NBR)+
  ggsave("./Output/avg_ton_ins_segment01.png")

# clrs = c("avg_ins"="black","avg_ton"="blue","avg_yel"="yellow","avg_red"="red") 
# ggplot(data = data, aes(x = time)) + 
#   geom_jitter(aes(y = avg_ins,fill ="Inspection"),colour="black" ,shape=1) + 
#   geom_jitter(aes(y = avg_ton), colour="blue",shape=3)+
#   geom_jitter(aes(y = avg_yel), colour="yellow",shape=6)+
#   geom_jitter(aes(y = avg_red), colour="red",shape=8)+
#   facet_grid(.~segment)+
# labs(y ="")+
#   ggsave("./Output/avg_time_ins_ton_yel_red.png")


kk<-gather(data,var,value,-c(time,LINE_SEG_NBR))

ggplot(data = kk, aes(x = time)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~LINE_SEG_NBR)+
  labs(y ="")+
  ggsave("./Output/avg_time_ins_ton_yel_red_legend_fixed01.png")


#### cumulative number of inspection and weight 


ab=arrange(ab,LINE_SEG_NBR,TRACK_SDTK_NBR,MILEPOST,year,quarter)
ab$yellow<-ifelse(ab$DEF_PRTY=="YEL",1,0)
ab$red<-ifelse(ab$DEF_PRTY=="RED",1,0)
for (i in 2:nrow(ab)){
  ab$cum_tonnage[1]<-ab$weight[1]
  ab$cum_ins[1]<-ab$inspection[1]
  ab$cum_yellow[1]<-ab$yellow[1]
  ab$cum_red[1]<-ab$red[1]
  if ( ab$LINE_SEG_NBR[i]==ab$LINE_SEG_NBR[i-1] && ab$TRACK_SDTK_NBR[i]==ab$TRACK_SDTK_NBR[i-1] && ab$MILEPOST[i]==ab$MILEPOST[i-1] ){
    ab$cum_tonnage[i]=ab$weight[i]+ab$cum_tonnage[i-1]
    ab$cum_ins[i]=ab$inspection[i]+ab$cum_ins[i-1]
    ab$cum_yellow[i]<-ab$yellow[i]+ab$cum_yellow[i-1]
    ab$cum_red[i]<-ab$red[i]+ab$cum_red[i-1]
  } else{
    
    ab$cum_tonnage[i]=ab$weight[i]
    ab$cum_ins[i]=ab$inspection[i]
    ab$cum_yellow[i]<-ab$yellow[i]
    ab$cum_red[i]<-ab$red[i]
  }
}

data_cum<-ab[,c(3,4,5,36,39,40,41,42)]
data_cum<-gather(data_cum,var,value,-c(MILEPOST,time,LINE_SEG_NBR,TRACK_SDTK_NBR))

ggplot(data = data_cum, aes(x = time)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~LINE_SEG_NBR)+
  labs(y ="")+
  ggsave("./Output/cum_time_ins_ton_yel_red_segment01.png")

ggplot(data = data_cum, aes(x = time)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~LINE_SEG_NBR+TRACK_SDTK_NBR)+
  labs(y ="")+
  ggsave("./Output/cum_time_ins_ton_yel_red_segment_line01.png")

ggplot(ab,aes(x=cum_tonnage,y=cum_ins))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative number of inspection")+
  ggsave("./Output/cum_ton_ins01.png")

model.cumins<-lm(cum_ins~(cum_tonnage),data=ab)
summary(model.cumins)  
stargazer(model.cumins, dep.var.labels=c("Inspection") 
          
          , title="Cumulative Inspection Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/cum_inspection_result01.tex")

sum<-ab%>%
  group_by(MILEPOST,LINE_SEG_NBR,TRACK_SDTK_NBR)%>%
  summarize(suminsp=sum(inspection),sumtonnage=sum(weight),sumyellow=sum(yellow),sumred=sum(red))
sum$peryellow=sum$sumyellow/sum$suminsp
sum$perred=sum$sumred/sum$suminsp

ggplot(sum,aes(x=sumtonnage,y=suminsp))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative number of inspection")+
  ggsave("./Output/sum_ton_ins01.png")

ggplot(sum,aes(x=sumtonnage,y=sumred))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative red tag")+
  ggsave("./Output/sum_numred_weight01.png")

ggplot(sum,aes(x=sumtonnage,y=sumyellow))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative yellow tag")+
  ggsave("./Output/sum_numyellow_weight01.png")

ggplot(sum,aes(x=sumtonnage,y=perred))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative percentage of red tag")+
  ggsave("./Output/sum_perred_weight01.png")

ggplot(sum,aes(x=sumtonnage,y=peryellow))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative tonnage",y="Cumulative percentage of yellow tag")+
  ggsave("./Output/sum_perred_weight01.png")

ggplot(sum,aes(x=suminsp,y=sumyellow))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative inspection",y="Cumulative number of yellow tag")+
  ggsave("./Output/sum_numyellow_inspection01.png")

ggplot(sum,aes(x=suminsp,y=sumred))+
  geom_jitter()+
  geom_smooth(method=lm)+
  labs(x="Cumulative inspection",y="Cumulative number of red tag")+
  ggsave("./Output/sum_numred_inspection01.png")






model.sumins<-lm(suminsp~(sumtonnage),data=sum)
summary(model.sumins)  
stargazer(model.sumins, dep.var.labels=c("Inspection") 
          
          , title="summation Inspection Regression Results", ci=FALSE, ci.level=0.90, single.row=TRUE,out="./Output/sum_inspection_result01.tex")


sum<-sum[,-c(8,9)]
aa<-gather(sum,var,value,-c(MILEPOST,TRACK_SDTK_NBR,LINE_SEG_NBR))

ggplot(data = aa, aes(x = MILEPOST)) + 
  geom_jitter(aes(y = value,col=var,shape=var) ) + 
  facet_grid(.~LINE_SEG_NBR+TRACK_SDTK_NBR)+
  labs(y ="")+
  ggsave("./Output/sum_time_ins_ton_yel_red_segment_line01.png")

