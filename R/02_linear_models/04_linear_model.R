library(ggplot2)
library(lubridate)

library(stargazer)
# Set directory
# data_dir = the data root folder (holds Input/ and 90/); set BNSF_DIR or edit here
data_dir <- Sys.getenv("BNSF_DIR", ".")
setwd(file.path(data_dir, "90"))
# Read CSV into R

####### surface
defect<- read.csv(file="./Intermediate_Data/surface_with_tonnage.csv", header=TRUE, sep=",")
defect<-defect[,-1]

var<-defect[,-c(4,5,6,7,10,14)]
cor(var)

#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)

n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]

defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))


ggplot(defect,aes(x=TOT_DFLT_MGT,y=DEF_AMPLTD))+
  geom_point()+
  facet_grid(.~ CLASS)+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(1,2.5))+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Cumulative weight" , y="Defect Amplitude")+
  ggsave("./Output/surface_linear_model_plot.png")
  
defect$CLASS = relevel(defect$CLASS, ref ="Class 5")

# defect size
model.1<-lm(DEF_AMPLTD~factor(CLASS)+cum_time,defect)
summary(model.1)
model.2<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT,defect)
summary(model.2)
model.3<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH,defect)
summary(model.3)
model.4<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(LINE_SEG_NBR),defect)
summary(model.4)
model.5<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(TRACK_SDTK_NBR)+TEST_PSPD,defect)
summary(model.5)
model.7<-lm((DEF_AMPLTD)~factor(CLASS)+cum_time+DEF_LGTH,defect)
summary(model.7)




stargazer(model.1,model.2,model.3,model.4,model.5,model.7, dep.var.labels=c("Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude"),notes = "\\parbox[t]{\\textwidth}{Different linear regressions were preformed to find the relationship between different variables and surface defect amplitude, also as mentioned before mileposts with decreasing degradation trend were ignored.} ", notes.align = "l"        
          
          , title="Regression Results for surface defect in 90 days interval",covariate.labels = c("Class 4","Cumulative time","Cumulative weight", "Defect length","Segment 2", "Segment 3","Line 1", "Line 2","Passenger Train speed") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "surface model", font.size ="tiny" ,single.row=TRUE,out="./Output/linear_model_result_surface.tex")



####### DIP
defect<- read.csv(file="./Intermediate_Data/DIP_with_tonnage.csv", header=TRUE, sep=",")
defect<-defect[,-1]

var<-defect[,-c(4,5,6,7,10,14)]
cor(var)

#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)

n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]
defect=defect[defect$CLASS>3,]
defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))


ggplot(defect,aes(x=TOT_DFLT_MGT,y=DEF_AMPLTD))+
  geom_point()+
  facet_grid(.~ CLASS)+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(1,2.5))+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Cumulative weight" , y="Defect Amplitude")+
  ggsave("./Output/DIP_linear_model_plot.png")

defect$CLASS = relevel(defect$CLASS, ref ="Class 5")

# defect size
model.1<-lm(DEF_AMPLTD~factor(CLASS)+cum_time,defect)
summary(model.1)
model.2<-lm(DEF_AMPLTD~factor(CLASS)+cum_time+TOT_DFLT_MGT,defect)
summary(model.2)
model.3<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH,defect)
summary(model.3)
model.4<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(LINE_SEG_NBR),defect)
summary(model.4)
model.5<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(TRACK_SDTK_NBR)+TEST_PSPD,defect)
summary(model.5)
model.7<-lm((DEF_AMPLTD)~factor(CLASS)+cum_time+DEF_LGTH,defect)
summary(model.7)



library(stargazer)

stargazer(model.1,model.2,model.3,model.4,model.5,model.7, dep.var.labels=c("Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude"),notes = "\\parbox[t]{\\textwidth}{ This table shows different linear models to find the relationships between different variables and DIP defect amplitude. Time or cumulative weight can be considered as main degradation factor. Rail class can be consider as categorical data to estimate different classes intercept.}", notes.align = "l"
          
          
          , title="Regression Results for DIP defect in 90 days interval",covariate.labels = c("Class 4","Cumulative time","Cumulative weight", "Defect length","Segment 2", "Segment 3", "Segment 4","Line 1", "Line 2","Passenger Train speed") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "DIP model", font.size ="tiny" ,single.row=TRUE,out="./Output/linear_model_result_DIP.tex")

####### Xlevel
defect<- read.csv(file="./Intermediate_Data/xlevel_with_tonnage.csv", header=TRUE, sep=",")
defect<-defect[,-1]

var<-defect[,-c(4,5,6,7,10,14)]
cor(var)

#defect<-subset(defect,defect$TOT_DFLT_MGT>0)
defect$quarter<-quarter(defect$TEST_DT)
defect$DEF_AMPLTD<-abs(defect$DEF_AMPLTD)

n<-nrow(defect)
n_train<-round(0.8*n)
set.seed(101)
train.indices<-sample(1:n,n_train)
train<-defect[train.indices,]
test<-defect[-train.indices,]
defect=defect[defect$CLASS>3,]
defect$CLASS = factor(defect$CLASS, levels = 4:5, c("Class 4", "Class 5"))


ggplot(defect,aes(x=TOT_DFLT_MGT,y=DEF_AMPLTD))+
  geom_point()+
  facet_grid(.~ CLASS)+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(1,2.5))+
  theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Cumulative weight" , y="Defect Amplitude")+
  ggsave("./Output/xlevel_linear_model_plot.png")

defect$TEST_FSPD=factor(defect$TEST_FSPD)

ggplot(defect,aes(x=TEST_FSPD,y=DEF_AMPLTD))+
  geom_boxplot()+
  theme_bw()+
  facet_grid(~CLASS)+
theme(strip.background=element_blank(), strip.text = element_text(size=12 , face="italic"))+
  labs(x="Freight train speed" , y="Defect amplitude")+
ggsave("./Output/xlevel_speed_ampl.png")

defect$TEST_FSPD= numeric(defect$TEST_FSPD)
defect$CLASS = relevel(defect$CLASS, ref ="Class 5")

# defect size
model.1<-lm(DEF_AMPLTD~factor(CLASS)+cum_time,defect)
summary(model.1)
model.2<-lm(DEF_AMPLTD~factor(CLASS)+cum_time+TOT_DFLT_MGT,defect)
summary(model.2)
model.3<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH,defect)
summary(model.3)
model.4<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(LINE_SEG_NBR),defect)
summary(model.4)
model.5<-lm(DEF_AMPLTD~factor(CLASS)+TOT_DFLT_MGT+DEF_LGTH+factor(TRACK_SDTK_NBR)+TEST_PSPD,defect)
summary(model.5)
model.7<-lm((DEF_AMPLTD)~factor(CLASS)+cum_time+DEF_LGTH,defect)
summary(model.7)

library(stargazer)

stargazer(model.1,model.2,model.3,model.4,model.5,model.7,dep.var.labels=c("Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude","Defect Amplitude"),notes =  "\\parbox[t]{\\textwidth}{This table shows different linear models to find the relationships between different variables and Xlevel defect amplitude.Time or cumulative weight can be considered as main degradation factor. rail class can be consider as categorical data to estimate different classes intercept.}", notes.align = "l"
          
          
          , title="Regression Results for xlevel defect in 90 days interval",covariate.labels = c("Class 4","Cumulative time","Cumulative weight", "Defect length","Segment 2", "Segment 3", "Segment 4","Line 1", "Line 2","Line 3","Passenger Train speed") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "xlevel model", font.size ="tiny" ,single.row=TRUE,out="./Output/linear_model_result_xlevel.tex")

defect$TEST_FSPD=factor(defect$TEST_FSPD)

# 
# 
# 
# stargazer(model365,model90,dep.var.labels=c("Defect Amplitude","Defect Amplitude")
#           
#           
#           , title="Regression Results for surface defect in 365 and 90 days interval",covariate.labels = c("Class 4","Cumulative time", "Defect length") ,digits=4 ,ci=FALSE, ci.level=0.90,label = "comparing", font.size ="tiny" ,single.row=TRUE,out="./Output/comparing_linear_model.tex")


