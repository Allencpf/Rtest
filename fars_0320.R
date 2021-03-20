##############Package###############
###code in 0320
install.packages("mlogit")
install.packages("VGAM")
install.packages("foreign")
install.packages("nnet")
install.packages("nlme")
install.packages("gamlss")
install.packages("gdata")
install.packages("reshape")
install.packages("vegan")
install.packages("stepNorm")
install.packages("pscl")
install.packages("Hmisc")
install.packages("compare")
install.packages("ggplot2")
install.packages("showtext")
install.packages("sysfonts")
install.packages("rms")
install.packages("vcd")
install.packages("ellipse")
install.packages("corrplot")
install.packages("bootstrap")
install.packages("psych")
install.packages("plyr")
install.packages("reshape2")
install.packages("GJRM")
install.packages("texreg") # 安装R包
install.packages("dplyr")
install.packages("broom") 
install.packages("Zelig")

library(mlogit)
library(VGAM)
library(foreign)
library(gamlss)
library(nnet)
library(reshape)
library(pscl)
library(ggplot2)
library(rms)
library(Hmisc)
library(vcd)
library(ellipse)
library(boot)
library(psych)
library(GJRM)
library(texreg)
library(dplyr)
library(broom)
library(Zelig)


require(plyr)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(VGAM)


################################## Star ###########################################
library(readr)
Odata <- read_csv("r_prgram/data/FARS/df_two.csv")
Mdata <- read_csv("r_prgram/data/FARS/df_two.csv")

attach(Mdata)
#Mdata<-subset(Mdata,ROLLOVER<9)                   #table(Mdata$ROLLOVER)         
#Mdata<-subset(Mdata,AIR_BAG>0 & AIR_BAG<28 & AIR_BAG_>0 & AIR_BAG_<28)       #table(Mdata$AIR_BAG)
#Mdata<-subset (Mdata,EJECTION<7 & EJECTION_<7)                   #table(Mdata$EJECTION)

#Mdata<-subset(Mdata,HOUR<99 & HOUR<99)                                #table(Mdata$HOUR)
#Mdata<-subset(Mdata,WEATHER>0 & WEATHER<11)     #table(Mdata$WEATHER) 

Mdata<-subset(Mdata,MAN_COLL<9)                  #table(Mdata$MAN_COLL)
#Mdata<-subset(Mdata,NUMOCCS<99)                   #table(Mdata$NUMOCCS) 
Mdata<-subset(Mdata,BODY_TYP<80 & BODY_TYP_<80)                  #table(Mdata$BODY_TYP)
Mdata<-subset(Mdata,BODY_TYP<50 | BODY_TYP>59)
Mdata<-subset(Mdata,BODY_TYP_<50|BODY_TYP_>59)
#Mdata<-subset(Mdata,TRAV_SP>0 & TRAV_SP<997)       #table(Mdata$TRAV_SP) 数据太少了
#Mdata<-subset(Mdata,TRAV_SP_>0 & TRAV_SP_<997)

#Mdata<-subset(Mdata,VNUM_LAN>0 & VNUM_LAN<8)       #table(Mdata$VNUM_LAN) 车道数
#Mdata<-subset(Mdata,VALIGN>0 & VALIGN<8)                     #table(Mdata$VALIGN)

Mdata<-subset(Mdata,AGE<998 & AGE2<998)                      #table(Mdata$AGE)
Mdata<-subset(Mdata,SEX<8 & SEX2<8)                        #table(Mdata$SEX)
#Mdata<-subset(Mdata,SEAT_POS<98)                  #table(Mdata$SEAT_POS)
Mdata<-subset(Mdata,REST_USE<98& REST_USE>0 & REST_USE2<98 & REST_USE2>0)      #table(Mdata$REST_USE)
Mdata<-subset(Mdata,DRINKING<8 & DRINKING2<8)                   #table(Mdata$DRINKING)
Mdata<-subset(Mdata,DRUGS<8 & DRUGS2<8)                      #table(Mdata$DRUGS)
#Mdata<-subset(Mdata,INJ_SEV<=4 & INJ_SEV2<=4)

#Mdata$TIME<-ifelse((Mdata$HOUR>=6 & Mdata$HOUR<=18),1,0)##1表示白天，0表示夜晚

attach(Mdata) # attach 之后就可以直接用Mdata里的变量名字了

Mdata1=data.frame(INJ_SEV,AGE,SEX,REST_USE,DRINKING,DRUGS,AIR_BAG,EJECTION, 
                  INJ_SEV2,AGE2,SEX2,REST_USE2,DRINKING2,DRUGS2,AIR_BAG2,EJECTION2,
                  LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL)
Mdata2=data.frame(INJ_SEV,AGE,SEX,REST_USE,DRINKING,DRUGS,AIR_BAG,EJECTION, 
                  INJ_SEV_,AGE_,SEX_,REST_USE_,DRINKING_,DRUGS_,AIR_BAG_,EJECTION_,
                  LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL)
detach(Mdata)
