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
Odata <- read_csv("r_prgram/data/df_two.csv")
Mdata <- read_csv("r_prgram/data/df_two.csv")


Mdata<-subset(Mdata,ROLLOVER<9)                   #table(Mdata$ROLLOVER)         
Mdata<-subset(Mdata,AIR_BAG>0 & AIR_BAG<28 & AIR_BAG_>0 & AIR_BAG_<28)       #table(Mdata$AIR_BAG)
Mdata<-subset (Mdata,EJECTION<7 & EJECTION_<7)                   #table(Mdata$EJECTION)

Mdata<-subset(Mdata,HOUR<99 & HOUR<99)                                #table(Mdata$HOUR)
Mdata<-subset(Mdata,WEATHER>0 & WEATHER<11)     #table(Mdata$WEATHER) 

Mdata<-subset(Mdata,MAN_COLL<9)                  #table(Mdata$MAN_COLL)
#Mdata<-subset(Mdata,NUMOCCS<99)                   #table(Mdata$NUMOCCS) 
Mdata<-subset(Mdata,BODY_TYP<80 & BODY_TYP_<80)                  #table(Mdata$BODY_TYP)
Mdata<-subset(Mdata,BODY_TYP<50 | BODY_TYP>59)
Mdata<-subset(Mdata,BODY_TYP_<50|BODY_TYP_>59)
#Mdata<-subset(Mdata,TRAV_SP>0 & TRAV_SP<997)       #table(Mdata$TRAV_SP) 数据太少了
#Mdata<-subset(Mdata,TRAV_SP_>0 & TRAV_SP_<997)

Mdata<-subset(Mdata,VNUM_LAN>0 & VNUM_LAN<8)       #table(Mdata$VNUM_LAN) 车道数
#Mdata<-subset(Mdata,VALIGN>0 & VALIGN<8)                     #table(Mdata$VALIGN)

Mdata<-subset(Mdata,AGE<998 & AGE_<998)                      #table(Mdata$AGE)
Mdata<-subset(Mdata,SEX<8 & SEX_<8)                        #table(Mdata$SEX)
#Mdata<-subset(Mdata,SEAT_POS<98)                  #table(Mdata$SEAT_POS)
Mdata<-subset(Mdata,REST_USE<98& REST_USE>0 & REST_USE_<98 & REST_USE_>0)      #table(Mdata$REST_USE)
Mdata<-subset(Mdata,DRINKING<8 & DRINKING_<8)                   #table(Mdata$DRINKING)
Mdata<-subset(Mdata,DRUGS<8 & DRUGS_<8)                      #table(Mdata$DRUGS)
#Mdata<-subset(Mdata,INJ_SEV<=4 & INJ_SEV2<=4)

Mdata$TIME<-ifelse((Mdata$HOUR>=6 & Mdata$HOUR<=18),1,0)##1表示白天，0表示夜晚

attach(Mdata) # attach 之后就可以直接用Mdata里的变量名字了

Mdata1=data.frame(INJ_SEV,AGE,SEX,REST_USE,DRINKING,DRUGS,AIR_BAG,EJECTION, 
                  INJ_SEV_,AGE_,SEX_,REST_USE_,DRINKING_,DRUGS_,AIR_BAG_,EJECTION_,
                  LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL)
Mdata2=data.frame(INJ_SEV,AGE,SEX,REST_USE,DRINKING,DRUGS,AIR_BAG,EJECTION, 
                  INJ_SEV_,AGE_,SEX_,REST_USE_,DRINKING_,DRUGS_,AIR_BAG_,EJECTION_,
                  LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL)
detach(Mdata)
# 
# 
# ######################## 相关性检验2 变量相关系数图#######################
library(corrplot)
mylist1<-data.frame(Mdata1$TIME,Mdata1$MAN_COLL,Mdata1$WEATHER,Mdata1$BODY_TYP,Mdata1$TRAV_SP,
                   Mdata1$VALIGN,Mdata1$AGE,Mdata1$SEX,Mdata1$SEAT_POS,Mdata1$REST_USE,Mdata1$DRINKING,Mdata1$DRUGS )
M<-cor(mylist1)
corrplot(M)
corrplot(M,order="AOE",type="upper",method="pie",tl.pos="d",tl.cex = 0.8)
corrplot(M,add=TRUE, type="lower", method="shade",order="AOE",
          diag=FALSE,tl.pos="n", cl.pos="n")
corrplot(M,add=TRUE, type="lower", method="number",order="AOE",
          diag=FALSE,tl.pos="n", cl.pos="n")



######################## 两个因变量（INJ_SEV 和 INJ_SEV2）的处理 #######################
 
subcrit0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
  if (Mdata1$INJ_SEV[i]==0){subcrit0[i]=0} # No apparent injury (O)
  else {subcrit0[i]=1}  # Possible injury(C) and Suspected Minor injury(B)
 }

Mdata1$outcome0<-subcrit0
# mode(Mdata1$outcome0)
# is.numeric(Mdata1$outcome0)
rm(subcrit0) # 从数据表中移除
#Mdata1$outcome0=factor(Mdata1$outcome0,levels = c(0,1))  #R中统一使用因子来表示离散型变量

# is.factor(Mdata1$outcome0)
# levels(Mdata1$outcome0)
# table(Mdata$outcome0)
subcrit1=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$INJ_SEV_[i]==0){subcrit1[i]=0} # No apparent injury (O)
  else {subcrit1[i]=1}  # Possible injury(C) and Suspected Minor injury(B) #suspected serious(A) and Fatal injury(K)
}
Mdata1$outcome1<-subcrit1
rm(subcrit1)
#Mdata1$outcome1=factor(Mdata1$outcome1,levels = c(0,1))
##############ever#################
#--------------------------------#
subcrit1=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
  if (Mdata1$AIR_BAG[i]==20){subcrit1[i]=0} #*Not Deployed
                       else {subcrit1[i]=1} #Deployed
}

Mdata1$outcome1<-subcrit1
rm(subcrit1)
Mdata1$outcome1=factor(Mdata1$outcome1,levels = c(0,1))
#--------------------------------#
subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
	     if (Mdata1$EJECTION[i]==0) {subcrit2[i]=0} #*Not Ejected
                            else  {subcrit2[i]=1} #Ejected
}
Mdata1$outcome2<-subcrit2
rm(subcrit2)
Mdata1$outcome2=factor(Mdata1$outcome2,levels = c(0,1))
#table(Mdata$outcome2)
# mode(Mdata$outcome2)


######################## 自变量的处理 #######################

# 将Mdata1 里的time 变量变成factor 因子变量
Mdata1$TIME=factor(Mdata1$TIME,levels = c(0,1))

#-----------安全气囊---------------------#
subcrit1=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$AIR_BAG[i]==20){subcrit1[i]=0} #*Not Deployed
  else {subcrit1[i]=1} #Deployed
}

Mdata1$AIR_BAG<-subcrit1
rm(subcrit1)
Mdata1$AIR_BAG=factor(Mdata1$AIR_BAG,levels = c(0,1))
## INJ_SEV2
subcrit1=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$AIR_BAG_[i]==20){subcrit1[i]=0} #*Not Deployed
  else {subcrit1[i]=1} #Deployed
}

Mdata1$AIR_BAG_<-subcrit1
rm(subcrit1)
Mdata1$AIR_BAG_=factor(Mdata1$AIR_BAG_,levels = c(0,1))
#-----------弹射---------------------#
subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$EJECTION[i]==0) {subcrit2[i]=0} #*Not Ejected
  else  {subcrit2[i]=1} #Ejected
}
Mdata1$EJECTION<-subcrit2
rm(subcrit2)
Mdata1$EJECTION=factor(Mdata1$EJECTION,levels = c(0,1))
###INJ_SEV2
subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$EJECTION_[i]==0) {subcrit2[i]=0} #*Not Ejected
  else  {subcrit2[i]=1} #Ejected
}
Mdata1$EJECTION_<-subcrit2
rm(subcrit2)
Mdata1$EJECTION_=factor(Mdata1$EJECTION_,levels = c(0,1))
#-----------天气---------------------#
WEATHER0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if      (Mdata1$WEATHER[i]==1)                      {WEATHER0[i]=0} #*Clear
  else if (Mdata1$WEATHER[i]==2)                      {WEATHER0[i]=1} #Rain 
  else if (Mdata1$WEATHER[i]==4 | Mdata1$WEATHER[i]==3) {WEATHER0[i]=2} #Snow
  else                                                {WEATHER0[i]=3} #fog / cloud
}

Mdata1$WEATHER<-WEATHER0
rm(WEATHER0)
# table(Mdata$WEATHER0)
Mdata1$WEATHER=factor(Mdata1$WEATHER,levels = c(0,1,2,3))



#-------碰撞方式----------#
MAN_COLL0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
  if      (Mdata1$MAN_COLL[i]==1) {MAN_COLL0[i]=0} #Front-to-Rear
  else if (Mdata1$MAN_COLL[i]==2) {MAN_COLL0[i]=1} #Front-to-Front
  else if (Mdata1$MAN_COLL[i]==6) {MAN_COLL0[i]=2} #Angle
  else if (Mdata1$MAN_COLL[i]==7) {MAN_COLL0[i]=3} #Sideswipe-same direction
  else                            {MAN_COLL0[i]=4} #Sideswipe-opposite direction
}

Mdata1$MAN_COLL<-MAN_COLL0
rm(MAN_COLL0)
# table(Mdata1$MAN_COLL)
Mdata1$MAN_COLL=factor(Mdata1$MAN_COLL,levels = c(0,1,2,3,4))



#-----------车辆类型---------------------#
BODY_TYP0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$BODY_TYP[i]<=13)                      {BODY_TYP0[i]=0} #*AUTOMOBILES
	else if (Mdata1$BODY_TYP[i]>=14 &Mdata1$BODY_TYP[i]<=19){BODY_TYP0[i]=1} #UTILITY VEHICLES
      else                                                                       {BODY_TYP0[i]=2} #Trucks
}

Mdata1$BODY_TYP0<-BODY_TYP0
rm(BODY_TYP0)
# table(Mdata1$BODY_TYP0)
Mdata1$BODY_TYP=factor(Mdata1$BODY_TYP,levels = c(0,1,2))



#------------线性--------------------#
VALIGN0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$VALIGN[i]==1){VALIGN0[i]=0} #*Straight
                         else {VALIGN0[i]=1} #Curve
}

Mdata1$VALIGN<-VALIGN0
rm(VALIGN0)
# table(Mdata1$VALIGN)
Mdata1$VALIGN=factor(Mdata1$VALIGN,levels = c(0,1))

#------------年龄--------------------#
AGE0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata)){
	     if (Mdata$AGE[i]>0   & Mdata$AGE[i]<=30){AGE0[i]=0} #* >0 & <=30
	else if (Mdata$AGE[i]>=31 & Mdata$AGE[i]<=60){AGE0[i]=1} #>=16 & <=60
                                          else {AGE0[i]=2} #>=61	
 }

Mdata1$AGE<-AGE0
rm(AGE0)
# table(Mdata1$AGE)
Mdata1$AGE=factor(Mdata1$AGE,levels = c(0,1,2))
#### INJ_SEV2
AGE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata)){
  if (Mdata$AGE_[i]>0   & Mdata$AGE_[i]<=30){AGE0[i]=0} #* >0 & <=30
  else if (Mdata$AGE_[i]>=31 & Mdata$AGE_[i]<=60){AGE0[i]=1} #>30 & <=60
  else {AGE0[i]=2} #>=61	
}

Mdata1$AGE_<-AGE0
rm(AGE0)
# table(Mdata1$AGE0)
Mdata1$AGE_=factor(Mdata1$AGE_,levels = c(0,1,2))
#-----------性别--------------------#
SEX0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$SEX[i]==1){SEX0[i]=0} #*Male
                       else {SEX0[i]=1} #Female
 }

Mdata1$SEX<-SEX0
rm(SEX0)
# table(Mdata1$SEX0)
Mdata1$SEX=factor(Mdata1$SEX,levels = c(0,1))
### INJ_SEV2
SEX0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$SEX_[i]==1){SEX0[i]=0} #*Male
  else {SEX0[i]=1} #Female
}

Mdata1$SEX_<-SEX0
rm(SEX0)
# table(Mdata1$SEX0)
Mdata1$SEX_=factor(Mdata1$SEX_,levels = c(0,1))

#-------------是否夜间-------------------#
LGT0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata$LGT_COND[i]==1){LGT0[i]=0} #白天
  else {LGT0[i]=1} #夜间
}

Mdata1$DAY<-LGT0
rm(LGT0)
# table(Mdata1$SEX0)
Mdata1$DAY=factor(Mdata1$DAY,levels = c(0,1))


#-------------夜间是否有光照-------------------#
LGT0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata$LGT_COND[i]==3){LGT0[i]=1} #有光照
  else {LGT0[i]=0} #无光照
}

Mdata1$LGT_COND<-LGT0
rm(LGT0)
# table(Mdata1$SEX0)
Mdata1$LGT_COND=factor(Mdata1$LGT_COND,levels = c(0,1))


#-------------座位-------------------#
SEAT_POS0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$SEAT_POS[i]>=11 & Mdata1$SEAT_POS[i]<=19){SEAT_POS0[i]=0}      #* front
			                                                else {SEAT_POS0[i]=1}     #  second及以后
}

Mdata1$SEAT_POS0<-SEAT_POS0
rm(SEAT_POS0)
# table(Mdata1$SEAT_POS0)
Mdata1$SEAT_POS0=factor(Mdata1$SEAT_POS0,levels = c(0,1))

#------------安全带--------------------#
REST_USE0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$REST_USE[i]<=3|Mdata1$REST_USE[i]==8){REST_USE0[i]=0} #*Used
                                                else {REST_USE0[i]=1} #Not Used
}

Mdata1$REST_USE<-REST_USE0
rm(REST_USE0)
# table(Mdata1$REST_USE0)
Mdata1$REST_USE=factor(Mdata1$REST_USE,levels = c(0,1))

###INJ_SEV2
REST_USE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$REST_USE_[i]<=3|Mdata1$REST_USE_[i]==8){REST_USE0[i]=0} #*Used
  else {REST_USE0[i]=1} #Not Used
}

Mdata1$REST_USE_<-REST_USE0
rm(REST_USE0)
# table(Mdata1$REST_USE0)
Mdata1$REST_USE_=factor(Mdata1$REST_USE_,levels = c(0,1))
#-----------酒驾---------------------#
DRINKING0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$DRINKING[i]==0){DRINKING0[i]=0} #*No
                           else {DRINKING0[i]=1} #Yes
}

Mdata1$DRINKING<-DRINKING0
rm(DRINKING0)
# table(Mdata1$DRINKING0)
Mdata1$DRINKING=factor(Mdata1$DRINKING,levels = c(0,1))

### INJ_SEV2
DRINKING0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DRINKING_[i]==0){DRINKING0[i]=0} #*No
  else {DRINKING0[i]=1} #Yes
}

Mdata1$DRINKING_<-DRINKING0
rm(DRINKING0)
# table(Mdata1$DRINKING0)
Mdata1$DRINKING_=factor(Mdata1$DRINKING_,levels = c(0,1))

#--------------吃药------------------#
DRUGS0=matrix(0,nrow(Mdata1),1)
 for (i in 1:nrow(Mdata1)){
	     if (Mdata1$DRUGS[i]==0){DRUGS0[i]=0} #*No
                         else {DRUGS0[i]=1} #Yes
}

Mdata1$DRUGS<-DRUGS0
rm(DRUGS0)
# table(Mdata1$DRUGS)
Mdata1$DRUGS=factor(Mdata1$DRUGS,levels = c(0,1))

### INJ_SEV2
DRUGS0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DRUGS_[i]==0){DRUGS0[i]=0} #*No
  else {DRUGS0[i]=1} #Yes
}

Mdata1$DRUGS_<-DRUGS0
rm(DRUGS0)
# table(Mdata1$DRUGS)
Mdata1$DRUGS_=factor(Mdata1$DRUGS_,levels = c(0,1))

save(Mdata1,file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/R_programming/r_prgram/data/CRRS/two.rda")
write.csv(Mdata1,file="/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/R_programming/r_prgram/data/CRRS/two.csv",quote=F,row.names = F)

Mdata1 <- load("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/R_programming/r_prgram/data/CRRS/two.rda")

# Mdata1 <- subset(Mdata1, select = -c(SEX0, WEATHER0, MAN_COLL0, BODY_TYP0, VALIGN0, REST_USE0, DRINKING0, DRUGS0))





#################相关性检验#####################
attach(Mdata1)
library(corrplot)
mylist1<-data.frame(AGE,SEX,REST_USE, DRINKING, DRUGS, AIR_BAG, EJECTION, 
                    AGE_, SEX_, REST_USE_, DRINKING_, DRUGS_, AIR_BAG_, EJECTION_,
                    TIME, WEATHER, MAN_COLL, LGT_COND, DAY)
mylist2 <- as.data.frame(lapply(mylist1,as.numeric)) # 将mylist1 中所有的factor variable convert to numeric variable
M<-cor(mylist2)
corrplot(M)
corrplot(M,order="AOE",type="upper",method="pie",tl.pos="d",tl.cex = 0.8)
corrplot(M,add=TRUE, type="lower", method="shade",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")
corrplot(M,add=TRUE, type="lower", method="number",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")

###驾驶员1
mylist1<-data.frame(AGE,SEX,REST_USE, DRINKING, DRUGS, AIR_BAG, EJECTION, 
                    TIME, WEATHER, MAN_COLL, LGT_COND, DAY)
mylist2 <- as.data.frame(lapply(mylist1,as.numeric)) # 将mylist1 中所有的factor variable convert to numeric variable
M<-cor(mylist2)
corrplot(M)
corrplot(M,order="AOE",type="upper",method="pie",tl.pos="d",tl.cex = 0.8)
corrplot(M,add=TRUE, type="lower", method="shade",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")
corrplot(M,add=TRUE, type="lower", method="number",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")
#### 驾驶员2
mylist1<-data.frame(AGE_, SEX_, REST_USE_, DRINKING_, DRUGS_, AIR_BAG_, EJECTION_,
                    TIME, WEATHER, MAN_COLL, LGT_COND, DAY)
mylist2 <- as.data.frame(lapply(mylist1,as.numeric)) # 将mylist1 中所有的factor variable convert to numeric variable
M<-cor(mylist2)
corrplot(M)
corrplot(M,order="AOE",type="upper",method="pie",tl.pos="d",tl.cex = 0.8)
corrplot(M,add=TRUE, type="lower", method="shade",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")
corrplot(M,add=TRUE, type="lower", method="number",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")

#res1 <- cor.mtest(mylist2, conf.level = .60)
#corrplot(M, p.mat = res1$p, sig.level = .01) 没通过相关性检验的打×

#############BIVARIATE PROBIT#################

eq1<- INJ_SEV ~ AGE + SEX + REST_USE + DRINKING + DRUGS + AIR_BAG + EJECTION + TIME + WEATHER + MAN_COLL + factor(LGT_COND)  + factor(VNUM_LAN)
eq2 <- INJ_SEV_ ~ AGE_ + SEX_ + REST_USE_ + DRINKING_ + DRUGS_ + AIR_BAG_ + EJECTION_ + TIME + WEATHER + MAN_COLL + factor(LGT_COND) + factor(VNUM_LAN)
f.list <- list(eq1, eq2)
mr <- c("N", "N")

library(GJRM)
bvp <- gjrm(f.list, data=Mdata1, Model="B", margins=mr)
summary(bvp)

eq1<- INJ_SEV ~ AGE + SEX + REST_USE + DRINKING + DRUGS + AIR_BAG + EJECTION + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
eq2 <- INJ_SEV_ ~ AGE_ + SEX_ + REST_USE_ + DRINKING_ + DRUGS_ + AIR_BAG_ + EJECTION_ + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
f.list <- list(eq1, eq2)
mr <- c("N", "N")

library(GJRM)
bvp1 <- gjrm(f.list, data=Mdata1, Model="B", margins=mr,  gamlssfit = TRUE, iterlimsp = 1000)
summary(bvp1)



eq1<- outcome0 ~ AGE + SEX + REST_USE + DRINKING + DRUGS + AIR_BAG + EJECTION + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
eq2 <- outcome1 ~  AGE_ + SEX_ + REST_USE_ + DRINKING_ + DRUGS_ + AIR_BAG_ + EJECTION_ + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
f.list <- list(eq1, eq2)
library(GJRM)
mr <- c("probit", "probit")

bpN <- gjrm(f.list, data=Mdata1, Model="B", margins=mr)
bpF <- gjrm(f.list, data=Mdata1, BivD = "F", Model="B", margins=mr)
bpC0 <- gjrm(f.list, data=Mdata1, BivD = "C0", Model="B", margins=mr)
bpC180 <- gjrm(f.list, data=Mdata1, BivD = "C180", Model="B", margins=mr)
bpG0 <- gjrm(f.list, data=Mdata1, BivD = "G0", Model="B", margins=mr)
bpG180 <- gjrm(f.list, data=Mdata1, BivD = "G180", Model="B", margins=mr)

rm(eq1, eq2,f.list)

AIC(bpN, bpF, bpC0, bpC180, bpG0, bpG180)
BIC(bpN, bpF, bpC0, bpC180, bpG0, bpG180)

conv.check(bpN)
summary(bpN)


eq1<- outcome0 ~ AGE + SEX + REST_USE + DRINKING + DRUGS + AIR_BAG + EJECTION + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
eq2 <- outcome1 ~  outcome0 + AGE_ + SEX_ + REST_USE_ + DRINKING_ + DRUGS_ + AIR_BAG_ + EJECTION_ + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
f.list <- list(eq1, eq2)
library(GJRM)
mr <- c("probit", "probit")

#bpN1 <- gjrm(f.list, data=Mdata1, Model="B", margins=mr, gamlssfit = TRUE)
bpN1 <- gjrm(f.list, data=Mdata1, Model="B", margins=mr)
bpF1 <- gjrm(f.list, data=Mdata1, BivD = "F", Model="B", margins=mr)
bpC01 <- gjrm(f.list, data=Mdata1, BivD = "C0", Model="B", margins=mr)
bpC1801 <- gjrm(f.list, data=Mdata1, BivD = "C180", Model="B", margins=mr)
bpG01 <- gjrm(f.list, data=Mdata1, BivD = "G0", Model="B", margins=mr)
bpG1801 <- gjrm(f.list, data=Mdata1, BivD = "G180", Model="B", margins=mr)

rm(eq1, eq2,f.list)
AIC(bpN1, bpF1, bpC01, bpC1801, bpG01, bpG1801)
AIC(bpN, bpF, bpC0, bpC180, bpG0, bpG180)
summary(bpN1)
summary(bpN)
print(bpN)

eq2<- outcome0 ~ outcome1 + AGE + SEX + REST_USE + DRINKING + DRUGS + AIR_BAG + EJECTION + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
eq1 <- outcome1 ~   AGE_ + SEX_ + REST_USE_ + DRINKING_ + DRUGS_ + AIR_BAG_ + EJECTION_ + TIME + WEATHER + MAN_COLL + LGT_COND + DAY
f.list <- list(eq1, eq2)
library(GJRM)
mr <- c("probit", "probit")

bpN1 <- gjrm(f.list, data=Mdata1, Model="B", margins=mr, gamlssfit = TRUE)
bpF1 <- gjrm(f.list, data=Mdata1, BivD = "F", Model="B", margins=mr)
bpC01 <- gjrm(f.list, data=Mdata1, BivD = "C0", Model="B", margins=mr)
bpC1801 <- gjrm(f.list, data=Mdata1, BivD = "C180", Model="B", margins=mr)
bpG01 <- gjrm(f.list, data=Mdata1, BivD = "G0", Model="B", margins=mr)
bpG1801 <- gjrm(f.list, data=Mdata1, BivD = "G180", Model="B", margins=mr)
AIC(bpN1, bpF1, bpC01, bpC1801, bpG01, bpG1801)
summary(bpF1)

LM.bpm(f.list, data = Mdata1, weights = NULL, subset = NULL, Model="B", hess = TRUE)
gt.bpm(bpN1)
OR(bpN1, nm.end = "outcome0")
RR(bpN1, nm.end = "outcome0")
vis.gjrm(bpN1, 3, fun = NULL)

jc.probs(bpN1, Mdata1$outcome0, Mdata1$outcome1, y3 = NULL, Mdata1, type = "joint", cond = 0,
         intervals = FALSE, n.sim = 100, prob.lev = 0.05, min.pr = 1e-323, max.pr = 1,
         cumul = "no")



AT(bpN1, nm.end = "outcome0", hd.plot = TRUE, cex.axis=1.5, cex.lab=1.5, cex.main=1.6)
AT(bpN1, nm.end = "outcome0", hd.plot = TRUE, cex.axis=1.5, cex.lab=1.5, cex.main=1.6, type = "univariate")
AT(bpN1, nm.end = "outcome0", hd.plot = TRUE, cex.axis=1.5, cex.lab=1.5, cex.main=1.6, type = "naive")
library(GJRM)
library(ggplot2)
par(mfrow = c(2, 2), mar=c(4.5, 4.5, 2, 2))
plot(bpN, eq=1,select=1, seWithMean = TRUE, scale = 0)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2, 2), cex.axis = 1.6, cex.lab = 1.6)
plot(bpN, eq=2, seWithMean = TRUE, scale = 0, shade = TRUE, pages = 1, jit=TRUE)



screenreg(list(bvp, bvp1,bvp2),
          custom.model.names = c("bvp", "bvp1","bvp2"), # 修改模型的名字
          digits = 3, # 设置有效数字位数 
          single.row = TRUE, # 将 standard errors和系数放在同一行
          ci.force = TRUE)  # 将standard errors 替换为置信区间

summary(bvp1) 
data("sanction")

z.out1 <- zelig(cbind(oc0, oc1) ~ AGE + SEX + AGE_, model = "blogit", data = Mdata1)
z.out2<- zelig(cbind(import, export) ~ coop + cost + target,
                model = "blogit", data = sanction)








#--------------提取样本数据------------------#
Fdata=Mdata1[,c(19:31,4,9)] # 选取第19-31列 （全有31-19+1 = 13），第4列TIME和第9列SPEED的数据。
# str(Fdata)
# 
# require(reshape2)
#######################变量中文化#######################
# install.packages("reshape")
# library(reshape)
newdata1=Fdata[,c(1:15)] 
newdata1=rename(newdata1,c(outcome0="车辆翻滚状态",outcome1="安全气囊弹出状态",outcome2="人体弹射状态",TIME="时间",WEATHER0="天气",
                       MAN_COLL0="碰撞方式",BODY_TYP0="车辆类型", VALIGN0="道路线形",AGE0="年龄",SEX0="性别",SEAT_POS0="座椅位置",
                       REST_USE0="安全带的使用",DRINKING0="是否饮酒驾驶",DRUGS0="是否吃药驾驶",TRAV_SP="车辆行驶速度"))

# mosaic(~时间+天气+碰撞方式+成员数量+车辆类型+单侧车道数+道路线形+年龄+性别+座椅位置+是否饮酒驾驶+是否吃药驾驶+车辆翻滚状态+安全气囊弹出状态+人员弹射状态,
#        data=newdata1[which(newdata1$??ȫ????ʹ??=="1"),],shade=TRUE,legend=TRUE)
## 图2-2 使用安全带马赛克图
mosaicplot(~时间+碰撞方式+性别+车辆翻滚状态,data=newdata1[which(newdata1$安全带的使用=="1"),],shade=TRUE,legend=TRUE)
## 图2-3 未使用安全带马赛克图
mosaicplot(~时间+碰撞方式+性别+车辆翻滚状态,data=newdata1[which(newdata1$安全带的使用=="0"),],shade=TRUE,legend=TRUE)

######################## 速度的转换#######################

 speed <- 1.609344*Fdata$TRAV_SP # 中美车速换算：1英里每小时 = 1.609344公里每小时

 Fdata$speed<-speed
 
 rm(speed)
 
 # summary(Fdata$speed)
 Fdata$speed=round(Fdata$speed,0)  #保留速度为整数值
 
 subset(Ndata<-subset(Fdata,speed>=10&speed<=160)) #保留车速在10-160之间的数据

 summary(Ndata)
#################Figure 1#####################
#################rollover#####################
m <- cbind(c(1,3),c(2,3))
layout(m,heights=c(2,1.5,2,1.5))
par(mar=c(0,0.5,1,0.5),oma=c(4,4,3,4))

  bk=15
  a1=hist(subset(Ndata,outcome0==1)$speed,breaks=bk,plot=FALSE)
  a1$counts=a1$counts/sum(a1$counts)
  summary(a1$counts)
  length(a1$counts)
  plot(a1,xaxt="n",xlab="",ylab="",main="",ylim=c(0,0.22),cex.axis=1.2,border="black")

  axis(3,xaxp=c(10,180,2),cex.axis=1.2)
#lines((density(subset(Ndata,outcome0==1)$speed)),col="red",lwd=1.5)
  box(col="black")
  mtext("Percentage", side = 2, line = 2.5, adj = 0.5, cex = 1)
  mtext("rollover observations", side = 3, line = -1.25, cex = 1,col="blue")

# b=hist(subset(Fdata,outcome0==0)$TRAV_SP,breaks=bk,plot=FALSE)
  b1=hist(subset(Ndata,outcome0==0)$speed,breaks=bk,plot=FALSE)
  b1$counts=b1$counts/sum(b1$counts)
  summary(b1$counts)
  length(b1$counts)
  plot(b1,xaxt="n",yaxt="n",xlab="",ylab="",main="",ylim=c(0,0.22),border="black",cex.axis=1.2)
  axis(3,xaxp=c(10,180,2),cex.axis=1.2)
#lines(density(subset(Ndata,outcome0==0)$speed),col="red",lwd=2)
  box(col="black")
  mtext("Non-rollover observations", side = 3, line = -1.25, cex = 1,col="blue")


c1=a1
c1$counts=a1$counts/b1$counts
#plot(c,main="",yaxt="n",ylim=c(0,1.5),cex.axis=1.2,border="black")
summary(c1$counts)
length(c1$counts)
x=seq( 20,160,10)
length(x)
plot( x,c1$counts,main="",yaxt="n",ylim=c(0,15),cex.axis=1.2,type="b",col="red",lwd=2)
#mtext("(c) Odds ratio of (a) vs. (b)", side = 3, line = -1.5, adj = 0.05, cex = 1,col="blue")
axis(4,cex.axis=1.2)
#lines(c(0,30),c(1,1),col="red",lty=2,lwd=2)
box(col="black")
mtext("Odds", side = 4, line = 2.5, adj = 0.5, cex = 1)
mtext("TRAV_SP", side = 1, line = 2.5, adj = 0.5, cex = 1)
mtext("Odds of rollover versus non-rollover observations", side = 3, line = -1.25, cex = 1,col="blue")

#################airbag#####################
m <- cbind(c(1,3),c(2,3))
layout(m,heights=c(2,1.5,2,1.5))
par(mar=c(0,0.5,1,0.5),oma=c(4,4,3,4))

bk=15
a2=hist(subset(Ndata,outcome1==1)$speed,breaks=bk,plot=FALSE)
# a=hist(subset(Fdata,outcome0==1)$TRAV_SP,breaks=bk,plot=FALSE)
a2$counts=a2$counts/sum(a2$counts)
summary(a2$counts)
length(a2$counts)
plot(a2,xaxt="n",xlab="",ylab="",main="",ylim=c(0,0.22),cex.axis=1.2,border="black")

axis(3,xaxp=c(10,180,2),cex.axis=1.2)
#lines((density(subset(Ndata,outcome0==1)$speed)),col="red",lwd=1.5)
box(col="black")
mtext("Percentage", side = 2, line = 2.5, adj = 0.5, cex = 1)
mtext("airbag observations", side = 3, line = -1.25, cex = 1,col="blue")

# b=hist(subset(Fdata,outcome0==0)$TRAV_SP,breaks=bk,plot=FALSE)
b2=hist(subset(Ndata,outcome1==0)$speed,breaks=bk,plot=FALSE)
b2$counts=b2$counts/sum(b2$counts)
summary(b2$counts)
length(b2$counts)
plot(b2,xaxt="n",yaxt="n",xlab="",ylab="",main="",ylim=c(0,0.2),border="black",cex.axis=1.2)
axis(3,xaxp=c(10,180,2),cex.axis=1.2)
#lines(density(subset(Ndata,outcome0==0)$speed),col="red",lwd=2)
box(col="black")
mtext("Non-airbag observations", side = 3, line = -1.25, cex = 1,col="blue")


c2=a2
c2$counts=a2$counts/b2$counts
#plot(c,main="",yaxt="n",ylim=c(0,1.5),cex.axis=1.2,border="black")
x=seq( 20,160,10)
plot( x,c2$counts,main="",yaxt="n",ylim=c(0,10),cex.axis=1.2,type="b",col="red",lwd=2)
#mtext("(c) Odds ratio of (a) vs. (b)", side = 3, line = -1.5, adj = 0.05, cex = 1,col="blue")
axis(4,cex.axis=1.2)
#lines(c(0,30),c(1,1),col="red",lty=2,lwd=2)
box(col="black")
mtext("Odds", side = 4, line = 2.5, adj = 0.5, cex = 1)
mtext("speed", side = 1, line = 2.5, adj = 0.5, cex = 1)
mtext("Odds of airbag versus non-airbag observations", side = 3, line = -1.25, cex = 1,col="blue")

#################ejection#####################
m <- cbind(c(1,3),c(2,3))
layout(m,heights=c(2,1.5,2,1.5))
par(mar=c(0,0.5,1,0.5),oma=c(4,4,3,4))

bk=15
a=hist(subset(Ndata,outcome2==1)$speed,breaks=bk,plot=FALSE)
# a=hist(subset(Fdata,outcome0==1)$TRAV_SP,breaks=bk,plot=FALSE)
a$counts=a$counts/sum(a$counts)
summary(a$counts)
length(a$counts)
plot(a,xaxt="n",xlab="",ylab="",main="",ylim=c(0,0.2),cex.axis=1.2,border="black")

axis(3,xaxp=c(10,180,2),cex.axis=1.2)
#lines((density(subset(Ndata,outcome0==1)$speed)),col="red",lwd=1.5)
box(col="black")
mtext("Percentage", side = 2, line = 2.5, adj = 0.5, cex = 1)
mtext("ejection observations", side = 3, line = -1.25, cex = 1,col="blue")

# b=hist(subset(Fdata,outcome0==0)$TRAV_SP,breaks=bk,plot=FALSE)
b=hist(subset(Ndata,outcome2==0)$speed,breaks=bk,plot=FALSE)
b$counts=b$counts/sum(b$counts)
summary(b$counts)
length(b$counts)
plot(b,xaxt="n",yaxt="n",xlab="",ylab="",main="",ylim=c(0,0.2),border="black",cex.axis=1.2)
axis(3,xaxp=c(10,180,2),cex.axis=1.2)
#lines(density(subset(Ndata,outcome0==0)$speed),col="red",lwd=2)
box(col="black")
mtext("Non-ejection observations", side = 3, line = -1.25, cex = 1,col="blue")


c=a
c$counts=a$counts/b$counts
x=seq( 20,160,10)
#plot(c,main="",yaxt="n",ylim=c(0,1.5),cex.axis=1.2,border="black")
plot( x,c$counts,main="",yaxt="n",ylim=c(0,60),cex.axis=1.2,type="b",col="red",lwd=2)
#mtext("(c) Odds ratio of (a) vs. (b)", side = 3, line = -1.5, adj = 0.05, cex = 1,col="blue")
axis(4,cex.axis=1.2)

lines( seq(length=15, from=10, to=160),c1$counts,main="",yaxt="n",ylim=c(0,14),cex.axis=1.2,type="b",col="blue",lwd=2)
lines( seq(length=15, from=10, to=160),c2$counts,main="",yaxt="n",ylim=c(0,10),cex.axis=1.2,type="b",col="green",lwd=2)
lines(c(0,30),c(1,1),col="red",lty=2,lwd=2)

box(col="black")
mtext("Odds", side = 4, line = 2.5, adj = 0.5, cex = 1)
mtext("speed", side = 1, line = 2.5, adj = 0.5, cex = 1)
mtext("Odds of ejection versus non-ejection observations", side = 3, line = -1.25, cex = 1,col="blue")




########################Binomial Logistic  #######################

#-----------ROLLOVER---------------------#
fit0<-glm(outcome0~TRAV_SP+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+REST_USE0+DRINKING0,family = binomial,data=Fdata)
summary(fit0)
AIC(fit0) # 8698.137

# model1 <- glm(outcome0~cs(TRAV_SP)+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+REST_USE0+DRINKING0+DRUGS0,data=Fdata, family=binomial())
# summary(model1)


# step(fit0) 

#-----------AIRBAG DEPOLYED--------------#
fit1<-glm(outcome1~TRAV_SP+TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,
          family = binomial,data=Fdata)

summary(fit1)
AIC(fit1)# 32347.71

#-----------EJECTION---------------------#
fit2<-glm(outcome2~TRAV_SP+TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,
          family = binomial,data=Fdata)

summary(fit2)
AIC(fit2) #880.0376


##########################诺模图1#######################
dd<-datadist(Fdata)
options(datadist="dd")
f1<-lrm((outcome0==1)~TRAV_SP+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+REST_USE0+DRINKING0,data=Fdata)
nom <- nomogram(f1,fun= function(x)1/(1+exp(-x)), lp=F,funlabel="Risk") ## or fun=plogis
plot(nom,font=1,cex=1.5,cex.axis=1.4)




######################## 因变量处理 #######################

#------------ROLLOVER+AIRBAG-------------#

COMB1=matrix(0,nrow(Fdata),1)
for (i in 1:nrow(Fdata)){
  if      (Fdata$outcome0[i]==0 & Fdata$outcome1[i]==0){COMB1[i]=0} #*  no rollover & no airbag
  else if (Fdata$outcome0[i]==0 & Fdata$outcome1[i]==1){COMB1[i]=1} #   no rollover & airbag
  else if (Fdata$outcome0[i]==1 & Fdata$outcome1[i]==0){COMB1[i]=2} #      rollover & no airbag
  else                                                 {COMB1[i]=3} #      rollover & airbag
} 

Fdata$COMB1<-COMB1
rm(COMB1)
#table(Fdata$COMB1)
Fdata$COMB1=factor(Fdata$COMB1,levels = c(0,1,2,3))

#------------ROLLOVER+Ejection--------------------#
COMB2=matrix(0,nrow(Fdata),1)
for (i in 1:nrow(Fdata)){
  if      (Fdata$outcome0[i]==0 & Fdata$outcome2[i]==0){COMB2[i]=0} #*  no rollover & no airbag
  else if (Fdata$outcome0[i]==0 & Fdata$outcome2[i]==1){COMB2[i]=1} #   no rollover & airbag
  else if (Fdata$outcome0[i]==1 & Fdata$outcome2[i]==0){COMB2[i]=2} #      rollover & no airbag
  else                                                 {COMB2[i]=3} #      rollover & airbag
} 

Fdata$COMB2<-COMB2
rm(COMB2)
#table(Fdata$COMB2)
Fdata$COMB2=factor(Fdata$COMB2,levels = c(0,1,2,3))

#------------AIRBAG+Ejection--------------------#
COMB3=matrix(0,nrow(Fdata),1)
for (i in 1:nrow(Fdata)){
  if      (Fdata$outcome1[i]==0 & Fdata$outcome2[i]==0){COMB3[i]=0} #*  no rollover & no airbag
  else if (Fdata$outcome1[i]==0 & Fdata$outcome2[i]==1){COMB3[i]=1} #   no rollover & airbag
  else if (Fdata$outcome1[i]==1 & Fdata$outcome2[i]==0){COMB3[i]=2} #      rollover & no airbag
  else                                                 {COMB3[i]=3} #      rollover & airbag
} 

Fdata$COMB3<-COMB3
rm(COMB3)
#table(Fdata$COMB3)
Fdata$COMB3=factor(Fdata$COMB3,levels = c(0,1,2,3))

#------------ROLLOVER+AIRBAG+Ejection--------------------#
COMB4=matrix(0,nrow(Fdata),1)
for (i in 1:nrow(Fdata)){
  if      (Fdata$outcome0[i]==0 & Fdata$outcome1[i]==0 & Fdata$outcome2[i]==0){COMB4[i]=0} #*  no rollover & no airbag
  else if (Fdata$outcome0[i]==0 & Fdata$outcome1[i]==1 & Fdata$outcome2[i]==0){COMB4[i]=1} #   no rollover & airbag
  else if (Fdata$outcome0[i]==0 & Fdata$outcome1[i]==0 & Fdata$outcome2[i]==1){COMB4[i]=2} #      rollover & no airbag
  else if (Fdata$outcome0[i]==0 & Fdata$outcome1[i]==1 & Fdata$outcome2[i]==1){COMB4[i]=3} #   no rollover & airbag
  else if (Fdata$outcome0[i]==1 & Fdata$outcome1[i]==0 & Fdata$outcome2[i]==0){COMB4[i]=4} #      rollover & no airbag
  else if (Fdata$outcome0[i]==1 & Fdata$outcome1[i]==1 & Fdata$outcome2[i]==0){COMB4[i]=5} #   no rollover & airbag
  else if (Fdata$outcome0[i]==1 & Fdata$outcome1[i]==0 & Fdata$outcome2[i]==1){COMB4[i]=6} #      rollover & no airbag
    else                                                                      {COMB4[i]=7} #      rollover & airbag
} 


Fdata$COMB4<-COMB4
rm(COMB4)
#table(Fdata$COMB4)
Fdata$COMB4=factor(Fdata$COMB4,levels = c(0,1,2,3,4,5,6,7))


######################## Multinomial Logistic(mlogit) #######################
# Edata=Fdata[,c(16:19,4:15)] 原来
 Edata=Fdata[,c(17:20,4:16)]


#------------ROLLOVER+AIRBAG--------------------#
 Data1 <- mlogit.data(Edata, shape = "wide", choice = "COMB1")
 # fit111<-mlogit(COMB1 ~ 0|TRAV_SP+TIME+WEATHER0+MAN_COLL0+NUMOCCS0+BODY_TYP0+VNUM_LAN0+VALIGN0+
 #                            AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data1)  
 # summary(fit111)

fit111<-mlogit(COMB1 ~ 0|TRAV_SP+TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+
                  AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data1)  
summary(fit111)
AIC(fit111) # 40901.84

 #------------ROLLOVER+Ejection--------------------#
 Data2 <- mlogit.data(Edata, shape = "wide", choice = "COMB2")
 # fit222<-mlogit(COMB2 ~ 0|TRAV_SP+TIME+WEATHER0+MAN_COLL0+NUMOCCS0+BODY_TYP0+VNUM_LAN0+VALIGN0+
 #                  AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data2)   
 # 
 # summary(fit222)
 
 fit222<-mlogit(COMB2 ~ 0|TRAV_SP+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+
                  AGE0+REST_USE0+DRINKING0,data=Data2)    
 
 summary(fit222) # log -4701.7
 AIC(fit222)
 #------------AIRBAG+Ejection--------------------#
 Data3 <- mlogit.data(Edata, shape = "wide", choice = "COMB3")
 # fit333<-mlogit(COMB3 ~ 0|TRAV_SP+TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+NUMOCCS0+VNUM_LAN0+
 #                  AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data3)
 # 
 # summary(fit333)
 
 fit333<-mlogit(COMB3 ~ 0|TRAV_SP+TIME+MAN_COLL0+BODY_TYP0+VALIGN0+
                          SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data3)    
 
 summary(fit333)
 
 # options(scipen = 200)  取消科学计数法
 
 #------------ROLLOVER+AIRBAG+Ejection--------------------# 
 Data4 <- mlogit.data(Edata, shape = "wide", choice = "COMB4")
  #fit444<-mlogit(COMB4 ~ 0|TRAV_SP+TIME+WEATHER0+MAN_COLL0+NUMOCCS0+BODY_TYP0+VNUM_LAN0+VALIGN0+
  #                AGE0+SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data4)    
 
  #summary(fit444)
 
 fit444<-mlogit(COMB4 ~ 0|TRAV_SP+TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0
                  SEX0+SEAT_POS0+REST_USE0+DRINKING0+DRUGS0,data=Data4)    
 
 summary(fit444)

 ######################## 半参数加性logit回归模型 #######################


 #########################rollove ###########################

 fit<-gamlss((outcome0==1)~WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+REST_USE0+DRINKING0+cs(speed),family=BI, data=Ndata)
 
 m <- cbind(c(1,2))
 layout(m,heights=c(4,1))
 par(mar=c(0,0,0.5,0),oma=c(4,4,1,4))
 
 aa1=predict(fit1,what = "mu",type = "terms",se.fit = TRUE)

summary(aa1$fit)

 x1=Ndata$speed[order(Ndata$speed)]
 y01=aa1$fit[order(Ndata$speed),8]
 
 plot(x1,y01,ylim=c(-1.5,2),type="l",lwd=2,xlab="",ylab="",cex.axis=1,xaxt="n",yaxp=c(-1,1,2))
 
 mtext("平滑函数", side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 


 unik=!duplicated(x1)
  x1=x1[unik]
 y01=y01[unik]
 
 xp=seq(2,170,2)
 z=diff(y01)/diff(x1)

 
 par(new = TRUE)
 plot(xp,z,type="l",lwd=2,col="blue",lty=2,xaxt="n",axes = FALSE)
 axis(side=4,col="blue",col.axis="blue")

 
 lines(56,0.0295,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(58,0.028,"55km/h",col="red",cex=0.9)
  
 lines(104,0.0183,type="p",pch=1,col="red",cex=1.5,lwd=2)
 text(104,0.017,"100km/h",col="red",cex=0.9)
 
 lines(126.5,0.0215,type="p",pch=1,col="red",cex=1.5,lwd=2)
 text(126,0.023,"125km/h",col="red",cex=0.9)
 mtext("一阶导数", side = 4, line = 2.5, adj = 0.5, cex = 1,col="blue")
 
 y00=aa1$se.fit[order(Ndata$speed),8]

 plot(Ndata$speed[order(Ndata$speed)],y00,type="l",col="black",lty=1,lwd=2,yaxp=c(0.15,0.35,2))

 mtext("速度 (km/h)", side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 mtext("标准差", side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 
  
#########################airbag ###########################
 
 fit2<-gamlss((outcome1==1)~TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+SEX0+SEAT_POS0+
                           REST_USE0+DRINKING0+DRUGS0+cs(speed),family=BI, data=Ndata)
 
 m <- cbind(c(1,2))
 layout(m,heights=c(4,1))
 par(mar=c(0,0,0.5,0),oma=c(4,4,1,4))
 
 aa2=predict(fit2,what = "mu",type = "terms",se.fit = TRUE)
 # summary(aa1)
 summary(aa2$fit)
 # ss=aa1$fit
 x2=Ndata$speed[order(Ndata$speed)]
 y02=aa2$fit[order(Ndata$speed),11]
 # summary(y02)
 plot(x2,y02,ylim=c(-1.5,1.5),type="l",lwd=2,xlab="",ylab="",cex.axis=1,xaxt="n",yaxp=c(-1,1,2))
 
 
 mtext("平滑函数", side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 
 unik=!duplicated(x2)
 x2=x2[unik]
 y02=y02[unik]
 
 xp2=seq( 2,170,2)
 z2=diff(y02)/diff(x2)
 
 
 par(new = TRUE)
 plot(xp2,z2,type="l",lwd=2,col="blue",lty=2,xaxt="n",axes = FALSE)
 axis(side=4,col="blue",col.axis="blue")
 
 
 lines(45.8,0.0318,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(59,0.0315,"50km/h",col="red",cex=0.9)
 
 lines(107,0.0059,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(106,0.0075,"105km/h",col="red",cex=0.9)
 
 lines(141,0.0112,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(140,0.013,"135km/h",col="red",cex=0.9)
 mtext("一阶导数", side = 4, line = 2.5, adj = 0.5, cex = 1,col="blue")
 
 y04=aa2$se.fit[order(Ndata$speed),11]
 
 plot(Ndata$speed[order(Ndata$speed)],y04,type="l",col="black",lty=1,lwd=2,yaxp=c(0.15,0.35,2))
 
 
 mtext("速度 (km/h)", side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 mtext("标准差", side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 
 #########################ejection ###########################
 
 fit3<-gamlss((outcome2==1)~MAN_COLL0+BODY_TYP0+SEX0+SEAT_POS0+REST_USE0+cs(speed),family=BI, data=Ndata)
 
 m <- cbind(c(1,2))
 layout(m,heights=c(4,1))
 par(mar=c(0,0,0.5,0),oma=c(4,4,1,4))
 
 aa3=predict(fit3,what = "mu",type = "terms",se.fit = TRUE)
 # summary(aa1)
 summary(aa3$fit)
 # ss=aa1$fit
 x3=Ndata$speed[order(Ndata$speed)]
 y03=aa3$fit[order(Ndata$speed),6]
 summary(y03)
 plot(x3,y03,ylim=c(-1,3),type="l",lwd=2,xlab="",ylab="",cex.axis=1,xaxt="n",yaxp=c(0,2,2))
 
 
 mtext("平滑函数", side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")

 
 unik=!duplicated(x3)
 x3=x3[unik]
 y03=y03[unik]
 
 xp3=seq( 2,170,2)
 z3=diff(y03)/diff(x3)
 
 
 par(new = TRUE)
 plot(xp3,z3,type="l",lwd=2,col="blue",lty=2,xaxt="n",axes = FALSE)
 axis(side=4,col="blue",col.axis="blue")
 
 
 lines(49,0.0035,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(63,0.004,"50km/h",col="red",cex=0.9)
 
 lines(99,0.0348,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(85,0.0348,"100km/h",col="red",cex=0.9)
  
  lines(119,0.03,type="p",pch=1,col="red",cex=1.5,lwd=2)
   text(120,0.0285,"115km/h",col="red",cex=0.9)
 
 lines(140,0.036,type="p",pch=1,col="red",cex=1.5,lwd=2)
  text(155,0.036,"130km/h",col="red",cex=0.9)
 mtext("一阶导数", side = 4, line = 2.5, adj = 0.5, cex = 1,col="blue")
 
 y05=aa3$se.fit[order(Ndata$speed),6]
 summary(y05)
 
 plot(Ndata$speed[order(Ndata$speed)],y05,type="l",col="black",lty=1,lwd=2,yaxp=c(0,1,2))
 
 
 mtext("速度 (km/h)", side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 mtext("标准差", side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 #########################合成一张图 ###########################
 ######################### 平滑函数###########################
 m <- cbind(c(1,1))
 layout(m,heights=c(4,1))
 par(mar=c(0,0,0.5,0),oma=c(4,4,1,4))
 
  plot(x1,y01,ylim=c(-1.5,3),type="l",lwd=2,xlab="",ylab="",cex.axis=1,yaxp=c(-1,3,4),xaxp=c(0,180,9),col="blue")
 lines(x2,y02,ylim=c(-1.5,3),type="l",lwd=2,xlab="",ylab="",cex.axis=1,yaxp=c(-1,3,4),col="green")
 lines(x3,y03,ylim=c(-1.5,3),type="l",lwd=2,xlab="",ylab="",cex.axis=1,yaxp=c(-1,3,4),col="red")
 
 mtext("平滑函数",side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 mtext("速度 (km/h)",side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")

 
 legend("topleft",c("翻滚","安全气囊","弹射"),col=c("blue","green","red"),cex=0.85,lty=c(1,1,1),lwd=c(2,2,2),seg.len=3,box.col="blue",bty="n")
 ######################### 一阶导数 ###########################
 m <- cbind(c(1,1))
 layout(m,heights=c(4,1))
 par(mar=c(0,0,0.5,0),oma=c(4,4,1,4))
  plot(xp,z,ylim=c(0,0.04),type="l",lwd=2,col="blue",yaxp=c(0.01,0.03,2),xaxp=c(0,180,9))
 lines(xp2,z2,type="l",lwd=2,xlab="",ylab="",cex.axis=1,col="green")
 lines(xp3,z3,type="l",lwd=2,xlab="",ylab="",cex.axis=1,col="red")
 
 # lines(56,0.0295,type="p",pch=1,col="red",cex=1.5,lwd=2)
 # text(55,0.027,"55km/h",col="red",cex=0.9)
 # 
 # lines(104,0.0184,type="p",pch=1,col="red",cex=1.5,lwd=2)
 # text(104,0.017,"100km/h",col="red",cex=0.9)
 # 
 # lines(126,0.0215,type="p",pch=1,col="red",cex=1.5,lwd=2)
 # text(126,0.023,"125km/h",col="red",cex=0.9)
 mtext("一阶导数", side = 2, line = 2.5, adj = 0.5, cex = 1,col="blue")
 mtext("速度 (km/h)",side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")

  legend("topleft",c("翻滚","安全气囊","弹射"),col=c("blue","green","red"),cex=0.85,lty=c(1,1,1),lwd=c(2,2,2),seg.len=3,box.col="blue",bty="n")
 
 
 
 
 
 #########################speed*rest-use 4###############################
 #########################rollove ###########################
  
 fitA<-gamlss((outcome0==1)~WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+AGE0+DRINKING0+REST_USE0*cs(speed),family=BI, data=Ndata)
 aa4=predict(fitA,what = "mu",type = "terms",se.fit = TRUE)
 
 summary(aa4$fit)
 Ndata1=subset(Ndata,REST_USE0==0)
 Ndata2=subset(Ndata,REST_USE0==1)
 
 
 aa5=subset(aa4$fit[,7]+aa4$fit[,8]+aa4$fit[,9],Ndata$REST_USE0==0)
 aa6=subset(aa4$fit[,7]+aa4$fit[,8]+aa4$fit[,9],Ndata$REST_USE0==1)
 
 summary(aa5)
 m <- cbind(c(1,1))
 layout(m,heights=c(4,1))
 par(mar=c(0,0,0.5,0),oma=c(4,4,1,4))
 plot(Ndata1$speed[order(Ndata1$speed)],aa5[order(Ndata1$speed)],type="l",lty=1,ylim=c(-1.5,2.5),
      col="black",lwd=2,cex.axis=1,yaxt="n",cex.lab=1)
 
 axis(2,yaxp=c(-1,2,4),cex.axis=1)
 
 lines(Ndata2$speed[order(Ndata2$speed)],aa6[order(Ndata2$speed)],type="l",lty=2,col="black", lwd=2)
 
 legend("topleft",c("系安全带","不系安全带"),col=c("black","black"),cex=0.95,lty=c(1,2),lwd=c(2,2),seg.len=3,box.col="blue",bty="n")
 mtext("平滑函数",side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 mtext("速度 (km/h)",side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 #########################airbag ###########################
 
 fitB<-gamlss((outcome1==1)~TIME+WEATHER0+MAN_COLL0+BODY_TYP0+VALIGN0+SEX0+SEAT_POS0+
                DRINKING0+DRUGS0+REST_USE0*cs(speed),family=BI, data=Ndata)
 
 aa7=predict(fitB,what = "mu",type = "terms",se.fit = TRUE)
 
 summary(aa7$fit)
 Ndata1=subset(Ndata,REST_USE0==0)
 Ndata2=subset(Ndata,REST_USE0==1)
 summary(aa7)
 
 aa8=subset(aa7$fit[,10]+aa7$fit[,11]+aa7$fit[,12],Ndata$REST_USE0==0)
 aa9=subset(aa7$fit[,10]+aa7$fit[,11]+aa7$fit[,12],Ndata$REST_USE0==1)
 # summary(aa8)
 

 plot(Ndata1$speed[order(Ndata1$speed)],aa8[order(Ndata1$speed)],type="l",lty=1,ylim=c(-1.5,2),
      col="black",lwd=2,cex.axis=1,yaxt="n",cex.lab=1)
 
 axis(2,yaxp=c(-1,1,2),cex.axis=1)
 
 lines(Ndata2$speed[order(Ndata2$speed)],aa9[order(Ndata2$speed)],type="l",lty=2,col="black", lwd=2)
 
 legend("topleft",c("系安全带性","不系安全带"),col=c("black","black"),cex=0.95,lty=c(1,2),lwd=c(2,2),seg.len=3,box.col="blue",bty="n")
 mtext("平滑函数",side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 mtext("速度 (km/h)",side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 #########################ejection ###########################
 fitC<-gamlss((outcome2==1)~MAN_COLL0+BODY_TYP0+SEX0+SEAT_POS0+REST_USE0*cs(speed),family=BI, data=Ndata)
 
 aa10=predict(fitC,what = "mu",type = "terms",se.fit = TRUE)
 
 # summary(aa10$fit)
 Ndata1=subset(Ndata,REST_USE0==0)
 Ndata2=subset(Ndata,REST_USE0==1)
 summary(aa10)
 
 aa11=subset(aa10$fit[,5]+aa10$fit[,6]+aa10$fit[,7],Ndata$REST_USE0==0)
 aa12=subset(aa10$fit[,5]+aa10$fit[,6]+aa10$fit[,7],Ndata$REST_USE0==1)
 # summary(aa12)
 
 plot(Ndata1$speed[order(Ndata1$speed)],aa11[order(Ndata1$speed)],type="l",lty=1,ylim=c(-0.5,1.5),
      col="black",lwd=2,cex.axis=1,yaxt="n",cex.lab=1,xlab="",ylab="")
 
 axis(2,yaxp=c(0,1,1),cex.axis=1)
 
 par(new = TRUE)
 plot(Ndata2$speed[order(Ndata2$speed)],aa12[order(Ndata2$speed)],type="l",lty=2,col="blue", lwd=2,xaxt="n",xlab="",ylab="",axes = FALSE)

 axis(side=4,col="blue",col.axis="blue")
 

 # lines(Ndata2$speed[order(Ndata2$speed)],aa12[order(Ndata2$speed)],type="l",lty=2,col="black", lwd=2)
 
 legend("bottomright",c("系安全带性","不系安全带"),col=c("black","blue"),cex=0.95,lty=c(1,2),lwd=c(2,2),seg.len=3,box.col="blue",bty="n")
 mtext("平滑整数",side = 2, line = 2.5, adj = 0.5, cex = 1,col="black")
 mtext("速度 (km/h)",side = 1, line = 2.5, adj = 0.5, cex = 1,col="black")
 
 
 
 
 #######my test ########
 
 treat.eq <- import ~ coop + cost
 out.eq <- export ~ cost + target
 f.list <- list(treat.eq, out.eq)
 mr <- c("probit", "probit")
 library(GJRM)
 bvp <- gjrm(f.list, data=sanction, Model="B", margins=mr)
 summary(bvp)
 
 ###########VGAM###########
 library(VGAM)
 data(gew)
 Hlist <- list("(Intercept)" = diag(2),
               "capital.g" = rbind(1,0),
               "value.g" = rbind(1,0),
               "capital.w" = rbind(0,1),
               "value.w" = rbind(0,1))
 
zef <- vglm(cbind(invest.g, invest.w) ~ capital.g + value.g + capital.w + value.w,
            SURff(divisor = "sqrt"), data = gew,
            constraints = Hlist, maxit = 1, epsilon = 1e-11)
summary(zef)