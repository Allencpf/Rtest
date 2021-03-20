install.packages('janitor')
install.packages('stargazer') # stargazer可以整理R输出为LaTeX, HTML以及ASCII三种格式
install.packages("xtable")
install.packages("flextable")
install.packages("VGAMdata")

###########library############
library(readr)
library(dplyr)
library(janitor)
library(stargazer)
library(texreg)
library(xtable)
library(flextable)
library(officer)
library(VGAMdata)

Mdata <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/data/Output_fars/dftwo0226.csv")

##########data process#########
attach(Mdata)

Mdata<-subset(Mdata,INJ_SEV<5 & INJ_SEV_<5)                   #table(Mdata$ROLLOVER) 
Mdata<-subset(Mdata,ROLLOVER_x<9 & ROLLOVER_x_<9)                   #table(Mdata$ROLLOVER)         
Mdata<-subset(Mdata,AIR_BAG>0 & AIR_BAG<28 & AIR_BAG_>0 & AIR_BAG_<28)       #table(Mdata$AIR_BAG)
#Mdata<-subset (Mdata,EJECTION<7 & EJECTION_<7)                   #table(Mdata$EJECTION)
#Mdata<-subset(Mdata,TRAV_SP>0 & TRAV_SP<997)       #table(Mdata$TRAV_SP) 数据太少了
#Mdata<-subset(Mdata,TRAV_SP_>0 & TRAV_SP_<997)
#### PERSON
Mdata<-subset(Mdata,AGE<998 & AGE_<998)                      #table(Mdata$AGE)
Mdata<-subset(Mdata,SEX<8 & SEX_<8)                        #table(Mdata$SEX)
Mdata<-subset(Mdata,REST_USE<98& REST_USE>0 & REST_USE_<98 & REST_USE_>0)      #table(Mdata$REST_USE)
#Mdata<-subset(Mdata,DRINKING<8 & DRINKING2<8)                   #table(Mdata$DRINKING)
#Mdata<-subset(Mdata,DRUGS<8 & DRUGS2<8)                      #table(Mdata$DRUGS)
Mdata<-subset(Mdata,HOSPITAL<9 & HOSPITAL_<9)                        # 送入医院的方式
#### ACCIDENT
Mdata<-subset(Mdata,HOUR<99 & HOUR_<99)                                #table(Mdata$HOUR)
#Mdata<-subset(Mdata,WEATHER>0 & WEATHER<11)     #table(Mdata$WEATHER) 
Mdata<-subset(Mdata,MAN_COLL<11)                  #table(Mdata$MAN_COLL)
#Mdata<-subset(Mdata,NUMOCCS<99)                   #table(Mdata$NUMOCCS) a count of the number of occupants in this vehicle.
#Mdata<-subset(Mdata,BODY_TYP_x<80 & BODY_TYP_x_<80)                  #table(Mdata$BODY_TYP)
#Mdata<-subset(Mdata,BODY_TYP<50 | BODY_TYP>59)
#Mdata<-subset(Mdata,BODY_TYP_<50|BODY_TYP_>59)
#Mdata<-subset(Mdata,VNUM_LAN>0 & VNUM_LAN<8)       #table(Mdata$VNUM_LAN) 车道数
Mdata<-subset(Mdata,VALIGN>0 & VALIGN<8 & VALIGN_ >0 & VALIGN_<8)                     #table(Mdata$VALIGN) 车辆行动状态（直行/左转/右转）

Mdata<-subset(Mdata,RUR_URB_x <9 ) ## 道路类型， 1：乡村、2：城市
#Mdata<-subset(Mdata,NHS <9 ) ## 国道系统， 1：乡村、2：城市
Mdata<-subset(Mdata,LGT_COND<7)                  #table(Mdata$光照条件
#Mdata<-subset(Mdata,SEAT_POS<98)                  #table(Mdata$SEAT_POS)
# ARR_HOUR 

#Mdata<-subset(Mdata,INJ_SEV<=4 & INJ_SEV2<=4)

Mdata$TIME<-ifelse((Mdata$HOUR>=6 & Mdata$HOUR<=18),1,0)##1表示白天，0表示夜晚

Mdata1 <- select(Mdata, 
                 c(INJ_SEV,AGE,SEX,REST_USE,DR_DRINK,DRUGS,AIR_BAG,EJECTION,EXTRICAT,DOA,DEATH_TM,DEATH_HR,DEATH_MN,LAG_HRS,LAG_MINS,WORK_INJ,HOSPITAL,DEATH_MO,
                   ROLLOVER_x,L_RESTRI, CDL_STAT,PREV_ACC, PREV_SPD, PREV_OTH, FIRST_YR,LAST_YR,SPEEDREL,VPAVETYP,OWNER,MOD_YEAR_x,GVWR,TRAV_SP,FIRE_EXP_x,L_STATUS,BODY_TYP_x,
                   INJ_SEV_,AGE_,SEX_,REST_USE_,DR_DRINK_,DRUGS_,AIR_BAG_,EJECTION_,EXTRICAT_,DOA_,DEATH_TM_,DEATH_HR_,DEATH_MN_,LAG_HRS_,LAG_MINS_,WORK_INJ_,HOSPITAL_,DEATH_MO_,
                   ROLLOVER_x_,L_RESTRI_,CDL_STAT_,PREV_ACC_,PREV_SPD_,PREV_OTH_,FIRST_YR_,LAST_YR_,SPEEDREL_,VPAVETYP_,OWNER_,MOD_YEAR_x_,GVWR_,TRAV_SP_,FIRE_EXP_x_,L_STATUS_,BODY_TYP_x_,
                   unicid,TYP_INT,FUNC_SYS_x,VSPD_LIM,LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL,VPAVETYP,VSURCOND,ACC_TYPE))




#####accident-level########


Mdata1$TIME<-ifelse((Mdata1$TIME ==1),0,1)##0表示白天，1表示夜晚
Mdata1$TIME=factor(Mdata1$TIME,levels = c(0,1))
#-----------天气---------------------#
Mdata1<-subset(Mdata1,WEATHER>0 & WEATHER<11)     #table(Mdata1$WEATHER)
Mdata1<-subset(Mdata1,WEATHER!=6 & WEATHER!=7 & WEATHER != 8)                                #table(Mdata1$WEATHER)

WEATHER0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata)){
  if      (Mdata1$WEATHER[i]==1)                    {WEATHER0[i]=0} #*Clear
  else if (Mdata1$WEATHER[i]==2)                      {WEATHER0[i]=2} #Rain 
  else if (Mdata1$WEATHER[i]==4 | Mdata1$WEATHER[i]==3) {WEATHER0[i]=3} #Snow
  else                                                {WEATHER0[i]=1} #fog / cloud
}

Mdata1$WEATHER<-WEATHER0
rm(WEATHER0)
# table(Mdata$WEATHER0)
Mdata1$WEATHER=factor(Mdata1$WEATHER,levels = c(0,1,2,3))

#-------碰撞方式----------#
Mdata1<-subset(Mdata1,MAN_COLL<10)                  #table(Mdata1$MAN_COLL)

MAN_COLL0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if      (Mdata1$MAN_COLL[i]==6) {MAN_COLL0[i]=0} #Angle
  else if (Mdata1$MAN_COLL[i]==2) {MAN_COLL0[i]=1} #Front-to-Front
  else if (Mdata1$MAN_COLL[i]==1) {MAN_COLL0[i]=2} #Front to rear
  else                            {MAN_COLL0[i]=3} #Sideswipe
}

Mdata1$MAN_COLL<-MAN_COLL0
rm(MAN_COLL0)
# table(Mdata1$MAN_COLL)
Mdata1$MAN_COLL=factor(Mdata1$MAN_COLL,levels = c(0,1,2,3))
#####person-level######
#------------年龄--------------------#
Mdata1<-subset(Mdata1,AGE<=120 & AGE_ <= 120)                  #table(Mdata1$AGE)

AGE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata)){
  if (Mdata1$AGE[i]>0   & Mdata1$AGE[i]<25){AGE0[i]=2} # Young
  else if (Mdata1$AGE[i]>=25 & Mdata1$AGE[i]<=30){AGE0[i]=3} # Adult 25-30
  else if (Mdata1$AGE[i]>=31 & Mdata1$AGE[i]<=60){AGE0[i]=0} #  31 - 60
  else {AGE0[i]=1} #>=61	
}

Mdata1$AGE1<-AGE0
rm(AGE0)
# table(Mdata1$AGE)
Mdata1$AGE1=factor(Mdata1$AGE1,levels = c(0,1,2,3))
#### INJ_SEV2
AGE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata)){
  if (Mdata1$AGE_[i]>0   & Mdata1$AGE_[i]<25){AGE0[i]=2} # Young
  else if (Mdata1$AGE_[i]>=25 & Mdata1$AGE_[i]<=30){AGE0[i]=3} # Adult 25-30
  else if (Mdata1$AGE_[i]>=31 & Mdata1$AGE_[i]<=60){AGE0[i]=0} #  31 - 60
  else {AGE0[i]=1} #>=61	
}

Mdata1$AGE1_<-AGE0
rm(AGE0)
# table(Mdata1$AGE)
Mdata1$AGE1_=factor(Mdata1$AGE1_,levels = c(0,1,2,3))
#-----------性别--------------------#
Mdata1<-subset(Mdata1,SEX < 3 & SEX_ < 3)                  #table(Mdata1$SEX)

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
#------------安全带--------------------#
Mdata1<-subset(Mdata1,REST_USE < 21 & REST_USE_<21)       #table(Mdata$REST_USE)


REST_USE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$REST_USE[i]<= 15){REST_USE0[i]=1} #*Used
  else {REST_USE0[i]=0} #Not Used
}

Mdata1$REST_USE<-REST_USE0
rm(REST_USE0)
# table(Mdata1$REST_USE0)
Mdata1$REST_USE=factor(Mdata1$REST_USE,levels = c(0,1))

###INJ_SEV2
REST_USE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$REST_USE_[i]<= 15){REST_USE0[i]=1} #*Used
  else {REST_USE0[i]=0} #Not Used
}

Mdata1$REST_USE_<-REST_USE0
rm(REST_USE0)
# table(Mdata1$REST_USE0)
Mdata1$REST_USE_=factor(Mdata1$REST_USE_,levels = c(0,1))

#-----------酒驾---------------------#

Mdata1$DR_DRINK=factor(Mdata1$DR_DRINK,levels = c(0,1))

### INJ_SEV2
Mdata1$DR_DRINK_=factor(Mdata1$DR_DRINK_,levels = c(0,1))




#-----------DIE 是否死亡---------------------#
Mdata1<-subset (Mdata1,DEATH_MO<99 & DEATH_MO_<99)                   #table(Mdata$DEATH)

subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DEATH_MO[i]==88) {subcrit2[i]=0} #Non - fatal
  else  {subcrit2[i]=1} #died 
}
Mdata1$DIE<-subcrit2
rm(subcrit2)
Mdata1$DIE=factor(Mdata1$DIE,levels = c(0,1))
###INJ_SEV2
subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DEATH_MO_[i]==88) {subcrit2[i]=0} #Non - fatal
  else  {subcrit2[i]=1} #died 
}
Mdata1$DIE_<-subcrit2
rm(subcrit2)
Mdata1$DIE_=factor(Mdata1$DIE_,levels = c(0,1))
#####vehicle-level############
#-----------车辆类型---------------------#
Mdata1<-subset(Mdata1,BODY_TYP_x<80 & BODY_TYP_x_<80)                  #table(Mdata$BODY_TYP)
Mdata1<-subset(Mdata1,BODY_TYP_x<50 | BODY_TYP_x>59)
Mdata1<-subset(Mdata1,BODY_TYP_x_<50|BODY_TYP_x_>59)

BODY_TYP0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$BODY_TYP_x[i]<=13)                      {BODY_TYP0[i]=0} #*AUTOMOBILES
  else if (Mdata1$BODY_TYP_x[i]>=14 &Mdata1$BODY_TYP_x[i]<=19){BODY_TYP0[i]=1} #UTILITY VEHICLES
  else if (Mdata1$BODY_TYP_x[i]>19 &Mdata1$BODY_TYP_x[i]<=40){BODY_TYP0[i]=2} # 货车 皮卡
  else                                                          {BODY_TYP0[i]=3} #Trucks
}

Mdata1$BODY_TYP<-BODY_TYP0
rm(BODY_TYP0)
# table(Mdata1$BODY_TYP0)
Mdata1$BODY_TYP=factor(Mdata1$BODY_TYP,levels = c(0,1,2,3))
###INJ2
BODY_TYP0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$BODY_TYP_x_[i]<=13)                      {BODY_TYP0[i]=0} #*AUTOMOBILES
  else if (Mdata1$BODY_TYP_x_[i]>=14 &Mdata1$BODY_TYP_x_[i]<=19){BODY_TYP0[i]=1} #UTILITY VEHICLES
  else if (Mdata1$BODY_TYP_x_[i]>19 &Mdata1$BODY_TYP_x_[i]<=40){BODY_TYP0[i]=2} # 货车 皮卡
  else                                                          {BODY_TYP0[i]=3} #Trucks
}

Mdata1$BODY_TYP_<-BODY_TYP0
rm(BODY_TYP0)
# table(Mdata1$BODY_TYP0)
Mdata1$BODY_TYP_=factor(Mdata1$BODY_TYP_,levels = c(0,1,2,3))

#-----------rollover 车辆翻滚--------------------#

SEX0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$ROLLOVER_x[i]==0){SEX0[i]=0} # no rollover
  else {SEX0[i]=1} #rollover
}

Mdata1$ROLLOVER<-SEX0
rm(SEX0)
# table(Mdata1$SEX0)
Mdata1$ROLLOVER=factor(Mdata1$ROLLOVER,levels = c(0,1))
### INJ_SEV2
SEX0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$ROLLOVER_x_[i]==0){SEX0[i]=0} # no rollover
  else {SEX0[i]=1} #rollover
}

Mdata1$ROLLOVER_<-SEX0
rm(SEX0)
# table(Mdata1$SEX0)
Mdata1$ROLLOVER_=factor(Mdata1$ROLLOVER_,levels = c(0,1))


#######VGAM#########

# this is codeed in 0320
library(VGAM)
library(VGAMdata)
fit.npom <- vglm(INJ_SEV ~ REST_USE, cumulative(parallel = TRUE), data= Mdata1)

fnc <- transform(wffc.nc, finame = factor(iname), fsector = factor(sector),
                 fday = factor(ceiling(session/2)), mornaft = 1 - (session%%2),
                 fbeatboat = factor(beatboat))

m <- with(fnc, !is.element(comid,c(99,72,80,93, 45, 71, 97, 78))) ## 去除掉cmoid列中包含那些数字的行
fnc <- fnc[m,]

fnc <- transform(fnc, ordnum = ifelse(numbers <=2, "few", ifelse(numbers <=10, "more", "most")))

fnc$ordnum <- ordered(fnc$ordnum, levels = c("few", "more", "most"))

### fit a proportional odds model
fit.pom <- vglm(ordnum ~ fsector + mornaft + fday + finame, 
                family = cumulative(parallel = TRUE, reverse = TRUE), data=fnc)
### 检查拟合结果是和原数据对应正确的
head(fit.pom@y, 3)
colSums(fit.pom@y)