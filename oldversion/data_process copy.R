install.packages('janitor')
install.packages('stargazer') # stargazer可以整理R输出为LaTeX, HTML以及ASCII三种格式
install.packages("xtable")
install.packages("flextable")
install.packages("margins")
###########library############
library(readr)
library(dplyr)
library(janitor)
library(stargazer)
library(texreg)
library(xtable)
library(flextable)
library(officer)
library(margins)
library(VGAM)
#############import data 0225 ###########
#$p19 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2019NationalCSV/Person.CSV"))
#a19 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2019NationalCSV/accident.CSV"))
#v19 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2019NationalCSV/vehicle.CSV"))
#p18 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2018NationalCSV/Person.CSV"))
#a18 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2018NationalCSV/accident.CSV"))
#v18 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2018NationalCSV/vehicle.CSV"))
#p17 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2017NationalCSV/Person.CSV"))
#a17 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2017NationalCSV/accident.CSV"))
#v17 <- data.frame(read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/National/FARS2017NationalCSV/vehicle.CSV"))

#p79 <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/Vscode/data/Output_fars/p17-19.csv")
#v79 <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/Vscode/data/Output_fars/v17-19.csv")
#a79 <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/Vscode/data/Output_fars/a17-19.csv")

pv <- left_join(p79,v79, by=c("ST_CASE","VEH_NO"), all.x=TRUE)
pva <- left_join(a79, pv, by ="ST_CASE", all.x=TRUE)
compare_df_cols(p79,v79)

pva <- subset(pva,PER_TYP==1 & MAN_COLL != 0 & VE_TOTAL == 2)

pva %>%
  group_by(ST_CASE, VEH_NO) %>%
  filter(row_number() == 1) %>%
  ungroup()

############import data 0226###########
Odata <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/Vscode/data/Output_fars/dftwo0226.csv")
Mdata <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/Vscode/data/Output_fars/dftwo0226.csv")

##########data process#########
attach(Mdata)
#### VEHICLE 
Mdata<-subset(Mdata,ROLLOVER_x<9 & ROLLOVER_x_<9)                   #table(Mdata$ROLLOVER)         
#Mdata<-subset(Mdata,AIR_BAG>0 & AIR_BAG<28 & AIR_BAG_>0 & AIR_BAG_<28)       #table(Mdata$AIR_BAG)
#Mdata<-subset (Mdata,EJECTION<7 & EJECTION_<7)                   #table(Mdata$EJECTION)
#Mdata<-subset(Mdata,TRAV_SP>0 & TRAV_SP<997)       #table(Mdata$TRAV_SP) 数据太少了
#Mdata<-subset(Mdata,TRAV_SP_>0 & TRAV_SP_<997)
#### PERSON
#Mdata<-subset(Mdata,AGE<998 & AGE_<998)                      #table(Mdata$AGE)
#Mdata<-subset(Mdata,SEX<8 & SEX_<8)                        #table(Mdata$SEX)
#Mdata<-subset(Mdata,REST_USE<98& REST_USE>0 & REST_USE_<98 & REST_USE_>0)      #table(Mdata$REST_USE)
#Mdata<-subset(Mdata,DRINKING<8 & DRINKING2<8)                   #table(Mdata$DRINKING)
#Mdata<-subset(Mdata,DRUGS<8 & DRUGS2<8)                      #table(Mdata$DRUGS)
#Mdata<-subset(Mdata,HOSPITAL<9 & HOSPITAL_<9)                        # 送入医院的方式
#### ACCIDENT
#Mdata<-subset(Mdata,HOUR<99 & HOUR_<99)                                #table(Mdata$HOUR)
#Mdata<-subset(Mdata,WEATHER>0 & WEATHER<11)     #table(Mdata$WEATHER) 
#Mdata<-subset(Mdata,MAN_COLL<11)                  #table(Mdata$MAN_COLL)
#Mdata<-subset(Mdata,NUMOCCS<99)                   #table(Mdata$NUMOCCS) a count of the number of occupants in this vehicle.
#Mdata<-subset(Mdata,BODY_TYP_x<80 & BODY_TYP_x_<80)                  #table(Mdata$BODY_TYP)
#Mdata<-subset(Mdata,BODY_TYP<50 | BODY_TYP>59)
#Mdata<-subset(Mdata,BODY_TYP_<50|BODY_TYP_>59)
#Mdata<-subset(Mdata,VNUM_LAN>0 & VNUM_LAN<8)       #table(Mdata$VNUM_LAN) 车道数
#Mdata<-subset(Mdata,VALIGN>0 & VALIGN<8)                     #table(Mdata$VALIGN) 车辆行动状态（直行/左转/右转）

#Mdata<-subset(Mdata,RUR_URB_x <9 ) ## 道路类型， 1：乡村、2：城市
#Mdata<-subset(Mdata,NHS <9 ) ## 国道系统， 1：乡村、2：城市
#Mdata<-subset(Mdata,LGT_COND<7)                  #table(Mdata$光照条件
#Mdata<-subset(Mdata,SEAT_POS<98)                  #table(Mdata$SEAT_POS)
# ARR_HOUR 

#Mdata<-subset(Mdata,INJ_SEV<=4 & INJ_SEV2<=4)



attach(Mdata) # attach 之后就可以直接用Mdata里的变量名字了
Mdata$TIME<-ifelse((Mdata$HOUR>=6 & Mdata$HOUR<=18),1,0)##1表示白天，0表示夜晚
Mdata1=data.frame(INJ_SEV,AGE,SEX,REST_USE,DR_DRINK,DRUGS,AIR_BAG,EJECTION,EXTRICAT,DOA,DEATH_TM,DEATH_HR,DEATH_MN,LAG_HRS,LAG_MINS,WORK_INJ,HOSPITAL,DEATH_MO,
                  ROLLOVER_x,L_RESTRI, CDL_STAT,PREV_ACC, PREV_SPD, PREV_OTH, FIRST_YR,LAST_YR,SPEEDREL,VPAVETYP,OWNER,MOD_YEAR_x,GVWR,TRAV_SP,FIRE_EXP_x,L_STATUS,BODY_TYP_x,
                  INJ_SEV_,AGE_,SEX_,REST_USE_,DR_DRINK_,DRUGS_,AIR_BAG_,EJECTION_,EXTRICAT_,DOA_,DEATH_TM_,DEATH_HR_,DEATH_MN_,LAG_HRS_,LAG_MINS_,WORK_INJ_,HOSPITAL_,DEATH_MO_,
                  ROLLOVER_x_,L_RESTRI_,CDL_STAT_,PREV_ACC_,PREV_SPD_,PREV_OTH_,FIRST_YR_,LAST_YR_,SPEEDREL_,VPAVETYP_,OWNER_,MOD_YEAR_x_,GVWR_,TRAV_SP_,FIRE_EXP_x_,L_STATUS_,BODY_TYP_x_,
                  unicid,TYP_INT,FUNC_SYS_x,VSPD_LIM,LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL,VPAVETYP,VSURCOND,ACC_TYPE)
Mdata2=data.frame(INJ_SEV,AGE,SEX,REST_USE,DR_DRINK,DRUGS,AIR_BAG,EJECTION,EXTRICAT,DOA,DEATH_TM,DEATH_HR,DEATH_MN,LAG_HRS,LAG_MINS,WORK_INJ,HOSPITAL,DEATH_MO,
                  ROLLOVER_x,L_RESTRI, CDL_STAT,PREV_ACC, PREV_SPD, PREV_OTH, FIRST_YR,LAST_YR,SPEEDREL,VPAVETYP,OWNER,MOD_YEAR_x,GVWR,TRAV_SP,FIRE_EXP_x,L_STATUS,BODY_TYP_x,
                  INJ_SEV_,AGE_,SEX_,REST_USE_,DR_DRINK_,DRUGS_,AIR_BAG_,EJECTION_,EXTRICAT_,DOA_,DEATH_TM_,DEATH_HR_,DEATH_MN_,LAG_HRS_,LAG_MINS_,WORK_INJ_,HOSPITAL_,DEATH_MO_,
                  ROLLOVER_x_,L_RESTRI_,CDL_STAT_,PREV_ACC_,PREV_SPD_,PREV_OTH_,FIRST_YR_,LAST_YR_,SPEEDREL_,VPAVETYP_,OWNER_,MOD_YEAR_x_,GVWR_,TRAV_SP_,FIRE_EXP_x_,L_STATUS_,BODY_TYP_x_,
                  unicid,TYP_INT,FUNC_SYS_x,VSPD_LIM,LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL,VPAVETYP,VSURCOND,ACC_TYPE)
Mdata1 <- select(Mdata, 
              c(INJ_SEV,AGE,SEX,REST_USE,DR_DRINK,DRUGS,AIR_BAG,EJECTION,EXTRICAT,DOA,DEATH_TM,DEATH_HR,DEATH_MN,LAG_HRS,LAG_MINS,WORK_INJ,HOSPITAL,DEATH_MO,
                ROLLOVER_x,L_RESTRI, CDL_STAT,PREV_ACC, PREV_SPD, PREV_OTH, FIRST_YR,LAST_YR,SPEEDREL,VPAVETYP,OWNER,MOD_YEAR_x,GVWR,TRAV_SP,FIRE_EXP_x,L_STATUS,BODY_TYP_x,
                INJ_SEV_,AGE_,SEX_,REST_USE_,DR_DRINK_,DRUGS_,AIR_BAG_,EJECTION_,EXTRICAT_,DOA_,DEATH_TM_,DEATH_HR_,DEATH_MN_,LAG_HRS_,LAG_MINS_,WORK_INJ_,HOSPITAL_,DEATH_MO_,
                ROLLOVER_x_,L_RESTRI_,CDL_STAT_,PREV_ACC_,PREV_SPD_,PREV_OTH_,FIRST_YR_,LAST_YR_,SPEEDREL_,VPAVETYP_,OWNER_,MOD_YEAR_x_,GVWR_,TRAV_SP_,FIRE_EXP_x_,L_STATUS_,BODY_TYP_x_,
                unicid,TYP_INT,FUNC_SYS_x,VSPD_LIM,LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL,VPAVETYP,VSURCOND,ACC_TYPE))
Mdata2 <- select(Mdata, 
                 c(INJ_SEV,AGE,SEX,REST_USE,DR_DRINK,DRUGS,AIR_BAG,EJECTION,EXTRICAT,DOA,DEATH_TM,DEATH_HR,DEATH_MN,LAG_HRS,LAG_MINS,WORK_INJ,HOSPITAL,DEATH_MO,
                   ROLLOVER_x,L_RESTRI, CDL_STAT,PREV_ACC, PREV_SPD, PREV_OTH, FIRST_YR,LAST_YR,SPEEDREL,VPAVETYP,OWNER,MOD_YEAR_x,GVWR,TRAV_SP,FIRE_EXP_x,L_STATUS,BODY_TYP_x,
                   INJ_SEV_,AGE_,SEX_,REST_USE_,DR_DRINK_,DRUGS_,AIR_BAG_,EJECTION_,EXTRICAT_,DOA_,DEATH_TM_,DEATH_HR_,DEATH_MN_,LAG_HRS_,LAG_MINS_,WORK_INJ_,HOSPITAL_,DEATH_MO_,
                   ROLLOVER_x_,L_RESTRI_,CDL_STAT_,PREV_ACC_,PREV_SPD_,PREV_OTH_,FIRST_YR_,LAST_YR_,SPEEDREL_,VPAVETYP_,OWNER_,MOD_YEAR_x_,GVWR_,TRAV_SP_,FIRE_EXP_x_,L_STATUS_,BODY_TYP_x_,
                   unicid,TYP_INT,FUNC_SYS_x,VSPD_LIM,LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL,VPAVETYP,VSURCOND,ACC_TYPE))
detach(Mdata)

##########变量处理########## 
##########因变量处理########## 
Mdata1<-subset(Mdata1,INJ_SEV<5 & INJ_SEV_<5)     #table(Mdata1$WEATHER)
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

Mdata1$AGE<-AGE0
rm(AGE0)
# table(Mdata1$AGE)
Mdata1$AGE=factor(Mdata1$AGE,levels = c(0,1,2,3))
#### INJ_SEV2
AGE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata)){
  if (Mdata1$AGE_[i]>0   & Mdata1$AGE_[i]<25){AGE0[i]=2} # Young
  else if (Mdata1$AGE_[i]>=25 & Mdata1$AGE_[i]<=30){AGE0[i]=3} # Adult 25-30
  else if (Mdata1$AGE_[i]>=31 & Mdata1$AGE_[i]<=60){AGE0[i]=0} #  31 - 60
  else {AGE0[i]=1} #>=61	
}

Mdata1$AGE_<-AGE0
rm(AGE0)
# table(Mdata1$AGE)
Mdata1$AGE_=factor(Mdata1$AGE_,levels = c(0,1,2,3))
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
#-----------安全气囊---------------------#
Mdata1<-subset(Mdata1,AIR_BAG>0 & AIR_BAG<28 & AIR_BAG_>0 & AIR_BAG_<28)       #table(Mdata$AIR_BAG)

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
#------------安全带--------------------#
Mdata1<-subset(Mdata1,REST_USE < 21 & REST_USE_<21)       #table(Mdata$REST_USE)


REST_USE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$REST_USE[i]<= 15){REST_USE0[i]=0} #*Used
  else {REST_USE0[i]=1} #Not Used
}

Mdata1$REST_USE<-REST_USE0
rm(REST_USE0)
# table(Mdata1$REST_USE0)
Mdata1$REST_USE=factor(Mdata1$REST_USE,levels = c(0,1))

###INJ_SEV2
REST_USE0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$REST_USE_[i]<= 15){REST_USE0[i]=0} #*Used
  else {REST_USE0[i]=1} #Not Used
}

Mdata1$REST_USE_<-REST_USE0
rm(REST_USE0)
# table(Mdata1$REST_USE0)
Mdata1$REST_USE_=factor(Mdata1$REST_USE_,levels = c(0,1))

#-----------酒驾---------------------#

Mdata1$DR_DRINK=factor(Mdata1$DR_DRINK,levels = c(0,1))

### INJ_SEV2
Mdata1$DR_DRINK_=factor(Mdata1$DR_DRINK_,levels = c(0,1))

#--------------吃药------------------#
Mdata1<-subset(Mdata1,DRUGS<9 & DRUGS_<9)       #table(Mdata$DRUGS)

DRUGS0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DRUGS[i]==1){DRUGS0[i]=1} #yes
  else {DRUGS0[i]=0} #no
}

Mdata1$DRUGS<-DRUGS0
rm(DRUGS0)
# table(Mdata1$DRUGS)
Mdata1$DRUGS=factor(Mdata1$DRUGS,levels = c(0,1))

### INJ_SEV2
DRUGS0=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DRUGS_[i]==1){DRUGS0[i]=1} #Yes
  else {DRUGS0[i]=0} #no
}

Mdata1$DRUGS_<-DRUGS0
rm(DRUGS0)
# table(Mdata1$DRUGS)
Mdata1$DRUGS_=factor(Mdata1$DRUGS_,levels = c(0,1))

#-----------弹射---------------------#
Mdata1<-subset (Mdata1,EJECTION<7 & EJECTION_<7)                   #table(Mdata$EJECTION)

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
#-----------EXTRICAT是否使用设备脱离汽车---------------------#
Mdata1<-subset (Mdata1,EXTRICAT<9 & EXTRICAT_<9)             #table(Mdata1$EXTRICAT)

Mdata1$EXTRICAT=factor(Mdata1$EXTRICAT,levels = c(0,1))
###INJ_SEV2
Mdata1$EXTRICAT_=factor(Mdata1$EXTRICAT_,levels = c(0,1))

#-----------DOA 是否死于途中---------------------#
Mdata1<-subset (Mdata1,DOA<9 & DOA_<9)                   #table(Mdata$EJECTION)

subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DOA[i]==0) {subcrit2[i]=0} #*Not applicable
  else if (Mdata1$DOA[i]==7) {subcrit2[i]=1} #*died at scene
  else  {subcrit2[i]=2} #died at route
}
Mdata1$DOA<-subcrit2
rm(subcrit2)
Mdata1$DOA=factor(Mdata1$DOA,levels = c(0,1,2))
###INJ_SEV2
subcrit2=matrix(0,nrow(Mdata1),1)
for (i in 1:nrow(Mdata1)){
  if (Mdata1$DOA_[i]==0) {subcrit2[i]=0} #*Not applicable
  else if (Mdata1$DOA_[i]==7) {subcrit2[i]=1} #*died at scene
  else  {subcrit2[i]=2} #died at route
}
Mdata1$DOA_<-subcrit2
rm(subcrit2)
Mdata1$DOA_=factor(Mdata1$DOA_,levels = c(0,1,2))

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
#-----------MOD_YEAR 车辆翻滚--------------------#
Mdata1$MOD_YEAR<-ifelse((Mdata1$MOD_YEAR_x < 2010),0,1)##
Mdata1$MOD_YEAR_<-ifelse((Mdata1$MOD_YEAR_x_ < 2010),0,1)##


#############数据/统计/图表output##############
### 数据
write.csv(
  Mdata1, file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/data/Output_fars/outputr/0315Mdata1.csv", 
  fileEncoding = "UTF-8" #导出文件编码
)

### 统计结果
capture.output(summary(Mdata1), file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/writting/output/Mdata1.txt" )
capture.output(AIC(bpN, bpF, bpC0, bpC180, bpG0, bpG180), file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/writting/output/AIC.txt" )
capture.output(AIC(bpN1, bpF1, bpC01, bpC1801, bpG01, bpG1801), file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/writting/output/AIC1.txt" )
capture.output(summary(bpN), file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/writting/output/bpN.txt" )
capture.output(summary(bpC01), file = "/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/writting/output/bpC01.txt" )

#### INJ he INj2 的列联表
ll <- table(Mdata1$INJ_SEV,Mdata1$INJ_SEV_)
tt <- xtable_to_flextable(xtable(addmargins(ll)))
library(officer)
doc = read_docx()
doc = body_add_flextable(doc,tt)
print(doc,"/Users/cpf/OneDrive - bjtu.edu.cn/cpf/paper_writting/NHTSA/writting/output/INJ.docx")



#################相关性检验#####################
attach(Mdata1)
library(corrplot)
mylist1<-data.frame(INJ_SEV,AGE,SEX,REST_USE,DR_DRINK,DRUGS,AIR_BAG,EJECTION,EXTRICAT,DOA,DEATH_TM,DEATH_HR,DEATH_MN,LAG_HRS,LAG_MINS,WORK_INJ,HOSPITAL,DEATH_MO,
                    ROLLOVER_x,L_RESTRI, CDL_STAT,PREV_ACC, PREV_SPD, PREV_OTH, FIRST_YR,LAST_YR,SPEEDREL,VPAVETYP,OWNER,MOD_YEAR_x,GVWR,TRAV_SP,FIRE_EXP_x,L_STATUS,BODY_TYP_x,
                    INJ_SEV_,AGE_,SEX_,REST_USE_,DR_DRINK_,DRUGS_,AIR_BAG_,EJECTION_,EXTRICAT_,DOA_,DEATH_TM_,DEATH_HR_,DEATH_MN_,LAG_HRS_,LAG_MINS_,WORK_INJ_,HOSPITAL_,DEATH_MO_,
                    ROLLOVER_x_,L_RESTRI_,CDL_STAT_,PREV_ACC_,PREV_SPD_,PREV_OTH_,FIRST_YR_,LAST_YR_,SPEEDREL_,VPAVETYP_,OWNER_,MOD_YEAR_x_,GVWR_,TRAV_SP_,FIRE_EXP_x_,L_STATUS_,BODY_TYP_x_,
                    unicid,TYP_INT,FUNC_SYS_x,VSPD_LIM,LGT_COND,VNUM_LAN,TIME,WEATHER,MAN_COLL,VPAVETYP,VSURCOND,ACC_TYPE)
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


###########MODEL_GJRM############
eq1<- DIE ~ AGE + SEX + REST_USE + DR_DRINK + DRUGS + AIR_BAG + EJECTION + EXTRICAT + DOA + ROLLOVER + TIME + BODY_TYP + WEATHER + MAN_COLL + LGT_COND 
eq2 <- DIE_ ~  AGE_ + SEX_ + REST_USE_ + DR_DRINK_ + DRUGS_ + AIR_BAG_ + EJECTION_ + EXTRICAT_ + DOA_+ ROLLOVER_ + TIME + BODY_TYP_+ WEATHER + MAN_COLL + LGT_COND
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


eq1<- DIE ~ AGE + SEX + REST_USE + DR_DRINK + DRUGS + AIR_BAG + EJECTION + EXTRICAT + DOA + ROLLOVER + TIME + BODY_TYP + WEATHER + MAN_COLL + LGT_COND 
eq2 <- DIE_ ~ REST_USE + SEX_ + REST_USE_ + DR_DRINK_ + DRUGS_ + AIR_BAG_ + EJECTION_ + EXTRICAT_ + DOA_+ ROLLOVER_ + TIME + BODY_TYP_+ WEATHER + MAN_COLL + LGT_COND
f.list <- list(eq1, eq2)

eq1<- DIE ~ AGE + SEX + REST_USE
eq2 <- DIE_ ~ REST_USE +AGE_ + SEX_ + REST_USE_ 
f.list <- list(eq1, eq2)

bpN1 <- gjrm(f.list, data=Mdata1, Model="B", margins=mr, gamlssfit = TRUE)
bpF1 <- gjrm(f.list, data=Mdata1, BivD = "F", Model="B", margins=mr)
bpC01 <- gjrm(f.list, data=Mdata1, BivD = "C0", Model="B", margins=mr)
bpC1801 <- gjrm(f.list, data=Mdata1, BivD = "C180", Model="B", margins=mr)
bpG01 <- gjrm(f.list, data=Mdata1, BivD = "G0", Model="B", margins=mr)
bpG1801 <- gjrm(f.list, data=Mdata1, BivD = "G180", Model="B", margins=mr)
AIC(bpN1, bpF1, bpC01, bpC1801, bpG01, bpG1801)
rm(eq1, eq2,f.list)

mare <- predict.CopulaCLM(bpN1, eq=1)


###########MODEL_VGAM#############

e1<- DIE ~ AGE + SEX + REST_USE
fitlogit <- vglm(ordered(INJ_SEV) ~ AGE + SEX + REST_USE, cumulative(parallel = TRUE),data=Mdata1)
fitprobit <- vglm(ordered(INJ_SEV) ~ AGE + SEX + REST_USE, cumulative(link = "probitlink", parallel=TRUE), data=Mdata1 )
marg <- margeff((fitprobit))
## INJ_SEV 为4， 去掉第一行误差项， 剩余变量的边际效应
rowMeans(marg[-1,"4",])

apply(marg[-1, "4", ], 1, sd) 

eta1 <- predict(fitlogit)[, 1]
var(eta1) / (var(eta1) + (pi^2)/3)

eta2 <- predict(fitprobit)[, 1]
var(eta2) / (var(eta2) + 1)
