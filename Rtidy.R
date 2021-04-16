########1.install/library package######
install.packages("tidyverse")
install.packages("rmarkdown")
install.packages("pander")
library(tidyverse)
library(mvord)

#########2.data_process##############
df <- read_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dftwodriver20210401.csv")

df1 = df %>% 
  select(where(is.numeric)) %>% 
  select(CASENUM, VE_TOTAL.1, VE_FORMS.1, PVH_INVL.1, PERMVIT.1, 
         MONTH.1, DAY_WEEK.1, YEAR.1, HOUR.1, MINUTE.1,HARM_EV.1,
         MAN_COLL.1, RELJCT1.1, TYP_INT.1, REL_ROAD.1, WRK_ZONE.1,
         LGT_COND.1, WEATHER.1, SCH_BUS.1, MAX_SEV.1, NUM_INJ.1, ##accident level
         VEH_NO.1,NUMOCCS.1, UNITTYPE.1, HIT_RUN.1, BODY_TYP.1, MOD_YEAR.1, TOW_VEH.1, 
         J_KNIFE.1, GVWR.1, CARGO_BT.1, HAZ_ID.1,HAZ_INV.1,HAZ_PLAC.1,
         BUS_USE.1, SPEC_USE.1, TRAV_SP.1, ROLLOVER.1, DEFORMED.1, M_HARM.1, MAX_VSEV.1, 
         NUM_INJ.1, VEH_ALCH.1, DR_PRES.1, DR_ZIP.1, SPEEDREL.1, VSPD_LIM.1,VTRAFCON.1,VTRAFWAY.1, 
         VNUM_LAN.1, VALIGN.1, VSURCOND.1, VPROFILE.1,ACC_TYPE.1, # VEhecile 1
         AGE.1, SEX.1, PER_TYP.1, INJ_SEV.1, SEAT_POS.1, REST_USE.1, AIR_BAG.1, EJECTION.1,
         DRINKING.1, DRUGS.1, HOSPITAL.1, STR_VEH.1,LOCATION.1, # Person 1
         VEH_NO.2,NUMOCCS.2, UNITTYPE.2, HIT_RUN.2, BODY_TYP.2, MOD_YEAR.2, TOW_VEH.2, 
         J_KNIFE.2, GVWR.2, CARGO_BT.2, HAZ_ID.2,HAZ_INV.2,HAZ_PLAC.2,
         BUS_USE.2, SPEC_USE.2, TRAV_SP.2, ROLLOVER.2, DEFORMED.2, M_HARM.2, MAX_VSEV.2, 
         NUM_INJ.2, VEH_ALCH.2, DR_PRES.2, DR_ZIP.2, SPEEDREL.2, VSPD_LIM.2,VTRAFCON.2,VTRAFWAY.2, 
         VNUM_LAN.2, VALIGN.2, VSURCOND.2, VPROFILE.2,ACC_TYPE.2, # VEhecile 2
         AGE.2, SEX.2, PER_TYP.2, INJ_SEV.2, SEAT_POS.2, REST_USE.2, AIR_BAG.2, EJECTION.2,
         DRINKING.2, DRUGS.2, HOSPITAL.2, STR_VEH.2,LOCATION.2 # Person 2
         )
df2 = df1 %>%
  select(-YEAR.1) %>% 
  filter(if_all(everything(), ~ !is.na(.x))) %>% ## 在everything列内选出所有不含空值的行
  ### Accident level
  filter(WEATHER.1 < 15) %>% mutate(WEATHHER = case_when(WEATHER.1 == 1 ~ 0, # clear,
                                                         WEATHER.1 == 2 ~ 2, # Rain
                                                         WEATHER.1 == 3 | WEATHER.1 == 4 ~ 3,#SNOW
                                                         TRUE       ~ 1)) %>% #fog, cloud 
  mutate(WEATHER2 = if_else(WEATHER.1 ==1,0,1)) %>% 
  filter(LGT_COND.1 < 4) %>%  mutate(LGT = case_when(LGT_COND.1 ==1 ~ 0, 
                                                   LGT_COND.1 ==2 ~ 1,
                                                   LGT_COND.1 ==3 ~ 2)) %>% 
  filter(between(MAN_COLL.1, 1,15)) %>%  mutate(MANCOLL = case_when(MAN_COLL.1 == 1 ~ 0,  ## front to rear
                                                                    MAN_COLL.1 == 2 ~ 1, ## front to front
                                                                    MAN_COLL.1 == 6 ~ 2, ## Angle
                                                                    TRUE ~ 3)) %>%     ## Sidewipe
  
  ## Person level
  filter(AGE.1 < 120) %>% mutate(AGE1.1 = cut(AGE.1, breaks = c(0,25,40,60,100), labels = c(0,1,2,3), include.lowest = TRUE)) %>% 
  filter(AGE.2 < 120) %>% mutate(AGE1.2 = cut(AGE.2, breaks = c(0,25,40,60,100), labels = c(0,1,2,3), include.lowest = TRUE))  %>% 
  filter(INJ_SEV.1 < 7) %>% mutate(INJ_SEV.1 = case_when(INJ_SEV.1 == 0 ~ 0,
                                                         INJ_SEV.1 == 1 ~ 1, 
                                                         INJ_SEV.1 == 2 ~ 2, 
                                                         TRUE ~ 3)) %>% 
  filter(INJ_SEV.2 < 7) %>% mutate(INJ_SEV.2 = case_when(INJ_SEV.2 == 0 ~ 0,
                                                         INJ_SEV.2 == 1 ~ 1, 
                                                         INJ_SEV.2 == 2 ~ 2, 
                                                         TRUE ~ 3)) %>% 
  filter(SEX.1 < 3) %>% mutate(SEX.1 = if_else(SEX.1 == 2,0, 1)) %>% # 男1，女0
  filter(SEX.2 < 3) %>% mutate(SEX.2 = if_else(SEX.2 == 2,0, 1)) %>% 
  filter(REST_USE.1 < 98) %>%  mutate(REST_USE.1 = ifelse(REST_USE.1 < 15, 1,0)) %>% 
  filter(REST_USE.2 < 98) %>%  mutate(REST_USE.2 = ifelse(REST_USE.2 < 15, 1,0)) %>% 
  mutate(DRINKING.1 = case_when(DRINKING.1 == 0 ~ 0,DRINKING.1 == 1 ~ 1,TRUE ~ 2)) %>% 
  mutate(DRINKING.2 = case_when(DRINKING.2 == 0 ~ 0,DRINKING.2 == 1 ~ 1,TRUE ~ 2)) %>% 
  filter(between(AIR_BAG.1, 1, 27)) %>%  mutate(AIR_BAG.1 = if_else(AIR_BAG.1 == 20, 0, 1)) %>% 
  filter(between(AIR_BAG.2, 1, 27)) %>%  mutate(AIR_BAG.2 = if_else(AIR_BAG.2 == 20, 0, 1)) %>% 
  
  ## Vehicle level 
  filter(BODY_TYP.1 < 80 & BODY_TYP.1<50 | BODY_TYP.1 >59) %>%  mutate(BODY_TYP.1 = case_when(BODY_TYP.1 <= 13 | BODY_TYP.1 ==17 ~ 0 , # mobile
                                                                                              between(BODY_TYP.1,14,19) & BODY_TYP.1 != 17 ~ 1, #suv
                                                                                              between(BODY_TYP.1,20,39) ~ 2,  # cargo
                                                                                              TRUE ~ 3 # Truck
                                                                                              )) %>% 
  filter(BODY_TYP.2 < 80 & BODY_TYP.2<50 | BODY_TYP.2 >59) %>%  mutate(BODY_TYP.2 = case_when(BODY_TYP.2 <= 13 | BODY_TYP.2 ==17 ~ 0 , # mobile
                                                                                              between(BODY_TYP.2,14,19) & BODY_TYP.2 != 17 ~ 1, #suv
                                                                                              between(BODY_TYP.2,20,39) ~ 2,  # cargo
                                                                                              TRUE ~ 3)) %>%  # Truck
  filter(ROLLOVER.1 < 8) %>%  mutate(ROLLOVER.1 = if_else(ROLLOVER.1 == 0, 0, 1)) %>% 
  filter(ROLLOVER.2 < 8) %>%  mutate(ROLLOVER.2 = if_else(ROLLOVER.2 == 0, 0, 1))  
  

####数据汇总

df2 %>% 
  summarise(
    nobs = n(),
    n = sum(!is.na(as.numeric(AGE1))),
    mean.age = mean(as.numeric(AGE1), na.rm=TRUE),
    sd.age = sd(as.numeric(AGE1), na.rm = TRUE)
  )




#########3.宽表变长表############
knitr::kable(df2[1:5,], align = "c")
df_long = df2 %>% 
  pivot_longer(-c(CASENUM, VE_TOTAL.1, VE_FORMS.1, PVH_INVL.1, PERMVIT.1, 
                  MONTH.1, DAY_WEEK.1, HOUR.1, MINUTE.1,HARM_EV.1,
                  MAN_COLL.1, RELJCT1.1, TYP_INT.1, REL_ROAD.1, WRK_ZONE.1,
                  LGT_COND.1, WEATHER.1, SCH_BUS.1, MAX_SEV.1, NUM_INJ.1,
                  WEATHHER, WEATHER2, LGT, MANCOLL),
               names_to = c(".value", "VENO"),
               names_sep = "\\.",
               values_drop_na = TRUE) %>% 
  write_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dftwoall_long0416.csv")
  


df4= df3 %>%
  pivot_longer(-c(CASENUM, LGT, MANCOLL, WEATHHER, HOUR.1, WEATHER2),
               names_to = c(".value", "VENO"),
               names_sep = "\\.",
               values_drop_na = TRUE)
df4 %>% write_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dftwo_long0413.csv")
dfdemo = df3 %>% 
  slice_sample(n=50) %>%  #随机选取50行
  select(CASENUM, AGE1.1, AGE1.2, REST_USE.1, REST_USE.2, INJ_SEV.1, INJ_SEV.2, WEATHHER) %>% 
  pivot_longer(-c(CASENUM, WEATHHER),
               names_to = c(".value", "VENO"),
               names_sep = "\\.",
               values_drop_na = TRUE)
write_csv(dfdemo, "")

  


########3.plot #####
df4 %>%
  ggplot(aes(WEATHHER, LGT)) +
  geom_tile(aes(fill = INJ_SEV)) +
  geom_text(aes(label = INJ_SEV)) +
  scale_fill_gradient(low = "white", high = "darkred") 

df4 %>% 
  filter(HOUR.1<25) %>% 
  ggplot(aes(HOUR.1, INJ_SEV)) +
  geom_point(aes(color = VENO)) +
  labs(x = "小时", y = "INJ_SEV", color = "双方驾驶员") +
  theme(legend.position = "top")
df4 %>% 
  ggplot(aes(x = INJ_SEV)) +
  geom_histogram(fill = "steelblue", color = "black", binwidth = 0.5) 



##########4. mvord model###############
df3 = df2 %>% 
  filter(between(TYP_INT.1, 2, 7)) %>% 
  select(CASENUM,VEH_NO.1,VEH_NO.2,REST_USE.1,REST_USE.2,SEX.1, SEX.2, INJ_SEV.1,INJ_SEV.2,HOUR.1,  AGE1.1,AGE.1, DRINKING.1, DRUGS.1, AIR_BAG.1, EJECTION.1,ROLLOVER.1,
         AGE1.2,AGE.2 ,DRINKING.2, DRUGS.2, AIR_BAG.2, EJECTION.2, ROLLOVER.2, MANCOLL, LGT, WEATHHER, WEATHER2, TYP_INT.1) %>% 
  drop_na() %>% 
  write_csv("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dftwo0408.csv")

df5 = df3 %>% 
  group_split(LGT, WEATHER2)

df6 = df3 %>% 
  group_nest(LGT, WEATHER2)

fit.mvf <- mvord(formula = MMO2(REST_USE.1, REST_USE.2, INJ_SEV.1,INJ_SEV.2) ~ 0 + AGE1.1+ DRINKING.1 + DRUGS.1 +AIR_BAG.1 + EJECTION.1 + ROLLOVER.1 + AGE1.2 + DRINKING.2+  DRUGS.2  + AIR_BAG.2+ EJECTION.2 + ROLLOVER.2 + MANCOLL + LGT  + WEATHHER, 
                 threshold.constraints = c(1,2,3,4),
                 link = mvlogit(),data = df3)

fit.mvf6 <- mvord(formula = MMO2(REST_USE.1, REST_USE.2, INJ_SEV.1,INJ_SEV.2) ~ 0 + AGE1.1+ DRINKING.1 +DRUGS.1 +AIR_BAG.1 +  AGE1.2 + DRINKING.2+ DRUGS.2 + AIR_BAG.2, 
                 threshold.constraints = c(1,2,3,4),
                 link = mvprobit(),data = df5[6][[1]])

df6 %>% 
  mutate(model = map(data, ~ mvord(formula = MMO2(REST_USE.1, REST_USE.2, INJ_SEV.1,INJ_SEV.2) ~ 0 + AGE1.1+ DRINKING.1 + DRUGS.1 +AIR_BAG.1 + EJECTION.1 + ROLLOVER.1 + AGE1.2 + DRINKING.2+  DRUGS.2  + AIR_BAG.2+ EJECTION.2 + ROLLOVER.2 + MANCOLL + LGT  + WEATHHER, 
                                   threshold.constraints = c(1,2,3,4),
                                   link = mvlogit(),data = .x)))

by_WL = df5 %>% 
  mutate(model = map(df5, ~ fit.mvf))

l <- list(a = list(var.1 = 1, var.2 = 2, var.3 = 3)
          , b = list(var.1 = 4, var.2 = 5, var.3 = 6)
          , c = list(var.1 = 7, var.2 = 8, var.3 = 9)
          , d = list(var.1 = 10, var.2 = 11, var.3 = 12)
)


