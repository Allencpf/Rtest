library(GJRM)

###############JOINT MODELS WITH BINARY MARGINS#####################

############## Example 1 ############
### import dataframe: df_three
df <- read.csv("df_three.csv")
df2<-subset(df,INJ_SEV<=1 & INJ_SEV2 <=1)   
### Classic bivariate probit
df_out <- gjrm(list(INJ_SEV ~ LAG_HRS + DAY_WEEK + LGT_COND, 
                 INJ_SEV2 ~ LAG_HRS + DAY_WEEK + LGT_COND),
            data = df2,
            margins = c("probit", "probit"),
            Model = "B") 
conv.check(df_out)
summary(df_out)
AIC(df_out) # 903.9574
BIC(df_out) # 938.067

### Bivariate probit with splines
df_out2 <- gjrm(list(INJ_SEV ~ LAG_HRS + s(DAY_WEEK) + LGT_COND, 
                     INJ_SEV2 ~ LAG_HRS + s(DAY_WEEK) + LGT_COND),
                data = df2,
                margins = c("probit", "probit"),
                Model = "B") 

conv.check(df_out2)
summary(df_out2)
AIC(df_out2) # 903.9574
BIC(df_out2) # 938.067



#################### my_work ####################
out3 <- gjrm(list(INJ_SEV ~ AGE + SEX + REST_USE + AIR_BAG + EJECTION + DRINKING + DRUGS + L_TYPE + LAG_HRS + DAY_WEEK + LGT_COND + VSURCOND, 
                 INJ_SEV2 ~ AGE2 + SEX2 + REST_USE2 + AIR_BAG2 + EJECTION2 + DRINKING2 + DRUGS2 + L_TYPE2 + LAG_HRS+ DAY_WEEK + LGT_COND + VSURCOND),
            data = df,
            margins = c("N", "N"),
            Model = "BPO", BivD = "N")

conv.check(out3)
summary(out3)
AIC(out3) #70862.15
BIC(out3) #71065.25

out4 <- gjrm(list(INJ_SEV ~ AGE + SEX + REST_USE + AIR_BAG + EJECTION + DRINKING + L_TYPE + LAG_HRS + DAY_WEEK + LGT_COND + VSURCOND, 
                  INJ_SEV2 ~ AGE2 + SEX2 + REST_USE2 + AIR_BAG2 + EJECTION2 + DRINKING2 + L_TYPE2 + LAG_HRS+ DAY_WEEK + LGT_COND + VSURCOND),
             data = df,
             margins = c("N", "N"),
             Model = "BPO", BivD = "N")
AIC(out4) #70862.15
BIC(out4) #71065.25