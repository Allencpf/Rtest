install.packages("bit64")
library(bit64)
library(data.table)
library(dplyr)


df_v <- fread("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/CRSS/CRSS2016/VIOLATN.CSV")
df_c <- fread("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Original/NHTSA/CRSS/CRSS2016/DISTRACT.CSV")
dfp <- fread("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dfp20210330.csv")
dfv <- fread("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dfv20210330.csv")
dfa <- fread("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dfa20210330.csv")
dfvv <- fread("/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dfvv20210330.csv")
dfvv <- dfvv[!duplicated(dfvv[,.(CASENUM,VEH_NO)]),]

dfvvv <- dfv[dfvv, on = .(CASENUM = CASENUM, VEH_NO = VEH_NO), roll = TRUE]
dfpvvv <- dfp[dfvvv, on = .(CASENUM = CASENUM, VEH_NO = VEH_NO), roll = FALSE]
dfpvvva <- dfa[dfpvvv, on = .(CASENUM = CASENUM), roll = FALSE]


set1 <- dfpvvva[, .(VE_FORMS), by = .(CASENUM)][,.(VE_FORMS)] == 2 # groupby CASENUM，找到其中VE_FORMS=2的事故

dfpvvva <- subset(dfpvvva, dfpvvva$CASENUM&set1[,1])

table(dfpvvva[, .(VE_FORMS), by = .(CASENUM)][,.(VE_FORMS)] == 2)

dfpvvva <- subset(dfpvvva, dfpvvva$PER_TYP ==1) # 选取PER_TYP为司机 

df1 <- subset(dfpvvva, dfpvvva$VEH_NO == 1)
df2 <- subset(dfpvvva, dfpvvva$VEH_NO == 2 | dfpvvva$VEH_NO == 3 | dfpvvva$VEH_NO == 4)

# df <- df1[df2, on = .(CASENUM = CASENUM), roll = FALSE]
df <- inner_join(df1, df2, by = "CASENUM", suffix=c(".1",".2")) # dplyyr： 1是驾驶员1；2是驾驶员2

df <- subset(df, df$VNUMBER1.1 == 1 & df$VNUMBER2.2 == 2)

table(df$VNUMBER1.1)
table(df$VNUMBER2.2)

fwrite(df, file = "/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dftwodriver20210401.csv")
crss <- fread(file = "/Users/cpf/OneDrive - bjtu.edu.cn/Data/Modified/NHTSAdata/data/output_crss/dftwodriver20210401.csv")
