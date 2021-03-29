
library(data.table)

df <- read_csv("/Users/cpf/Documents/temporal/3副本alldata20140715.csv")


###########model#######
df <- subset(df, df$W3P>0)
df <- transform(df, TYPE1 = df$TYPE+1)
df <- transform(df, STATUA_ = df$STATUA1+1)
df <- subset(df, df$SITE<7)
df <- subset(df, !is.na(df$TC)&TRUE)
df <- transform(df, W3P1 = ifelse(df$W3P == 1, 3, ifelse(df$W3P == 2,2,1)))




fit.mvor <- mvord(formula = MMO2(INJ_SEV, INJ_SEV_) ~ 0 + AGE1 + REST_USE + AGE1_ + REST_USE_, data = Mdata1)

fitdf <- mvord(formula = MMO2(STATUA1, W3P) ~ 0 + GEN + factor(AGE) + factor(from) + factor(TYPE) + RUSH + awaitnon + awaitped+ factor(SITE) , data = df)

fitdf2 <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 + GEN + factor(AGE) + factor(from) + factor(TYPE) + RUSH + awaitnon + awaitped+ factor(SITE) , data = df)
fitdf3 <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 +GEN + factor(AGE) + factor(from) + factor(TYPE) + RUSH + TIME + awaitnon + awaitped+ factor(SITE) , 
                link = mvlogit(),data = df)

df1 <- subset(df, !is.na(df$ca3r)&TRUE)
fitdf5 <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 +GEN + factor(AGE) + factor(from) + factor(TYPE) + factor(ca3r) + TIME + awaitnon + awaitped+ factor(SITE) , 
                link = mvlogit(),data = df1)

fitdf5 <- mvord(formula = MMO2(STATUA1, W3P) ~ 0 + factor(ca3r), data = df)


pred_fitdf3 <- predict(fitdf3, type = "prob")
marpred_fitdf3 <- marginal_predict(fitdf3, type = "class")
jp <- joint_probabilities(fitdf3, response.cat = df[,c("STATUA1","W3P1")], type = "prob")


fitdf4 <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 +GEN + factor(AGE) + factor(from) + factor(TYPE) + RUSH + TIME + awaitnon + awaitped+ factor(SITE) , 
                link = mvlogit(),data = df, threshold.constraints = rep(1,2),
                coef.constraints = rep(1,2))
fitdf6 <- mvord(formula = MMO2(STATUA1, W3P1, TYPE) ~ 0 +GEN + factor(AGE) + factor(from)+ RUSH + TIME + awaitnon + awaitped+ factor(SITE) , 
                link = mvlogit(),data = df)


df2 <- subset(df, df$TYPE == 0)

fitdf2_people <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 +GEN + factor(AGE) + factor(from) +  RUSH + TIME + awaitnon + awaitped+ factor(SITE) , 
                link = mvlogit(),data = df2)
df3 <- subset(df, df$TYPE == 1)
fitdf_cycle <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 +GEN + factor(AGE) + factor(from) +  RUSH + TIME + awaitnon + awaitped+ factor(SITE) , 
                       link = mvlogit(),data = df3)
df4 <- subset(df, df$TYPE == 2)
fitdf_moto <- mvord(formula = MMO2(STATUA1, W3P1) ~ 0 +GEN + factor(AGE) + factor(from) +  RUSH + TIME + awaitnon + awaitped+ factor(SITE) , 
                     link = mvlogit(),data = df4)



agree_prob_list <- lapply(1:3, function(i)
  joint_probabilities(fitdf4, rep(i, 2)))
agree_prob <- Reduce("+", agree_prob_list)
plot(df$TIME, fitdf3,
     xlab = "word length", ylab = "probability of agreement")

fitstat<- vgam(STATUA1 ~ GEN + factor(AGE) + factor(from) + factor(TYPE) + RUSH + s(TIME) + awaitnon + awaitped+ factor(SITE) , cumulative(link = logit, reverse = TRUE, parallel = TRUE), data = df)

fitw3p<- vglm(W3P1 ~ GEN + factor(AGE) + factor(from) + factor(TYPE) + RUSH + TIME + awaitnon + awaitped+ factor(SITE) , cumulative(link = logit, reverse = TRUE, parallel = TRUE), data = df)




#######plot
plot(fitw3p, se = TRUE, scol = "limegreen", lcol = "blue", scale = 4)

ooo <- with(fitw3p, order(TIME))



plot(as(fitw3p, "vgam"), se = TRUE)
plot(fitw3p, se = TRUE, overlay = TRUE, lcol = 3:4, scol = 3:4, main="(a)")

matplot(with(df, TIME), fitted(fitw3p), type = "l", col=1:3,
        ylab = "Fitted value", xlab = "Log exposure time", main="(b)")


preddat3 <- data.frame(TIME = 
                         seq(from = min(df$TIME, na.rm = TRUE),
                             to = max(df$TIME, na.rm = TRUE),
                             length.out = 1000))

testdata <- data.frame(GEN=mean(df$GEN), AGE = mean )

Newdata <- data.frame(TIME = TIME,TYPE = 0)


fitdf1 <- mvord(formula = MMO2(STATUA1, W3P) ~ 0 + GEN + AGE + from + TYPE + awaitnon + awaitped+ SITE , data = df)


fitddf1 <- mvord(formula = MMO2(STATUA1, TYPE1) ~ 0 + GEN + factor(AGE) + factor(from) + factor(W3P) + RUSH +  awaitnon + awaitped+ factor(SITE) , data = df)

df$STATUA1 <- factor(df$STATUA1, ordered = TRUE)
df$TYPE <- factor(df$TYPE, ordered = TRUE)
fitddf <- mvord(formula = MMO2(STATUA1, TYPE) ~ GEN + AGE + from + W3P + awaitnon + awaitped + factor(SITE), 
                threshold.values = list(c(0,NA),
                                        c(0,NA)),data = df)


capture.output(summary(fitdf3), file = "/Users/cpf/Documents/paper/NHTSA/writting/output/crossing behavior.txt" )
