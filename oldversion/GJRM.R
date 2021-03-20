install.packages('GJRM')

library(GJRM)

####################################
# JOINT MODELS WITH BINARY MARGINS #
####################################

############
## Example 1
############
### Generate dataframe: dataSimd
set.seed(0)
n <- 400
Sigma <- matrix(0.5, 2, 2); diag(Sigma) <- 1
u <- rMVN(n, rep(0,2), Sigma) # rMVN generates random multivariate normal variates

x1 <- round(runif(n)); x2 <- runif(n); x3 <- runif(n)

f1 <- function(x) cos(pi*2*x) + sin(pi*x)
f2 <- function(x) x+exp(-30*(x-0.5)^2)

y1 <- ifelse(-1.55 + 2*x1 + f1(x2) + u[,1] >0, 1, 0)
y2 <- ifelse(-0.25 - 1.25*x1 + f2(x2) + u[,2] >0, 1, 0)

dataSimd <- data.frame(y1,y2,x1,x2,x3)

### Classic bivariate probit
out <- gjrm(list(y1 ~ x1 + x2 + x3, 
                 y2 ~ x1 + x2 + x3),
            data = dataSimd,
            margins = c("probit", "probit"),
            Model = "B") # "B" (bivariate model), "T" (trivariate model) "BSS" (bivariate model with non-random sample selection), "TSS" (trivariate model with double non-random sample selection)

conv.check(out)
summary(out)
AIC(out) #840.3036
BIC(out) # 876.2268

### Bivariate probit with splines
out <- gjrm(list(y1 ~ x1 + s(x2) + s(x3), 
                 y2 ~ x1 + s(x2) + s(x3)),
            data = dataSimd,
            margins = c("probit", "probit"),
            Model = "B") 
conv.check(out)
summary(out)
AIC(out) # 774.315
BIC(out) # 832.3607

### estimated smooth function plots - red lines are true curves

x2 <- sort(x2)
f1.x2 <- f1(x2)[order(x2)] - mean(f1(x2))
f2.x2 <- f2(x2)[order(x2)] - mean(f2(x2))
f3.x3 <- rep(0, length(x3)) # length(x3)个0

par(mfrow=c(2,2), mar=c(4.5, 4.5, 2, 2))
plot(out, eq=1, select = 1, seWithMean=TRUE, scale = 0)
lines(x2, f1.x2, col="red")
plot(out, eq = 1, select=2, seWithMean=TRUE, scale=0)
lines(x3, f3.x3, col="green")
plot(out, eq=2, select = 1, seWithMean=TRUE, scale=0)
lines(x2, f2.x2, col="red")
plot(out, eq=2, select = 2, seWithMean=TRUE, scale=0)
lines(x3, f3.x3, col="yellow")


### Bivariate probit with splines and varying dependence parameter
eq.mu.1 <- y1 ~ x1 + s(x2)
eq.mu.2 <- y2 ~ x1 + s(x2)
eq.theta <-   ~ x1 + s(x2)

fl <- list(eq.mu.1, eq.mu.2, eq.theta)
outD <- gjrm(fl, data = dataSimd,
             margins = c("probit", "probit"),
             Model = "B")
conv.check(outD)
summary(outD)

AIC(outD) # 772.4862
BIC(outD) # 840.6409
######### 
##my try 
###########
eq.mu.11 <- y1 ~ x1 + s(x2) + s(x3)
eq.mu.22 <- y2 ~ x1 + s(x2) + s(x3)
eq.theta1 <-   ~ x1 + s(x2) + s(x3)

fl <- list(eq.mu.11, eq.mu.22, eq.theta1)
outE <- gjrm(fl, data = dataSimd,
             margins = c("probit", "probit"),
             Model = "B")
conv.check(outE)
summary(outE)
AIC(outE) # 757.7778
BIC(outE) # 860.0944
############

outD$theta

plot(outD, eq=1, seWithMean=TRUE)
plot(outD, eq=2, seWithMean=TRUE)
plot(outD, eq=3, seWithMean=TRUE)
graphics.off() #关闭图片


##########
## Example 2
##########
### Generate data with one endogenous variable
### and exclusion restriction
set.seed(0) 
n <- 400

Sigma <- matrix(0.5, 2, 2); diag(Sigma) <- 1 
u <- rMVN(n, rep(0,2), Sigma)

cov <- rMVN(n, rep(0, 2), Sigma)
cov <- pnorm(cov)
x1 <- round(cov[,1]); x2 <- cov[,2]

f1 <- function(x) cos(pi*2*x) + sin(pi*x)
f2 <- function(x) x+exp(-30*(x-0.5)^2)

y1 <- ifelse(-1.55 + 2*x1 + f1(x2) + u[,1] >0, 1, 0)
y2 <- ifelse(-0.25 - 1.25*x1 + f2(x2) + u[,2] >0, 1, 0)

dataSim <- data.frame(y1, y2, x1, x2)

### 检测是否有内生性假设
LM.bpm(list(y1 ~ x1 + s(x2), y2 ~ y1 +s(x2)), dataSim, Model="B") #输出 0
### Classic recursive bivariate probit
out <- gjrm(list(y1 ~ x1 + x2,
                 y2 ~ y1 + x2),
            data = dataSim,
            margins = c("probit", "probit"),
            Model = "B")
conv.check(out)
summary(out)
AIC(out); BIC(out) # 1106.017; 1133.957

### Flexible recursive bivariate probit
out <- gjrm(list(y1 ~ x1 + s(x2),
                 y2 ~ y1 + s(x2)),
            data = dataSim,
            margins = c("probit", "probit"),
            Model = "B")
conv.check(out)
summary(out)
AIC(out); BIC(out) # 1106.017; 1133.957

### 测试估计后有没有内生性
gt.bpm(out) # 输出 0.921552
### treatment effect, risk ration and odds ration with CIs
mb(y1, y2, Model="B") # ATE assuming random assignment (%): -18.2
AT(out, nm.end = "y1", hd.plot=TRUE)  # Treatment effect (%) with 95% interval: 39.7 (-15.8,78.5)
RR(out, nm.end= "y1") # Risk ratio with 95% interval: 2.466 (0.588,8.719)
OR(out, nm.end = "y1") #Odds ratio with 95% interval: 5.424 (0.409,206.259)
AT(out, nm.end = "y1", type = "univariate")



data("hiv", package = "GJRM")
data("hiv.polys", package = "GJRM")
xt <- list(polys = hiv.polys)




