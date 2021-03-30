##############original data process##############
install.packages("data.table")
library(data.table)

load("~/Documents/paper/R_programming/Rdatabook/04690-0001-Data.rda") # load workspace

acl <- as.data.table(da04690.0001)

acl <- acl[, .( V1, V1801, V2101, V2064,
  V3007, V2623, V2636, V2640,
  V2000,
  V2200, V2201, V2202,
  V2613, V2614, V2616,
  V2618, V2681,
  V7007, V6623, V6636, V6640,
  V6201, V6202,
  V6613, V6614, V6616,
  V6618, V6681)]
setnames(acl, names(acl), c(
  "ID", "Sex", "RaceEthnicity", "SESCategory", "Employment_W1", "BMI_W1", "Smoke_W1", "PhysActCat_W1", "AGE_W1",
  "SWL_W1", "InformalSI_W1", "FormalSI_W1", "SelfEsteem_W1", "Mastery_W1", "SelfEfficacy_W1", "CESD11_W1", "NChronic12_W1",
  "Employment_W2", "BMI_W2", "Smoke_W2", "PhysActCat_W2", "InformalSI_W2", "FormalSI_W2",
  "SelfEsteem_W2", "Mastery_W2", "SelfEfficacy_W2", "CESD11_W2", "NChronic12_W2"
))

acl[, ID := factor(ID)]
acl[, SESCategory := factor(SESCategory)]
acl[, SWL_W1 := SWL_W1 * -1]  # 使这一列变为负数

saveRDS(acl, "advancedr_acl_data.RDS", compress = "xz")


#############chapter 1 univariate distribution##################
install.packages("MASS")
install.packages("cowplot")
install.packages("JWileymisc")
library(knitr) 
library(ggplot2) 
library(cowplot) 
library(MASS) 
library(JWileymisc) 
library(data.table)
options(width = 70, digits = 2)


ggplot(mtcars, aes(mpg)) +   # 旧车每加仑英里的堆积点图 fig1.1 累积点图
  geom_dotplot()  

ggplot(iris, aes(Sepal.Length)) + geom_histogram() ## fig1.2 直方图

ggplot(data.table(lynx = as.vector(lynx)), aes(lynx)) + geom_histogram() ## fig1.3 直方图，

ggplot(data.table(lynx = as.vector(lynx)), aes(log(lynx))) + geom_histogram() ## fig1.4 直方图，1.3 中的log(lynx)

ggplot(iris, aes(Sepal.Length)) + geom_density() ## fig1.5 密度图 与fig1.2 一样

ggplot(iris, aes(Sepal.Length)) + geom_density(adjust = .5) ## fig1.6
ggplot(iris, aes(Sepal.Length)) + geom_density(adjust = 5)  ## fig 1.7 adjust 越大越平滑

ggplot(iris, aes(sample = Sepal.Length)) + geom_qq() ## fig 1.8 Q-Q plot 普通数据看起来像一条直线。 Sepal.Length似乎很正常

qplot(
  x = qnorm(
    p = ppoints(length(iris$Sepal.Length)), mean = mean(iris$Sepal.Length),
    sd = sd(iris$Sepal.Length)),
  y = sort(iris$Sepal.Length),
  xlab = "Theoretical Normal Quantiles", ylab = "Sepal Length") + geom_abline(slope = 1, intercept = 0) ## fig 1.9在x轴上显示理论规范（基于对均值和标准差的预测）


ggplot(data.table(lynx = as.vector(lynx)), aes(sample = lynx)) + geom_qq(distribution = qlnorm) 

ggplot(data.table(lynx = as.vector(lynx)), aes(sample = lynx)) + geom_qq(distribution = qpois, dparams = list(lambda = mean(lynx))) ## fig 1.11 esting whether lynx data are consistent with a Poisson distribution

ggplot(iris, aes(Sepal.Length)) + 
  geom_density() + 
  stat_function(fun = dnorm,args = list(mean = mean(iris$Sepal.Length),
                                        sd = sd(iris$Sepal.Length)),
                                        colour = "blue") ## fig1.12 法线和密度图（默认平滑度为1）

#---------------拟合其他的分布------------------#
set.seed(1234)
y <- rbeta(150, 1, 4) 
head(y)

y.fit <- fitdistr(y, densfun = "beta",
                  start = list(shape1 = .5, shape2 = .5)) #从fitdistr（）中，我们可以获得分布的估计参数。我们显式地提取那些。

y.fit$estimate

summary(y.fit)
logLik(y.fit)

y.fit2 <- fitdistr(y, densfun = "normal") 
logLik(y.fit2)

plot(testDistribution(y)) ## 图1-13。叠加正态分布的密度图和正态Q-Q图

test.beta <- plot(testDistribution(y, "beta",
                       starts = list(shape1 = .5, shape2 = .5),
                       varlab = "Y", plot = FALSE))

test.normal <- plot(testDistribution(y, "normal", varlab = "Y", plot = FALSE))
plot_grid(test.beta$DensityPlot, test.beta$QQPlot,    ##Figure 1-14. Shows density plot with superimposed beta or normal distributions along with Q-Q plot fits
          test.normal$DensityPlot, test.normal$QQPlot, ncol = 2) ### plot_grid画多个图 2x2


set.seed(1234)
y <- rnbinom(500, mu = 5, size = 2) 
plot(testDistribution(y, "poisson")) #图1-15。离散观测到的比例和以蓝色表示的Poisson的理论概率
plot(testDistribution(y, "nbinom")) #图1-16。从负二项分布中观察到的具有理论概率的离散比例（以蓝色绘制）







