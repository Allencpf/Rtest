###################################################
### 3. Implementation
###################################################

library("mvord")
data("data_mvord_toy", package = "mvord")
str(data_mvord_toy)

data_toy_long <- cbind.data.frame(i = rep(1:100, 2), 
  j = rep(1:2, each = 100), Y = c(data_mvord_toy$Y1, data_mvord_toy$Y2), 
  X1 = rep(data_mvord_toy$X1, 2), X2 = rep(data_mvord_toy$X2, 2), 
  f1 = rep(data_mvord_toy$f1, 2), f2 = rep(data_mvord_toy$f2, 2))
str(data_toy_long)

system.time({res <- mvord(formula = MMO(Y, i, j) ~ 0 + X1 + X2, data = data_toy_long)})
system.time({res <- mvord(formula = MMO2(Y1, Y2) ~ 0 + X1 + X2, data = data_mvord_toy)})

names_constraints(formula = Y ~ 0 + X1 + X2 + f2, data = data_mvord_toy)

formula <- MMO2(Y1, Y2) ~ 1 + X1 : X2 + f1 + f2 * X1
names_constraints(formula, data = data_mvord_toy)

predict(res, subjectID = 1:6)
predict(res, type = "cum.prob", subjectID = 1:6)
predict(res, type = "class", subjectID = 1:6)

###################################################
### 4. Examples
###################################################

###################################################
### 4.1. Example 1: A simple model of firm ratings assigned by
### multiple raters
###################################################

data("data_cr", package = "mvord")
head(data_cr, n = 3)
str(data_cr, vec.len = 2.9)

op <- par(mfrow = c(1, 4), 
          oma = c(0, 1.1, 0, 0), 
          mar = c(2, 3, 5, 1))
cexf <- 1.8
barplot(table(data_cr$rater1),  ylim = c(0, 500), las = 1, main = "rater1", 
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2, 0.4, 0.6, 0.6))
barplot(table(data_cr$rater2), ylim = c(0, 500), las = 1, main = "rater2", 
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2, 0.4, 0.6, 0.6))
barplot(table(data_cr$rater3), ylim = c(0, 500), las = 1, main = "rater3", 
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2, 0.4, 0.6, 0.6))
barplot(table(data_cr$rater4), las = 1, ylim = c(0, 500), main = "rater4", 
        cex.lab = cexf, cex.names = cexf, cex.main = 2, cex.axis = cexf, col=rgb(0.2, 0.4, 0.6, 0.6))
par(op)

system.time({res_cor_probit_simple <-
    mvord(formula =
              MMO2(rater1, rater2, rater3, rater4) ~ 0 + LR + LEV + PR + RSIZE + BETA, data = data_cr)})
summary(res_cor_probit_simple, call = FALSE)

thresholds(res_cor_probit_simple)
coef(res_cor_probit_simple)
error_structure(res_cor_probit_simple)[[11]]

###################################################
### 4.2. Example 2: A more elaborate model of ratings assigned by
### multiple raters to a cross-section of firms
###################################################

system.time({
    res_cor_logit <- mvord(formula = MMO2(rater1, rater2, rater3, rater4) ~
                               0 + LR + LEV + PR + RSIZE + BETA, data = data_cr, link = mvlogit(), 
                           coef.constraints = cbind(
                               c(1, 1, 1, 1), 
                               c(1, 2, 3, 4), 
                               c(1, 1, 1, 1), 
                               c(1, 1, 1, 2), 
                               c(1, 1, 2, 3)), 
                           threshold.constraints = c(1, 1, 2, 3))
})
summary(res_cor_logit, call = FALSE)
constraints(res_cor_logit)$BETA

cols <- rev(colorspace::sequential_hcl(round(200), 
                                       h = 260, c = c(80, 0), 
                                       l = c(30, 90), 
                                       power = 0.7))
scatterplot.mvord <- function(tab, 
                              zlim = NULL, 
                              col = cols, 
                              xlab = NULL, ylab = NULL, main = NULL, 
                              percent = FALSE, 
                              col.one.to.one.line = grey(0.4), 
                              col.bar.legend = TRUE, 
                              ...) {
    if (percent == "all") tab <- tab/sum(tab)*100
    if (percent == "row") tab <- sweep(tab, 1, rowSums(tab), "/")
    if (percent == "col") tab <- sweep(tab, 2, colSums(tab), "/")
    
    if (percent %in% c("all", "row", "col")) {
        zlim = c(0, 100)
        tab <- round(tab*100, 4)
    }

    tab[tab == 0] <- NA
    
    if (is.null(zlim))  zlim <- range(tab, na.rm = TRUE)
    
    plot.seq.x <- seq_len(nrow(tab))
    plot.seq.y <- seq_len(ncol(tab))
    labels.x <- rownames(tab)
    labels.y <- colnames(tab)
    
    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- ""
    
  image(x = plot.seq.x, y = plot.seq.y, z = tab, zlim = zlim, col = col, 
        xlab = "", ylab = "", 
        main = main, axes = FALSE, 
        xlim = c(min(plot.seq.x) - 1, max(plot.seq.x) + 1), 
        ylim = c(min(plot.seq.y) - 1, max(plot.seq.y) + 1), ...)
    axis(1, at = plot.seq.x, line = 0.5, labels = labels.x)
    axis(2, at = plot.seq.y, line = 0.5, labels = labels.y, las = 1)
    title(xlab= xlab)
    title(ylab= ylab, line = 4)
    
    if (!is.null(col.one.to.one.line))
        segments(min(plot.seq.x) - 0.5, min(plot.seq.y) - 0.5, 
                 max(plot.seq.x) + 0.5, max(plot.seq.y) + 0.5, lty = 3, 
             col = col.one.to.one.line)
    
    starting.par.settings <- par(no.readonly = TRUE)
    mai <- par("mai")
    fin <- par("fin")
    x.legend.fig <- c(1 - (mai[4]/fin[1]), 1)
    y.legend.fig <- c(mai[1]/fin[2], 1 - (mai[3]/fin[2]))
    x.legend.plt <- c(x.legend.fig[1] + (0.08 * (x.legend.fig[2] -
                                                 x.legend.fig[1])), x.legend.fig[2] - (0.6 * (x.legend.fig[2] -
                                                                                              x.legend.fig[1])))
    y.legend.plt <- y.legend.fig
    cut.pts <- seq(zlim[1], zlim[2], length = length(col) + 1)
    z <- (cut.pts[1:length(col)] + cut.pts[2:(length(col) + 1)])/2
    par(new = TRUE, pty = "m", plt = c(x.legend.plt, y.legend.plt))
    image(x = 1, y = z, z = matrix(z, nrow = 1, ncol = length(col)), 
          col = col, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis = 0.8, tcl = -0.1)
    box()
    mfg.settings <- par()$mfg
    par(starting.par.settings)
    par(mfg = mfg.settings, new = FALSE)
}

op <- par(mfrow = c(2, 2), 
          oma = c(1, 1, 0, 0), 
          mar = c(4, 5, 2, 3))
op <- par(mfrow = c(2, 2), 
          oma = c(0, 0, 0, 0), 
          mar = c(4, 5, 2, 3))
scatterplot.mvord(
    table(res_cor_logit$rho$y[, 1], marginal_predict(res_cor_logit, type = "class")[, 1]), 
                  main = "rater 1", ylab = "predicted", xlab = "observed", percent = "row")
scatterplot.mvord(
    table(res_cor_logit$rho$y[, 2], marginal_predict(res_cor_logit, type = "class")[, 2]), 
                  main = "rater 2", ylab = "predicted", xlab = "observed", percent = "row")
scatterplot.mvord(
    table(res_cor_logit$rho$y[, 3], marginal_predict(res_cor_logit, type = "class")[, 3]), 
                  main = "rater 3", ylab = "predicted", xlab = "observed", percent = "row")
scatterplot.mvord(
    table(res_cor_logit$rho$y[, 4], marginal_predict(res_cor_logit, type = "class")[, 4]), 
                  main = "rater 4", ylab = "predicted", xlab = "observed", percent = "row")
par(op)

BICvec <- c(BIC(res_cor_probit_simple), BIC(res_cor_logit))
AICvec <- c(AIC(res_cor_probit_simple), AIC(res_cor_logit))
loglikvec <- c(logLik(res_cor_probit_simple), logLik(res_cor_logit))
tab <- cbind(loglikvec, BICvec, AICvec)
colnames(tab) <- c("logLik", "BIC", "AIC")
rownames(tab) <- c("Example 1", "Example 2")
print(xtable::xtable(tab, label = "tab:bic"), 
      only.contents = TRUE, math.style.negative = TRUE, include.colnames = FALSE)

###################################################
### 4.3. Example 3: Ratings assigned by one rater to a panel of firms
###################################################

data("data_cr_panel", package = "mvord")
str(data_cr_panel, vec.len = 3)
head(data_cr_panel, n = 3)

system.time({
    res_AR1_probit <- mvord(
        formula = MMO(rating, firm_id, year) ~ LR + LEV + PR + RSIZE +  BETA, 
        data = data_cr_panel, 
        error.structure = cor_ar1(~ BSEC), 
        coef.constraints = c(rep(1, 4), rep(2, 4)), 
        threshold.constraints = c(rep(1, 8)), 
        threshold.values = rep(list(c(0, NA, NA, NA)), 8), 
        link = mvprobit(), 
        control = mvord.control(solver = "BFGS",  solver.optimx.control = list(trace = TRUE)))
})
summary(res_AR1_probit, short = TRUE, call = FALSE)
error_structure(res_AR1_probit)
head(error_structure(res_AR1_probit, type = "corr"), n = 3)
head(error_structure(res_AR1_probit, type = "sigmas"), n = 1)


