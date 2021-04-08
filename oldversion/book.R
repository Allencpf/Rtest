xs.nz <- transform(xs.nz, age50 = age - 50) # Intercepts will be important

rr.binom <-
  rrvglm(cbind(asthma, cancer, diabetes, heartattack, stroke) ~ ethnicity + sex + age50 + smokenow + 
           fh.cancer + fh.heartdisease + depressed + embarrassed + fedup + hurt + miserable +nofriend + moody + nervous + tense + worry + worrier,  # 11 psychological nofriend + moody + nervous + tense + worry + worrier, # variables
           noRRR = ~ ethnicity + sex + age50 + smokenow + fh.cancer + fh.heartdisease, binomialff(multiple.responses = TRUE), data = xs.nz)

round(sort(concoef(rr.binom)[, 1]), digits = 2)

model3 <- rcim(auuc, Rank = 1, fam = multinomial,
               M = ncol(auuc)-1, cindex = 2:(ncol(auuc)-1), trace = TRUE)


data("ships", package = "MASS")
ships <- within(ships, year <- as.factor(year))
ships <- within(ships, period <- as.factor(period))
Shipmodel <- vglm(incidents ~ type + year + period, poissonff, data = ships,
                    subset = (service > 0), offset = log(service))

rcim.ship <- rcim(Qvar(Shipmodel, "type"),fam = uninormal("explink"), maxit = 99)
quasiVar <- qvar(rcim.ship)
grc0.alcoff <- rcim(moffset(alcoff, "6", postfix = "*"), rprefix = "Hour.24.", cprefix = "Day.")

plot(grc0.alcoff, rcol = "blue", ccol = "orange", rfirst = 14, cfirst = 1, rtype = "h", ctype = "h", lwd = 3, ylim = c(-1.5, 1.5), las = 1, cylab = "Effective daily effects", rylab = "Hourly effects",
     rxlab = "Hour", cxlab = "Effective day") -> plot.grc0.alcoff


