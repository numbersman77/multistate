# parametric estimation of cumulative hazards and transition probabilities using flexsurv
paramWei <- flexsurvreg(Surv(Tstart, Tstop, status) ~ trans + shape(trans), data = sim1, dist = "weibull")
mfwei <- msfit.flexsurvreg(paramWei, t = ts, trans = tmat)

# extract cumulative hazard estimates
parHaz1 <- subset(mfwei$Haz, trans == 1)
parHaz2 <- subset(mfwei$Haz, trans == 2)
parHaz3 <- subset(mfwei$Haz, trans == 3)

# transition probabilities out of 0
parPred <- data.frame(matrix(NA, nrow = length(ts), ncol = 6))
colnames(parPred) <- c("time", "P00", "P01", "P02", "P11", "P12")
for (i in 1:length(ts)){
  fit.i <- pmatrix.fs(paramWei, t = ts[i], trans = tmat)
  parPred[i, "time"] <- ts[i]
  parPred[i, "P00"] <- fit.i[1, 1]
  parPred[i, "P01"] <- fit.i[1, 2]
  parPred[i, "P02"] <- fit.i[1, 3]
  parPred[i, "P11"] <- fit.i[2, 2]
  parPred[i, "P12"] <- fit.i[2, 3]

  if(i / 10 == round(i / 10)){print(paste("run ", i, " of ", length(ts), " done", sep = ""))}  
}

parPred <- data.frame(cbind(parPred, "PFS" = parPred[, "P00"], "OS" = parPred[, "P00"] + parPred[, "P01"]))



