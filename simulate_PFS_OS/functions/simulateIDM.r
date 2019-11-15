simulateIDM <- function(shape = rep(1, 3), scale, shapecens = 1, scalecens, n = 10, ts = seq(0, 100, by = .01)){
  
  require(mstate)

  # grid of timepoints
  tn <- length(ts)

  # transition matrix for illness-death model
  tmat <- trans.illdeath()

  # cumulative hazard for every transition
  Haz1 <- data.frame("time" = ts, 
                     "Haz" = -pweibull(ts, shape = shape[1], scale = scale[1], lower = FALSE, log = TRUE),
                     "trans" = rep(1, tn))
  Haz2 <- data.frame("time" = ts, 
                     "Haz" = -pweibull(ts, shape = shape[2], scale = scale[2], lower = FALSE, log = TRUE),
                     "trans" = rep(2, tn))
  Haz3 <- data.frame("time" = ts, 
                     "Haz" = -pweibull(ts, shape = shape[3], scale = scale[3], lower = FALSE, log = TRUE),
                     "trans" = rep(3, tn))
  Haz <- as.data.frame(rbind(Haz1, Haz2, Haz3))
  
  # censoring distribution
  cens <- data.frame(cbind("time" = ts, 
                           "Haz" = -pweibull(ts, shape = shapecens, scale = scalecens, lower = FALSE, log = TRUE)))

  # mssample help says you need time and Haz. However, if you dig into the code of mssample1 on github
  # you see that you also need a column surv, likely just equal to exp(- Haz)
  cens[, "surv"] <- exp(- cens[, "Haz"])

  # now simulate from multistate model
  sim1 <- mssample(Haz = Haz, trans = tmat, M = n, cens = cens, output = "data", do.trace = 25)
  sim1 <- as.data.frame(cbind("id" = sim1$id, "Tstart" = sim1$Tstart, "Tstop" = sim1$Tstop, 
                            "duration" = sim1$duration, "from" = sim1$from, "to" = sim1$to,
                            "status" = sim1$status, "trans" = sim1$trans))
  sim1[sim1[, "Tstart"] == sim1[, "Tstop"], "Tstop"] <- sim1[sim1[, "Tstart"] == sim1[, "Tstop"], "Tstart"] + 10^-4

  return(sim1)
}






