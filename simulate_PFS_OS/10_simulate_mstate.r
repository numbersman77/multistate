# Weibull cumulative hazard

# grid of timepoints
ts <- seq(0, 100, by = 1)
tn <- length(ts)

# number of patients
M <- 10 ^ 2

# transition matrix for illness-death model
tmat <- trans.illdeath()

# exponential rates if shape = 1
shape <- c(1, 1, 1) / 2

# median time to event, for every transition
med <- c(5, 10, 15)
lambda <- log(2) / med    
scale <- 1 / lambda

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
medcens <- 30
lambdacens <- log(2) / medcens
scalecens <- 1 / lambdacens
cens <- data.frame(cbind("time" = ts, 
                   "Haz" = -pweibull(ts, shape = 1, scale = scalecens, lower = FALSE, log = TRUE)))

# mssample help says you need time and Haz. However, if you dig into the code of mssample1 on github
# you see that you also need a column surv, likely just equal to exp(- Haz)
cens[, "surv"] <- exp(- cens[, "Haz"])

# now simulate from multistate model
set.seed(1977)
sim1 <- mssample(Haz = Haz, trans = tmat, M = M, cens = cens, output = "data", do.trace = 25)
sim1 <- as.data.frame(cbind("id" = sim1$id, "Tstart" = sim1$Tstart, "Tstop" = sim1$Tstop, 
                            "duration" = sim1$duration, "from" = sim1$from, "to" = sim1$to,
                            "status" = sim1$status, "trans" = sim1$trans))
ind <- (sim1[, "Tstart"] == sim1[, "Tstop"])
sim1[ind, "Tstop"] <- sim1[ind, "Tstart"] + 10^-4

sim1$trt <- 1
attr(sim1, "trans") <- tmat
class(sim1) <- c("msdata", "data.frame")
sim1 <- expand.covs(sim1, c("trt")) 
events(sim1)






