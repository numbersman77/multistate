# nonparametric estimation of cumulative hazards and transition probabilities using mstate
c1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data = sim1, method = "efron")

nt <- 3 # give number of transitions, to be able to easily change that later when considering more transitions
rt <- rep(0, nt)
newd <- data.frame("trans" = 1:nt)
attr(newd, "trans") <- tmat
class(newd) <- c("msdata", "data.frame")

# 1st model - Markov, no proportionality assumption on the baseline hazards
# newd only needed as argument for msfit if we have more than just strata(trans)
# on the RHS of the coxph object above
newd$strata <- 1:3
msf1 <- msfit(c1, newdata = newd, trans = tmat)

# extract cumulative hazard estimates
nonparHaz1 <- subset(msf1$Haz, trans == 1)
nonparHaz2 <- subset(msf1$Haz, trans == 2)
nonparHaz3 <- subset(msf1$Haz, trans == 3)

# transition probabilities out of 0
nonparPred <- probtrans(msf1, predt = 0)
P00 <- nonparPred[[1]][, c("time", "pstate1")]
P01 <- nonparPred[[1]][, c("time", "pstate2")]
P02 <- nonparPred[[1]][, c("time", "pstate3")]
P11 <- nonparPred[[2]][, c("time", "pstate2")]
P12 <- nonparPred[[2]][, c("time", "pstate3")]
nonparPred <- data.frame(cbind("time" = P00$time, "P00" = P00$pstate1, "P01" = P01$pstate2, 
                               "P02" = P02$pstate3, "P11" = P11$pstate2, "P12" = P12$pstate3, 
                               "PFS" = P00$pstate1, "OS" = P00$pstate1 + P01$pstate2))



