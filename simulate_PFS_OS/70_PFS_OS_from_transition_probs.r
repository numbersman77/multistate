tte <- derivePFS_OS_events(sim1)

# plot PFS and OS
par(mfrow = c(1, 2), las = 1)
with(nonparPred, plot(time, PFS, type = "l", col = "red", xlim = c(0, 20), main = "PFS"))
with(parPred, lines(time, PFS, type = "l", col = "blue"))
legend(3, 1, c("nonparametric", "parametric", "Kaplan-Meier raw data"), lty = 1, col = c("red", "blue", 1), bty = "n")
lines(survfit(Surv(pfs, pfsfail) ~ 1, data = tte, conf.int = FALSE))

with(nonparPred, plot(time, OS, type = "l", col = "red", xlim = c(0, 60), main = "OS"))
with(parPred, lines(time, OS, type = "l", col = "blue"))
lines(survfit(Surv(os, osfail) ~ 1, data = tte, conf.int = FALSE))








