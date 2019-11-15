# plots of cumulative hazards
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 0, 0), las = 1)
plot(msf1, lty = c(2, 2, 2), col = 1:3, xlab = "time since randomization (months)",
     ylab = "Stratified baseline hazards", legend.pos = c(10, 4),  
     main = "no assumption on hazards", ylim = c(0, 5))

# true cumulative hazards
lines(ts, Haz1$Haz, col = 1, lwd = 2)
lines(ts, Haz2$Haz, col = 2, lwd = 2)
lines(ts, Haz3$Haz, col = 3, lwd = 2)

# nonparametric cumulative hazards
lines(nonparHaz1$time, nonparHaz1$Haz, col = 1, lty = 2, type = "s")
lines(nonparHaz2$time, nonparHaz2$Haz, col = 2, lty = 2, type = "s")
lines(nonparHaz3$time, nonparHaz3$Haz, col = 3, lty = 2, type = "s")

# parametric cumulative hazards
lines(parHaz1$time, parHaz1$Haz, col = 1, lty = 3)
lines(parHaz2$time, parHaz2$Haz, col = 2, lty = 3)
lines(parHaz3$time, parHaz3$Haz, col = 3, lty = 3)





