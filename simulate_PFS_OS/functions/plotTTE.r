plotTTE <- function(time, event, group, q0 = c(0.8, 0.9), grid = 1000, xli = NA, ylimHR = c(0, 2)){
  
  # x% of patients still event-free
  qs <- quantile(time, probs = q0)
  
  # x-axis
  if (is.na(xli)){xli <- c(0, max(time))} else {xli <- c(0, xli)}
  
  # survival functions
  par(las = 1, mar = c(4.5, 5, 3, 1))
  layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
  plot(survfit(Surv(time, event) ~ group), xlim = xli, mark = "/", col = 2:3, lty = 1, main = "survival functions", xlab = "time", ylab = "S(t)")
  legend("bottomleft", levels(group), col = 2:3, lty = 1, lwd = 1, bty = "n")
  
  # kernel estimates for each quantiles
  h1 <- matrix(NA, ncol = length(qs), nrow = grid)
  h2 <- h1
  xs <- h1
  i1 <- (group == levels(group)[1])
  i2 <- (group == levels(group)[2])
  
  for (i in 1:length(qs)){
    tmp <- muhaz(time[i1], event[i1], max.time = qs[i], min.time = 0, n.est.grid = grid, bw.method = "l")
    xs[, i] <- tmp$est.grid
    h1[, i] <- tmp$haz.est
    h2[, i] <- muhaz(time[i2], event[i2], max.time = qs[i], min.time = 0, n.est.grid = grid, bw.method = "l")$haz.est
  }
  
  plot(0, 0, type = "n", xlim = xli, ylim = c(0, max(h1, h2)), main = "hazard functions", xlab = "time", ylab = "h(t)")
  for (i in 1:length(qs)){
    lines(xs[, i], h1[, i], col = 2, lwd = 1)
    lines(xs[, i], h2[, i], col = 3, lwd = 1)
  }
  
  # ratio of hazards
  plot(0, 0, type = "n", xlim = xli, ylim = ylimHR, main = "ratio of hazard functions, and hazard ratio", xlab = "time", ylab = expression("h"[1]*"(t) / h"[2]*"(t)"))
  
  # add HR to plot
  cox1 <- summary(coxph(Surv(time, event) ~ group))
  hr <- exp(coef(cox1)[1])
  segments(0, hr, max(time), hr, col = 1, lty = 2)
  segments(0, 1, max(time), 1, col = 1, lty = 2)
  
  for (i in 1:length(qs)){lines(xs[, i], h2[, i] / h1[, i], col = i + 1, lwd = 2)}
  
  return(hr)
}

