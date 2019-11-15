# plot estimated transition probabilities
par(mfrow = c(3, 2), mar = c(4.5, 4.5, 3, 1), las = 1)

for (i in 1:5){
  plot(nonparPred[, "time"], nonparPred[, i + 1], type = "l", col = "red", xlab = "time", ylab = "transition probability", main = colnames(nonparPred)[i + 1])
  lines(parPred[, "time"], parPred[, i + 1], col = "blue")
}

plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
legend(-1, 1, c("nonparametric", "parametric"), lty = 1, col = c("red", "blue"), bty = "n")