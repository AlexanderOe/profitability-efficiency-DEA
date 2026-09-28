library(hyperbolicDEA)
x1 <- c(1, 2, 4, 4)
x2 <- c(5, 3, 2, 4)
y <- c(1, 1, 1, 1)

WR <- matrix(c(0, 1, -1, 0, -1, 1), nrow = 2, byrow = TRUE)

cost_est <- wrDEA(cbind(x1, x2), y, ORIENTATION = "in", RTS = "CRS", WR = WR)
TE_est <- wrDEA(cbind(x1, x2), y, ORIENTATION = "in", RTS = "CRS")

CE <- cost_est$eff
TE <- TE_est$eff
AE <- CE/TE

# Finding the reallocated D' before technical efficiency adjustment
cost_est$mu[4,] * 1/CE[4]

# 0.8 on mu 1 meaning a reallocation of 0.8 monetary units from x1 to x2
# to obtain the optimal input mix for DMU D. However, this is only reallocation
# hence there is no cost improvement. To obtain the allocative cost efficient
# point which only adjusts for AE, one can estimate the AE equiproportional 
# reduction.

(4-0.8) * AE[4] # x1 D' is (4-0.8) and D* adjusted to AE
(4+0.8) * AE[4] # x2 D' is (4+0.8) and D* adjusted to AE

################
# Illustration #
################
# illustration value space
pdf("AE_illustration.pdf", width = 6, height = 6)
plot(x1,x2, xlim = c(0,6), ylim = c(0,6), xaxs = "i",yaxs = "i", xlab = "",
     ylab = "")
lines(c(7,4,2,1,1),c(2,2,3,5,6), col = "black")
text(cbind(x1, x2),c("A", "B", "C", "D"), pos = c(2,3,3,4), col = "red")
lines(c(5,0),c(0,5), col = "blue")
lines(c(8, 0), c(0, 8), col = adjustcolor("blue", alpha.f = 0.5), lty = "dashed")
#text(x = 1, y = 4.5, labels = "Iso Cost", col = "blue", pos = 3)
lines(c(0,4),c(0,4), col = "black", lty = 2)

# Add the optimal mix for DMU D
points(x = 3.2, y = 4.8)
text(x = 3.2, y = 4.8, labels = "D'", pos = 3, col = "red")
lines(c(0,3.2),c(0,4.8), col = "black", lty = 2)

# Add the optimal mix for DMU D adjusted for AE
points(x = 3, y = 4.5)
text(x = 3, y = 4.5, labels = expression(hat(D)), pos = 2, col = "red")

dev.off()
