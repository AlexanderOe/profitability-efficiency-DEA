library(hyperbolicDEA)

# Example data
dea_data <- data.frame(
  x1 = c(80, 20, 60, 30, 10, 90),  # Input 1
  x2 = c(20, 40, 70, 50, 45, 15),  # Input 2
  x3 = c(30, 25, 27, 45, 35, 75),   # Input 3
  y1 = c(100, 85, 70, 50, 60, 80),   # Output 1
  y2 = c(95, 80, 65, 55, 45, 70)    # Output 2
)

rownames(dea_data) <- paste0("DMU", 1:6)
dea_data

# Technical Efficiency
TE_est <- hyperbolicDEA(dea_data[,1:3], dea_data[,4:5], RTS = "crs", 
                    ALPHA = 0.5)
TE <- TE_est$eff


# Profitability Efficiency with different structures
#
# First equivalent to classical profitability efficiency
# using a one to one trade-off matrix between outputs and inputs
T_1 <- matrix(c(1, -1, 0, 0, 0,
                -1, 1, 0, 0, 0,
                0, 0, 1, -1, 0, 
                0, 0, 0, 1, -1, 
                0, 0, -1, 0, 1), nrow = 5, byrow = T)

PE_T1_est <- hyperbolicDEA(dea_data[,1:3], dea_data[,4:5], RTS = "crs", 
                        ALPHA = 0.5, WR = T_1)


# Optimal profitability values
# Multiply the lambdas with the respective input and output

opt_values <- PE_T1_est$lambdas %*% as.matrix(dea_data)

# Efficiency scores
PE_T1 <- PE_T1_est$eff
AE_T1 <- PE_T1/TE

# Decomposed AE scores as we have common price, not adjusted for that
# with alpha 0.5 it is equally distributed across inputs and outputs
AE_T1_in <- sqrt(rowSums(opt_values[, 1:3]) / rowSums((dea_data[, 1:3] * TE)))
AE_T1_out <- sqrt(rowSums(dea_data[, 4:5]) / rowSums((opt_values[, 4:5] * TE)))



# Alternative estimation using information on mu's as illustrated in
# Figure 2 in the manuscript to avoid scaling issues. Hence, to obtain
# the optimal values we have the equiproportional reduction/increase in 
# inputs/outputs due to the efficiency score and then adjust for the
# reverse trade-offs using the mu's to get the optimal point. 
opt_values_T1_alt <- NULL
for (i in 1:nrow(dea_data)) {
  input_values <- dea_data[i, 1:3] * PE_T1_est$eff[i] - t(T_1[, 3:5]) %*% PE_T1_est$mus[i,]
  output_values <- dea_data[i, 4:5] * 1/PE_T1_est$eff[i] - t(T_1[, 1:2]) %*% PE_T1_est$mus[i,]
  opt_values_T1_alt <- rbind(opt_values_T1_alt, c(input_values, output_values))
}

opt_values_T1_alt
opt_values



# Adjust trade-offs to focus on increase in 
# reallocation for input 1. Due to the perspective from 
# the peer, a focus on increase is connected to a negative
# trade-off parameter. See trade-off 5.
T_2 <- matrix(c(1, -1, 0, 0, 0,
                -1, 1, 0, 0, 0,
                0, 0, 0, -1, 1, 
                0, 0, 0, 1, -1, 
                0, 0, -1, 0, 1), nrow = 5, byrow = T)

PE_T2_est <- hyperbolicDEA(dea_data[,1:3], dea_data[,4:5], RTS = "crs", 
                                      ALPHA = 0.5, WR = T_2)

PE_T2 <- PE_T2_est$eff
AE_T2 <- PE_T2/TE

opt_values_T2 <- PE_T2_est$lambdas %*% as.matrix(dea_data)

# Alternative estimation using information on mu's
opt_values_T2_alt <- NULL
for (i in 1:nrow(dea_data)) {
  input_values <- dea_data[i, 1:3] * PE_T2_est$eff[i] - t(T_2[, 3:5]) %*% PE_T2_est$mus[i,]
  output_values <- dea_data[i, 4:5] * 1/PE_T2_est$eff[i] - t(T_2[, 1:2]) %*% PE_T2_est$mus[i,]
  opt_values_T2_alt <- rbind(opt_values_T2_alt, c(input_values, output_values))
}

opt_values_T2_alt
opt_values_T2


#####################
# Results presented #
# in the manuscript #
#####################

# Efficiency scores
round(cbind(TE, AE_T1, PE_T1, AE_T2, PE_T2), 3)

# Optimal values
round(opt_values, 3)

round(opt_values_T2, 3)

# Optimal values prior to technical efficiency
round(cbind(1/TE_est$eff*opt_values[, 1:3], TE_est$eff*opt_values[, 4:5]), 3)

round(cbind(1/TE_est$eff*opt_values_T2[, 1:3], TE_est$eff*opt_values_T2[, 4:5]), 3)


