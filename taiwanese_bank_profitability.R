library(hyperbolicDEA)
library(stargazer)


# Data from Juo et al. (2015), made available by Barbero and Zofio (2023).
# See README.md for the full references.
bank_data <- read.csv("Taiwanese_bank_data.csv", check.names = FALSE)


# In the raw data kindly provided by Jose Zofio and Javier Barbero,
# are some minor deviations probably introduced due to some Excel
# automatic formatting. Specifically, the number of employees show
# some small decimal points. Since the number of employees should 
# be an integer, these values are rounded.
bank_data[,"x2(10)"] <- round(bank_data[,"x2(10)"],0)


# Selecting the relevant columns for inputs and outputs in the year 2010
inputs <- bank_data[, c("x1(10)", "x2(10)", "x3(10)")]
outputs <- bank_data[, c("y1(10)", "y2(10)")]

input_prices <- bank_data[, c("w1(10)", "w2(10)", "w3(10)")]
output_prices <- bank_data[, c("p1(10)", "p2(10)")]


# Descriptive statistics
stargazer(cbind(inputs, outputs),
          title="Descriptive Statistics", 
          summary.stat = c("mean","median", "max" ,"min", "sd"), digits=0,
          covariate.labels = c("Financial funds", "Employees", "Physical capital",
                               "Financial investments", "Loans"))


# Descriptive statistics prices
stargazer(cbind(input_prices, output_prices),
          title="Descriptive Statistics", 
          summary.stat = c("mean","median", "max" ,"min", "sd"), digits=4,
          covariate.labels = c("Financial funds", "Employees", "Physical capital",
                               "Financial investments", "Loans"))


#########################################
# Analysis of profitability efficiency #
########################################

# For easier data manipulations transform into matrix
inputs <- as.matrix(inputs)
outputs <- as.matrix(outputs)
input_prices <- as.matrix(input_prices)
output_prices <- as.matrix(output_prices)

# Technical Efficiency
TE_est <- hyperbolicDEA(inputs, outputs, RTS = "crs", 
                        ALPHA = 0.5)
TE <- TE_est$eff



# First equivalent to classical profitability efficiency
# using a one to one trade-off matrix between outputs and inputs
T_1 <- matrix(c(1, -1, 0, 0, 0,
                -1, 1, 0, 0, 0,
                0, 0, 1, -1, 0, 
                0, 0, 0, 1, -1, 
                0, 0, -1, 0, 1), nrow = 5, byrow = T)

# Following Model 6 in the manuscript, each Model is developed with 
# the prices of the DMU under observation. 

PE_T1 <- c()
PE_T1_lambdas <- NULL
PE_T1_mus <- NULL
for (i in 1:nrow(inputs)) {
  PE_est <- hyperbolicDEA(inputs %*% diag(input_prices[i, ]),
                         outputs %*% diag(output_prices[i, ]),
                         RTS = "crs", WR = T_1, ALPHA = 0.5)
                    
  PE_T1 <- c(PE_T1, PE_est$eff[i])
  PE_T1_lambdas <- rbind(PE_T1_lambdas, PE_est$lambdas[i,])
  PE_T1_mus <- rbind(PE_T1_mus, PE_est$mus[i,])
}

AE_T1 <- PE_T1/TE


# Testing equivalence to classic approach (Model 2 in manuscript)
PE_classic <- nlprofitDEA(inputs, outputs, input_prices, output_prices, RTS = "crs")
all.equal(PE_T1, PE_classic$profit_eff) # TRUE

# Estimation of optimal quantities using information on mu's as illustrated 
# in Figure 2 in the manuscript to avoid scaling issues. Hence, to obtain
# the optimal values we have the equiproportional reduction/increase in 
# inputs/outputs due to the efficiency score and then adjust for the
# reverse trade-offs using the mu's to get the optimal point. The mu's 
# are measured in monetary values, hence we need to divide by the prices
# to get the quantities.
opt_values_T1 <- NULL
for (i in 1:nrow(inputs)) {
  input_values <- inputs[i, ] * PE_T1[i] - t(T_1[, 3:5]) %*% PE_T1_mus[i,] / input_prices[i,]
  output_values <- outputs[i, ] * 1/PE_T1[i] - t(T_1[, 1:2]) %*% PE_T1_mus[i,] / output_prices[i,]
  opt_values_T1 <- rbind(opt_values_T1, c(input_values, output_values))
}


# Adjust trade-offs to focus on increase in 
# reallocation for input 1. Due to the perspective from 
# the peer, a focus on increase is connected to a negative
# trade-off parameter. See trade-off 5.
T_2 <- matrix(c(1, -1, 0, 0, 0,
                -1, 1, 0, 0, 0,
                0, 0, 0, -1, 1, 
                0, 0, 0, 1, -1, 
                0, 0, -1, 0, 1), nrow = 5, byrow = T)

PE_T2 <- c()
PE_T2_lambdas <- NULL
PE_T2_mus <- NULL
for (i in 1:nrow(inputs)) {
  PE_est <- hyperbolicDEA(inputs %*% diag(input_prices[i, ]),
                          outputs %*% diag(output_prices[i, ]),
                          RTS = "crs", WR = T_2, ALPHA = 0.5)
  
  PE_T2 <- c(PE_T2, PE_est$eff[i])
  PE_T2_lambdas <- rbind(PE_T2_lambdas, PE_est$lambdas[i,])
  PE_T2_mus <- rbind(PE_T2_mus, PE_est$mus[i,])
}



AE_T2 <- PE_T2/TE

opt_values_T2 <- NULL
for (i in 1:nrow(inputs)) {
  input_values <- inputs[i, ] * PE_T2[i] - t(T_2[, 3:5]) %*% PE_T2_mus[i,] / input_prices[i,]
  output_values <- outputs[i, ] * 1/PE_T2[i] - t(T_2[, 1:2]) %*% PE_T2_mus[i,] / output_prices[i,]
  opt_values_T2 <- rbind(opt_values_T2, c(input_values, output_values))
}


#####################
# Results presented #
# in the manuscript #
#####################

# Efficiency scores
cbind(bank_data[, 1, drop = FALSE], round(cbind(TE, AE_T1, PE_T1, AE_T2, PE_T2), 3))

# Some differences in efficiency scores as T2 is less discriminatory
# Higher value in input 1 due to restriction on reallocation
diff_eff <- PE_T2 - PE_T1
diff_values <- opt_values_T2[, 1] - opt_values_T1[, 1]

diff_results <- cbind(bank_data[, 1, drop = FALSE], round(cbind(diff_eff, diff_values), 3))
# Drop zero values
diff_results <- diff_results[!(diff_results[,2]==0 & diff_results[,3]==0), ]
diff_results

# Optimal values
cbind(bank_data[, 1, drop = FALSE], round(opt_values_T1))
cbind(bank_data[, 1, drop = FALSE], round(opt_values_T2))

# Optimal values using the classical approach
cbind(bank_data[, 1, drop = FALSE], PE_classic$opt_value)


####################################################
# Additional results not presented in the manuscript
####################################################
# Lambda values
cbind(bank_data[, 1, drop = FALSE], round(PE_T1_lambdas, 3))
cbind(bank_data[, 1, drop = FALSE], round(PE_T2_lambdas, 3))


# Optimal values prior to technical efficiency
opt_TE_adj_T1 <- cbind(1/TE*opt_values_T1[, 1:3], TE*opt_values_T1[, 4:5])
cbind(bank_data[, 1, drop = FALSE], round(opt_TE_adj_T1))

opt_TE_adj_T2 <- cbind(1/TE*opt_values_T2[, 1:3], TE*opt_values_T2[, 4:5])
cbind(bank_data[, 1, drop = FALSE], round(opt_TE_adj_T2))

# Checking similarity of classic and new approach and interpretation of
# profitability efficiency as square root ratio of observed to optimal 
# profitability (PA). Same profitability scores as the classical 
# approach example for DMU 2 and adjusted to new optimal allocation with T2
PA_DMU2 <- sum(outputs[2, ] * output_prices[2, ])/sum(inputs[2, ] * input_prices[2,])
PA_classic_DMU2 <- sum(PE_classic$opt_value[2, 4:5] * output_prices[2, ])/sum(PE_classic$opt_value[2, 1:3] * input_prices[2,])
PA_T1_DMU2 <- sum(opt_values_T1[2, 4:5] * output_prices[2, ])/sum(opt_values_T1[2, 1:3] * input_prices[2,])
PA_T2_DMU2 <- sum(opt_values_T2[2, 4:5] * output_prices[2, ])/sum(opt_values_T2[2, 1:3] * input_prices[2,])

# Efficiency scores accordingly
sqrt(PA_DMU2/PA_classic_DMU2)
sqrt(PA_DMU2/PA_T1_DMU2)
sqrt(PA_DMU2/PA_T2_DMU2)


#######################################
# Analysis for the Discussion section #
#######################################
# Estimating profit efficiency for comparison using Price means as Pastor et. al (2024)
# The measure is only defined in vrs and has issues with negative profits
input_prices_mean <- matrix(rep(colMeans(input_prices), each = nrow(input_prices)),
                            nrow = nrow(input_prices))
output_prices_mean <- matrix(rep(colMeans(output_prices), each = nrow(output_prices)),
                             nrow = nrow(output_prices))

profit_est <- lprofitDEA(inputs, outputs, input_prices_mean, output_prices_mean, RTS = "vrs")
profit_est$profit_eff
profit_est$opt_value
profit_est$lambdas

# Market shares bank of Taiwan and comparison to Export-Import bank
inputs[2, ]/colSums(inputs)
outputs[2, ]/colSums(outputs)

inputs[2, ] / inputs[1, ]
outputs[2, ] / outputs[1, ]

# Market volume with reallocation according to profit
colSums(profit_est$opt_value)/colSums(cbind(inputs, outputs))
# Market volume with reallocation in accordance to classical profitability
colSums(PE_classic$opt_value)/colSums(cbind(inputs, outputs))
# Market volume with reallocation in accordance to T1
colSums(opt_values_T1)/colSums(cbind(inputs, outputs))
# Market volume with reallocation in accordance to T2
colSums(opt_values_T2)/colSums(cbind(inputs, outputs))


