remove(list = ls())
library(readr)
library(readxl)
#install.packages("zoo")
library("zoo")

# Import and Clean Data
historic_data <- read_excel("MACSS/Topics in Economics/HW 3/wrds_data.xlsx")
historic_data = historic_data[1:28]
historic_data$quarter = as.yearqtr(historic_data$Date, format = "%Y-%m")

v1 = names(historic_data)[2:28]
quarterly_data = aggregate(historic_data[v1],by=historic_data["quarter"],FUN=sum)


# Regression models
quarterly_data$risk_premium = quarterly_data$`S&P RETURN` - quarterly_data$`RISK-FREE RATE`
quarterly_data[9:28] <- quarterly_data[9:28] - quarterly_data[,5]

regressions <- lapply(9:28, function(x) lm(quarterly_data[,x] ~ 0 + quarterly_data$risk_premium))


# Stress Test Simulation (Severely Adverse Domestic Scenario)
adverse_scenario <- read_csv("MACSS/Topics in Economics/HW 3/2021-table_3a_supervisory_severely_adverse_domestic.csv")
adverse_scenario$market_return = diff(c(39219.6, adverse_scenario$`Dow Jones Total Stock Market Index (Level)`))/c(39219.6, adverse_scenario$`Dow Jones Total Stock Market Index (Level)`)[1:13]

adverse_scenario$risk_premium = adverse_scenario$market_return - adverse_scenario$`3-month Treasury rate`

coefs = sapply(1:length(regressions), function(x) coefficients(regressions[[x]]))

for (i in 1:20) {
  adverse_scenario[,i + 20]  <- adverse_scenario$`3-month Treasury rate` + coefs[i] * adverse_scenario$risk_premium
  names(adverse_scenario)[i + 20] <- names(quarterly_data)[8 + i]
}

adverse_scenario$total_return_adverse = rowMeans(adverse_scenario[21:40])

# Testing

# Goodness of Fit
r2_values = sapply(1:length(regressions), function(x) summary(regressions[[x]])$adj.r.squared)

# Residual Analysis
for (sector in 1:20) {
  plot(residuals(regressions[[i]]))
  abline(0,0)
  title(main = names(historic_data)[i+1],
        xlab = "Observations", ylab = "Residuals")
}

# Backtesting

backtest = rowMeans(quarterly_data[212:232,9:28])



# Stress Test Simulation (Supervisory Baseline Domestic Scenario)
baseline_scenario <- read_csv("MACSS/Topics in Economics/HW 3/2021-table_2a_supervisory_baseline_domestic.csv")
baseline_scenario$market_return = diff(c(39219.6, baseline_scenario$`Dow Jones Total Stock Market Index (Level)`))/c(39219.6, baseline_scenario$`Dow Jones Total Stock Market Index (Level)`)[1:13] # 39219.6 is the Q4 return for 2020 in the historical data

baseline_scenario$risk_premium = baseline_scenario$market_return - baseline_scenario$`3-month Treasury rate`

coefs = sapply(1:length(regressions), function(x) coefficients(regressions[[x]]))

for (i in 1:20) {
  baseline_scenario[,i + 20]  <- baseline_scenario$`3-month Treasury rate` + coefs[i] * baseline_scenario$risk_premium
  names(baseline_scenario)[i + 20] <- names(quarterly_data)[8 + i]
}

baseline_scenario$total_return_adverse = rowMeans(baseline_scenario[21:40])


