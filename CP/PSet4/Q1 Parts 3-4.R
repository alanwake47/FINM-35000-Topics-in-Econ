remove(list = ls())
library(readr)
library(readxl)
library(data.table)
library(stringr)

# Load Data
reddit <- read_csv("MACSS/Topics in Economics/HW 4/problem_set4.csv")
returns <- read_csv("MACSS/Topics in Economics/HW 4/crsp_daily.csv")

# Clean Data
reddit$date = substr(reddit$timestamp, 1, 10)
reddit$date = format(as.Date(reddit$date, format = "%Y-%m-%d"), "%Y%m%d")

sub_returns = returns[returns$date %in% unique(reddit$date),]
sub_returns = sub_returns[complete.cases(sub_returns$TICKER),]
sub_returns$mentions = 0

# Define list of all tickers that are mentioned at least once
relevant_tickers = c()
for (ticker in unique(sub_returns$TICKER)) {
  if (length(reddit$date[str_detect(reddit$tickers, ticker)]) > 0) {
    relevant_tickers = append(ticker, relevant_tickers)
    print(ticker)
  }
}

# For each relevant ticker, find dates that they're mentioned on, and define mentions 
#(commented out due to run time, line 38 loads dataset obtained after running lines 31-37)

#for (ticker in relevant_tickers) {
#  for (date in reddit$date[str_detect(reddit$tickers, ticker)]) {
#    print(ticker)
#    print(date)
#    sub_returns$mentions = ifelse(sub_returns$date == date & sub_returns$TICKER == ticker, sub_returns$mentions + 1, sub_returns$mentions)
#  }
#}
sub_returns <- read_csv("MACSS/Topics in Economics/HW 4/sub_returns.csv")

sub_returns$indicator = ifelse(sub_returns$mentions > 0, 1, 0)

sub_returns = data.table(sub_returns)
sub_returns = sub_returns[order(sub_returns$TICKER, sub_returns$date)]
sub_returns = sub_returns[, lag.mentions:=c(NA, mentions[-.N]), by=sub_returns$TICKER]
sub_returns = sub_returns[, lag.indicator:=c(NA, indicator[-.N]), by=sub_returns$TICKER]



# Regressions

mentioned = lm(RET ~ indicator, sub_returns)
mentioned_lag = lm(RET ~ lag.indicator, sub_returns)

num_mentions = lm(RET ~ mentions, sub_returns[sub_returns$TICKER %in% relevant_tickers])
num_mentions_lag = lm(RET ~ lag.mentions, sub_returns[sub_returns$TICKER %in% relevant_tickers])

summary(mentioned)
summary(mentioned_lag)
summary(num_mentions)
summary(num_mentions_lag)


