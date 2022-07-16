#to make sure I am not forgetting none of the libraries
library(dplyr)
library(quantmod)
library(ggplot2)
library(timeDate)
library(chron)
library(curl)
library(plotly)

#getSymbols gets daily close stock prices
#Liquid assets - total of 95M (mainly listed on exchanges in the US):
getSymbols("IXN") #Equity
getSymbols("SPY") #Equity
getSymbols("LQD") #Fixed Income - IG
getSymbols("IEF") #Fixed Income
getSymbols("VNQ") #Real Assets
getSymbols("GLD") #Commodities

asset_IXN <- getSymbols("IXN", auto.assign = FALSE)
asset_SPY <- getSymbols("SPY", auto.assign = FALSE)
asset_LQD <- getSymbols("LQD", auto.assign = FALSE)
asset_IEF <- getSymbols("IEF", auto.assign = FALSE)
asset_VNQ <- getSymbols("VNQ", auto.assign = FALSE)
asset_GLD <- getSymbols("GLD", auto.assign = FALSE)

#step2 - merge all Tto get the monthly returns all in one table
a_ret_IXN <- monthlyReturn(getSymbols("IXN", auto.assign = FALSE))
a_ret_SPY <- monthlyReturn(getSymbols("SPY", auto.assign = FALSE))
a_ret_LQD <- monthlyReturn(getSymbols("LQD", auto.assign = FALSE))
a_ret_IEF <- monthlyReturn(getSymbols("IEF", auto.assign = FALSE))
a_ret_VNQ <- monthlyReturn(getSymbols("VNQ", auto.assign = FALSE))
a_rets_GLD <- monthlyReturn(getSymbols("GLD", auto.assign = FALSE))

joined_monthlyreturns <- merge.xts(a_ret_IXN, a_ret_SPY, a_ret_LQD, 
                                  a_ret_IEF, a_ret_VNQ, a_rets_GLD)
# client's AUM: 95 Million USD
# Portfolio weightings/allocation:
IXN_alloc <- 0.175
SPY_alloc <- 0.221
LQD_alloc <- 0.182
IEF_alloc <- 0.113
VNQ_alloc <- 0.079
GLD_alloc <- 0.23

joined_portfolio_ret <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio = IXN_alloc*monthly.returns + SPY_alloc*monthly.returns.1 +
           LQD_alloc*monthly.returns.2 + IEF_alloc*monthly.returns.3 + 
           VNQ_alloc*monthly.returns.4 + GLD_alloc*monthly.returns.5)

#QUESTION 1
#I want to calculate the risk for each individual security
## sigma for last 12 months (I am multiplying by sqrt(12) to annualyze)
#this is how many monthly observations we have in our data frame:
time_index <- nrow(joined_monthlyreturns) 
#need to convert to data frame to be able to subset based on indexing:
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) 
IXN_sigma <- sd(joined_monthlyreturns$monthly.returns[(time_index-11) : time_index]) * sqrt(12) 
SPY_sigma <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index]) * sqrt(12)
LQD_sigma <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index]) * sqrt(12)
IEF_sigma <- sd(joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12)
VNQ_sigma <- sd(joined_monthlyreturns$monthly.returns.4[(time_index-11) : time_index]) * sqrt(12)
GLD_sigma <- sd(joined_monthlyreturns$monthly.returns.5[(time_index-11) : time_index]) * sqrt(12)

#Calculating sigma for entire portfolio
#now calculating sigma for the portfolio variable:
time_index <- nrow(joined_monthlyreturns) 
#this is how many monthly observations we have in our data frame 
#need this to pick up that last month in df
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) 
#need to convert to data frame to be able to subset based on indexing
portfolio_sigma <- sd(joined_monthlyreturns$portfolio[(time_index-11) : time_index]) * sqrt(12) 
    print(IXN_sigma) 
    print(SPY_sigma)
    print(LQD_sigma)
    print(IEF_sigma)
    print(VNQ_sigma)
    print(GLD_sigma)
    print(portfolio_sigma)


#QUESTION 2
#Last 12 months returns for each security and for the portfolio
last_12_months_portfolio <- joined_portfolio_ret[(time_index-11) : time_index, ]

#Question 3 - 12 months sharp ratio for each security
# Share ratio will have a 12M expected return, and a 12M sigma
# considering the risk free rate as 0.001
riskfree <- 0.001

#we've calculated the expectation using the mean() function
IXN_sharpe <- (mean(joined_monthlyreturns$monthly.returns[(time_index-11) : time_index])-riskfree ) / IXN_sigma
SPY_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index])-riskfree ) / SPY_sigma
LQD_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index])-riskfree ) / LQD_sigma
IEF_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index])-riskfree ) / IEF_sigma
VNQ_sharpe <- (mean(joined_monthlyreturns$monthly.returns.4[(time_index-11) : time_index])-riskfree ) / VNQ_sigma
GLD_sharpe <- (mean(joined_monthlyreturns$monthly.returns.5[(time_index-11) : time_index])-riskfree ) / GLD_sigma
    print(IXN_sharpe)
    print(SPY_sharpe)
    print(LQD_sharpe)
    print(IEF_sharpe)
    print(VNQ_sharpe)
    print(GLD_sharpe)
#if I wanted to calculate this annualize
# IXN_sharpe <- ((mean(1+joined_monthlyreturns$monthly.returns[(time_index-11) : time_index])^12)-1-riskfree ) / IXN_sigma

#Based on the results for sharp and sigma; the assets with the worst sharp value are: GLD, LQD and IEF; 
#on the other hand the worst assets in value for sigma are: GLD, IX, SPY
    
#Attending to this, I would say to sell GLD from the portfolio. 
#Keep the other assets, rebalance, using the cash from GLD selling + the available 35M  When 
#you buy more of VNQ
#also, the SPY can be increased in % allocation attending as well to the same variables of risk.
    
    # Portfolio weightings/allocation after rebalance:
    IXN_alloc <- 0.155
    SPY_alloc <- 0.229
    LQD_alloc <- 0.187
    IEF_alloc <- 0.136
    VNQ_alloc <- 0.293
    GLD_alloc <- 0

# returns after rebalancing:
joined_portfolio_ret <- as.data.frame(joined_monthlyreturns) %>%
      mutate(portfolio = IXN_alloc*monthly.returns + SPY_alloc*monthly.returns.1 +
               LQD_alloc*monthly.returns.2 + IEF_alloc*monthly.returns.3 + 
               VNQ_alloc*monthly.returns.4 + GLD_alloc*monthly.returns.5)


