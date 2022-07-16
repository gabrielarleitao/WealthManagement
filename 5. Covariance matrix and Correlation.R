##################################
# Created by Thomas Kurnicki
##################################
### Hult Inter. Business School
##################################
# Wealth and Risk Management in R
##################################
# thomas.kurnicki@faculty.hult.edu
##################################
# Office hours 30min after class
##################################

# Need to instal the quantmod package
#install.packages("quantmod")
library(quantmod) #loading the quantmod package
##############################################
#step 1:
#downloading pricing data from the yahoo finance portal
getSymbols("WFC")

#now, let's past this data in a data frame that's called differently:
stock1 <- getSymbols("WFC", auto.assign = FALSE)
#what is this xts object? 
#we're going to pull in an equity index S&P500:
stock2 <- getSymbols("SPY", auto.assign = FALSE)

#let's now pull in a fixed income iShares Core U.S. Aggregate Bond ETF index: 
fixed_income <- getSymbols("AGG", auto.assign = FALSE)

#Step2
#now we want to join all the 3 different securities into one data frame: 
joined_prices <- merge.xts(stock1, stock2, fixed_income)

#Step3:
#but we just need the closing prices - which are in the 4th, 10th and 16th variables:
joined_prices_only <- joined_prices[,c(4,10,16)]
#can we code the above differently? 
joined_prices_only <- joined_prices[,seq(from=4, to=16, by=6)]

#Step4:
#we want to calculate daily ROR by using the log() function that calculates natural logarithm:
library(dplyr)
joined_returns <- as.data.frame(joined_prices_only) %>%
            mutate(WFC_ROR = log(WFC.Close/lag(WFC.Close)))%>%
            mutate(SPY_ROR = log(SPY.Close/lag(SPY.Close)))%>%
            mutate(AGG_ROR = log(AGG.Close/lag(AGG.Close)))

#Step5:
#Do we need to Compound the returns? Do them monthly? quarterly? annually? (if we go beyond 12 months then we need to annualize) 
n <- 25 #how many days do we need to combine

joined_returns <- as.data.frame(joined_prices_only) %>%
  mutate(WFC_ROR = log(WFC.Close/lag(WFC.Close, n)))%>% #we're passing an argument saying how long time series to use in calculation
  mutate(SPY_ROR = log(SPY.Close/lag(SPY.Close, n)))%>%
  mutate(AGG_ROR = log(AGG.Close/lag(AGG.Close, n)))

#In wealth management - we usually have monthly data and then we aggregate to 1Y, 3Y, 5Y performance.

#####
#An alternative for calculating all those things manually
stock1_returns <- dailyReturn(getSymbols("WFC", auto.assign = FALSE))

stock2_returns <- dailyReturn(getSymbols("SPY", auto.assign = FALSE))

fixed_income_returns <- dailyReturn(getSymbols("AGG", auto.assign = FALSE))

joined_dailyreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns)

#monthly
stock1_returns <- monthlyReturn(getSymbols("WFC", auto.assign = FALSE))

stock2_returns <- monthlyReturn(getSymbols("SPY", auto.assign = FALSE))

fixed_income_returns <- monthlyReturn(getSymbols("AGG", auto.assign = FALSE))

joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns)

#annual
stock1_returns_annual<- annualReturn(getSymbols("WFC", auto.assign = FALSE))

stock2_returns_annual <- annualReturn(getSymbols("SPY", auto.assign = FALSE))

fixed_income_returns_annual <- annualReturn(getSymbols("AGG", auto.assign = FALSE))

joined_annualreturns <- merge.xts(stock1_returns_annual, stock2_returns_annual, fixed_income_returns_annual)

##############################################################
##############################################################
##############################################################
### Calculating different risk metrics #######################
##############################################################
##############################################################
##############################################################

#before we go into risk metrics, lets calculate portfolio returns
# for all the stocks together, so we need to know asset allocation

WFC_alloc <- 0.3
SPY_alloc <- 0.25
AGG_alloc <- 0.45
#these have to sum to 1

joined_portfolio_ret <- as.data.frame(joined_monthlyreturns) %>%
                          mutate(portfolio = WFC_alloc*monthly.returns + SPY_alloc*monthly.returns.1+
                                              AGG_alloc*monthly.returns.2)
#let's take a look at the portfolio returns timeseries:
plot(joined_portfolio_ret$portfolio, type="l")
#now this is semi-stationary. It has the same mean accross all the series, and different sigma here and there. 

#############################################################
##### Adding a benchmark before we do risk analysis##########
##### We'll use Vanguard Russell 1000 ETF (VONE) ############
#############################################################

benchmark_returns <- monthlyReturn(getSymbols("VONE", auto.assign = FALSE))

joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns, benchmark_returns)

################################
#let's go back to calculating risk for each individual security
#################################
##############
## sigma for last 12 months (need to annualize by sqrt(12))
##############

time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) #need to convert to data frame to be able to subset based on indexing
WFC_sigma <- sd(joined_monthlyreturns$monthly.returns[(time_index-11) : time_index]) * sqrt(12) #we need to annualize by sqrt(12)
SPY_sigma <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index]) * sqrt(12)
AGG_sigma <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index]) * sqrt(12)
RUS1000_sigma <- sd(joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12)

print(c("WFC", WFC_sigma, "SPY", SPY_sigma, "AGG", AGG_sigma, "RUS1000", RUS1000_sigma))

################################
### beta for the last 12 months
################################

time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame
last_12_months <- joined_monthlyreturns[(time_index-11) : time_index, ]
##we will use the RUS1000 as the benchmark to regress against
WFC_reg <- lm(monthly.returns ~ monthly.returns.3 ,data=last_12_months)  #we run a linear regression with the bencharmk returns (RUS1000) as X and asset returns as Y
summary(WFC_reg)

SPY_reg <- lm(monthly.returns.1 ~ monthly.returns.3 ,data=last_12_months)  #we run a linear regression with the bencharmk returns as X and asset returns as Y
summary(SPY_reg)

AGG_reg <- lm(monthly.returns.2 ~ monthly.returns.3 ,data=last_12_months)  #we run a linear regression with the bencharmk returns as X and asset returns as Y
summary(AGG_reg) #take a look at the super low beta, and a p value that is >0.1 - the coefficient is statistically insignificant

##############
## TrackingError for last 12 months (need to annualize by sqrt(12))
##############

time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) #need to convert to data frame to be able to subset based on indexing
WFC_te <- sd(joined_monthlyreturns$monthly.returns[(time_index-11) : time_index]-joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12) #we need to annualize by sqrt(12)
SPY_te <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index]-joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12)
AGG_te <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index]-joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12)

print(c("WFC", WFC_te, "SPY", SPY_te, "AGG", AGG_te))
#what do these numbers mean?

#########################################################################
#########################################################################
######## Calculating sigma and beta and TE for entire portfolio##########
#########################################################################

#we've seen this code before:
WFC_alloc <- 0.3
SPY_alloc <- 0.25
AGG_alloc <- 0.45
#these have to sum to 1

#creating our portfolio returns viable: 
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio = WFC_alloc*monthly.returns + SPY_alloc*monthly.returns.1+
           AGG_alloc*monthly.returns.2)

#now calculating sigma for the portfolio variable:

time_index <- nrow(joined_monthlyreturns) #this is how many monthly observations we have in our data frame need this to pick up that last month in df
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) #need to convert to data frame to be able to subset based on indexing
portfolio_sigma <- sd(joined_monthlyreturns$portfolio[(time_index-11) : time_index]) * sqrt(12) 

#running regression for last 12 months between portfolio and RUS1000 benchmark 
portfolio_vs_RUS1000_reg <- lm(portfolio ~ monthly.returns.3 ,data=joined_monthlyreturns[((time_index-11) : time_index),])  #we run a linear regression with the bencharmk returns (RUS1000) as X and asset returns as Y
summary(portfolio_vs_RUS1000_reg)
# what does this beta and this p value mean ? 

#tracking error between portfolio and benchmark:
portfolio_te <- sd(joined_monthlyreturns$portfolio[(time_index-11) : time_index]-joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12)


##############################################################
##############################################################
##############################################################
### Sharpe and Treynor risk metrics    #######################
##############################################################
##############################################################
##############################################################

#Calculating Sharpe for single securities and for the entire portfolio:
# Share ratio will have a 12M expected return, and a 12M sigma
# the risk free rate is 0.001
riskfree <- 0.001

#we've calculated the expectation using the mean() function
WFC_sharpe <- (mean(joined_monthlyreturns$monthly.returns[(time_index-11) : time_index])-riskfree ) / WFC_sigma
  SPY_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index])-riskfree ) / SPY_sigma
  AGG_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index])-riskfree ) / AGG_sigma
  portfolio_sharpe <- (mean(joined_monthlyreturns$portfolio[(time_index-11) : time_index])-riskfree ) / portfolio_sigma
  
print(c(WFC_sharpe, SPY_sharpe, AGG_sharpe, portfolio_sharpe)) #why don't these differ so much? 

#we'l calculate treynor's ratio for the same: 
WFC_treynor <- (mean(joined_monthlyreturns$monthly.returns[(time_index-11) : time_index])-riskfree ) / WFC_reg$coefficients[2] #this will give us the beta
  SPY_treynor <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index])-riskfree ) / SPY_reg$coefficients[2]
  AGG_treynor <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index])-riskfree ) / AGG_reg$coefficients[2]
  portfolio_treynor <- (mean(joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index])-riskfree ) / portfolio_vs_RUS1000_reg$coefficients[2]
  
  print(c(WFC_treynor, SPY_treynor, AGG_treynor, portfolio_treynor)) # do these tell a different story than sharpe?

  
  ##############################################################
  ##############################################################
  ##############################################################
  ### Covariance matrix and Correlation    ####################
  ##############################################################
  ##############################################################
  ##############################################################
  
#we're going back to the 3 assets we had WFC, SPY, AGG - but we'll use all data that's available - not just 12M
  
cov(joined_monthlyreturns, use='complete.obs') #this will produce a covariance matrix - what can you tell about it?
cor(joined_monthlyreturns, use='complete.obs') #what can you tell about this correlation matrix

#here's a correlation matrix just for the last 12M: How does it differ from the "all data" matrix? 
cor(joined_monthlyreturns[(time_index-11) : time_index,], use='complete.obs')

#install.packages("corrplot")
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

rquery.cormat(joined_monthlyreturns)


