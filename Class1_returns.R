

##created by Team_12
##Hult Business School
##Wealth and Investment Management Risk in R

#install.packages("quantmod")

library(quantmod)





library(dplyr)

APPL_return <-monthlyReturn(getSymbols("AAPL",auto.assign= FALSE))
MSFT_return <-monthlyReturn(getSymbols("MSFT",auto.assign= FALSE))
AMZN_return <- monthlyReturn(getSymbols("AMZN",auto.assign= FALSE))
V_return <- monthlyReturn(getSymbols("V",auto.assign= FALSE))
VCIT_return <- monthlyReturn(getSymbols("VCIT",auto.assign= FALSE))
GOOGL_return <- monthlyReturn(getSymbols("GOOGL",auto.assign= FALSE))
TSM_return <-monthlyReturn(getSymbols("TSM",auto.assign= FALSE))
BABA_return <-monthlyReturn(getSymbols("BABA",auto.assign= FALSE))
ADBE_return <- monthlyReturn(getSymbols("ADBE",auto.assign= FALSE))
TCEHY_return <- monthlyReturn(getSymbols("TCEHY",auto.assign= FALSE))

#merging returns
joined_monthlyreturns <-merge.xts(APPL_return,MSFT_return,AMZN_return,V_return,VCIT_return,GOOGL_return,TSM_return, BABA_return,ADBE_return,TCEHY_return )

#colnames(joined_monthlyreturns)<-c("AAPL","MSFT","AMZN","V","VCIT","GOOGL","TSM","BABA","ADBE","TCEHY")


#expected_monthly_return <- as.data.frame(colMeans(joined_monthlyreturns))
#colnames(expected_monthly_return)<-c("Expected Monthly Return")

###############
AAPL_alloc <- 0.1766
MSFT_alloc <- 0.1299
AMZN_alloc <- 0.1292
V_alloc <- 0.1113
VCIT_alloc <- 0.0963
GOOGL_alloc <- 0.0816
TSM_alloc <- 0.0725
BABA_alloc <- 0.0725
ADBE_alloc <- 0.0656
TCEHY_alloc <- 0.0646


joined_portfolio_ret <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio = AAPL_alloc * monthly.returns +
         MSFT_alloc * monthly.returns.1 +
         AMZN_alloc * monthly.returns.2 +
         V_alloc * monthly.returns.3 + 
         VCIT_alloc * monthly.returns.4 +
         GOOGL_alloc * monthly.returns.5+
         TSM_alloc  * monthly.returns.6 + 
         BABA_alloc *  monthly.returns.7+
         ADBE_alloc * monthly.returns.8 +
         TCEHY_alloc * monthly.returns.9)




benchmark_returns <- monthlyReturn(getSymbols("SPY", auto.assign= FALSE))



joined_monthlyreturns <- merge.xts(APPL_return,
                                   MSFT_return,
                                   AMZN_return,
                                   V_return,
                                   VCIT_return,
                                   GOOGL_return,
                                   TSM_return,
                                   BABA_return,
                                   ADBE_return,
                                   TCEHY_return,
                                   benchmark_returns)

joined_monthlyreturns <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio = AAPL_alloc * monthly.returns +
           MSFT_alloc * monthly.returns.1 +
           AMZN_alloc * monthly.returns.2 +
           V_alloc * monthly.returns.3 + 
           VCIT_alloc * monthly.returns.4 +
           GOOGL_alloc * monthly.returns.5+
           TSM_alloc  * monthly.returns.6 + 
           BABA_alloc *  monthly.returns.7+
           ADBE_alloc * monthly.returns.8 +
           TCEHY_alloc * monthly.returns.9)

joined_monthlyreturns 

APPL_expected_return <-mean(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])
MSFT_expected_return <-mean(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index])
AMZN_expected_return <-mean(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index])
V_expected_return <-mean(joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index])
VCIT_expected_return <-mean(joined_monthlyreturns$monthly.returns.4[(time_index-11):time_index])
GOOGL_expected_return <-mean(joined_monthlyreturns$monthly.returns.5[(time_index-11):time_index])
TSM_expected_return <-mean(joined_monthlyreturns$monthly.returns.6[(time_index-11):time_index])
BABA_expected_return <-mean(joined_monthlyreturns$monthly.returns.7[(time_index-11):time_index])
ADBE_expected_return <-mean(joined_monthlyreturns$monthly.returns.8[(time_index-11):time_index])
TCEHY_expected_return <-mean(joined_monthlyreturns$monthly.returns.9[(time_index-11):time_index])
Portfolio_expected_return <-mean(joined_monthlyreturns$portfolio[(time_index-11):time_index])
Benchmark_expected_return <-mean(joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])

print(c("APPL", APPL_expected_return,
        "MSFT",MSFT_expected_return,
        "AMZN",AMZN_expected_return,
        "V",V_expected_return,
        "VCIT",VCIT_expected_return,
        "GOOGL",GOOGL_expected_return,
        "TSM",TSM_expected_return,
        "BABA",BABA_expected_return,
        "ADBE",ADBE_expected_return,
        "TCEHY",TCEHY_expected_return,
        "Portfolio",Portfolio_expected_return))

#months in the data frame
time_index<- nrow(joined_monthlyreturns)

joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)
#how far I go in the data frame


#annualized
APPL_sigma <- sd(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])*sqrt(12)
MSFT_sigma <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index])*sqrt(12)
AMZN_sigma <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index])*sqrt(12)
V_sigma <- sd(joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index])*sqrt(12)
VCIT_sigma <- sd(joined_monthlyreturns$monthly.returns.4[(time_index-11):time_index])*sqrt(12)
GOOGL_sigma <- sd(joined_monthlyreturns$monthly.returns.5[(time_index-11):time_index])*sqrt(12)
TSM_sigma <- sd(joined_monthlyreturns$monthly.returns.6[(time_index-11):time_index])*sqrt(12)
BABA_sigma <- sd(joined_monthlyreturns$monthly.returns.7[(time_index-11):time_index])*sqrt(12)
ADBE_sigma <- sd(joined_monthlyreturns$monthly.returns.8[(time_index-11):time_index])*sqrt(12)
TCEHY_sigma <- sd(joined_monthlyreturns$monthly.returns.9[(time_index-11):time_index])*sqrt(12)
SPY500_sigma <- sd(joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)
Portfolio_sigma <- sd(joined_monthlyreturns$portfolio[(time_index-11):time_index])*sqrt(12)

print(APPL_sigma)
print(MSFT_sigma)
print(AMZN_sigma)
print(V_sigma)
print(VCIT_sigma)
print(GOOGL_sigma)
print(TSM_sigma)
print(BABA_sigma)
print(ADBE_sigma)
print(TCEHY_sigma)
print(SPY500_sigma)
print(Portfolio_sigma)

print(c("APPL", APPL_sigma,
        "MSFT",MSFT_sigma,
        "AMZN",AMZN_sigma,
        "V",V_sigma,
        "VCIT",VCIT_sigma,
        "GOOGL",GOOGL_sigma,
        "TSM",TSM_sigma,
        "BABA",BABA_sigma,
        "ADBE",ADBE_sigma,
        "TCEHY",TCEHY_sigma,
        "Portfolio",Portfolio_sigma))


#Calculating betas

time_index <- nrow(joined_monthlyreturns)

last_12_months <- joined_monthlyreturns[(time_index-11):time_index,]

APPL_reg <- lm(monthly.returns ~ monthly.returns.10, data =last_12_months) 
summary(APPL_reg)
MSFT_reg <- lm(monthly.returns.1 ~ monthly.returns.10, data =last_12_months) 
summary(MSFT_reg)
AMZN_reg<- lm(monthly.returns.2 ~ monthly.returns.10, data =last_12_months) 
summary(AMZN_reg)
V_reg <- lm(monthly.returns.3 ~ monthly.returns.10, data =last_12_months) 
summary(V_reg)
VCIT_reg <- lm(monthly.returns.4 ~ monthly.returns.10, data =last_12_months) 
summary(VCIT_reg )
GOOGL_reg <- lm(monthly.returns.5 ~ monthly.returns.10, data =last_12_months) 
summary(GOOGL_reg)
TSM_reg <- lm(monthly.returns.6 ~ monthly.returns.10, data =last_12_months) 
summary(TSM_reg )
BABA_reg <- lm(monthly.returns.7 ~ monthly.returns.10, data =last_12_months) 
summary(BABA_reg)
ADBE_reg <- lm(monthly.returns.8 ~ monthly.returns.10, data =last_12_months) 
summary(ADBE_reg)
TCEHY_reg<- lm(monthly.returns.9 ~ monthly.returns.10, data =last_12_months) 
summary(TCEHY_reg)
Portfolio_reg<- lm(portfolio ~ monthly.returns.10, data =last_12_months) 
summary(Portfolio_reg)

print(c("APPL", APPL_reg$coefficients[2],
        "MSFT",MSFT_reg$coefficients[2],
        "AMZN",AMZN_reg$coefficients[2],
        "V",V_reg$coefficients[2],
        "VCIT",VCIT_reg$coefficients[2],
        "GOOGL",GOOGL_reg$coefficients[2],
        "TSM",TSM_reg$coefficients[2],
        "BABA",BABA_reg$coefficients[2],
        "ADBE",ADBE_reg$coefficients[2],
        "TCEHY",TCEHY_reg$coefficients[2],
        "Portfolio",Portfolio_reg$coefficients[2]))


#tracking errors

APPL_te <- sd(joined_monthlyreturns$monthly.returns[(time_index-11):time_index]-
               joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

MSFT_te <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index]-
               joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)


AMZN_te <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index]-
               joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

V_te <- sd(joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

VCIT_te <- sd(joined_monthlyreturns$monthly.returns.4[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

GOOGL_te <- sd(joined_monthlyreturns$monthly.returns.5[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

TSM_te <- sd(joined_monthlyreturns$monthly.returns.6[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

BABA_te <- sd(joined_monthlyreturns$monthly.returns.7[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

ADBE_te <- sd(joined_monthlyreturns$monthly.returns.8[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

TCEHY_te <- sd(joined_monthlyreturns$monthly.returns.9[(time_index-11):time_index]-
                joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

Portfolio_te <- sd(joined_monthlyreturns$portfolio[(time_index-11):time_index]-
                 joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

print(c("APPL",APPL_te,
        "MSFT",MSFT_te,
        "AMZN",AMZN_te,
        "V",V_te,
        "VCIT",VCIT_te,
        "GOOGL",GOOGL_te,
        "TSM",TSM_te,
        "BABA",BABA_te,
        "ADBE",ADBE_te,
        "TCEHY",TCEHY_te,
        "Portfolio",Portfolio_te))

riskfree <- 0.001

Portfolio_sharpe <- (mean(joined_monthlyreturns$portfolio[(time_index-11):time_index])-riskfree) / Portfolio_sigma
APPL_sharpe <- (mean(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])-riskfree) / APPL_sigma
MSFT_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index])-riskfree) / MSFT_sigma
AMZN_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index])-riskfree) / AMZN_sigma
V_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index])-riskfree) / V_sigma
VCIT_sharpe <- (mean(joined_monthlyreturns$monthly.returns.4[(time_index-11):time_index])-riskfree) / VCIT_sigma
GOOGL_sharpe <- (mean(joined_monthlyreturns$monthly.returns.5[(time_index-11):time_index])-riskfree) / GOOGL_sigma
TSM_sharpe <- (mean(joined_monthlyreturns$monthly.returns.6[(time_index-11):time_index])-riskfree) / TSM_sigma
BABA_sharpe <- (mean(joined_monthlyreturns$monthly.returns.7[(time_index-11):time_index])-riskfree) / BABA_sigma
ADBE_sharpe <- (mean(joined_monthlyreturns$monthly.returns.8[(time_index-11):time_index])-riskfree) / ADBE_sigma
TCEHY_sharpe <- (mean(joined_monthlyreturns$monthly.returns.9[(time_index-11):time_index])-riskfree) / TCEHY_sigma


print(c("APPL",APPL_sharpe,
        "MSFT",MSFT_sharpe,
        "AMZN",AMZN_sharpe,
        "V",V_sharpe,
        "VCIT",VCIT_sharpe,
        "GOOGL",GOOGL_sharpe,
        "TSM",TSM_sharpe,
        "BABA",BABA_sharpe,
        "ADBE",ADBE_sharpe,
        "TCEHY",TCEHY_sharpe,
        "Portfolio",Portfolio_sharpe))

#sortino - related to market risk

#treynor - compare to market data - benchmark

Portfolio_treynor <- (mean(joined_monthlyreturns$portfolio[(time_index-11):time_index])-riskfree) / Portfolio_reg$coefficients[2]


APPL_treynor <- (mean(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])-riskfree) / APPL_reg$coefficients[2]
MSFT_treynor <- (mean(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index])-riskfree) / MSFT_reg$coefficients[2]
AMZN_treynor <- (mean(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index])-riskfree) / AMZN_reg$coefficients[2]
V_treynor <- (mean(joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index])-riskfree) / V_reg$coefficients[2]
VCIT_treynor <- (mean(joined_monthlyreturns$monthly.returns.4[(time_index-11):time_index])-riskfree) / VCIT_reg$coefficients[2]
GOOGL_treynor <- (mean(joined_monthlyreturns$monthly.returns.5[(time_index-11):time_index])-riskfree) / GOOGL_reg$coefficients[2]
TSM_treynor <- (mean(joined_monthlyreturns$monthly.returns.6[(time_index-11):time_index])-riskfree) / TSM_reg$coefficients[2]
BABA_treynor <- (mean(joined_monthlyreturns$monthly.returns.7[(time_index-11):time_index])-riskfree) / BABA_reg$coefficients[2]
ADBE_treynor <- (mean(joined_monthlyreturns$monthly.returns.8[(time_index-11):time_index])-riskfree) / ADBE_reg$coefficients[2]
TCEHY_treynor <- (mean(joined_monthlyreturns$monthly.returns.9[(time_index-11):time_index])-riskfree) / TCEHY_reg$coefficients[2]

print(c("APPL",APPL_treynor,
        "MSFT",MSFT_treynor,
        "AMZN",AMZN_treynor,
        "V",V_treynor,
        "VCIT",VCIT_treynor,
        "GOOGL",GOOGL_treynor,
        "TSM",TSM_treynor,
        "BABA",BABA_treynor,
        "ADBE",ADBE_treynor,
        "TCEHY",TCEHY_treynor,
        "Portfolio",Portfolio_treynor))
