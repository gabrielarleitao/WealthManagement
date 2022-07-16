library(dplyr)
#step1
stock1 <- getSymbols("AAPL", auto.assign = FALSE)
stock2 <- getSymbols("MSFT", auto.assign = FALSE)
stock3 <- getSymbols("AMZN", auto.assign = FALSE)
stock4 <- getSymbols("V", auto.assign = FALSE)
stock5 <-getSymbols("VCIT", auto.assign = FALSE)
stock6 <-getSymbols("GOOGL", auto.assign = FALSE)
stock7 <-getSymbols("BABA", auto.assign = FALSE)
stock8 <-getSymbols("TCEHY", auto.assign = FALSE)
stock9 <-getSymbols("TSM", auto.assign = FALSE)
stock10 <-getSymbols("ADBE", auto.assign = FALSE)

#step2
joint_client_prices <- merge.xts(stock1, stock2, stock3, stock4, 
                                 stock5, stock6, stock7, stock8, 
                                 stock9, stock10)

#step3
joint_prices_only <- joint_client_prices[,c(6,12,18,24,30,36,42,48,54,60)]

#step4
##monthly
stock1_returns <- monthlyReturn(getSymbols("AAPL", auto.assign = FALSE))
stock2_returns <- monthlyReturn(getSymbols("MSFT", auto.assign = FALSE))
stock3_returns <- monthlyReturn(getSymbols("AMZN", auto.assign = FALSE))
stock4_returns <- monthlyReturn(getSymbols("V", auto.assign = FALSE))
stock5_returns <- monthlyReturn(getSymbols("VCIT", auto.assign = FALSE))
stock6_returns <- monthlyReturn(getSymbols("GOOGL", auto.assign = FALSE))
stock7_returns <- monthlyReturn(getSymbols("BABA", auto.assign = FALSE))
stock8_returns <- monthlyReturn(getSymbols("TCEHY", auto.assign = FALSE))
stock9_returns <- monthlyReturn(getSymbols("TSM", auto.assign = FALSE))
stock10_returns <- monthlyReturn(getSymbols("ADBE", auto.assign = FALSE))

#merging returns
joint_monthly_client_returns <- merge.xts(stock1_returns, stock2_returns, stock3_returns, stock4_returns, stock5_returns, 
                                          stock6_returns, stock7_returns, stock8_returns, stock9_returns, stock10_returns)

#portfolio returns
AAPL_alloc <- 0.176567
MSFT_alloc <- 0.129896
AMZN_alloc <- 0.129243
V_alloc <- 0.111292
VCIT_alloc <- 0.096279
GOOGL_alloc <- 0.081593
BABA_alloc <- 0.072454
TCEHY_alloc <- 0.072454
TSM_alloc <- 0.065601
ADBE_alloc <- 0.064621

joined_portfolio_ret <- as.data.frame(joint_monthly_client_returns) %>%
  mutate(portfolio = AAPL_alloc * monthly.returns + 
           MSFT_alloc * monthly.returns.1 + 
           AMZN_alloc * monthly.returns.2 +
           V_alloc * monthly.returns.3 + 
           VCIT_alloc * monthly.returns.4 + 
           GOOGL_alloc * monthly.returns.5 + 
           BABA_alloc * monthly.returns.6 + 
           TCEHY_alloc * monthly.returns.7 + 
           TSM_alloc * monthly.returns.8 + 
           ADBE_alloc * monthly.returns.9)
colnames(joined_portfolio_ret) <- c("AAPL_returns", "MSFT_returns", "AMZN_returns",
                                    "V_returns", "VCIT_returns", "GOOGL_returns",
                                    "BABA_returns", "TCEHY_returns", "TSM_returns", 
                                    "ADBE_returns", "Joint_Portfolio")



#create a frame with the monthlyReturn function - the ticker for SPY
benchmark_returns.1 <- monthlyReturn(getSymbols("SPY", auto.assign = FALSE))
benchmark_returns.2 <- monthlyReturn(getSymbols("^IXIC", auto.assign = FALSE))
#i want to add this to my portfolio this benchmark
#i can join the 3 security separetly with the benchmark or join both frames direcly. 
joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns, stock3_returns, stock4_returns, stock5_returns, 
                                   stock6_returns, stock7_returns, stock8_returns, stock9_returns, stock10_returns, 
                                   benchmark_returns.1, benchmark_returns.2 )

##############
time_index <- nrow(joined_monthlyreturns)
joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)

#to calculate risk for each individual security
AAPL_sigma <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11) : time_index]) * sqrt(12) 
MSFT_sigma <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11) : time_index]) * sqrt(12) 
AMZN_sigma <- sd(joined_monthlyreturns$monthly.returns.3[(time_index-11) : time_index]) * sqrt(12) 
V_sigma <- sd(joined_monthlyreturns$monthly.returns.4[(time_index-11) : time_index]) * sqrt(12) 
VCIT_sigma <- sd(joined_monthlyreturns$monthly.returns.5[(time_index-11) : time_index]) * sqrt(12) 
GOOGL_sigma <- sd(joined_monthlyreturns$monthly.returns.6[(time_index-11) : time_index]) * sqrt(12) 
BABA_sigma <- sd(joined_monthlyreturns$monthly.returns.7[(time_index-11) : time_index]) * sqrt(12) 
TSM_sigma <- sd(joined_monthlyreturns$monthly.returns.8[(time_index-11) : time_index]) * sqrt(12) 
ADBE_sigma <- sd(joined_monthlyreturns$monthly.returns.9[(time_index-11) : time_index]) * sqrt(12) 
TCEHY_sigma <- sd(joined_monthlyreturns$monthly.returns.10[(time_index-11) : time_index]) * sqrt(12) 

SPY_sigma <- sd(joined_monthlyreturns$monthly.returns.11[(time_index-11) : time_index]) * sqrt(12)
IXIC_sigma <- sd(joined_monthlyreturns$monthly.returns.12[(time_index-11) : time_index]) * sqrt(12)

print(AAPL_sigma)
print(MSFT_sigma)
print(AMZN_sigma)
print(V_sigma)
print(VCIT_sigma) 
print(GOOGL_sigma) 
print(BABA_sigma) 
print(TSM_sigma) 
print(ADBE_sigma) 
print(TCEHY_sigma)

print(SPY_sigma)
print(IXIC_sigma)
