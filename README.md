# Stock-Analysis-Best-Portofolio-Finding-2017
Project for the Financial Statistic course

## Introduction

The analysis processes and results are explained in 財務統計報告-以統計方法計算投資組合及預測.pdf
The book I refer to for theories behind the analysis is [Statistics and Finance: An Introduction](https://www.amazon.com/Statistics-Finance-Introduction-Springer-Texts/dp/0387202706).

## Structure
1. smoothing_spline.R :   
  Code for removing spline (To let the time series be stationary.)
  
2. r_portofolio.R :   
  Code for implementing formulas to get the efficient frontier.  
  After finding the frontier, we can get the best return(tangency) portofolio and min. risk protofolio.
  
  
3. stock portofolio analysis and others.R :    
  Code combining all stages of analysis.   
  Including
    - Data Crawling from Yahoo Stock
    - Plotting stocks' prices and returns
    - Fit Spline
    - Find best portofolio under different conditions
    - Test CAPM assumption - to determine whether a stock is aggresive compared to the market
    - Calculate Value at Risk
    - Predict Stock prices


