---
title: "ACTL1101 Assignment Part A"
author: "Jiho Kim"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("/Users/kimchii/downloads/AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date) 
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
start_date_index <- 1
end_date_index <- nrow(amd_df)

amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name 
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0


for (i in 1:nrow(amd_df)) {
  # If the previous price is 0, it's the first day
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (amd_df$close[i] < previous_price) {
    # If the current day's price is less than the previous day's price
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else {
    # Otherwise, hold
    amd_df$trade_type[i] <- "hold"
    amd_df$costs_proceeds[i] <- 0
  }
  
  # If it's the last day, sell all accumulated shares
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    accumulated_shares <- 0
  }
  
  # Update accumulated shares in the dataframe
  amd_df$accumulated_shares[i] <- accumulated_shares
  
  # Update the previous price for the next iteration
  previous_price <- amd_df$close[i]
}

```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 

Trading period: 20/05/2019 to 20/04/2020

```{r period}
# Fill your code here
start_date_index <- 1
end_date_index <- 254
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df

amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0 # Corrected column name 
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0


for (i in start_date_index:end_date_index) {
  # If the previous price is 0, it's the first day
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (amd_df$close[i] < previous_price) {
    # If the current day's price is less than the previous day's price
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else {
    # Otherwise, hold
    amd_df$trade_type[i] <- "hold"
    amd_df$costs_proceeds[i] <- 0
  }
  
  # If it's the last day, sell all accumulated shares
  if (i == end_date_index) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    accumulated_shares <- 0
  }
  
  # Update accumulated shares in the dataframe
  amd_df$accumulated_shares[i] <- accumulated_shares
  
  # Update the previous price for the next iteration
  previous_price <- amd_df$close[i]
}
```

```{r}
#calculating profits
  profit<-(sum(amd_df$costs_proceeds))
  print(paste("profit: ",profit))
  capital_investment<-sum(amd_df$costs_proceeds[amd_df$costs_proceeds<0])
  print(paste("Capital Investment: ",-capital_investment))
  print((paste("ROI: ", (-profit/capital_investment)*100)))
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
# STEP 5
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df

amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0 # Corrected column name 
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
amd_df$acc_cost <- NA
amd_df$avg_price <- NA

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
acc_cost <- 0
avg_price <- 0

for (i in start_date_index:end_date_index) {
  # If the previous price is 0, it's the first day
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    acc_cost <- acc_cost - amd_df$close[i]*share_size
  } else if (amd_df$close[i] < previous_price) {
    # If the current day's price is less than the previous day's price
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    acc_cost <- acc_cost - amd_df$close[i]*share_size
  } else if (amd_df$close[i] >= 1.2*avg_price) {
    amd_df$trade_type[i] <- "sell"
    accumulated_shares <- accumulated_shares/2
    amd_df$costs_proceeds[i] <- amd_df$close[i]*accumulated_shares
    acc_cost <- acc_cost/2
  } else {
    # Otherwise, hold
    amd_df$trade_type[i] <- "hold"
    amd_df$costs_proceeds[i] <- 0
  }
  
  # If it's the last day, sell all accumulated shares
  if (i == end_date_index) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    accumulated_shares <- 0
  }
  
  # Update accumulated shares and accumulated cost in the dataframe
  amd_df$accumulated_shares[i] <- accumulated_shares
  amd_df$acc_cost[i] <- acc_cost
  
  if (amd_df$trade_type[i] == "buy") {
    avg_price <- -acc_cost/accumulated_shares
  }
  amd_df$avg_price[i] <- avg_price
  
  # Update the previous price for the next iteration
  previous_price <- amd_df$close[i]
}

#calculating profits
  profit<-(sum(amd_df$costs_proceeds))
  print(paste("profit: ",profit))
  capital_investment<-sum(amd_df$costs_proceeds[amd_df$costs_proceeds<0])
  print(paste("Capital Investment: ",-capital_investment))
  print((paste("ROI: ", (-profit/capital_investment)*100)))
```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.

Discussion: The P/L performance decreased from the original trading strategy to the new profit-taking strategy, as the stock prices gradually increased from $26.68 to $56.39 from 20/05/2019 to 20/04/2020. As the final selling price for the last trading day was significantly greater than most share purchases within the period, the first strategy benefited greatest. On the other-hand, the profit-taking strategy which sells half of obtained stocks after a certain threshold increase under-performed the former method as stocks were sold quicker and frequently before selling its remaining shares at the end of the trading period.

The gradual increase in stock prices within the trading period can be attributed to AMD's successful growth in the 2019-2020 and 2020-2021 financial year, with the 2020 fourth quarter & full year financial results showing a quarterly revenue of $3.2B, up 53% year-over-year and a full year revenue $9.76B up 45% from the previous year. AMD also introduced multiple graphics computing processors in 2020, including AMD EPYC processors which could complete computationally strenuous calculations, such as that of a weather forecast model approximately 68% faster than competitors. 

It is however important to note that each trading strategy has its limitations. The first trading strategy aims to buy shares when share prices decreases from its previous closing value, selling all purchased shares at the end of its trading period. This trading strategy has a very clear vulnerability: the performance of the strategy solely lies on the final selling value of the stock at the end of the trading period. If the strategy were to be implemented within a time period in which stock prices were decreasing, or were to spike and drop at the end of the trading period, losses will incur.

As opposed to the first strategy, the profit-taking strategy aims to reduce the long-term uncertainty of stock prices at the end of the trading period, and sells stock when the current value of stocks are 20% higher than the average stock purchase costs. The result of this strategy is a yield that is less significant than that of the former strategy, but with less risk associated, as it will underperform the first strategy when stock prices tend to an upwards trend, and outperform (or mitigate losses better) when stock prices tend to a downwards trend.
