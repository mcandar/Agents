# basic martingale bet modelling for binomial probability distribution

games <- 1e4
tosses <- sample(c(0,1),games,replace = T)
balance <- 1e4
balancehistory <- NULL
consecutive <- 0 # consecutive losses
consecutivehistory <- NULL
maxbalance <- 0
for(i in seq(length(tosses))){
  # currentrisk <- 0.5*(2^(consecutive+1))
  if(balance > maxbalance)
    maxbalance <- balance
  currentrisk <- (maxbalance*0.0001)*(2^(consecutive+1))
  if(tosses[i] == 0){ # lose
    balance <- balance - currentrisk
    balancehistory <- c(balancehistory,balance)
    consecutive <- consecutive + 1
  }
  else{ # win
    balance <- balance + currentrisk
    balancehistory <- c(balancehistory,balance)
    consecutive <- 0 # consecutive losses
  }
  consecutivehistory <- c(consecutivehistory,consecutive)
}

print(balance)
print(max(consecutivehistory))

library(plotly)

plot_ly(x=seq(games),y=balancehistory,type = "scatter",mode = "lines") %>%
  layout(xaxis = list(title = "Games"),yaxis = list(title = "Balance"),title = "Martingale Betting")

# plot(1:1e3,balancehistory)




### OPTIMIZATION
optm <- NULL
for(j in 1:1e2){
  games <- 1e4
  tosses <- sample(c(0,1),games,replace = T)
  balance <- 5e3
  balancehistory <- NULL
  consecutive <- 0 # consecutive losses
  consecutivehistory <- NULL
  maxbalance <- 0
  for(i in seq(length(tosses))){
    if(balance > maxbalance)
      maxbalance <- balance
    currentrisk <- (maxbalance*0.00001)*(2^(consecutive+1))
    if(tosses[i] == 0){ # lose
      balance <- balance - currentrisk
      balancehistory <- c(balancehistory,balance)
      consecutive <- consecutive + 1
    }
    else{ # win
      balance <- balance + currentrisk
      balancehistory <- c(balancehistory,balance)
      consecutive <- 0 # consecutive losses
    }
    consecutivehistory <- c(consecutivehistory,consecutive)
  }
  if(any(balancehistory < 0))
    optm <- c(optm,"LOSE")
  else
    optm <- c(optm,"WIN")
}

plot_ly(x = optm,type = "histogram")



### BASIC IMPLEMENTATION TO STOCK MARKET

# orders <- 1e4
sims <- 1e4
price <- vector(mode = "numeric",length = sims)
spot <- 1.20001

# brownian motion for random pricing
for(i in seq(length(price))){
  temp <- abs(rnorm(1,0))
  spot <- round(spot + runif(1,-temp,temp)/1000,digits = 5)
  price[i] <- spot
}

price

pcs <- data.frame(Time=1:length(price),Price=price)
library(dygraphs)
dygraph(pcs,main = "Random Pricing of a Stock") %>%
  dyAxis("x",label = "Time") %>%
  dyAxis("y",label = "Price") %>%
  dySeries("Price",label = "Stock1")

balance <- 1e4 # 10.000 USD for start
balancehistory <- NULL
consecutive <- 0 # consecutive losses
consecutivehistory <- NULL
maxbalance <- 0
limit <- 100
SLTP <- limit/1e5 # in case for a binomial distribution, stop loss and take profit values should be same
order.isopen <- FALSE # no market orders present, initialized
trigger <- FALSE
lots <- 10

for(i in seq(price)){
  if(!order.isopen){ # if no order is sent, randomly decide to place one or not
    trigger <- sample(c(TRUE,FALSE),1,replace = T) # randomly decide to send orders
    
    if(trigger){ # if an order is triggered
      order_op <- price[i]
      price_sl <- order_op - SLTP
      price_tp <- order_op + SLTP
      order.isopen <- TRUE
    }
  }
  if(order.isopen){ # open and modify order only LONG
    diff <- price[i]-order_op
    profit <- currentrisk*(diff)*100
    balance <- balance + profit

    if(price[i] < price_sl | price[i] > price_tp){ # close order if hit tp or sl
      order.isopen <- FALSE
      
      # check consecutive losses
      if(profit < 0)
        consecutive <- consecutive + 1
      else if (profit >= 0)
        consecutive <- 0
  
      # check max balance
      if(balance > maxbalance)
        maxbalance <- balance
      
      consecutivehistory <- c(consecutivehistory,consecutive) # update consecutive loses history
      balancehistory <- c(balancehistory,balance) # update balance history
      currentrisk <- round((maxbalance*0.0001)*(2^(consecutive+1)),digits = 2) # update risk
    }
  }
}

library(plotly)
plot_ly(x=seq(balancehistory),y=balancehistory,type = "scatter",mode = "lines") %>%
  layout(xaxis = list(title = "Orders"),yaxis = list(title = "Balance"),title = "Martingale Stock Trading")

print(balance)
print(max(consecutivehistory))
print(length(balancehistory)) # total number of orders