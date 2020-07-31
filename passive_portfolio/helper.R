data_clean <- function(x){

# Fetch Data --------------------------------------------------------------

data.env <- new.env()
stocks <- x

n <- 1:length(stocks)
m <- length(stocks)

# Create the object data using 5 random numbers
data <- rnorm(10000)
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2000-01-01"), length = length(data), by = "days")
tt <- xts(x = data, order.by = dates)
stockdata <- getSymbols(stocks, env = data.env)


# Format Data -------------------------------------------------------------

for (i in stocks){
  data.env[[i]] <- data.env[[i]][,-c(1:5)]  
  colnames(data.env[[i]]) <- i
  tt <- merge(tt, data.env[[i]], join = 'inner')
}

#drop dummy column
tt <- tt[,-1]
#drop NA row
tt <- tt[-1,]


# Calculate Return & Beta -------------------------------------------------

#Transform prices to monthly returns
{
  hist <- periodReturn(tt[,1], period = 'monthly', type='arithmetic')
  for (i in 2:m){
    h <- periodReturn(tt[,i], period = 'monthly', type='arithmetic')
    hist <- merge(hist, h, join = 'inner')  
  }  
  colnames(hist) <- stocks
  hist <- round(hist, 3)
  hist <- hist[-1,]
}
return(hist)
}

get_capm <-function(hist){
  
  #Get and format SPY data
  spy.env <- new.env()
  stockdata <- getSymbols("SPY", env = spy.env)
  spy.env[["SPY"]] <- spy.env[["SPY"]][,-c(1:5)]  
  colnames(spy.env[["SPY"]]) <- "blackhead"
  blackhead <- spy.env[["SPY"]]
  blackhead <- periodReturn(blackhead[,1], period = 'monthly', type='arithmetic')
  colnames(blackhead) <- "blackhead"
  blackhead <- round(blackhead, 3)
  
  
  hist <- merge(hist,blackhead, join = 'inner')
  
  m <- (ncol(hist))
  n <- 1:(m-1)
  
  #Get CAPM Betas
  {
    x <- lm(hist[,n] ~ hist$blackhead)
    v <- x$coefficients
    betas <- round(as.numeric(v[2,n]),2)
    hist[,-m]
  }
  return(betas)
}

create_portfolio <- function(hist, stocks, betas, lower_factor, upper_factor){
#set up portfolio function
fund.names <- stocks
pspec <- portfolio.spec(assets = fund.names)


# Constraints -------------------------------------------------------------

#The full investment constraint is a special case of the leverage constraint that specifies the weights must sum to 1
# Update the constraint to relax the sum of weights constraint
pspec <- add.constraint(pspec, type="weight_sum",
                        min_sum=0.99, max_sum=1.01,
                        indexnum=1)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                        B=betas,lower=lower_factor, upper= upper_factor)
#The box constraint defines the max and min weight for each asset
#pspec <- add.constraint(portfolio=pspec,
                        #type="box", min=0.0, max=0.25)


# Optimize ----------------------------------------------------------------

qu <- add.objective(portfolio=pspec, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="StdDev")
opt_qu <- optimize.portfolio(R=hist, portfolio=qu, 
                             optimize_method="random", trace=TRUE)

return(opt_qu)
}

create_df <- function(weights, investment, stocks){
amt_to_invest <- dollar_format()(round(as.numeric(weights),2)*investment)
df <- data.frame(matrix(nrow = 1,data = amt_to_invest))
colnames(df) <- stocks
return(df)
}

print_desc <- function(objective_measures, betas, weights, hist){
  
  out <- objective_measures
  #portfolio mean
  mean <- round(out$mean*12,2)
  #portfolio standard deviation
  stdev <- round(out$CVaR*sqrt(12),2) #changed to CVaR 7/31
  #portfolio beta
  portfolio_beta <- sum(as.vector(weights)*betas)
  
  numbers <- c(paste(mean*100, "%", sep=""), paste(stdev*100, "%", sep=""), round(portfolio_beta,2), round(nrow(hist)/12,1))
  letters <- c("Annual Expected Return", "Annual CVaR", "Portfolio Beta", "Years of Data")
  
  #assemble data frame
  df2 <- data.frame(matrix(nrow = 1, data = numbers))
  colnames(df2) <- letters
  return(df2)
}

meanorstdev <- function(opt_qu){
  values <- as.numeric(opt_qu$objective_measures)
  return(values)
}

