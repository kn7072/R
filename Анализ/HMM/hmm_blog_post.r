
#----------------------------------------------------------------
# Hiden Markov Model of S&P 500 log returns
# See documentation for depmixS4 package 
# http://cran.r-project.org/web/packages/depmixS4/depmixS4.pdf and presentation 
# on Singapore R Users Group Site on HMM February 14, 2014
# http://www.meetup.com/R-User-Group-SG/files/

# https://www.r-bloggers.com/getting-started-with-hidden-markov-models-in-r/
library(depmixS4)
library(TTR)
library(ggplot2)
library(reshape2)
library(xts)

## Bull and Bear Markets ##
# Load S&P 500 returns from Yahoo
Sys.setenv(tz = "UTC")
sp500 <- getYahooData("^GSPC", start = 19500101, end = 20120909, freq = "daily")
head(sp500)
tail(sp500)

# Preprocessing
# Compute log Returns
ep <- endpoints(sp500, on = "months", k = 1)
sp500LR <- sp500[ep[2:(length(ep)-1)]]
sp500LR$logret <- log(sp500LR$Close) - lag(log(sp500LR$Close))
sp500LR <- na.exclude(sp500LR)
head(sp500LR)

# Build a data frame for ggplot
sp500LRdf <- data.frame(sp500LR)
sp500LRdf$Date <-as.Date(row.names(sp500LRdf),"%Y-%m-%d")

# Plot the S&P 500 returns
ggplot( sp500LRdf, aes(Date) ) + 
  geom_line( aes( y = logret ) ) +
  labs( title = "S&P 500 log Returns")


# Construct and fit a regime switching model
mod <- depmix(logret ~ 1, family = gaussian(), nstates = 4, data = sp500LR)
set.seed(1)
fm2 <- fit(mod, verbose = FALSE)
#
summary(fm2)
print(fm2)
str(fm2)
