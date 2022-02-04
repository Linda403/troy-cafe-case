install.packages("readr")
install.packages("dplyr")
install.packages("here")
install.packages("lubridate")
install.packages("tidyr")
install.packages("gt")
install.packages("tidymodels")

library(readr)      # Readr package allows us to load the data
library(dplyr)      # Dplyr makes it easy to group and summarize data
library(ggplot2)    # I will use ggplot for some of the charts
library(here)       # Here package makes navigating my directories easier
library(lubridate)  # Lubridate package makes working with dates easier
library(tidyr)      # Will use some functions in this package for tables
library(gt)         # Make my tables prettier
library(tidymodels) # I will use some of the utilities here to present models cleanly

tx <- read_csv(here("coffee-shop-tx-1.csv"))
cx <- read_csv(here("coffee-shop-cx-1.csv"))

# Group transactions by cid and calculate summary statistics
# I will save the new analysis into a dataframe called df

analysis_period_end <- ymd("2020-01-01")

df <- tx %>% 
  group_by(cid) %>% 
  summarize(recency = as.numeric(analysis_period_end - max(tx_date)),
            frequency = n_distinct(invoice),
            monetary = sum(tx_amt))
#don't understand this part

head("df")
summary("df")
hist(df$recency)


quintiles <- quantile(df$recency, probs = seq(0, 1, 0.2))

# The quantile() function in R takes a vector of data and divides it
# according to a sequency of probabilities. In this case, I create
# a sequence that starts at 0 and then breaks at each 20% of the distance 
# up until and including 1. Here's what it looks like:

df$r_score[df$recency > quintiles[5]] <- 1
df$r_score[df$recency > quintiles[4] & df$recency <= quintiles[5]] <- 2
df$r_score[df$recency > quintiles[3] & df$recency <= quintiles[4]] <- 3
df$r_score[df$recency > quintiles[2] & df$recency <= quintiles[3]] <- 4
df$r_score[df$recency <= quintiles[2]] <- 5

quintiles <- quantile(df$frequency, probs = seq(0, 1, 0.20))

df$f_score[df$frequency < quintiles[2]] <- 1
df$f_score[df$frequency >= quintiles[2] & df$frequency < quintiles[3]] <- 2
df$f_score[df$frequency >= quintiles[3] & df$frequency < quintiles[4]] <- 3
df$f_score[df$frequency >= quintiles[4] & df$frequency < quintiles[5]] <- 4
df$f_score[df$frequency >= quintiles[5]] <- 5