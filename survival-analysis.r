# Survival analysis tutorial
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# Vilija Joyce
# 20220312

#########################################################
# Load libraries
#########################################################

install.packages("survival")
install.packages("survminer")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyverse")

library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(tidyverse)

#########################################################
# Create example dataset with start and follow-up dates
# tibble is like a data.frame (part of tidyverse package)
#########################################################

test1 <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
    )

# Ck
glimpse(test1)
test1

# Note that date columns appear as chr or character columns
# Convert to date columns

test1 %>% 
  mutate(
    sx_date = as.Date(sx_date, format = "%Y-%m-%d"), 
    last_fup_date = as.Date(last_fup_date, format = "%Y-%m-%d") 
    )
# Note that dates now recorded as proper dates, not character vars

# Calculate survival time by subracting follow-up date from start date
# Convert to years
test1 %>% 
  mutate(
    os_yrs = 
      as.numeric(
        difftime(last_fup_date, 
                 sx_date, 
                 units = "days")) / 365.25
    )

#########################################################
# Use lung dataset to run survival analysis
#########################################################

