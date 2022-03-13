#' ---
#' title: "Survival Analysis Tutorial"
#' author: "Vilija Joyce"
#' date: "20220312"
#' output:
#'    html_document:
#'      toc: true
#'      highlight: haddock
#' ---
#' 
#+ setup, include=FALSE
require(survival)
require(survminer)
require(lubridate)
require(dplyr)
require(tidyverse)

#' ## Create example dataset with start and follow-up dates
#' tibble is like a data.frame (part of tidyverse package)

test1 <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
    )

#' Ck
glimpse(test1)
test1

#' Note that date columns appear as chr or character columns
#' Convert to date columns

test1 %>% 
  mutate(
    sx_date = as.Date(sx_date, format = "%Y-%m-%d"), 
    last_fup_date = as.Date(last_fup_date, format = "%Y-%m-%d") 
    )
#' Note that dates now recorded as proper dates, not character vars

#' Calculate survival time by subracting follow-up date from start date
#' Convert to years
test1 %>% 
  mutate(
    os_yrs = 
      as.numeric(
        difftime(last_fup_date, 
                 sx_date, 
                 units = "days")) / 365.25
    )


#' ## Use lung cancer dataset to run survival analysis
#' Ck
glimpse(lung)

#' Create survival object (time and a "+" if the person was censored)
#' aka status=1 aka patient did not experience event (death) by end of study
Surv(lung$time, lung$status)[1:10]

#' Estimate survival curves with Kaplan-Meier method
#' Generate overall survival curve for entire cohort and assign it to object f1
#' Review names of that object
f1 <- survfit(Surv(time, status) ~ 1, data = lung)
names(f1)
#' "surv" is the survival probability corresponding to each time 

#' Basic K-M plot using base R 
#' Label axes
plot(survfit(Surv(time, status) ~ 1, data = lung),
	xlab = "Days",
	ylab = "Overall survival probability")

#' Fancy K-M plot using ggsurvplot
ggsurvplot(
	fit = survfit(Surv(time, status) ~ 1, data = lung),
	xlab = "Days",
	ylab = "Overall survival probability")
	
#' Probability of surviving beyond a certain number of years 
#' 1-month probability of survival
#' 3-month 
#' 6-month
#' 1-year 
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 30)
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 90)
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 180)
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)
#' 95.6%, 88.2%, 72.2%, 40.9%
#' Also see 95% CI lower and upper bounds

#' Median survival time
survfit(Surv(time, status) ~ 1, data = lung)
#' 310 days

#' Compare survival times between groups
#' Difference by sex?
survdiff(Surv(time, status) ~ sex, data = lung)

#' Cox regression
#' Effect size for one variable (like sex) or multiple variables
coxph(Surv(time, status) ~ sex, data = lung)
coxph(Surv(time, status) ~ sex + age + wt.loss, data = lung)

#' gtsummary version of the output 
#' Hazard ratio with confidence intervals and p-value in formatted table 
coxph(Surv(time, status) ~ sex, data = lung) %>% 
  gtsummary::tbl_regression(exp = TRUE) 
  
  coxph(Surv(time, status) ~ sex + age + wt.loss, data = lung) %>% 
  gtsummary::tbl_regression(exp = TRUE) 

#' ## Check for assumption that hazards are proportional at each point in time throughout follow-up.

res.cox <- coxph(Surv(time, status) ~ sex + age + wt.loss, data = lung)
res.cox

test.ph <- cox.zph(res.cox)
test.ph
#' P-value significant? No, p-value not significant - 
#' proportional hazards assumption hold (not violated)

plot(test.ph)
ggcoxzph(test.ph)
#' Deviation from a zero-slope (horizontal) line? 
#' No - p-values not significant - 
#' proportional hazards assumption holds (not violated)