#' ---
#' title:  "Ratemaking Capstone v1"
#' author: "ALR"
#' date:   "July 24, 2018"
#' output: html_document
#' ---



#+ include=FALSE
# knitr::opts_chunk$set(echo = FALSE)
require(alrtools)
require(magrittr)
require(dplyr)
require(ChainLadder)
require(tree)



#+ include=FALSE
# Description:
#   Test of whether the capstone works!
#   This is an R file with comments using RMarkdown syntax
#



#+ include=FALSE
getwd()
dir('./share', pattern = 'RData')



#+ include=FALSE
# Load all the data
load('./share/claims.RData')
load('./share/pol_dates.RData')
load('./share/pol_rating.RData')



#+ include=FALSE
# Are there closed claims with 0 payment?
claims %>% 
  filter(status == 'C', claim_ultimate < 1)



summary(claims)


# Get average severity
avg_severity <- claims %>% 
  mutate(year = floor(claim_made / 100)) %>% 
  filter(status == 'C') %>% 
  group_by(year) %>% 
  summarize(count = n(), severity = sum(claim_ultimate)) %>% 
  mutate(avg_severity = severity / count)

lm_as <- lm(log(avg_severity) ~ year, data = avg_severity)

own_trend <- lm_as$coefficients[2] %>% exp - 1
industry_trend <- 0.03



claims %>% head
claims$cm_year <- floor(claims$claim_made / 100)
claims %>% head





















#' ## Headers for each object
names(pol_dates)
names(pol_rating)
names(pol_rating)
names(claims)



#' Are the training and testing sets mutually exclusive?
pols <- unique(pol_dates$policy_number)
pols %>% length
pol_dates %>% nrow
pols <- unique(pol_dates$policy_number)
pols %>% length
pol_dates %>% nrow


sum(pols %in% pols)
sum(pols %in% pols)


pols <- unique(pol_rating$policy_number)
pols %>% length


pols <- unique(pol_rating$policy_number)
pols %>% length


sum(pols %in% pols)
sum(pols %in% pols)



#' Are status and status.1 the same?  If so, remove status.1
sum(!claims$status == claims$status.1)
claims$status.1 <- NULL



#' pols3 objects have multiple rows per policy.
#' What is the key on this table?
#' Check that it is policy_number and variable
nrow(pol_rating[, c('policy_number', 'variable')])
nrow(unique(pol_rating[, c('policy_number', 'variable')]))
#' Since these have the same number of rows 
#' then policy_number, variable is a key.



#' We need to un-melt the pol_rating object and join it with the pol_dates object.
polw <- tidyr::spread(pol_rating, variable, value)
pol <- merge(pol_dates, polw)
#' Did the merge work?
nrow(pol)
sum(pol$policy_number %in% pols)



#' Some of the data need to be converted.
pol$inception <- as.numeric(pol$inception)
pol$expiration <- as.numeric(pol$expiration)
pol$revenue <- as.numeric(pol$revenue)
pol$five_year_claims <- as.numeric(pol$five_year_claims)
pol$employee_count <- as.numeric(pol$employee_count)





#' We now need to calculate trend in our loss data, if it has any.
#' It might be best to create a one-way table generator first.
#' Let's first add current incurred to the policy table.
inc <- claims %>% group_by(policy_number) %>% 
  summarize(
    total_incurred = sum(claim_ultimate),
    claim_counts = n()
  )

pol_inc <- merge(pol, inc, all.x = TRUE)

pol_inc$total_incurred[is.na(pol_inc$total_incurred)] <- 0
pol_inc$claim_counts[is.na(pol_inc$claim_counts)] <- 0


sum(pol_inc$total_incurred)
sum(claims$claim_ultimate)

sum(pol_inc$claim_counts)
nrow(claims)


pol_inc$py <- left(pol_inc$inception, 4) %>% as.numeric


pol_inc %>% group_by(py) %>% 
    summarize(
      pol_counts = n(),
      claim_counts = sum(claim_counts),
      total_incurred = sum(total_incurred),
      revenue = sum(revenue),
      employee_count = sum(employee_count),
      five_year_claims = sum(five_year_claims)
    ) %>% as.data.frame



# Get policy data in claims table
claims <- merge(claims, pol)
claims$policy_year <- floor(as.numeric(claims$inception) / 100)

claims_py <- claims %>% group_by(policy_year, status) %>% 
  summarize(count = n(), inc = sum(claim_ultimate))






log(claims$claim_ultimate) %>% hist



sum(pol_inc$claim_counts)
nrow(claims)


mean(pol_inc$claim_counts)
sd(pol_inc$claim_counts)



# Get state group
state <- read.csv('./data/states.csv', stringsAsFactors = TRUE)
state_levels <- state$State
pstates <- factor(pol_inc$state, levels = state_levels)
pol_inc$StateGroup <- state$Frequency.Group[pstates]







model1 <- glm(claim_counts ~ revenue + StateGroup + discipline, data = pol_inc)
summary(model1)





tpol <- tree(
  formula = claim_counts ~ revenue + discipline,
  data = pol_inc
)


plot(tpol, type = 'uniform')
text(tpol, pretty = 5, col = 'blue', cex = 0.8)





