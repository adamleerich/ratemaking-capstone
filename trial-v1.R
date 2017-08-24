#' ---
#' title:  "Ratemaking Capstone v1"
#' author: "ALR"
#' date:   "August 21, 2017"
#' output: html_document
#' ---



#+ include=FALSE
# knitr::opts_chunk$set(echo = FALSE)
require(alrtools)
require(magrittr)
require(dplyr)
require(ChainLadder)


#+ include=FALSE
# Description:
#   Test of whether the capstone works!
#   This is an R file with comments using RMarkdown syntax
#



#+ include=FALSE
getwd()
dir('./share', pattern = 'RData')



#' The data files available are
#'   
#'   File              | Notes
#'   ----------------- |------------------------
#'   pol1_train.RData  | Policy number, dates
#'   po13_train.RData  | Other info
#'   clms_train.RData  | Individual claims
#'   tri_train.RData   | Triangle of claims
#'   pol1_test.RData   | Same for test
#'   pol3_test.RData   | Same for test
#'   
#'   



#+ include=FALSE
# Load all the data
load("./share/clms_train.RData")
load("./share/pol1_test.RData")
load("./share/pol1_train.RData")
load("./share/pol3_test.RData")
load("./share/pol3_train.RData")
load("./share/tri_train.RData")



#+ include=FALSE
# Are there closed claims with 0 payment?
clms_train %>% 
  filter(status == 'C', claim_at_val < 1)



summary(clms_train)


# Get average severity
avg_severity <- clms_train %>% 
  mutate(year = floor(claim_made / 100)) %>% 
  filter(status == 'C') %>% 
  group_by(year) %>% 
  summarize(count = n(), severity = sum(claim_at_val)) %>% 
  mutate(avg_severity = severity / count)

lm_as <- lm(log(avg_severity) ~ year, data = avg_severity)

own_trend <- lm_as$coefficients[2] %>% exp - 1
industry_trend <- 0.03



# Does the Mack method work
tri_train
tri <- tri_train[, 2:11] %>% as.matrix %>% unname %>% as.triangle

mm <- MackChainLadder(tri)
cm <- ClarkLDF(tri)

cumprod(mm$f[10:1])
ldfs_mm <- 1 / summary(mm)$ByOrigin$Dev.To.Date




clms_train %>% head
clms_train$cm_year <- floor(clms_train$claim_made / 100)
clms_train %>% head
ldfs_mm


clms_train$ldf <- ldfs_mm[clms_train$cm_year - 2006]



















#' ## Headers for each object
names(pol1_train)
names(pol1_test)
names(pol3_train)
names(pol3_test)
names(clms_train)
names(tri_train)



#' Are the training and testing sets mutually exclusive?
pols_train <- unique(pol1_train$policy_number)
pols_train %>% length
pol1_train %>% nrow
pols_test <- unique(pol1_test$policy_number)
pols_test %>% length
pol1_test %>% nrow


sum(pols_train %in% pols_test)
sum(pols_test %in% pols_train)


pols_train <- unique(pol3_train$policy_number)
pols_train %>% length


pols_test <- unique(pol3_test$policy_number)
pols_test %>% length


sum(pols_train %in% pols_test)
sum(pols_test %in% pols_train)



#' Are status and status.1 the same?  If so, remove status.1
sum(!clms_train$status == clms_train$status.1)
clms_train$status.1 <- NULL



#' Is the current incurred equal to the most recent diagonal?
sum(clms_train$claim_at_val)
sum(diag(as.matrix(tri_train[, 11:2])))



#' pols3 objects have multiple rows per policy.
#' What is the key on this table?
#' Check that it is policy_number and variable
nrow(pol3_train[, c('policy_number', 'variable')])
nrow(unique(pol3_train[, c('policy_number', 'variable')]))
#' Since these have the same number of rows 
#' then policy_number, variable is a key.



#' We need to un-melt the pol3 object and join it with the pol1 object.
polw <- tidyr::spread(pol3_train, variable, value)
pol_train <- merge(pol1_train, polw)
#' Did the merge work?
nrow(pol_train)
sum(pol_train$policy_number %in% pols_train)



#' Some of the data need to be converted.
pol_train$inception <- as.numeric(pol_train$inception)
pol_train$expiration <- as.numeric(pol_train$expiration)
pol_train$revenue <- as.numeric(pol_train$revenue)
pol_train$five_year_claims <- as.numeric(pol_train$five_year_claims)
pol_train$employee_count <- as.numeric(pol_train$employee_count)





#' We now need to calculate trend in our loss data, if it has any.
#' It might be best to create a one-way table generator first.
#' Let's first add current incurred to the policy table.
inc <- clms_train %>% group_by(policy_number) %>% 
  summarize(
    total_incurred = sum(claim_at_val),
    claim_counts = n()
  )

pol_inc <- merge(pol_train, inc, all.x = TRUE)

pol_inc$total_incurred[is.na(pol_inc$total_incurred)] <- 0
pol_inc$claim_counts[is.na(pol_inc$claim_counts)] <- 0


sum(pol_inc$total_incurred)
sum(clms_train$claim_at_val)

sum(pol_inc$claim_counts)
nrow(clms_train)


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
clms <- merge(clms_train, pol_train)
clms$policy_year <- floor(clms$inception / 100)

clms_py <- clms %>% group_by(policy_year, status) %>% 
  summarize(count = n(), inc = sum(claim_at_val))


