#' ---
#' title:   "Capstone Day 1: Prepare Data"
#' author:  "Adam Rich"
#' date:    "July 25, 2018"
#' output:  pdf_document
#' ---

#' The goal for day 1 is to create two datasets
#' 
#'   * `pol_final` for analyzing frequency
#'   * `claims_final` for analyzing severity
#' 
#'     
#' There are a few steps that have to be done
#' 
#'   1. Load the four data files to memory
#'   1. Spread `pol_rating` 
#'   1. Join the new wide `pol_rating` object to `pol_dates`
#'   1. Join with the state lookup table 
#'   1. Put rating characteristics back in claims table
#'   1. Aggregate claims data by policy
#'   1. Join agg claims with policy data
#'   1. Add some derived columns
#'   1. Do some sense checking
#'   1. Save files
#'   

#' I'll use `tidyverse` package because it automatically loads `dplyr` and `tidyr`.
#' I'll need `tidyr` to "reshape" or "spread" the `pol_rating` data object.
#' The capstone project ZIP also came with *resources.R* so let's `source` that, too.

require(tidyverse)
source('c:/home/git/other/ratemaking-capstone/R/resources.R')



#' ## Load the four data files to memory

#' Note: Putting paranthesis around a statement
#' will force that statement's return value to be printed.
#' Usually a function's return value *is* printed,
#' but some, like `load` try to be "silent".
#' Since we are writing a report, I want to see the output.
#' 
#' The return value of `load` is a vector giving the names of the objects loaded.

(load("c:/home/git/other/ratemaking-capstone/share/claims.RData"))
(load("c:/home/git/other/ratemaking-capstone/share/pol_dates.RData"))
(load("c:/home/git/other/ratemaking-capstone/share/pol_rating.RData"))
state_lookup <- read.csv('c:/home/git/other/ratemaking-capstone/share/states.csv', stringsAsFactors = FALSE)



#' This is what the four data frames look like.

head(claims)
str(claims)

head(pol_dates)
str(pol_dates)

head(pol_rating)
str(pol_rating)

head(state_lookup)
str(state_lookup)






#' ## Spread `pol_rating` 

pol_rating_wide <- pol_rating %>% 
  spread(key = variable, value = value)

head(pol_rating_wide)
str(pol_rating_wide)





#' ## Join the new wide `pol_rating` object to `pol_dates`

pol <- pol_rating_wide %>% inner_join(pol_dates)

head(pol)
str(pol)



#' ## Join with the state lookup table 

#' The data frames `state_lookup` and `pol` do **not** spell "state" the same.
#' One has an uppercase 'S' the other lowercase.
#' Since R is case-sensitive, this is a problem.
#' This statement would give an error


#+ eval=FALSE
# Gives an error!
pol_state <- pol %>% inner_join(state_lookup)


#' One option is to use the `by` arg of `inner_join`.
pol_state <- pol %>% 
  inner_join(state_lookup, by = c("state" = "State"))

#' Another option is to rename the columns of `state_lookup` first.
#' Then do the join.
#' I like this one because I want to rename the other columns anyway.

names(state_lookup) <- c('state', 'state_group', 'state_population')
pol_state <- pol %>% 
  inner_join(state_lookup)

#' OK, let's do this a third time, because I actually 
#' don't want `state_population` in the joined table.

state_group_lookup <- state_lookup[, c('state', 'state_group')]
pol_state <- pol %>% 
  inner_join(state_group_lookup)

head(pol_state)
str(pol_state)




#' ## Put rating characteristics back in claims table
#' 
#' Let's do this step before aggregating the claims data
#' and adding to the policy data.
#' The reason is that I don't need "total claim count" by policy 
#' added back to the claims data.

claims_final <- claims %>% inner_join(pol_state)

head(claims_final)
str(claims_final)








#' ## Aggregate claims data by policy
#' 
#' Like everything in R there is more than one way to do this.
#' Who knows which is better...

# One option for aggregating claims
claims_agg <- claims %>% 
  group_by(policy_number) %>% 
  summarize(
    total_ultimate = sum(claim_ultimate), 
    claim_count = n())

# Another option for aggregating claims

claims$count <- 1
claims_agg <- claims %>% 
  group_by(policy_number) %>% 
  summarize(
    total_ultimate = sum(claim_ultimate), 
    claim_count = sum(count))


head(claims_agg)
str(claims_agg)





#' ## Join agg claims with policy data
#' 
#' Doing the join is easy.
#' But because this is a left join,
#' Any policies without claims will have NAs in the `total_ultimate`
#' and `claim_count` columns.
#' But, this will cause problems in modeling later,
#' so we will change NAs in these columns to 0.

pol_final <- pol_state %>%
  left_join(claims_agg)

#' There are different ways to replace NAs with zeros.
#' I'll use one for `total_ultimate`.
pol_final$total_ultimate[is.na(pol_final$total_ultimate)] <- 0

#' And, another for `claim_count`.
pol_final$claim_count <- 
  ifelse(is.na(pol_final$claim_count), 0, pol_final$claim_count)

head(pol_final)
str(pol_final)




#' ## Add some derived columns

# I want this claim in both!
pol_final$years_in_business <- 
  year_yyyymm(pol_final$inception) - as.integer(pol_final$year_started) + 1

claims_final$years_in_business <- 
  year_yyyymm(claims_final$inception) - as.integer(claims_final$year_started) + 1

pol_final$average_severity <- ifelse(
  pol_final$claim_count == 0, 
  0, 
  pol_final$total_ultimate / pol_final$claim_count
)

head(pol_final)
head(claims_final)





#' ## Do some sense checking
#' 
#' You should always check that you didn't lose or make up any data
#' especially when doing joins.

# Next two statements need to output the same number.
sum(pol_final$claim_count)
nrow(claims)

# Next three statements need to output the same number.
sum(pol_final$total_ultimate)
sum(claims$claim_ultimate)
sum(pol_final$average_severity * pol_final$claim_count)

# Next three statements need to output the same number.
nrow(pol_final)
length(unique(pol_final$policy_number))
length(unique(pol$policy_number))






#' ## Save files
#' 
#' I'm going to be fancy, because I want a timestamp in the file name.

fname <- paste0(
  'c:/home/git/other/ratemaking-capstone/demo/data-', 
  format(Sys.time(), '%Y-%m-%d-%H%M'), 
  '.RData')

print(fname)
save(pol_final, claims_final, file = fname)


