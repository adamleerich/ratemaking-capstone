# Author: Adam L. Rich
# Date:   August 16, 2017
# Description:
#
#   R script to create data needed for ratemaking capstone
#


require(devtools)
require(magrittr)
require(rmarkdown)
require(knitr)
require(alrtools)
require(MASS)
require(reshape2)
source('./R/resources.R')



val_date <- 201612




#############################################
# POLICIES
#   Line is A&E
#   Exposure base is revenue
#
#   Frequency of claim is a function of
#     Region
#     Revenue
#     Primary Discipline
# 
#   Also include
#     Number of Architects
#     Whether they use written contracts
#     Longevity in business
#     Number of Claims in Last Five Years
#
#############################################



# Number of policies
n <- 1e5
policies <- data.frame(index = 1:n)



set.seed(912387)

policies$policy_number <- paste0(
  'C1AE', right(paste0('00000000', sample(1:1e6, n)), 8))

policies$policy_year <- sample(
  x = 2007:2016, 
  size = n, 
  prob = c(1, 2, 3, 4, 5, 5, 5, 5, 5, 5), 
  replace = TRUE
)

policies$duration_months <- 12


policies$policy_month <- sample(1:12, n, replace = TRUE)

policies$inception <- paste0(
  policies$policy_year, 
  right(paste0('00', policies$policy_month), 2)
)

a <- (policies$policy_month + policies$duration_months)
table(a)
b <- floor((a - 1) / 12)
table(b)

policies$expiration <- paste0(
  policies$policy_year + b, 
  right(paste0('00', a - 12*b), 2)
)



# Revenue
#   Minimum revenue should be 50e3
#   Maximum should be 5e6

revenue_low   <- c(50e3, 100e3, 250e3, 500e3, 1e6, 2.5e6)
revenue_high  <- c(revenue_low[-1], 5e6)
n_rev         <- 6

policies$revenue_bucket <- sample(
  x = 1:6,
  size = n,
  prob = c(6, 5, 4, 3, 2, 1),
  replace = TRUE
)

a <- runif(n)
b <- policies$revenue_bucket
policies$revenue <- round((revenue_high[b] - revenue_low[b]) * a + 
  revenue_low[b], 0)

# State/Region
#   Use CSV table for groupings
states <- read.csv('./data/states.csv', stringsAsFactors = FALSE)

a <- sample(1:nrow(states), n, prob = states$Population, replace = TRUE)

policies$state <- states$State[a]
policies$state_group <- states$Frequency.Group[a]

a <- factor(policies$state_group, levels = c('Low', 'Mid', 'High'))

policies$state_relativity <- c(1.0, 1.25, 1.5)[as.integer(a)]

# If revenue is over $4m state does not matter
policies$state_relativity[policies$revenue >= 4e6] <- 3.5




# Disciplines
#  Use CSV table for relativities
disciplines <- read.csv('./data/disciplines.csv', stringsAsFactors = FALSE)
a <- sample(
  x = 1:nrow(disciplines), 
  size = n, 
  prob = disciplines$Probability, 
  replace = TRUE
)

policies$discipline <- disciplines$Discipline[a]
policies$discipline_relativity <- disciplines$Relativity[a]
policies$discipline_group <- paste0('d', policies$discipline_relativity)




# Revenue frequency
#   Expected is 0.05 per million in claims
#   Remember to adjust rev as it is annual!

policies$revenue_frequency <- policies$revenue / 1e6 * 
  0.05 * policies$duration_months / 12

policies$expected_frequency <- 
  policies$revenue_frequency * policies$discipline_relativity *
  policies$state_relativity


# Get expected claim counts
odf <- 1.5

policies$claim_count <- rnegbin(
  n = n, 
  mu = policies$expected_frequency, 
  theta = policies$expected_frequency / (odf - 1)
)




#############################################
# GLM TEST for FREQ
# TODO Need to flesh this out
#############################################

glm_freq <- glm(
  data = policies,
  formula = claim_count ~ 
    state_group + discipline_group + revenue + duration_months,
  family = quasipoisson
)

summary(glm_freq)





#############################################
# CLAIMS
#############################################

# Number of claims total
m <- sum(policies$claim_count)
claims <- data.frame(claimindex = 1:m)

# Merge with policies table
lpolicies <- policies[policies$claim_count > 0, ]
a <- cumsum(lpolicies$claim_count)
b <- c(1, a[-length(a)] + 1)
lpolicies$claimindex_start <- b
claims <- cbind(claims, lookup(claims, lpolicies))



# Claim made date
#   Select a month at random from the duration
a <- floor(runif(m) * (claims$duration_months + 3))
claims$claim_made <- add_yyyymm(claims$inception, a)

# Claim closed date
#   Expect duration to be 2 years from report
#   Most between 1 and 3, so normal with s.d. = 0.5, mu = 2
a <- round(rnorm(m, mean = 24, sd = 6), 0)
a[a < 0] <- 0
claims$claim_closed <- add_yyyymm(claims$claim_made, a)

# Claim status
# Valuation date is val_date
claims$status <- ifelse(claims$claim_closed <= val_date, 'C', 'O')


# [August 21, 2017 ALR]
# Fixed to use claim made date instead of policy date for trend
claims$claim_made_year <- year_yyyymm(claims$claim_made)



# Do some claims testing







#############################################
# CLAIM SEVERITY
#   Average severity in 2016 will be $95k
#   Expect claims inflation of 3%
#   Model ultimate values using lognormal
#############################################

years              <- min(claims$claim_made_year):max(claims$claim_made_year)
inflation          <- 0.00
n_years            <- length(years)
inflation_factors  <- (1 + inflation) ^ (years - 2016)
avg_severity       <- inflation_factors * 95000





# lognormal mean
#
#   E[X] = exp(mu + sigma^2/2)
#   log(E[x]) = mu + sigma^2/2
#   mu = log(E[x]) - sigma^2/2
#

# Pick sigma as 1.4 and keep constant for each year
sigma <- rep(1.0, length(avg_severity))
mu <- log(avg_severity) - sigma^2/2

lparam <- data.frame(
  claim_made_year = years,
  sev_mu = mu,
  sev_sigma = sigma
)

# Ultimate claim value
claims$sev_mu <- NULL
claims$sev_sigma <- NULL
a <- lookup(claims, lparam)
claims$sev_mu <- a$sev_mu
claims$sev_sigma <- a$sev_sigma

claims$claim_ultimate <- rlnorm(m, claims$sev_mu, claims$sev_sigma)
claims$count <- 1



# [August 21, 2017 ALR]
# Make sure that the claim trend is calculatable

avg_severity <- aggregate(
  x = claims[, c('claim_ultimate', 'count')], 
  by = claims[, 'claim_made_year', drop = FALSE], 
  FUN = sum
)

avg_severity$avg_severity <- 
  avg_severity$claim_ultimate / avg_severity$count

#############################################
# Add some unimportant data to policies
#############################################

policies$year_started <- 
  policies$policy_year - sample(0:20, size = n, replace = TRUE)

a <- rnorm(n, 100000, 50000)
a[a < 50000] <- 50000
policies$employee_count <- floor(policies$revenue / a) + 1

policies$use_written_contracts <- 
  sample(c('Y', 'N'), n, prob = c(0.8, 0.2), replace = TRUE)


policies$five_year_claims <- 
  rnegbin(n = n, mu = policies$expected_frequency, 
          theta = policies$expected_frequency / (odf - 1)) + 
  rnegbin(n = n, mu = policies$expected_frequency, 
          theta = policies$expected_frequency / (odf - 1)) + 
  rnegbin(n = n, mu = policies$expected_frequency, 
          theta = policies$expected_frequency / (odf - 1)) + 
  rnegbin(n = n, mu = policies$expected_frequency, 
          theta = policies$expected_frequency / (odf - 1)) + 
  rnegbin(n = n, mu = policies$expected_frequency, 
          theta = policies$expected_frequency / (odf - 1))

policies$five_year_claims <- 
  floor(pmin(5, 2016 - policies$year_started) * 
  policies$five_year_claims / 5)


#############################################
# Save Data sets
#############################################


pol_dates <- policies[, c("policy_number", 
                     "inception", 
                     "expiration")]

pol_rating_wide <- policies[, c("policy_number",
                     "revenue", 
                     "state", 
                     "discipline", 
                     "year_started", 
                     "employee_count", 
                     "use_written_contracts", 
                     "five_year_claims")]




# Policy attributes, melted
pol_rating <- melt(pol_rating_wide, id.vars = 'policy_number')
pol_rating[!duplicated(pol_rating$variable), ]



# Claims
clms <- claims[ , 
  c("index", 
    "status",
    "policy_number", 
    "claim_made", 
    "claim_closed", 
    "status", 
    "claim_ultimate")]

clms$claim_closed[clms$status == 'O'] <- NA


save(claims,      file = './share/claims.RData')
save(pol_dates,    file = './share/pol_dates.RData')
save(pol_rating,    file = './share/pol_rating.RData')




