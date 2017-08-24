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
source('resources.R')



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

policies$duration_months <- sample(
  x = c(6:18), 
  size = n, 
  prob = c(2, 1, 1, 1, 1, 5, 100, 10, 5, 1, 1, 1, 20), 
  replace = TRUE
)

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
states <- read.csv('states.csv', stringsAsFactors = FALSE)

a <- sample(1:nrow(states), n, prob = states$Population, replace = TRUE)

policies$state <- states$State[a]
policies$state_group <- states$Frequency.Group[a]

a <- factor(policies$state_group, levels = c('Low', 'Mid', 'High'))

policies$state_relativity <- c(1.0, 1.25, 1.5)[as.integer(a)]

# If revenue is over $4m state does not matter
policies$state_relativity[policies$revenue >= 4e6] <- 2.0




# Disciplines
#  Use CSV table for relativities
disciplines <- read.csv('disciplines.csv', stringsAsFactors = FALSE)
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
odf <- 2.5

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
inflation          <- 0.03
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

plot(avg_severity$avg_severity)
lm_as <- lm(formula = log(avg_severity) ~ claim_made_year, 
   data = avg_severity)
plot(lm_as)
summary(lm_as)





# Figure out claim values at each dev point
claims <- within(claims, {
  d000 <- 0
  d012 <- claims$claim_ultimate
  d024 <- claims$claim_ultimate
  d036 <- claims$claim_ultimate
  d048 <- claims$claim_ultimate
  d060 <- claims$claim_ultimate
  d072 <- claims$claim_ultimate
  d084 <- claims$claim_ultimate
  d096 <- claims$claim_ultimate
  d108 <- claims$claim_ultimate
  d120 <- claims$claim_ultimate
})

claims$age_at_close <- 
  diff_yyyymm(claims$policy_year * 100, claims$claim_closed)

claims$age_at_open <- 
  diff_yyyymm(claims$policy_year * 100, claims$claim_made)

claims$age_at_val <- 
  diff_yyyymm(claims$policy_year * 100, val_date)

for(i in 1:10) {
  
  a <- runif(m)
  val <- add_yyyymm(200700, i * 12)
  val_prior <- add_yyyymm(val, -12)
  d <- paste0('d', right(paste0('000', i * 12), 3))
  d_prior <- paste0('d', right(paste0('000', (i - 1) * 12), 3))
  inc_prior <- claims[, d_prior]
  
  claims[, d] <- NA
  
  # If not yet open, set to 0
  l <- claims$age_at_open > (i * 12)
  claims[l, d] <- 0
  
  # If closed, set to ultimate
  l <- claims$age_at_close <= (i * 12)
  claims[l, d] <- claims$claim_ultimate[l]
  
  # If runif > 0.7, leave at last
  l <- (a > 0.7) & (is.na(claims[, d]))
  claims[l, d] <- claims[l, d_prior]
  
  # Otherwise interpolate to Ultimate
  l <- (a <= 0.7) & (is.na(claims[, d]))
  claims[l, d] <- (claims$claim_ultimate[l] - claims[l, d_prior]) * 
    a[l] / 0.7 + claims[l, d_prior]
  
}

claims$claim_at_val[claims$policy_year == 2007] <- 
  claims$d120[claims$policy_year == 2007]
claims$claim_at_val[claims$policy_year == 2008] <- 
  claims$d108[claims$policy_year == 2008]
claims$claim_at_val[claims$policy_year == 2009] <- 
  claims$d096[claims$policy_year == 2009]
claims$claim_at_val[claims$policy_year == 2010] <- 
  claims$d084[claims$policy_year == 2010]
claims$claim_at_val[claims$policy_year == 2011] <- 
  claims$d072[claims$policy_year == 2011]
claims$claim_at_val[claims$policy_year == 2012] <- 
  claims$d060[claims$policy_year == 2012]
claims$claim_at_val[claims$policy_year == 2013] <- 
  claims$d048[claims$policy_year == 2013]
claims$claim_at_val[claims$policy_year == 2014] <- 
  claims$d036[claims$policy_year == 2014]
claims$claim_at_val[claims$policy_year == 2015] <- 
  claims$d024[claims$policy_year == 2015]
claims$claim_at_val[claims$policy_year == 2016] <- 
  claims$d012[claims$policy_year == 2016]


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

# Policies
pol_train <- sample(policies$policy_number, 50000)
pol_test <- policies$policy_number[
  !policies$policy_number %in% pol_train]


pol1 <- policies[, c("policy_number", 
                     "inception", 
                     "expiration")]

pol2 <- policies[, c("policy_number",
                     "revenue", 
                     "state", 
                     "discipline", 
                     "year_started", 
                     "employee_count", 
                     "use_written_contracts", 
                     "five_year_claims")]

pol1_train <- pol1[pol1$policy_number %in% pol_train, ]
pol1_test  <- pol1[pol1$policy_number %in% pol_test, ]



# Policy attributes, melted
pol3 <- melt(pol2, id.vars = 'policy_number')
pol3[!duplicated(pol3$variable), ]

pol3_train <- pol3[pol3$policy_number %in% pol_train, ]
pol3_test  <- pol3[pol3$policy_number %in% pol_test, ]


# Claims
clms <- claims[ , 
  c("index", 
    "status",
    "policy_number", 
    "claim_made", 
    "claim_closed", 
    "status", 
    "claim_at_val")]

clms$claim_closed[clms$status == 'O'] <- NA

clms_train <- clms[clms$policy_number %in% pol_train &
                     clms$claim_made <= val_date, ]







# Triangle
claims_train <- claims[claims$policy_number %in% pol_train, ]
square_train <- aggregate(
  x = claims_train[, c("d012","d024","d036","d048","d060",
        "d072","d084","d096","d108","d120")], 
  by = claims_train[, 'policy_year', drop = FALSE], 
  FUN = sum
)

claims_test <- claims[claims$policy_number %in% pol_test, ]
square_test <- aggregate(
  x = claims_test[, c("d012","d024","d036","d048","d060",
                       "d072","d084","d096","d108","d120")], 
  by = claims_test[, 'policy_year', drop = FALSE], 
  FUN = sum
)

d1 <- rep(0:9, each = 10)
d2 <- rep(0:9, times = 10)
a <- as.matrix(square_train[2:11])
a[d1 + d2 > 9] <- NA
a <- cbind(2007:2016, a)
tri_train <- as.data.frame(a)
names(tri_train) <- names(square_train)

d1 <- rep(0:9, each = 10)
d2 <- rep(0:9, times = 10)
a <- as.matrix(square_test[2:11])
a[d1 + d2 > 9] <- NA
a <- cbind(2007:2016, a)
tri_test <- as.data.frame(a)
names(tri_test) <- names(square_test)



# Save objects
save(pol1_train,  file = './share/pol1_train.RData')
save(pol1_test,   file = './share/pol1_test.RData')
save(pol3_train,  file = './share/pol3_train.RData')
save(pol3_test,   file = './share/pol3_test.RData')
save(clms_train,  file = './share/clms_train.RData')
save(tri_train,   file = './share/tri_train.RData')
save(claims,      file = 'claims.RData')
save(policies,    file = 'policies.RData')





