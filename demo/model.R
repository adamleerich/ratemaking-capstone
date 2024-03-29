#' ---
#' title:   "Check fitting methods against chosen parameters"
#' author:  "Adam Rich"
#' date:    "July 26, 2018"
#' output:  pdf_document
#' ---



#' ## Prep Stuff
#' 
#' These objects were created at the same time as the
#' smaller problem datasets.
#' Also load required packages.
(load('c:/home/git/other/ratemaking-capstone/data/created-data-full.RData'))
source('c:/home/git/other/ratemaking-capstone/R/resources.R')
require(tidyverse)
require(actuar)
require(MASS)
require(fitdistrplus)
require(tree)



#' ## Purpose
#' 
#' The purpose of this R file is to 
#' try out the fitting methods I expect will work
#' and show that they provide something very close
#' to the pre-selected frequency and severity models.
#' 



#' ## Data Checks
#' 
#' Make sure that column types are as expected.
str(policies)



#' ## Pre-chosen Relativites and Parameters
#' 
#' The base frequency is 0.05 claims per million in revenue.
#'
#' State relativities are
#' 
#'   * Low = 1.0
#'   * Med = 1.25
#'   * High 1.5
#'   * For companies with revenue over $4m, override with 3.5
#'   
#' Discipline relativities are supposed to be:
disciplines[, c('Discipline', 'Relativity')]





#' ## Severity Curve
#' 
#' I'll start with the severity curve since it is 
#' probably easier than the frequency.
#' I know that the claims come from a lognormal distribution
#' with the same parameters, regardless of any policy
#' characteristics.
#' 
#' An effective way to know whether data is from a 
#' lognormal distribution is to do a qqnorm plot
#' on the log values.

qqnorm(log(losses$claim_ultimate))



#' The moments of the sample data are

mean_loss <- mean(losses$claim_ultimate)
sd_loss <- sd(losses$claim_ultimate)

mean_loss
sd_loss


#' ## Test a Gamma
#' 
#' A gamma distribution with the same moments would have these parameters
#' (k is shape, theta is scale):

k <- (mean_loss / sd_loss)^2
theta <- mean_loss / k

k * theta
sqrt(k) * theta

losses_gamma <- rgamma(
  n = nrow(losses),
  shape = k,
  scale = theta
)

mean(losses_gamma)
sd(losses_gamma)



#' The `qqnorm` plot for this just doesn't fit a straight line.

qqnorm(log(losses_gamma))





#' ## Severity Parameters
#' 
#' The actual lognormal parameters used are
(p <- unique(losses[, c('sev_mu', 'sev_sigma')]))

mu_actual <- p[, 1]
sigma_actual <- p[, 2]




#' Using the `fitdistrplus` package,
#' we get these fitted parameters:
(b <- fitdistrplus::fitdist(losses$claim_ultimate, 'lnorm'))

mu_fitted <- b$estimate[1] %>% unname
sigma_fitted <- b$estimate[2] %>% unname



#' A function for lognormal limited expected value is

levlnorm <- function(x, mu, sigma) {
  exp(mu + sigma^2/2) *
    pnorm((log(x) - mu - sigma^2) / sigma) +
    x * (1 - plnorm(x, mu, sigma))
}


#' The expected values, unlimited and then limited at
#' $1m, $5m, and $10m are:
actual_U <- exp(mu_actual + sigma_actual^2/2)
actual_1 <- levlnorm(1e6, mu_actual, sigma_actual)
actual_2 <- levlnorm(2e6, mu_actual, sigma_actual)
actual_5 <- levlnorm(3e6, mu_actual, sigma_actual)



#' Check using "brute force"

integrate(
  lower = 1,
  upper = 100e6,
  f = function(t) {t * dlnorm(t, mu_actual, sigma_actual)}
)

integrate(
  lower = 1,
  upper = 100e6,
  f = function(t) {pmin(t, 1e6) * dlnorm(t, mu_actual, sigma_actual)}
)

integrate(
  lower = 1,
  upper = 100e6,
  f = function(t) {pmin(t, 2e6) * dlnorm(t, mu_actual, sigma_actual)}
)

integrate(
  lower = 1,
  upper = 100e6,
  f = function(t) {pmin(t, 5e6) * dlnorm(t, mu_actual, sigma_actual)}
)


#' The severities are so low in this distribution that the limits 
#' of $1m, $2m, and $5m are basically the same price.
#' 



#' ## Frequency Prep
#' 
#' There is an engineered break at $4m in revenue.  Below that, 
#' revenue, state group and discipline are all predictive.
#' Above that, state group is no longer predictive.
#' 
#' One thing that is really important to check before 
#' doing a GLM on frequency is whether a Poisson error is 
#' appropriate.
#' This check is done by comparing the mean and variance of the 
#' observations.

mean(policies$claim_count)
var(policies$claim_count)

var(policies$claim_count) / mean(policies$claim_count)

#' They are *not* close so Poisson is not appropriate.
#' However, a negative binomial distribution has
#' variance greater than the mean.
#' 
#' The problem with the glm.nb is that it assumes that 
#' theta is a constant.  But, I know that 
#' var(X) = o.d.f * E[X]
#' where o.d.f. is a constant.
#' So, I need a family with a log link that allows for this linear relationship
#' between variance and mean.
#' Quasipoisson does this.
#' 
#' 
#' Create some new columns in the dataset
#' Re-base the factors, too:
#' 
#'   * Base state = Low group (lowest relativity)
#'   * Base discipline = Landscape Architecture (lowest relativity)
#'   

policies$revmillions <- policies$revenue / 1e6

dlevels <- c(
  "Landscape Architecture", "Surveyor", "Mechanical Engineering", 
  "Civil Engineer", "Architect", "Structural Engineer")

slevels <- c("Low", "High", "Mid")

policies$state_group_factor <- factor(
  x = policies$state_group,
  levels = slevels
)

policies$discipline_factor <- factor(
  x = policies$discipline,
  levels = dlevels
)

policies$use_written_contracts_factor <- as.factor(policies$use_written_contracts)



#' ## Engineered Break
#' 
#' Not all revenue bands have the same model!

tpol <- tree(
  claim_count ~ revmillions + discipline_factor + 
    state_group_factor + year_started + 
    employee_count + use_written_contracts_factor, 
  policies
)

plot(tpol)
text(tpol)





pol_low <- policies[policies$revenue < 4e6, ]
pol_high <- policies[policies$revenue >= 4e6, ]
nrow(pol_low)
nrow(pol_high)







#' ## Frequency models
#' 
#' I expect this to be the correct frequency model!
fit_low <- glm(
  claim_count ~ offset(log(revmillions)) + state_group_factor + discipline_factor,
  data = pol_low,
  family = quasipoisson()
)

fit_high <- glm(
  claim_count ~ offset(log(revmillions)) + discipline_factor,
  data = pol_high,
  family = quasipoisson()
)

summary(fit_low)
summary(fit_high)


copy.model(fit_low)
copy.model(fit_high)




