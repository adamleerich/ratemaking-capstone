# What is the best sigma to use for severity dist?

mu <- function(exp, sigma) {
  log(exp) - (sigma^2) / 2
}


plot(x <- 1:10/2, mu(95000, x))

muestra <- function(exp, sigma, n) {
  rlnorm(n, mu(exp, sigma), sigma)
}



# For normal, expect that 95% of claims will be within +/- 1.96 std.dev.
#
#   mu + 1.96 * sigma [normal]
#   exp(mu + 1.96 * sigma) [lognormal]
#   exp(mu) * exp(Q * sigma), Q = 1.96 (or qnorm that gives desired confidence)
#
#   exp(mu) * exp(Q * sigma)    exp(mu + Q * sigma)
#   ------------------------ = ---------------------
#    exp(mu + sigma^2 / 2)     exp(mu + sigma^2 / 2)
#
#   = exp(Q * sigma - sigma^2 / 2)
#  
#   I want that to be less than or equal to M
#
#   M >= exp(Q * sigma - sigma^2 / 2)
#   
#   0 = 1/2 * sigma^2 - Q * sigma + log(M)
#
#   sigma = Q - sqrt(Q^2 - 2log(M))
#   


sigma <- function(a, M) {
  D <- qnorm(a)
  b <- D^2 - 2*log(M)
  ifelse(D - sqrt(b) <= 0, D + sqrt(b), D - sqrt(b))
}

plot(function(M) {sigma(0.99, M)}, from = 5, to = 10)


sigma(0.99, 10)




# sigma of 1.4 is ideal
b <- muestra(95000, 1.4, 500)
hist(b)
hist(log(b))

a <- sapply(1:100, function(x) {muestra(95000, 1.4, 500) %>% mean})
hist(a, freq = FALSE)


