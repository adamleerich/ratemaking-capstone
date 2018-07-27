require(testthat)


# How to create a family for GLMs



pfam <- poisson()
qfam <- quasipoisson()


# What is different?

expect_equivalent(pfam$link, qfam$link)
expect_equivalent(pfam$linkfun, qfam$linkfun)
expect_equivalent(pfam$linkinv, qfam$linkinv)
expect_equivalent(pfam$variance, qfam$variance)
expect_equivalent(pfam$dev.resids, qfam$dev.resids)
expect_equivalent(pfam$mu.eta, qfam$mu.eta)
expect_equivalent(pfam$validmu, qfam$validmu)
expect_equivalent(pfam$valideta, qfam$valideta)


expect_equivalent(pfam$initialize, qfam$initialize)
expect_equivalent(pfam$family, qfam$family)
expect_equivalent(pfam$aic, qfam$aic)


pfam$initialize
qfam$initialize









q0 <- quasi(link = 'log', variance = 'constant')
q1 <- quasi(link = 'log', variance = 'mu')
q2 <- quasi(link = 'log', variance = 'mu^2')
q3 <- quasi(link = 'log', variance = 'mu^3')



# Do not match
expect_equivalent(q0$dev.resids, q1$dev.resids)
expect_equivalent(q0$dev.resids, q2$dev.resids)
expect_equivalent(q0$dev.resids, q3$dev.resids)

# Do not match
expect_equivalent(q0$variance, q1$variance)
expect_equivalent(q0$variance, q2$variance)
expect_equivalent(q0$variance, q3$variance)





q0$dev.resids
q1$dev.resids
q2$dev.resids
q3$dev.resids







f1 <- quasi(link = 'log', variance = 'constant')
f2 <- quasi(link = 'log', variance = 'mu')
f3 <- quasipoisson()


unclass(f1$family)
unclass(f2$family)


unclass(f1$link)
unclass(f2$link)


unclass(f1$linkfun)
unclass(f2$linkfun)


unclass(f1$linkinv)
unclass(f2$linkinv)


unclass(f1$variance)
unclass(f2$variance)


unclass(f1$dev.resids)
unclass(f2$dev.resids)


unclass(f1$aic)
unclass(f2$aic)


unclass(f1$mu.eta)
unclass(f2$mu.eta)


unclass(f1$initialize)
unclass(f2$initialize)


unclass(f1$validmu)
unclass(f2$validmu)


unclass(f1$valideta)
unclass(f2$valideta)


unclass(f1$varfun)
unclass(f2$varfun)



