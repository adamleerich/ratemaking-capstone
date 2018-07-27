#' ## Test different GLMs against the actual exp frequencies
#' 

pol_low <- policies[policies$revenue < 4e6, ]
nrow(pol_low)

fit_p <- glm(
  claim_count ~ revenue + state_group + discipline, 
  data = pol_low,
  family = poisson()
)

#' I expect this to be the correct model!
fit_p_logrev <- glm(
  claim_count ~ log(revenue) + state_group + discipline, 
  data = pol_low,
  family = poisson()
)

fit_p_offset <- glm(
  claim_count ~ offset(log(revenue)) + state_group + discipline, 
  data = pol_low,
  family = poisson()
)

#' I expect this to be the correct model!
fit_p_0 <- glm(
  claim_count ~ log(revenue) + state_group + discipline + 0, 
  data = pol_low,
  family = poisson()
)


fit_p$aic
fit_p_logrev$aic
fit_p_offset$aic
fit_p_0$aic


copy.model <- function(m) {
  d <- data.frame(
    variable = names(m$coefficients),
    coefficient = m$coefficients
  )
  copy.table(d)
  d
}





pre_nb <- predict(fit_nb)
pre_qp <- predict(fit_qp)


summary(fit_p)
summary(fit_qp)

testthat::expect_equivalent(fit_qp, fit_p)





fit_nb <- glm.nb(
  claim_count ~ revenue + state_group + discipline, 
  data = pol_low
)

fit_nb_logrev <- glm.nb(
  claim_count ~ log(revenue) + state_group + discipline, 
  data = pol_low
)
summary(fit_nb)
summary(fit_nb_logrev)





fit_qp <- glm(
  claim_count ~ revenue + state_group + discipline, 
  data = pol_low,
  family = quasipoisson()
)

fit_qp_logrev <- glm(
  claim_count ~ log(revenue) + state_group + discipline, 
  data = pol_low,
  family = quasipoisson()
)


fit_1 <- glm(
  claim_count ~ 1, 
  data = pol_low,
  family = poisson()
)
summary(fit_1)
fit_1$coefficients[1] %>% exp
mean(pol_low$claim_count)
