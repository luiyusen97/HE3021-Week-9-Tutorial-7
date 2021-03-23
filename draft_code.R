library(haven)
library(tidyverse)
library(stats)
library(aod)
library(prediction)
library(margins)
library(AER)

filpaths <- list.files(path = paste0(getwd(),"//rawdata//"), 
                       full.names = TRUE)
mroz <- read_dta(file = filpaths[1])
wage2 <- read_dta(file = filpaths[2])

mroz_logit <- glm(formula = inlf ~ nwifeinc + educ + kidslt6 + age + exper,
                  data = mroz, family = binomial(link = "logit"))
mroz_probit <- glm(formula = inlf ~ nwifeinc + educ + kidslt6 + age + exper,
                  data = mroz, family = binomial(link = "probit"))
# negative sign for net wife income. The more income the wife has, the less likely she is to participate.
# positive sign for education. The more educated, the easier it is to secure a job
# negative sign for kids less than 6. The more young kids, the less time for job
# negative sign for age, the older the woman, the less likely to seek employment, since declining abilities
# positive sign for experience, the more experienced the woman at work, the more wage she expects, so the more likely she participates
test_individual <- data.frame()
test_individual <- rbind(test_individual, c(20, 10, 0, 30, 10))
colnames(test_individual) <- c("nwifeinc", "educ", "kidslt6", "age", "exper")

prediction(mroz_logit, at = test_individual)
predict_test_value_logit <- 0.7881
margins(mroz_logit, at = test_individual)
partial_effect_exper_value_logit <- 0.01969

prediction(mroz_probit, at = test_individual)
predict_test_value_probit <- 0.79
margins(mroz_probit, at = test_individual)
partial_effect_exper_value_probit <- 0.01993

wald_test_age_exper0 <- wald.test(Sigma = vcov(mroz_logit), b = coef(mroz_logit), Terms = c(5, 6))
wald_test_age_exper0_pvalue <- wald_test_age_exper0$result$chi2[3] # below minimum printed value

wage_model_IV <- ivreg(formula = lwage ~ educ| sibs, data = wage2)
summary(wage_model_IV)
wage_model_sibs <- lm(formula = lwage ~ sibs, data = wage2)

educ_brthord_model <- lm(formula = educ ~ brthord, data = wage2)
# statistically significant, fulfills instrumental relevancy condition
wald.test(Sigma = vcov(educ_brthord_model), b = coef(educ_brthord_model), Terms = 2)$result$chi2[3]

wage_model_IV_sibs_brthord <- ivreg(formula = lwage ~ educ + sibs | brthord + sibs, data = wage2)
display <- summary(wage_model_IV_sibs_brthord)
educ_brthord_sibs_model <- lm(formula = educ ~ brthord + sibs, data=wage2)
summary(educ_brthord_sibs_model)
cor(x=educ_brthord_sibs_model$fitted.values,y=educ_brthord_sibs_model$model$sibs)

reduced_form_educ <- lm(educ ~ brthord + sibs, data = wage2)
wald.test(Sigma = vcov(reduced_form_educ), b = coef(reduced_form_educ), Terms = 3)$result$chi2[3]
cor(x = reduced_form_educ$fitted.values, y = reduced_form_educ$model$sibs)
