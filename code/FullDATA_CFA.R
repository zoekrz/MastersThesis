###############################################################################
## Quant data: CFA with FULL dataset ##
###############################################################################
#load necessary packages
library(lavaan)
library(semPlot)
library(ggcorrplot)
library(moments)
library(psych)
library(tidyverse)

options(scipen = 999) #limit decimals


#2. preprocessing: no imputation needed (see Rogers, 2024; Kline, 2023)
colSums(is.na(combined_waves)) #practically no missing values in the justice questions
#why are there missing values in some of the justice variables?
View(combined_waves[rowSums(is.na(combined_waves[, justice_cols])) > 0, ]) #people dropped out

#remove those rows with missing values
combined_waves <- combined_waves[complete.cases(combined_waves[, justice_cols]),]
3371 - 3357 # 14 cases were deleted

#2. preprocessing: examine standard deviations that are < .25
justice_cols <- c(
  "justice_gen_1",
  "justice_gen_2",
  "justice_gen_3",
  "justice_gen_4",
  "justice_tax_1",
  "justice_tax_2",
  "justice_tax_3",
  "justice_tax_4",
  "justice_sub_1",
  "justice_sub_2",
  "justice_sub_3",
  "justice_sub_4"
)
vec_sd <- apply(
  combined_waves[, justice_cols], 1, sd, na.rm = TRUE
)
which(vec_sd < 0.25) #no participant shows a sd < 0.25, so no participant is 
#excluded due to a lack of variability (Collier, 2020, p.18)

#check if data is normally distributed, as ordinal data with enough categories
# (≥ 5), big datasets and non-skewed distributions
# can be treated as continious according to Rhemtulla et al.,2012)
skewness(combined_waves[, justice_cols], na.rm = TRUE)
# skewness between -1 and 1 indicates symmetrical distributions (source!) #here all good

#check if some items show multicollinearity (vif)
cor_ma_factors_comwaves <- cor(combined_waves[, c(
  "justice_gen_1",
  "justice_tax_1",
  "justice_sub_1",
  "justice_gen_2",
  "justice_tax_2",
  "justice_sub_2",
  "justice_gen_3",
  "justice_tax_3",
  "justice_sub_3",
  "justice_gen_4",
  "justice_tax_4",
  "justice_sub_4"
)])
ggcorrplot(cor_ma_factors_comwaves, lab = TRUE, title = "Cor Matrix: Justice variables in combined_waves ordered by factors")
#there are no higher bivariate correlations than >.9 as suggested to be a problem (Kline, 2023, p. 56)

#check for intern consistency of items (cronbachs alpha)
#make subscales

c_util <- c("justice_gen_1", "justice_tax_1", "justice_sub_1")
c_equal <- c("justice_gen_2",
             "justice_tax_2",
             "justice_sub_2")
c_suff <- c("justice_gen_3",
            "justice_tax_3",
            "justice_sub_3")
c_limit <- c("justice_gen_4",
             "justice_tax_4",
             "justice_sub_4")

alpha_util <- alpha(combined_waves[,c_util])
alpha_equal <- alpha(combined_waves[,c_equal])
alpha_suff <- alpha(combined_waves[,c_suff])
alpha_limit <- alpha(combined_waves[, c_limit])
print(alpha_util) # alpha = 0.37
print(alpha_equal) # alpha = 0.56
print(alpha_suff) # alpha = 0.53
print(alpha_limit) # alpha = 0.56
#all alphas are rather low; normal threshold is considered to be 0.7 (source)
#not so high enough internal consistency; I will use omega on the factor model to see, where it may lie

################################################################################
# H1 to H4: four factor model
#create model and fit

model1 <- '
#latent regressions justice
util =~ justice_gen_1 + justice_tax_1 + justice_sub_1
equal =~ justice_gen_2 + justice_tax_2 + justice_sub_2
suff =~ justice_gen_3 + justice_tax_3 + justice_sub_3
lim =~ justice_gen_4 + justice_tax_4 + justice_sub_4

#set latent variance to 1 for identification
util ~~ 1*util
equal ~~ 1*equal
suff ~~ 1*suff
lim ~~ 1*lim

#estimate covariances between all latent factors
util ~~ equal + suff + lim
equal ~~ suff + lim
suff ~~ lim

#estimate residual variances
justice_gen_1 ~~ justice_gen_1
justice_gen_2 ~~ justice_gen_2
justice_gen_3 ~~ justice_gen_3
justice_gen_4 ~~ justice_gen_4
justice_tax_1 ~~ justice_tax_1
justice_tax_2 ~~ justice_tax_2
justice_tax_3 ~~ justice_tax_3
justice_tax_4 ~~ justice_tax_4
justice_sub_1 ~~ justice_sub_1
justice_sub_2 ~~ justice_sub_2
justice_sub_3 ~~ justice_sub_3
justice_sub_4 ~~ justice_sub_4

# measured intercepts
justice_gen_1 ~ 1
justice_gen_2 ~ 1
justice_gen_3 ~ 1
justice_gen_4 ~ 1
justice_tax_1 ~ 1
justice_tax_2 ~ 1
justice_tax_3 ~ 1
justice_tax_4 ~ 1
justice_sub_1 ~ 1
justice_sub_2 ~ 1
justice_sub_3 ~ 1
justice_sub_4 ~ 1

'

cfa1 <- lavaan(
  model1,
  data = combined_waves,
  estimator = "MLR"
) #I get the warning that the cov matrix of latent variables is not positive definite
#I therefore investigate through using lavInspect

lavInspect(cfa1, "cov.lv") #some latent correlations (i.e., between equal & suff and equal & lim) are > 1, which is strange
summary(cfa1, standardized = TRUE)
semPaths(cfa1, whatLabels = "stand", layout = "tree") #visualise model BUT! I cannot interpret it

#visualise cor matrix with theorised latent factors next to each other
cor_matrix_factors <- cor(combined_waves[, c(
  "justice_gen_1",
  "justice_tax_1",
  "justice_sub_1",
  "justice_gen_2",
  "justice_tax_2",
  "justice_sub_2",
  "justice_gen_3",
  "justice_tax_3",
  "justice_sub_3",
  "justice_gen_4",
  "justice_tax_4",
  "justice_sub_4"
)])
ggcorrplot(cor_matrix_factors, lab = TRUE, title = "Cor Matrix: Justice variables in combined_waves ordered by factors")

#visualise cor matrix with contexts next to each other
cor_matrix_context <- cor(combined_waves[, c("justice_gen_1",
                                              "justice_gen_2",
                                              "justice_gen_3",
                                              "justice_gen_4",
                                              "justice_tax_1",
                                              "justice_tax_2",
                                              "justice_tax_3",
                                              "justice_tax_4",
                                              "justice_sub_1",
                                              "justice_sub_2",
                                              "justice_sub_3",
                                              "justice_sub_4"
)])
ggcorrplot(cor_matrix_context, lab = TRUE, title = "Cor Matrix: Justice variables in combined_waves ordered by contexts")

#check fit measures 
fitMeasures(cfa1)
fitMeasures(cfa1, "rmsea") #rmsea of 0.147 indicating bad fit

modindices(cfa1) #checking modification indices; they indicate I should free the variances of equal and suff factors
#this I cannot do as then I have an uneridentifies model
#next they indicate I should add intercepts for the latent factors; as far as I know I can do so but then interpretation gets more difficult
# further, they say, that the utilitarian factor should load onto all equal outcomes. Thsi does not make sense from theoretical perspective, 
# but indicates that there is something wrong with the utilitarian factor; I consider removing it from the model.
# but first I check for sources of that the covariance is not positive-definite

#check for sources of the error that sample covariance is not positive-definite (it cannot be inverted)
sapply(subset_combined, var, na.rm = TRUE) # there are no zero variances

#check eigenvalues of covariance matrix
cov_matrix_subset <- cov(subset_combined[, c("justice_gen_1",
                                             "justice_gen_2",
                                             "justice_gen_3",
                                             "justice_gen_4",
                                             "justice_tax_1",
                                             "justice_tax_2",
                                             "justice_tax_3",
                                             "justice_tax_4",
                                             "justice_sub_1",
                                             "justice_sub_2",
                                             "justice_sub_3",
                                             "justice_sub_4"
)])
eigen(cov_matrix_subset)
#ADD INTERPRETATION OF EIGENVALUES

################################################
#adress latent correlations higher than 1 through restricting first loadings to be 1
model2 <- '
#latent regressions justice
util =~ 1*justice_gen_1 + justice_tax_1 + justice_sub_1
equal =~ 1*justice_gen_2 + justice_tax_2 + justice_sub_2
suff =~ 1*justice_gen_3 + justice_tax_3 + justice_sub_3
lim =~ 1*justice_gen_4 + justice_tax_4 + justice_sub_4

#set latent variance to 1 for identification
util ~~ 1*util
equal ~~ 1*equal
suff ~~ 1*suff
lim ~~ 1*lim

#estimate covariances between all latent factors
util ~~ equal + suff + lim
equal ~~ suff + lim
suff ~~ lim

#estimate residual variances
justice_gen_1 ~~ justice_gen_1
justice_gen_2 ~~ justice_gen_2
justice_gen_3 ~~ justice_gen_3
justice_gen_4 ~~ justice_gen_4
justice_tax_1 ~~ justice_tax_1
justice_tax_2 ~~ justice_tax_2
justice_tax_3 ~~ justice_tax_3
justice_tax_4 ~~ justice_tax_4
justice_sub_1 ~~ justice_sub_1
justice_sub_2 ~~ justice_sub_2
justice_sub_3 ~~ justice_sub_3
justice_sub_4 ~~ justice_sub_4

# measured intercepts
justice_gen_1 ~ 1
justice_gen_2 ~ 1
justice_gen_3 ~ 1
justice_gen_4 ~ 1
justice_tax_1 ~ 1
justice_tax_2 ~ 1
justice_tax_3 ~ 1
justice_tax_4 ~ 1
justice_sub_1 ~ 1
justice_sub_2 ~ 1
justice_sub_3 ~ 1
justice_sub_4 ~ 1
'
cfa2 <- lavaan(model2, data = combined_waves, estimator = "MLR") #I still get the error warning
summary(cfa2, standardized = TRUE)

################################################################################
#add methods factors (for each context) --> several context factors because of (SOURCE )
# not hypothesised!!

model3 <- '
#latent regressions justice
util =~ justice_gen_1 + justice_tax_1 + justice_sub_1
equal =~ justice_gen_2 + justice_tax_2 + justice_sub_2
suff =~ justice_gen_3 + justice_tax_3 + justice_sub_3
lim =~ justice_gen_4 + justice_tax_4 + justice_sub_4

#latent regressions context
gen =~ justice_gen_1 + justice_gen_2 + justice_gen_3 + justice_gen_4
tax =~ justice_tax_1 + justice_tax_2 + justice_tax_3 + justice_tax_4
sub =~ justice_sub_1 + justice_sub_2 + justice_sub_3 + justice_sub_4

#set latent variances to 1 for identification
util ~~ 1*util
equal ~~ 1*equal
suff ~~ 1*suff
lim ~~ 1*lim
gen ~~ 1*gen
tax ~~ 1*tax
sub ~~ 1*sub

#latent means are restricted to zero as data is standardised

#estimate covariances between all latent factors
util ~~ equal + suff + lim
equal ~~ suff + lim
suff ~~ lim

#estimate residual variances
justice_gen_1 ~~ justice_gen_1
justice_gen_2 ~~ justice_gen_2
justice_gen_3 ~~ justice_gen_3
justice_gen_4 ~~ justice_gen_4
justice_tax_1 ~~ justice_tax_1
justice_tax_2 ~~ justice_tax_2
justice_tax_3 ~~ justice_tax_3
justice_tax_4 ~~ justice_tax_4
justice_sub_1 ~~ justice_sub_1
justice_sub_2 ~~ justice_sub_2
justice_sub_3 ~~ justice_sub_3
justice_sub_4 ~~ justice_sub_4

# measured intercepts
justice_gen_1 ~ 1
justice_gen_2 ~ 1
justice_gen_3 ~ 1
justice_gen_4 ~ 1
justice_tax_1 ~ 1
justice_tax_2 ~ 1
justice_tax_3 ~ 1
justice_tax_4 ~ 1
justice_sub_1 ~ 1
justice_sub_2 ~ 1
justice_sub_3 ~ 1
justice_sub_4 ~ 1
'
cfa3 <- lavaan(model3, data = combined_waves, estimator = "MLR")
summary(cfa3)
lavInspect(cfa3)
fitMeasures(cfa3)
print(fitMeasures(cfa3, c("chisq", "df", "pvalue"))) #significant chisqd: X^2(36) = 368.895, p < .001
print(fitMeasures(cfa3, c("rmsea",  "rmsea.ci.lower", "rmsea.ci.upper"))) # RMSEA = .052 (90% CI .048 - .057) indicating good to mediocre fit
print(fitMeasures(cfa3, "tli")) # TLI = .943 --> is bigger than .90, so there is acceptable fit
print(fitMeasures(cfa3, "cfi")) # CFI = .969 --> is bigger than .95, so indicates good fit
print(fitMeasures(cfa3, "srmr")) # SRMR = 0.041 --> is smaller than .05, indicating good fit
semPaths(cfa3, whatLabels = "stand", layout = "tree", intercepts = FALSE) #visualisation
modindices(cfa3)
?modindices
###################################################################################################
# H5 to H6: participants only distinguish between distribution insensitive and distribution sensitive
# first without the method factors: (it indicates bad fit)
model4 <- '
#latent regressions justice
dis_insen =~ justice_gen_1 + justice_tax_1 + justice_sub_1
dis_sen =~ justice_gen_2 + justice_tax_2 + justice_sub_2 + justice_gen_3 + justice_tax_3 + justice_sub_3 + justice_gen_4 + justice_tax_4 + justice_sub_4

#set latent variances to 1 for identification
dis_insen ~~ 1*dis_insen
dis_sen ~~ 1*dis_sen

#latent means are restricted to zero as data is standardised

#estimate covariances between all latent factors
dis_insen ~~ dis_sen

#estimate residual variances
justice_gen_1 ~~ justice_gen_1
justice_gen_2 ~~ justice_gen_2
justice_gen_3 ~~ justice_gen_3
justice_gen_4 ~~ justice_gen_4
justice_tax_1 ~~ justice_tax_1
justice_tax_2 ~~ justice_tax_2
justice_tax_3 ~~ justice_tax_3
justice_tax_4 ~~ justice_tax_4
justice_sub_1 ~~ justice_sub_1
justice_sub_2 ~~ justice_sub_2
justice_sub_3 ~~ justice_sub_3
justice_sub_4 ~~ justice_sub_4

# measured intercepts
justice_gen_1 ~ 1
justice_gen_2 ~ 1
justice_gen_3 ~ 1
justice_gen_4 ~ 1
justice_tax_1 ~ 1
justice_tax_2 ~ 1
justice_tax_3 ~ 1
justice_tax_4 ~ 1
justice_sub_1 ~ 1
justice_sub_2 ~ 1
justice_sub_3 ~ 1
justice_sub_4 ~ 1
'
cfa4 <- lavaan(model4, data = combined_waves, estimator = "MLR")
fitMeasures(cfa4)
print(fitMeasures(cfa4, c("chisq", "df", "pvalue"))) #significant
print(fitMeasures(cfa4, c("rmsea",  "rmsea.ci.lower", "rmsea.ci.upper"))) #bad fit
print(fitMeasures(cfa4, "tli")) #bad fit
print(fitMeasures(cfa4, "cfi")) #bad fit
print(fitMeasures(cfa4, "srmr")) #bad fit

# implementing with method factors
model5 <- '
#latent regressions justice
dis_insen =~ justice_gen_1 + justice_tax_1 + justice_sub_1
dis_sen =~ justice_gen_2 + justice_tax_2 + justice_sub_2 + justice_gen_3 + justice_tax_3 + justice_sub_3 + justice_gen_4 + justice_tax_4 + justice_sub_4

#latent regressions context
gen =~ justice_gen_1 + justice_gen_2 + justice_gen_3 + justice_gen_4
tax =~ justice_tax_1 + justice_tax_2 + justice_tax_3 + justice_tax_4
sub =~ justice_sub_1 + justice_sub_2 + justice_sub_3 + justice_sub_4

#set latent variances to 1 for identification
dis_insen ~~ 1*dis_insen
dis_sen ~~ 1*dis_sen
gen ~~ 1*gen
tax ~~ 1*tax
sub ~~ 1*sub

#latent means are restricted to zero as data is standardised

#estimate covariances between all latent factors
dis_insen ~~ dis_sen

#estimate residual variances
justice_gen_1 ~~ justice_gen_1
justice_gen_2 ~~ justice_gen_2
justice_gen_3 ~~ justice_gen_3
justice_gen_4 ~~ justice_gen_4
justice_tax_1 ~~ justice_tax_1
justice_tax_2 ~~ justice_tax_2
justice_tax_3 ~~ justice_tax_3
justice_tax_4 ~~ justice_tax_4
justice_sub_1 ~~ justice_sub_1
justice_sub_2 ~~ justice_sub_2
justice_sub_3 ~~ justice_sub_3
justice_sub_4 ~~ justice_sub_4

# measured intercepts
justice_gen_1 ~ 1
justice_gen_2 ~ 1
justice_gen_3 ~ 1
justice_gen_4 ~ 1
justice_tax_1 ~ 1
justice_tax_2 ~ 1
justice_tax_3 ~ 1
justice_tax_4 ~ 1
justice_sub_1 ~ 1
justice_sub_2 ~ 1
justice_sub_3 ~ 1
justice_sub_4 ~ 1
'
cfa5 <- lavaan(model5, data = combined_waves, estimator = "MLR")
fitMeasures(cfa5)
print(fitMeasures(cfa5, c("chisq", "df", "pvalue"))) #significant
print(fitMeasures(cfa5, c("rmsea",  "rmsea.ci.lower", "rmsea.ci.upper"))) # good fit to mediocre
print(fitMeasures(cfa5, "tli")) #bad fit becaus it's < .9
print(fitMeasures(cfa5, "cfi")) # CFI = .936 --> acceptable fit
print(fitMeasures(cfa5, "srmr")) # SRMR = .037

# as a next step, I test the two models (cfa3 and cfa5) against each other and see which model is better fitting the data
#the models are not nested, so I test them with AIC and BIC
AIC(cfa3)
BIC(cfa3)
AIC(cfa5)
BIC(cfa5) # model 3 shows better fit: AIC = 124203 vs. 124546 / BIC = 124533 vs. 124845
