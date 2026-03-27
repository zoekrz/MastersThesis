###############################################################################
## Quant data: CFA with FULL dataset ##
###############################################################################
#load necessary packages
library(lavaan)
library(semPlot)
library(ggcorrplot)
library(moments)

options(scipen = 999) #limit decimals


#2. preprocessing: no imputation needed (see Rogers, 2024; Kline, 2023)
colSums(is.na(combined_waves)) #practically no missing values in the justice questions
#why are there missing values in some of the justice variables?
View(combined_waves[rowSums(is.na(combined_waves[, justice_cols])) > 0, ])

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
which(vec_sd < 0.25) #two participants show a sd < 0.25

View(combined_waves[2376, justice_cols])
View(combined_waves[3096, justice_cols]) # both of them have a lot of missing values in the justice items


#check if data is normally distributed, as ordinal data with enough categories
# (≥ 5), big datasets and non-skewed distributions
# can be treated as continious according to Rhemtulla et al.,2012)
skewness(combined_waves[, justice_cols], na.rm = TRUE)
# skewness between -1 and 1 indicates symmetrical distributions (source!) #here all good

#check if some items show multicollinearity
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
  data = subset_combined,
  estimator = "MLR"
)
lavInspect(cfa1, "cov.lv") #some latent correlations are > 1, which is strange
summary(cfa1, standardized = TRUE)
semPaths(cfa1, whatLabels = "stand", layout = "tree") #visualise model

#visualise cor matrix with theorised latent factors next to each other
cor_matrix_factors <- cor(subset_combined[, c(
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
ggcorrplot(cor_matrix_factors, lab = TRUE, title = "Cor Matrix: Justice variables in subset_combined ordered by factors")

#visualise cor matrix with contexts next to each other
cor_matrix_context <- cor(subset_combined[, c("justice_gen_1",
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
ggcorrplot(cor_matrix_context, lab = TRUE, title = "Cor Matrix: Justice variables in subset_combined ordered by contexts")

#check fit measures 
fitMeasures(cfa1)
fitMeasures(cfa1, "rmsea") #rmsea of 0.147 indicating bad fit

modindices(cfa1)

#check for sources of the error that sample covariance is not positive-definite (it cannot be inverted)
sapply(subset_combined, var, na.rm = TRUE) #no zero variances

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
cfa2 <- lavaan(model2, data = subset_combined, estimator = "MLR")
summary(cfa2, standardized = TRUE)

################################################################################
#add methods factors

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
cfa3 <- lavaan(model3, data = subset_combined, estimator = "MLR")
lavInspect(cfa3, "cov.lv")
