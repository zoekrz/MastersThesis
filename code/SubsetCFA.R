###############################################################################
## Quant data: CFA with subset ##
###############################################################################
#load necessary packages
library(lavaan)
library(semPlot)
library(ggcorrplot)

options(scipen = 999)

# scale the data
#subset_com_scaled <- as.data.frame(scale(subset_combined[,c("justice_gen_1", "justice_gen_2", "justice_gen_3", "justice_gen_4", "justice_tax_1", "justice_tax_2", "justice_tax_3", "justice_tax_4", "justice_sub_1", "justice_sub_2", "justice_sub_3", "justice_sub_4")]))

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
'

# leave out latent context factors: #latent regressions context
#gen =~ justice_gen_1 + justice_gen_2 + justice_gen_3 + justice_gen_4
#tax =~ justice_tax_1 + justice_tax_2 + justice_tax_3 + justice_tax_4
#sub =~ justice_sub_1 + justice_sub_2 + justice_sub_3 + justice_sub_4

cfa1 <- lavaan(
  model1,
  data = subset_combined,
  ordered = c(
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
  ),
  estimator = "WLSMV",
  parameterization = "theta"
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
'
cfa2 <- lavaan(model2, data = subset_com_scaled)
summary(cfa2, standardized = TRUE)

#this doesn't help

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
'
cfa3 <- lavaan(model3, data = subset_com_scaled)
lavInspect(cfa3, "cov.lv")
