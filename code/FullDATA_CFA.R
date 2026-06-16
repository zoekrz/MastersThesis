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
library(semTools)
library(DescTools)
library(car)
library(dplyr)
library(robustbase)
library(performance)
library(modeest)

options(scipen = 999) #limit decimals

#here one could load the publicly available dataset "combined_waves.csv"
#read.csv("combined_waves.csv")

#add necessary object from subset CFA: 
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

# sample characteristics
## age
table(combined_waves$age) # 869,1484,1004
prop.table(table(combined_waves$age))
table(combined_waves$gender) # 1598, 1744, 15 
prop.table(table(combined_waves$gender))
table(combined_waves$language_region)
prop.table(table(combined_waves$language_region))
table(combined_waves$canton) #didn't add this into my table
prop.table(table(combined_waves$canton))
table(combined_waves$income)
prop.table(table(combined_waves$income))
table(combined_waves$education)
prop.table(table(combined_waves$education))
table(combined_waves$residence)
prop.table(table(combined_waves$residence))
table(combined_waves$renting)
prop.table(table(combined_waves$renting))

combined_waves$income <- factor(
  combined_waves$income,
  levels = c(
    "Bottom 10%",
    "Below average",
    "Average",
    "Above average",
    "Top 10%",
    "Prefer not to say"
  )
)
ggplot(data.frame(x = combined_waves$income), aes(x = x, y = after_stat(count / sum(count)))) +
  geom_bar(fill = "grey") +
  labs(title = "Income distribution in quantitative dataset", x = "Income category", y = "Percentage (%)") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  theme_classic()

# population characteristics from BFS: chi squared tests
## from dataset cc-d-01.02.03.02 (Ständige Wohnbevölkerung nach Staatsangehörigkeitskategorie, Alter und Kanton, 1. Quartal 2026)
counts_age_popul <- c("18-39"= 2463, "40-64" = 3085, "65 or older" = 1740) # in 1000
sum_counts_age_popul <- sum(counts_age_popul) # 7288
prop_age_popul <- counts_age_popul/sum_counts_age_popul #percentages reported in thesis
quant_counts_age <- table(combined_waves$age)
chi_age <- chisq.test(quant_counts_age, p = prop_age_popul) #statistical significant
cramersv_age <- sqrt(chi_age$statistic / (sum(quant_counts_age) * (length(quant_counts_age) -1)))
print(cramersv_age) #low Cramér's V, means they are well aligning --> here low (small effect) (Khalilzadeh & Tasci, 2017)

counts_gender_popul <- c("male" = 3732, "female" = 3818) # in 1000
sum_counts_gender_popul <- sum(counts_gender_popul)
prop_gender_popul <- counts_gender_popul/sum_counts_gender_popul # percentages reported in thesis
filtered_combined_waves <- combined_waves[combined_waves$gender == "male" | combined_waves$gender == "female", ]
quant_counts_gender <- table(filtered_combined_waves$gender) 
chi_gender <- chisq.test(quant_counts_gender, p = prop_gender_popul) # not significant
cramersv_gender <- sqrt(chi_gender$statistic / (sum(quant_counts_gender) * (length(quant_counts_gender) -1))) #very low

## from publication Müller & Roth, 2022 (not language region but people that speak this language... used this as a proxy)
prop_language_reg_popul <- c("German-speaking region" = 62.3, "Italian-speaking region" = 8.0, "French-speaking region"= 22.8, "Romansh-speaking region" = 0.5)/100 #like this 6.4 missing values #problem here is it's not summing up to 1 as probability, therefore I add proportionally around 6% of each
prop_language_reg_popul <- c("German-speaking region" = 66.4, "Italian-speaking region" = 8.6, "French-speaking region"= 24.4, "Romansh-speaking region" = 0.6)/100
filtered_com_wave1 <- combined_waves %>% 
  filter(wave == "wave1")
quant_counts_language_reg <- table(filtered_com_wave1$language_region)
chi_language_reg <- chisq.test(quant_counts_language_reg, p = prop_language_reg_popul)
cramersv_language_reg <- sqrt(chi_language_reg$statistic /(sum(quant_counts_language_reg)*(length(quant_counts_language_reg)-1)))
print(cramersv_language_reg) #impossibly large, therefore, I cannot report this probably the language regions are not really representative, because I took both samples into the calculation

# income no available data. but the two samples show a very different distribution
filtered_com_wave3 <- combined_waves %>%
  filter(wave == "wave3")
prop.table(table(filtered_com_wave1$income))
prop.table(table(filtered_com_wave3$income))

## from Bildungsstand der Bevölkerung – Daten des Indikators
prop_education_popul <- c("No high school diploma"= 13.9, "High school or vocational training" = 39.9, "Degree from a university or university of applied sciences"= 46.2)/100
filtered_combined_waves2 <- combined_waves[combined_waves$education == "No high school diploma" | combined_waves$education == "High school or vocational training" | combined_waves$education == "Degree from a university or university of applied sciences",]
quant_counts_education <- table(filtered_combined_waves2$education)
chi_education <- chisq.test(quant_counts_education, p = prop_education_popul)
cramersv_education <- sqrt(chi_education$statistic / (sum(quant_counts_education) * (length(quant_counts_education)-1)))

## from https://www.aboutswitzerland.eda.admin.ch/de/bevoelkerung
prop_residence_popul <- c("city" = 74, "countryside" = 26)/100 #only distinguished into city and countryside, will take agglomeration together with countryside
filtered_combined_waves3 <- combined_waves %>% 
  filter(!is.na(residence)) %>%
  filter(residence != " other") %>%
  mutate(residence = dplyr::recode(
    residence,
    "city" = "city",
    "agglomeration" = "city",
    "countryside" = "countryside"
  ))
quant_counts_residence <- table(filtered_combined_waves3$residence)
chi_residence <- chisq.test(quant_counts_residence, p = prop_residence_popul)
cramersv_residence <- sqrt(chi_residence$statistic / (sum(quant_counts_residence)* (length(quant_counts_residence)-1)))

## from https://www.bfs.admin.ch/asset/de/36396463
prop_renting_popul <- c("tenant" = 61.1, "owner" = 35.7)/100 #same problem again; not adding up to 1 --> around 3 percent each added
prop_renting_popul <- c("tenant" = 63.1, "owner" = 36.9)/100
filtered_combined_waves4 <- combined_waves %>% 
  filter(wave == "wave1")
quant_counts_renting <- table(filtered_combined_waves4$renting)
chi_renting <- chisq.test(quant_counts_renting, p = prop_renting_popul)
cramersv_renting <- sqrt(chi_renting$statistic / (sum(quant_counts_renting)* (length(quant_counts_renting)-1)))


#2. preprocessing: no imputation needed (see Rogers, 2024; Kline, 2023)
colSums(is.na(combined_waves)) #practically no missing values in the justice questions
#why are there missing values in some of the justice variables?
View(combined_waves[rowSums(is.na(combined_waves[, justice_cols])) > 0, ]) #people dropped out

#remove those rows with missing values
combined_waves <- combined_waves[complete.cases(combined_waves[, justice_cols]),]
3371 - 3357 # 14 cases were deleted

#2. preprocessing: examine standard deviations that are < .25
vec_sd <- apply(
  combined_waves[, justice_cols], 1, sd, na.rm = TRUE
)
which(vec_sd < 0.25) #no participant shows a sd < 0.25, so no participant is 
#excluded due to a lack of variability (Collier, 2020, p.18)
range(vec_sd) 


#check if data is (univariate) normally distributed, as ordinal data with enough categories
# (≥ 5), big datasets and non-skewed distributions
# can be treated as continious according to Rhemtulla et al.,2012)
moments::skewness(combined_waves[, justice_cols], na.rm = TRUE)
describe(combined_waves[, justice_cols], na.rm = TRUE)
# skewness between -1 and 1 indicates symmetrical distributions --> Curran et al. (1996) even say, it is no problem if there is skewness <2 and kurtosis <7. Both is the case here.
# --> here all good, i can use MLR

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
range(cor_ma_factors_comwaves) #-.06 to .57 (here 1 but heatmap shows .57 to be the highest)
#there are no higher bivariate correlations than >.9 as suggested to be a problem (Kline, 2023, p. 56)
#still there can be multicollinearity: therefore I calculate the variation inflation factor
model_vif <- lm(justice_gen_1 ~ justice_tax_1 + justice_sub_1 + 
                  justice_gen_2 + justice_tax_2 + justice_sub_2 + 
                  justice_gen_3 + justice_tax_3 + justice_sub_3 + 
                  justice_gen_4 + justice_tax_4 + justice_sub_4, 
                data = combined_waves)
vif(model_vif)
range(vif(model_vif))

#check for multivariate outliers
mcd_justice <- covMcd(combined_waves[, justice_cols], alpha = 0.75) #minimum covariance determinant: robust version of estimating the center (Ghorbani, 2019) #here I use 0.75 as 0.5 is a less stable estimation
mahal_distr <- mahalanobis(combined_waves[, justice_cols], center = mcd_justice$center, cov = mcd_justice$cov) #measure distance 
combined_waves$mahal_dist <- NA
combined_waves$mahal_dist[as.numeric(rownames(combined_waves[, justice_cols]))] <- mahal_distr
n_vars <- length(justice_cols)
combined_waves$mahal_pvalue <- NA
combined_waves$mahal_pvalue[as.numeric(rownames(combined_waves[,justice_cols]))] <- 
  pchisq(mahal_distr, df = n_vars, lower.tail = FALSE)
combined_waves$is_outlier <- FALSE
combined_waves$is_outlier[as.numeric(rownames(combined_waves[, justice_cols]))] <- 
  combined_waves$mahal_pvalue[as.numeric(rownames(combined_waves[, justice_cols]))] < 0.001 # from  Whittaker & Schumacker, 2022; "Cases with squared MD values that exceed the χ2 critical with given df at an alpha of .001 have been suggested as multivariate outlying cases."
sum(combined_waves$is_outlier) # 351 outliers detected
outliers <- combined_waves[combined_waves$is_outlier == TRUE, ] 
outliers_performance <- check_outliers(combined_waves[,justice_cols], method = "mcd") #another command from the performance package comes to a similar result

#check why there may be so many outliers
summary(mahal_distr)
hist(mahal_distr, breaks = 50, main = "Mahalanobis Distances")
abline(v = qchisq(0.99, df = 12), col = "red", lwd = 2) #it looks like there are not so many "real" outliers
qchisq(0.99, df = 12) 
outlier_indices <- which(mahal_distr > qchisq(0.99, df = 12))
head(combined_waves[outlier_indices, justice_cols], 20) # most of them look normal to me, but some of the outliers are straight liners within one justice context (but not across all, as these were removed)
#I therefore make a subsample of combined_waves without these straight liners
combined_waves_cleaner <- combined_waves %>%
  rowwise() %>%
  filter(
    n_distinct(c_across(starts_with("justice_gen"))) > 1 &
      n_distinct(c_across(starts_with("justice_tax"))) > 1 &
      n_distinct(c_across(starts_with("justice_sub"))) > 1
  ) %>%
  ungroup()
nrow(combined_waves) - nrow(combined_waves_cleaner) #1075 people who straighlined at least on one context: more than one third, which is a lot.

#I will only remove people who straight lined on at least two of the contexts
combined_waves_clean <- combined_waves %>%
  rowwise() %>%
  filter(
    (n_distinct(c_across(starts_with("justice_gen"))) == 1) +
      (n_distinct(c_across(starts_with("justice_tax"))) == 1) +
      (n_distinct(c_across(starts_with("justice_sub"))) == 1) < 2
  ) %>%
  ungroup()
nrow(combined_waves) - nrow(combined_waves_clean) #this gives me a dataset reduced of 287 cases

#descriptive item statistics
#mean & sd
summary(combined_waves[,justice_cols])
describe(combined_waves[, justice_cols]) # mean, sd, median, skewness
modeest::mfv(combined_waves$justice_gen_1)  # mode of first general context item
modeest::mfv(combined_waves$justice_gen_2)
modeest::mfv(combined_waves$justice_gen_3)
modeest::mfv(combined_waves$justice_gen_4)
modeest::mfv(combined_waves$justice_tax_1)  
modeest::mfv(combined_waves$justice_tax_2)
modeest::mfv(combined_waves$justice_tax_3)
modeest::mfv(combined_waves$justice_tax_4)
modeest::mfv(combined_waves$justice_sub_1)  
modeest::mfv(combined_waves$justice_sub_2)
modeest::mfv(combined_waves$justice_sub_3)
modeest::mfv(combined_waves$justice_sub_4) # all modes are 4, is this realistic?


#check for intern consistency of items (cronbachs alpha) and item total correlation
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
c_dis_sen <- c("justice_gen_2",
               "justice_tax_2",
               "justice_sub_2",
               "justice_gen_3",
               "justice_tax_3",
               "justice_sub_3",
               "justice_gen_4",
               "justice_tax_4",
               "justice_sub_4")
alpha_util <- psych::alpha(combined_waves[,c_util])
alpha_equal <- psych::alpha(combined_waves[,c_equal])
alpha_suff <- psych::alpha(combined_waves[,c_suff])
alpha_limit <- psych::alpha(combined_waves[, c_limit])
alpha_dis_sen <- psych::alpha(combined_waves[, c_dis_sen])
print(alpha_util) # alpha = 0.37
print(alpha_equal) # alpha = 0.56
print(alpha_suff) # alpha = 0.53
print(alpha_limit) # alpha = 0.56
print(alpha_dis_sen) # alpha = 0.81uni
#all alphas are rather low; normal threshold is considered to be 0.7 (source)
#not so high enough internal consistency; I will use omega on the factor model to see, where it may lie (?)
# note: I get really similar results when running compRelSEM(cfa1) (Model that has the H1 to H4 structure) (omegas)


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
lavInspect(cfa1, "est")$theta #this shows residual variances of indicators: no Heywood cases
summary(cfa1, standardized = TRUE)
semPaths(cfa1, whatLabels = "stand", layout = "tree") #visualise model BUT! I cannot interpret it: there are no factor loading bigger than 1, which is good
ev <- eigen(lavInspect(cfa1, "cov.lv"))$values
ev #eigen values that are negative; is not possible, as eigenvalues denote variance explained by the factors; confirms the problem

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
ggcorrplot(cor_matrix_factors, lab = TRUE, title = "Correlations between justice variables ordered by factors")

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
ggcorrplot(cor_matrix_context, lab = TRUE, title = "Correlation between justice variables ordered by contexts")

#check fit measures 
fitMeasures(cfa1)
fitMeasures(cfa1, "rmsea") #rmsea of 0.147 indicating bad fit

modindices(cfa1) #checking modification indices; they indicate I should free the variances of equal and suff factors
#this I cannot do as then I have an uneridentifies model
#next they indicate I should add intercepts for the latent factors; as far as I know I can do so but then interpretation gets more difficult
# further, they say, that the utilitarian factor should load onto all equal outcomes. This does not make sense from a theoretical perspective, 
# but indicates that there is something wrong with the utilitarian factor; I consider removing it from the model.
# but first I try to adress the issue of implausibly high latent correlations

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
#add methods factors (for each context) --> several context factors because of the correlation matrix
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
summary(cfa3, standardized = TRUE)

fitMeasures(cfa3)
print(fitMeasures(cfa3, c("chisq", "df", "pvalue"))) #significant chisqd: X^2(36) = 368.895, p < .001
print(fitMeasures(cfa3, c("rmsea",  "rmsea.ci.lower", "rmsea.ci.upper"))) # RMSEA = .052 (90% CI .048 - .057) indicating good to mediocre fit
print(fitMeasures(cfa3, "tli")) # TLI = .943 --> is bigger than .90, so there is acceptable fit
print(fitMeasures(cfa3, "cfi")) # CFI = .969 --> is bigger than .95, so indicates good fit
print(fitMeasures(cfa3, "srmr")) # SRMR = 0.041 --> is smaller than .05, indicating good fit
semPaths(cfa3, whatLabels = "stand", layout = "tree", intercepts = FALSE) #visualisation
?semPaths

cfa3_estimations <- inspect(cfa3, "std.all")
cfa3_estimations$lambda # standardised factor loadings
(cfa3_estimations$lambda)^2*100 #percent of explained variance
1 - cfa3_estimations$theta # R^2 for all measured variables

## fit the the same model onto the cleaned dataset (without participants straight lining in at least two contexts)
cfa3_clean <- lavaan(model3, data = combined_waves_clean, estimator = "MLR")
summary(cfa3_clean, standardized = TRUE)
cfa3_clean_estimations <- inspect(cfa3_clean, "std.all")
(cfa3_clean_estimations$lambda)^2*100
print(fitMeasures(cfa3_clean, c("chisq", "df", "pvalue")))
print(fitMeasures(cfa3_clean, c("rmsea",  "rmsea.ci.lower", "rmsea.ci.upper")))
print(fitMeasures(cfa3_clean, "tli"))
print(fitMeasures(cfa3_clean, "cfi"))
print(fitMeasures(cfa3_clean, "srmr"))

######### with claude help ######### visualise the cfa3
p <- semPaths(cfa3, whatLabels = "stand", intercepts = FALSE, DoNotPlot = TRUE)

# Define node groups
justice_nodes <- which(p$graphAttributes$Nodes$names %in% c("utl", "eql", "sff", "lim"))
context_nodes <- which(p$graphAttributes$Nodes$names %in% c("gen", "tax", "sub"))
obs_nodes     <- setdiff(1:19, c(justice_nodes, context_nodes))

# Desired order of observed variables
desired_order <- c(
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
)
# Get the lavaan names (the "attr names") for the obs_nodes
obs_lavaan_names <- names(p$graphAttributes$Nodes$names)[obs_nodes]

# Reorder obs_nodes according to desired_order
obs_nodes <- obs_nodes[match(desired_order, obs_lavaan_names)]

# Assign y positions (3 levels)
p$layout[justice_nodes, 2] <-  0.8    # top
p$layout[obs_nodes, 2]     <-  0    # middle
p$layout[context_nodes, 2] <- -0.8    # bottom
p$layout[context_nodes[2], 2] <- -1  # tax even lower

# Assign x positions (spread evenly within each level)
p$layout[justice_nodes, 1] <- seq(-0.7,  0.7, length.out = length(justice_nodes))
p$layout[obs_nodes, 1]     <- seq(-1.25,    1.25,   length.out = length(obs_nodes))
p$layout[context_nodes, 1] <- seq(-1,  1, length.out = length(context_nodes))

p$graphAttributes$Nodes$loopRotation[context_nodes] <- pi

# Rename observed item labels
p$graphAttributes$Nodes$labels <- gsub("^jstc_g_", "gen_", p$graphAttributes$Nodes$labels)
p$graphAttributes$Nodes$labels <- gsub("^jstc_t_", "tax_", p$graphAttributes$Nodes$labels)
p$graphAttributes$Nodes$labels <- gsub("^jstc_s_", "sub_", p$graphAttributes$Nodes$labels)
p$graphAttributes$Nodes$label.cex <- 0.9

plot(p)


###########################################################


modindices(cfa3, sort. = TRUE)
?modindices

compRelSEM(cfa1)
compRelSEM(cfa3) # the McDonald's omegas are better here than on the cfa1 model.


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
print(fitMeasures(cfa5, c("rmsea",  "rmsea.ci.lower", "rmsea.ci.upper"))) # acceptable fit to mediocre
print(fitMeasures(cfa5, "tli")) #bad fit because it's < .9
print(fitMeasures(cfa5, "cfi")) # CFI = .936 --> acceptable fit
print(fitMeasures(cfa5, "srmr")) # SRMR = .037 -> good fit

# as a next step, I test the two models (cfa3 and cfa5) against each other and see which model is better fitting the data
#the models are not nested, so I test them with AIC and BIC
AIC(cfa3)
BIC(cfa3)
AIC(cfa5)
BIC(cfa5) # model 3 shows better fit: AIC = 124203 vs. 124546 / BIC = 124533 vs. 124845
AIC(cfa3) - AIC(cfa5)
BIC(cfa3) - BIC(cfa5)


##########################
#Exploratory analyses
model6 <- '
#latent regressions (justice)
lim =~ justice_sub_4 + justice_tax_4
suff =~ justice_sub_3 + justice_tax_3
equal =~ justice_sub_2 + justice_tax_2
util =~ justice_sub_1 + justice_tax_1

#latent regressions (contexts)
sub =~ justice_sub_1 + justice_sub_2 + justice_sub_3 + justice_sub_4
tax =~ justice_tax_1 + justice_tax_2 + justice_tax_3 + justice_tax_4 

#set latent variances to 1 for identification
util ~~ 1*util
equal ~~ 1*equal
suff ~~ 1*suff
lim ~~ 1*lim
sub ~~ 1*sub
tax ~~ 1*tax

#estimate covariances between all latent factors
util ~~ equal + suff + lim
equal ~~ suff + lim
suff ~~ lim

#estimate residual variances
justice_sub_1 ~~ justice_sub_1
justice_sub_2 ~~ justice_sub_2
justice_sub_3 ~~ justice_sub_3
justice_sub_4 ~~ justice_sub_4
justice_tax_1 ~~ justice_tax_1
justice_tax_2 ~~ justice_tax_2
justice_tax_3 ~~ justice_tax_3
justice_tax_4 ~~ justice_tax_4
'
cfa6 <- lavaan(model6, data = combined_waves, estimator = "MLR") 
summary(cfa6)
fitMeasures(cfa6)

