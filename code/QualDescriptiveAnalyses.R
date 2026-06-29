########################################################################
##### Qualitative Data: Descriptive Analyses ###############
########################################################################
library(psych)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

#load dataset
setwd("~/Documents/Uni/26FS/1_25MastersThesis/QualitativeInterview/ScreenerlisteKontakte")
dat <- read.csv("QualitativeSampleCharacteristics_260409.csv")

#clean dataset
dat <- dat[-1, -c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 24, 25, 26, 27, 28, 29, 30, 31)]
dat$duration <- as.numeric(dat$Duration..in.seconds.)
dat$Duration..in.seconds.<- NULL
dat$datetime <- dmy_hm(dat$RecordedDate)
dat$RecordedDate <- NULL

dat <- dat %>% #rename gender
  mutate(gender = dplyr::recode(gender,
                         "weiblich" = "Female",
                         "männlich"= "Male"))

dat <- dat %>% #rename age
  mutate(age = dplyr::recode(age, 
                      "zwischen 18 und 39 Jahren" = "18-39",
                      "zwischen 40 und 64 Jahren" = "40-64",
                      "65 oder älter" = "65 or older"))

#analyse the sample characteristics
range(dat$datetime) #participants that were included in the dataset filled out the form between the 16th and 18th of December
table(dat$age) #most participants were betwen 40 and 64 years old
prop.table(table(dat$age))
table(dat$gender) #15 particpants were men, 6 women
prop.table(table(dat$gender))
table(dat$canton) #most came from the canton Zurich, second most from canton Berne, no particpants from Basel!
prop.table(table(dat$canton))
table(dat$income) #most earn between 45'001 and 77'000 per year
prop.table(table(dat$income))

mean(dat$duration)
sd(dat$duration)

dat$income <- factor( #make income ordered
  dat$income,
  level = c(
    "Unter 34'000",
    "von CHF 34'001 – CHF 45'000",
    "von CHF 45'001 – CHF 77'000",
    "von CHF 77'001 – CHF 96'000",
    "Über 96'000 CHF",
    "Möchte ich nicht sagen"
  )
)

dat <- dat %>%
  mutate(income = dplyr::recode(income,
                         "Unter 34'000" = "Bottom 10%" ,
                         "von CHF 34'001 – CHF 45'000" = "Below average",
                         "von CHF 45'001 – CHF 77'000" = "Average",
                         "von CHF 77'001 – CHF 96'000" = "Above average",
                         "Über 96'000 CHF" = "Top 10%",
                         "Möchte ich nicht sagen"= "Prefer not to say"))

ggplot(data.frame(x = dat$income), aes(x = x, y = after_stat(count / sum(count)))) +
  geom_bar(fill = "grey") +
  labs(title = "Income distribution in qualitative dataset", x = "Income category", y = "Proportion") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  theme_classic()
  

table(dat$education) #2 have no matura, 14 have a Berufsausbildung or matrua, 5 have a university degree
prop.table(table(dat$education))
table(dat$urbanness) #10 are from the city, 7 from countryside, 4 from agglomeration
prop.table(table(dat$urbanness))
table(dat$renting) #18 are tenants, 2 are owners, 1 is in an unclear situation
prop.table(table(dat$renting))

mean(dat$confidence.in.politicians)
sd(dat$confidence.in.politicians)
barplot(
  table(dat$confidence.in.politicians),
  ylab = "Frequency",
  xlab = "10-point Likert scale",
  main = "Confidence in politicians", 
  xlim = c(0,15),
  ylim = c(0,10)
)

mean(dat$confidence.in.parliament)
sd(dat$confidence.in.parliament)
barplot(
  table(dat$confidence.in.parliament),
  ylab = "Frequency",
  xlab = "10-point Likert scale",
  main = "Confidnce in parliament",
  xlim = c(0, 15),
  ylim = c(0,10)
)

mean(dat$confidence.in.political.parties)
sd(dat$confidence.in.political.parties)
barplot(
  table(dat$confidence.in.political.parties),
  ylab = "Frequency",
  xlab = "10-point Likert scale",
  main = "Confidnce in political parties",
  xlim = c(0, 15),
  ylim = c(0,10)
)

barplot(
  table(dat$satisfaction.with.national.government),
  ylab = "Frequency",
  xlab = "10-point Likert scale",
  main = "Satisfaction with national government",
  xlim = c(0, 15),
  ylim = c(0,10)
)

table(dat$party.identification) #7 participants identified with SP, 5 didn't identify with any party, with SVP, FDP and Mitte each two, one person each for GLP, Would prefer not to comment and other

#make the same as in quant dataset
#Grüne & SP into 1 # Grünliberale Partei = 4 # Mitte & FDP = 5 # SVP = 6

dat <- dat %>%
  mutate(
    party.identification = dplyr::recode(
      party.identification,
      "Sozialdemokratische Partei der Schweiz (SP)" = "1",
      "Grünliberale Partei (GLP)" = "4",
      "Die Mitte (ehemals (CVP/BDP)" = "5",
      "Die Liberalen (FDP)" = "5",
      "Schweizerische Volkspartei (SVP)" = "6",
      "I don't identify with any party / non" = "other/none of them/prefer not to say",
      "I would prefer not to comment"   = "other/none of them/prefer not to say",
      "other" = "other/none of them/prefer not to say"
    ))
unique(dat$party.identification)

dat <- dat %>%
  mutate(education = dplyr::recode(
    education, 
    "Keine Matura" = "No high school diploma",
    "Matura oder Berufsausbildung" = "High school or vocational training" ,
    "Universitäts- oder Hochschulabschluss" = "Degree from a university or university of applied sciences"
  ))

#####################
# examine if the qual dataset could have come from the quant dataset
#1) generate a german subsample of the combined_waves
com_waves_german <- subset(
  combined_waves,
  language_region == "German-speaking region" &
    (gender == "male" |
       gender == "female") &
    (residence == "agglomeration" |
        residence == "city" | residence == "countryside") &
    (education == "Degree from a university or university of applied sciences" |
       education == "High school or vocational training" |
       education == "No high school diploma")
)
# gender
quant_prop_gender <- prop.table(table(com_waves_german$gender))
qual_counts_gender <- table(dat$gender)
expected_counts_gender <- quant_prop_gender * nrow(dat) #no expected counts under 5
chi_qual_income <- chisq.test(qual_counts_gender, p = quant_prop_gender) #no significant difference in the two distributions
cramersv_qual_gender <- sqrt(chi_qual_income$statistic / (sum(qual_counts_gender)* (length(qual_counts_gender)-1)))
# age
quant_prop_age <- prop.table(table(com_waves_german$age))
qual_counts_age <- table(dat$age)
quant_prop_age * 21 # no expected counts under 5
chi_qual_age <- chisq.test(qual_counts_age, p = quant_prop_age) #no significant difference in the two distributions
cramersv_qual_age <- sqrt(chi_qual_age$statistic / (sum(qual_counts_age)*(length(qual_counts_age)-1)))
# income
quant_prop_income <- prop.table(table(com_waves_german$income))
qual_counts_income <- table(dat$income)
quant_prop_income * 21 #several expected counts under 5 --> use monte carlo simulation
chi_qual_income <- chisq.test(qual_counts_income, p = quant_prop_income, simulate.p.value = TRUE) #no significant difference in the two distribution
cramersv_qual_income <- sqrt(chi_qual_income$statistic / (sum(qual_counts_income)*(length(qual_counts_income)-1)))

# renting
dat_cleanrenting <- subset(dat, renting == "tenant" | renting == "owner") #make cleaned version of dat without unclear renting situation
quant_prop_renting <- prop.table(table(com_waves_german$renting))
qual_counts_renting <- table(dat_cleanrenting$renting)
quant_prop_renting * 20 #no expected counts under 5
chi_qual_renting <- chisq.test(qual_counts_renting, p = quant_prop_renting) #there are significantly less homeowners in the qual sample than expected in regard to the quant sample
cramersv_qual_renting <- sqrt(chi_qual_renting$statistic / (sum(qual_counts_renting)*(length(qual_counts_renting)-1)))

# residence
quant_prop_residence <- prop.table(table(com_waves_german$residence))
qual_counts_residence <- table(dat$urbanness)
quant_prop_residence * 21 #one expected count under 5, so I use monte carlo simulation
chi_qual_residence <- chisq.test(qual_counts_residence, p = quant_prop_residence, simulate.p.value = TRUE) #no significant differences between distributions in residence
cramersv_qual_residence <- sqrt(chi_qual_residence$statistic / (sum(qual_counts_residence)*(length(qual_counts_residence)-1)))

#education
quant_prop_educ <- prop.table(table(com_waves_german$education))
qual_counts_educ <- table(dat$education)
quant_prop_educ * 21 #one expected count of <5
chi_qual_educ <- chisq.test(qual_counts_educ, p = quant_prop_educ, simulate.p.value = TRUE) #no significant differences between distributions in education
cramersv_qual_educ <- sqrt(chi_qual_educ$statistic / (sum(qual_counts_educ)*(length(qual_counts_educ)-1)))

#party identification 
com_waves_germ_pol <- subset(com_waves_german, wave == "wave1") # make new dataset of combined waves only with wave 1 data, as then political position collected equally
quant_prop_party <- prop.table(table(com_waves_germ_pol$political_position))
qual_counts_party <- table(dat$party.identification)
quant_prop_party *21 #several counts <5
chi_qual_party <- chisq.test(qual_counts_party, p = quant_prop_party, simulate.p.value = TRUE) 
cramersv_qual_party <- sqrt(chi_qual_party$statistic / (sum(qual_counts_party)*(length(qual_counts_party)-1)))

