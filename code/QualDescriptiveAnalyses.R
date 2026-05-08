########################################################################
##### Qualitative Data: Descriptive Analyses ###############
########################################################################
library(psych)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

#load dataset
dat <- read.csv("QualitativeSampleCharacteristics_260409.csv")

#clean dataset
dat <- dat[-1, -c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 24, 25, 26, 27, 28, 29, 30, 31)]
dat$duration <- as.numeric(dat$Duration..in.seconds.)
dat$Duration..in.seconds.<- NULL
dat$datetime <- dmy_hm(dat$RecordedDate)
dat$RecordedDate <- NULL

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
  mutate(income = recode(income,
                         "Unter 34'000" = "Under 34'000",
                         "von CHF 34'001 – CHF 45'000" = "34'001 – 45'000",
                         "von CHF 45'001 – CHF 77'000" = "45'001 – 77'000",
                         "von CHF 77'001 – CHF 96'000" = "77'001 – 96'000",
                         "Über 96'000 CHF" = "Over 96'000",
                         "Möchte ich nicht sagen"= "Prefer not to say"))

ggplot(data.frame(x = dat$income), aes(x = x)) +
  geom_bar() +
  labs(title = "Income distribution", x = "Income category in CHF", y = "Frequency") +
  theme(axis.text = element_text(angle = 45, hjust = 1))

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
    party.identification = recode(
      party.identification,
      "Sozialdemokratische Partei der Schweiz (SP)" = "1",
      "Grünliberale Partei (GLP)" = "4",
      "Die Mitte (ehemals (CVP/BDP)" = "5",
      "Die Liberalen (FDP)" = "5",
      "Schweizerische Volkspartei (SVP)" = "6",
      "I don't identify with any party / non" = "other/none of them/prefer not to say",
      "I would prefer not to comment"   = "other/none of them/prefer not to say",
      "other" = "other/none of hem/prefer not to say"
    ))
unique(dat$party.identification)
