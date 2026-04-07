########################################################################
##### Qualitative Data: Descriptive Analyses ###############
########################################################################
library(psych)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

#load dataset
dat <- read.csv("QualitativeSampleCharacteristics_260327.csv")

#clean dataset
dat <- dat[-1, -c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 24, 25, 26, 27, 28, 29, 30, 31)]
dat$duration <- as.numeric(dat$Duration..in.seconds.)
dat$Duration..in.seconds.<- NULL
dat$datetime <- dmy_hm(dat$RecordedDate)
dat$RecordedDate <- NULL

range(dat$datetime) #participants that were included in the dataset filled out the form between the 16th and 18th of December
table(dat$age) #most participants were betwen 40 and 64 years old
table(dat$gender) #15 particpants were men, 6 women
table(dat$canton) #most came from the canton Zurich, second most from canton Berne, no particpants from Basel!
table(dat$income) #most earn between 45'001 and 77'000 per year

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

table(dat$education)
