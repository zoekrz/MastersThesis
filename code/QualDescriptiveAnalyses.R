########################################################################
##### Qualitative Data: Descriptive Analyses ###############
########################################################################
library(psych)
library(lubridate)

#load dataset
dat <- read.csv("QualitativeSampleCharacteristics_260327.csv")

#clean dataset
dat <- dat[-1, -c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 24, 25, 26, 27, 28, 29, 30, 31)]
dat$duration <- as.numeric(dat$Duration..in.seconds.)
dat$Duration..in.seconds.<- NULL
dat$datetime <- dmy_hm(dat$RecordedDate)
dat$RecordedDate <- NULL

range(dat$datetime) #participants that were included in the dataset filled out the form between the 16th and 18th of December
