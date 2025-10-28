###############################################################################
## Quant data: Pre-Setting CFA ##
###############################################################################
#load data
setwd("~/Documents/Uni/25HS/1_25MastersThesis/QuantData")
wave1 <- read.csv("raw_conjoints_data.csv")
head(wave1)
wave3 <- read.csv("ccs_conjoint_CH_240225_1004.csv")
head(wave3)
wave3 <- wave3[-1,] #delete first row
wave3 <- wave3[-1,] #delete second row
rownames(wave3) <- NULL #set row numbering to start with 1


wave3$RecipientLastName <- NULL #delete participant last name (already empty column)
wave3$RecipientFirstName <- NULL #delete participant first name (already empty column)
wave3$RecipientEmail <- NULL #delete participant mail (already empty column)
wave3$ExternalReference <- NULL #(already empty column)

wave3$IPAddress <- NULL #delete the IP adress as it is not in wave 1
wave3$LocationLatitude <- NULL #delete location as it is not in wave 1
wave3$LocationLongitude <- NULL #delete location as it is not in wave 1
