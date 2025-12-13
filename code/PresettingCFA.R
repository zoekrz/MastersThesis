###############################################################################
## Quant data: Pre-Setting CFA ##
###############################################################################
#load necessary packages
library(stringr)
library(dplyr)
library(tidyr)

#set working directory
setwd("~/Documents/Uni/25HS/1_25MastersThesis/QuantData")
#load data
wave1 <- read.csv("raw_conjoints_data.csv")
wave3 <- read.csv("ccs_conjoint_CH_240225_1004.csv")
#save first row of wave3 (with the questions) in a subset
wave3quest <- subset(wave3[1, ])
wave3 <- wave3[-1, ] #delete first row
wave3 <- wave3[-1, ] #delete second row
rownames(wave3) <- NULL #set row numbering to start with 1

###############################
#filtering/clean data of wave3

#filter out rows where the Distribution channel is "preview" or the survey has not been finished
wave3 <- wave3[wave3$DistributionChannel != "preview" &
                 wave3$Finished != FALSE, ]

#filter out rows where participants already met quota or screened out is true
wave3 <- wave3[!(wave3$Q_TerminateFlag %in% c("QuotaMet", "Screened")), ]

#filter out rows which were before official start date
cutoff <- as.POSIXct("2025-02-13 10:00:00")
wave3 <- wave3[wave3$StartDate >= cutoff, ]

#store years of education as numeric,  NAs are introduced by coercion
wave3$education_year <- as.numeric(wave3$education_year)

#note for me: I left out the renaming of the columns wave3$X6_conjoint_plan1 etc., as I don't need them

#filter out people who didn't answer the consent form right, rename the column to "consent"
wave3 <- wave3[wave3$consent_choice == "I have read and understood the study information.,I have had enough time to decide about my participation.,I participate in this study voluntarily.,I consent that my data be collected and used anonymously.,I understand that I can stop participating at any moment without any disadvantages.", ]
names(wave3)[names(wave3) == "consent_choice"] <- "consent"

# filter out people who didn't answer trap questions right, delete trap questions columns
wave3 <- wave3[wave3$trap_question1 == "Agree", ]
unique(wave3$trap_question1) #check, if it worked
wave3 <- wave3[wave3$trap_question2 == "Underground", ]
unique(wave3$trap_question2)
wave3 <- wave3[wave3$trap_question3 == "Carbon Capture and Storage", ]
unique(wave3$trap_question3)
wave3$trap_question1 <- NULL
wave3$trap_question2 <- NULL
wave3$trap_question3 <- NULL

#filter people which took longer than the 9.5th decile and shorter than the 0.5st decile (only needed in wave 1 as there were no attention ckecks)
wave1$Duration..in.seconds. <- as.numeric(wave1$Duration..in.seconds.)
lower_bound <- quantile(wave1$Duration..in.seconds., 0.05, na.rm = TRUE)
upper_bound <- quantile(wave1$Duration..in.seconds., 0.95, na.rm = TRUE)
wave1 <- subset(wave1,
                Duration..in.seconds. >= lower_bound &
                  Duration..in.seconds. <= upper_bound)
####before: 2230 ppl, after 2008 ppl: more than 200 less

#transform duration in seconds to duration in minutes (also in wave1)
wave1$duration_min <- wave1$Duration..in.seconds. / 60
wave1$Duration..in.seconds. <- NULL
wave3$Duration..in.seconds. <- as.numeric(wave3$Duration..in.seconds.)
wave3$duration_min <- wave3$Duration..in.seconds. / 60
wave3$Duration..in.seconds. <- NULL

#delete some of the wave 3 columns
wave3$RecipientLastName <- NULL #delete participant last name (already empty column)
wave3$RecipientFirstName <- NULL #delete participant first name (already empty column)
wave3$RecipientEmail <- NULL #delete participant mail (already empty column)
wave3$ExternalReference <- NULL #(already empty column)
wave3$IPAddress <- NULL #delete the IP adress as it is not in wave 1
wave3$LocationLatitude <- NULL #delete location as it is not in wave 1
wave3$LocationLongitude <- NULL #delete location as it is not in wave 1


##############################################
#rename, reorder, make similar wave1 and wave3
##gender
unique(wave1$gender)
unique(wave3$gender)
wave1 <- wave1 %>% 
  mutate(gender = recode(gender,
                         "Männlich"="Male", 
                         "Weiblich" = "Female",  
                         "Nicht-binär" = "Non-binary / third gender"
  ))

##age
unique(wave1$age)
unique(wave3$age)
wave1 <- wave1 %>% 
  mutate(age = recode(age, 
                      "18-39 Jahre" = "18-39",
                      "40-64 Jahre" = "40-64",
                      "65-79 Jahre" = "65 or older",     
                      "80 Jahre oder älter" = "65 or older"))
wave3 <- wave3 %>%
  mutate(age = recode(age, 
                      "18-24"  = "18-39",
                      "25-34"  = "18-39",
                      "35-44"  = "40-64",
                      "45-54"  = "40-64",   
                      "55-64"  = "40-64"
  ))

##language region; same column name & same English values
names(wave1)[names(wave1) == "region"] <- "language_region" #rename
unique(wave3$language_region) #check values wave3
unique(wave1$language_region) #check values wave1
wave1 <- wave1 %>%
  mutate(
    language_region = recode(
      language_region,
      "Deutschsprachige Schweiz" = "German-speaking region",
      "Italienischsprachige Schweiz" = "Italian-speaking region",
      "Französischsprachige Schweiz" = "French-speaking region",
      "Rätoromanische Schweiz" = "Romansh-speaking region"
    )
  )
unique(wave1$language_region) #manipulation check
##canton
names(wave3)[names(wave3) == "kanton"] <- "canton"
unique(wave3$canton)
unique(wave1$canton)
wave1 <- wave1 %>%
  mutate(
    canton = na_if(canton, ""),
    canton = recode(
      canton,
      "Basel-Landschaft" = "Basel-Landschaft (BL)",
      "Glarus"   = "Glarus (GL)",
      "Schaffhausen"  = "Schaffhausen (SH)",
      "Ticino" = "Ticino (TI)",
      "Aargau" = "Aargau (AG)",
      "Valais" = "Valais/Wallis (VS)",
      "Geneva" = "Genève (GE)",
      "Fribourg" = "Fribourg (FR)",
      "Zurich" = "Zürich (ZH)",
      "Jura" = "Jura (JU)",
      "Basel-Stadt" =  "Basel-Stadt (BS)",
      "Thurgau" = "Thurgau (TG)",
      "Vaud" = "Vaud (VD)",
      "St. Gallen" = "St. Gallen (SG)",
      "Bern" =  "Bern/Berne (BE)",
      "Zug" = "Zug (ZG)",
      "Neuchâtel" = "Neuchâtel (NE)",
      "Solothurn" = "Solothurn (SO)",
      "Schwyz" = "Schwyz (SZ)",
      "Lucerne" = "Luzern (LU)",
      "Graubünden" = "Grisons (GR)",
      "Appenzell Ausserrhoden" = "Appenzell Ausserrhoden (AR)",
      "Uri" = "Uri (UR)" ,
      "Nidwalden" = "Nidwalden (NW)",
      "Appenzell Innerrhoden" =  "Appenzell Innerrhoden (AI)",
      "Obwalden" = "Obwalden (OW)"
    )
  )
##swiss citizen
names(wave1)[names(wave1) == "citizen"] <- "swiss_citizen"
wave1 <- wave1 %>%
  mutate(swiss_citizen = na_if(swiss_citizen, ""),
         swiss_citizen = recode(swiss_citizen, "Ja" = "Yes", "Nein" = "No"))
unique(wave1$swiss_citizen)
wave3 <- wave3 %>%
  mutate(swiss_citizen = na_if(swiss_citizen, ""))
unique(wave3$swiss_citizen)

##education: merging into 3 categories
names(wave3)[names(wave3) == "education_degree"] <- "education"
unique(wave1$education)
unique(wave3$education)
wave1 <- wave1 %>%
  mutate(
    education = recode(
      education,
      "Abschluss einer Fachhochschule oder Universität" = "Degree from a university or university of applied sciences",
      "Matura oder Berufsausbildung"  = "High school or vocational training",
      "Keine Matura" = "No high school diploma"
    )
  )
wave3 <- wave3 %>%
  mutate(education = na_if(education, ""),
         education = recode(
           education,
           "Doctoral or professional degree (e.g., PhD, MD, JD)" = "Degree from a university or university of applied sciences",
           "Master" = "Degree from a university or university of applied sciences",
           "Bachelor" = "Degree from a university or university of applied sciences",
           "Other (please specify)" = "other/prefer not to say",
           "Prefer not to say" = "other/prefer not to say"
         )
  )
unique(wave3$education)
unique(wave1$education)
## urbanness merging into 3 categories
names(wave3)[names(wave3) == "living_area"] <- "urbanness"
wave1 <- wave1 %>%
  mutate(urbanness = recode(urbanness, "Stadt" = "City", "Land" = "Countryside"))
unique(wave1$urbanness)
wave3 <- wave3 %>%
  mutate(urbanness = na_if(urbanness, ""),
         urbanness = recode(
           urbanness,
           "Big city" = "City",
           "Medium-sized or small town" = "City",
           "Outer neighborhood or suburb of a large city" = "Agglomeration",
           "Village" = "Countryside",
           "Farm" = "Countryside",
           "Other" = " other"
         )
  )
unique(wave3$urbanness)

##income
wave3quest$income #wave3: annual income
unique(wave3$income) #annual income per person
unique(wave1$income) #per household?
wave1 <- wave1 %>%
  mutate(
    income = recode(
      income,
      "Über 250,000" = "Top 10%",
      "CHF 150,001 – CHF 250,000" = "Above average",
      "CHF 100,001 – CHF 150,000" = "Average",
      "CHF 70,000 – CHF 100,000"  = "Below average",
      "Unter CHF 70,000" = "Bottom 10%",
      "Möchte ich nicht sagen" = "Prefer not to say"
    )
  )
wave3 <- wave3 %>%
  mutate(income = na_if(income, ""),
         income = recode(
           income,
           "Above CHF 96,000" = "Top 10%",
           "CHF 77,001 – CHF 96,000" = "Above average",
           "CHF 59,001 – CHF 77,000" = "Average",
           "CHF 45,001 – CHF 59,000" = "Average",
           "CHF 34,001 – CHF 46,000" = "Below average",
           "Under CHF 34,000"  = "Bottom 10%"
         )
  )
unique(wave3$income) #manipulation check
unique(wave1$income)

##political position / party affiliation
names(wave3)[names(wave3) == "political_position_1"] <- "political_position"
names(wave1)[names(wave1) == "party"] <- "political_position"
unique(wave1$political_position) #check categories
unique(wave3$political_position) #very high number of missing values
sum(wave3$political_position == "")
wave3 <- wave3 %>% mutate(political_position = na_if(political_position, ""))

wave3quest$political_position_1 #left to right scale What is right and what is left ( probably 1 left and 6 right, yes?)
wave1 <- wave1 %>%
  mutate(
    political_position = na_if(political_position, ""),
    political_position = recode(
      political_position,
      "Grüne Partei der Schweiz (GPS)" = "1",
      "Sozialdemokratische Partei der Schweiz (SP)" = "1",
      "Grünliberale Partei (GLP)" = "4",
      "Die Mitte (ehemals CVP/BDP)" = "5",
      "Die Liberalen (FDP)" = "5",
      "Schweizerische Volkspartei (SVP)" = "6",
      "Andere" = "other/none of them/prefer not to say",
      "Keine" = "other/none of them/prefer not to say",
      "Möchte ich nicht sagen" = "other/none of them/prefer not to say"
    )
  )

#rename justice variables & make them comparables
names(wave1)[names(wave1) == "justice_general_1"] <- "justice_gen_1"
names(wave1)[names(wave1) == "justice_general_2"] <- "justice_gen_2"
names(wave1)[names(wave1) == "justice_general_3"] <- "justice_gen_3"
names(wave1)[names(wave1) == "justice_general_4"] <- "justice_gen_4"
names(wave1)[names(wave1) == "justice_subsidy_1"] <- "justice_sub_1"
names(wave1)[names(wave1) == "justice_subsidy_2"] <- "justice_sub_2"
names(wave1)[names(wave1) == "justice_subsidy_3"] <- "justice_sub_3"
names(wave1)[names(wave1) == "justice_subsidy_4"] <- "justice_sub_4"

unique(wave1$justice_gen_1)
unique(wave3$justice_gen_1)

justice_approval <- c(
  "Stimme voll und ganz zu" = "Strongly agree",
  "Stimme zu" = "Agree",              
  "Stimme eher zu" = "Somewhat agree",        
  "Stimme eher nicht zu" = "Somewhat disagree",
  "Stimme nicht zu" = "Disagree",        
  "Stimme überhaupt nicht zu" = "Strongly disagree"
)


wave1 <- wave1 %>%
  mutate(
    justice_gen_1 = recode(justice_gen_1, !!!justice_approval),
    justice_gen_2 = recode(justice_gen_2, !!!justice_approval),
    justice_gen_3 = recode(justice_gen_3, !!!justice_approval),
    justice_gen_4 = recode(justice_gen_4, !!!justice_approval),
    justice_tax_1 = recode(justice_tax_1, !!!justice_approval),
    justice_tax_2 = recode(justice_tax_2, !!!justice_approval),
    justice_tax_3 = recode(justice_tax_3, !!!justice_approval),
    justice_tax_4 = recode(justice_tax_4, !!!justice_approval),
    justice_sub_1 = recode(justice_sub_1, !!!justice_approval),
    justice_sub_2 = recode(justice_sub_2, !!!justice_approval),
    justice_sub_3 = recode(justice_sub_3, !!!justice_approval),
    justice_sub_4 = recode(justice_sub_3, !!!justice_approval)
  )

wave3 <- wave3 %>%
  mutate(justice_gen_1 = na_if(justice_gen_1, ""),
         justice_gen_2 = na_if(justice_gen_2, ""),
         justice_gen_3 = na_if(justice_gen_3, ""),
         justice_gen_4 = na_if(justice_gen_4, ""),
         justice_tax_1 = na_if(justice_tax_1, ""),
         justice_tax_2 = na_if(justice_tax_2, ""),
         justice_tax_3 = na_if(justice_tax_3, ""),
         justice_tax_4 = na_if(justice_tax_4, ""),
         justice_sub_1 = na_if(justice_sub_1, ""),
         justice_sub_2 = na_if(justice_sub_2, ""),
         justice_sub_3 = na_if(justice_sub_3, ""),
         justice_sub_4 = na_if(justice_sub_3, "")
  )
# merge open justice questions in wave3, add empty open justice question to wave 1
wave3 <- wave3 %>%
  unite(
    justice_open_text,
    justice_open1_gen,
    justice_open2_tax,
    justice_open3_sub,
    remove = TRUE
  )
wave3 <- wave3 %>%
  mutate(
    justice_open_set = case_when(
      show_open_Q1 == "1" ~ 1,
      show_open_Q2 == "1" ~ 2,
      show_open_Q3 == "1" ~ 3,
      TRUE ~ NA_real_
    )
  )
wave1 <- wave1 %>%
  mutate(justice_open_text = NA)
wave1 <- wave1 %>%
  mutate(justice_open_set = NA)

#add empty renting column to wave3
wave3 <- wave3 %>%
  mutate(renting = NA)

# add column which identifies people from wave1 / wave3
wave1 <- wave1 %>%
  mutate(wave = "wave1")
wave3 <- wave3 %>%
  mutate(wave = "wave3")

##########################################################
#delete columns that are not in the wave3 data set
wave1 <- wave1 %>% select(-satisfaction_1, -household.size, -trust_1, -trust_2, -trust_3) #delete satisfaction, household size and trust variables, as they are not in wave3
wave1 <- wave1 %>%
  select(-contains("literacy")) #delete literacy variables
wave1 <- wave1 %>%
  select(-matches("^X[0-9]+")) #delete different choice and rating variables
wave1 <- wave1 %>%
  select(-matches("^choice[0-9]+")) #delete choice variables

#delete columns that are not in the wave1 dataset
wave3 <- wave3 %>%
  select(-starts_with("ccs_")) #delete if they have heard etc. from climate crisis
wave3 <- wave3 %>%
  select(-matches("^X[0-9]+")) #delete different conjoint variables
wave3 <- wave3 %>%
  select(-matches("_interest$")) #delete interest variables
wave3 <- wave3 %>%
  select(-matches("^c[0-9]+"))
wave3 <- wave3 %>%
  select(-matches("lreco_"))
wave3 <- wave3 %>%
  select(-matches("show_"))
wave3 <- wave3 %>%
  select(-matches("galtan_")) #delte galtan variables
wave3 <- wave3 %>%
  select(-matches("climate_")) 
wave3 <- wave3 %>%
  select( -net_zero_question,-living_area_8_TEXT,-education_degree_6_TEXT,-education_year, -contains("socio_ecological"),-end_feedback, -Q_TerminateFlag,-Q_R_Del,-screened_out
  )

#delete some unecessary columns
wave1 <- wave1 %>%
  select(-free.form, -id, -Status, -m, -Progress, -Finished, -DistributionChannel, -UserLanguage, -SelectedLanguage)
wave3 <- wave3 %>% 
  select(-Status, -m, -Progress, -Finished, -DistributionChannel, - UserLanguage, -SelectedLanguage)

#####################################
#rename and reorder columns so that they match
names(wave3)
names(wave1)

names(wave3)[names(wave3) == "StartDate"] <- "start_date"
names(wave3)[names(wave3) == "EndDate"] <- "end_date"
names(wave1)[names(wave1) == "StartDate"] <- "start_date"
names(wave1)[names(wave1) == "EndDate"] <- "end_date"
names(wave3)[names(wave3) == "RecordedDate"] <- "recorded_date"
names(wave1)[names(wave1) == "RecordedDate"] <- "recorded_date"

##
wave3 <- wave3 %>%
  relocate(
    end_date,
    recorded_date,
    ResponseId,
    wave,
    duration_min,
    language,
    language_region,
    consent,
    gender,
    age,
    canton,
    swiss_citizen,
    education,
    urbanness,
    renting,
    income,
    political_position,
    justice_gen_1,
    justice_gen_2,
    justice_gen_3,
    justice_gen_4,
    justice_tax_1,
    justice_tax_2,
    justice_tax_3,
    justice_tax_4,
    justice_sub_1,
    justice_sub_2,
    justice_sub_3,
    justice_sub_4,
    justice_open_text,
    justice_open_set,
    .after = start_date
  )

wave1 <- wave1 %>%
  relocate(
    end_date,
    recorded_date,
    ResponseId,
    wave,
    duration_min,
    language,
    language_region,
    consent,
    gender,
    age,
    canton,
    swiss_citizen,
    education,
    urbanness,
    renting,
    income,
    political_position,
    justice_gen_1,
    justice_gen_2,
    justice_gen_3,
    justice_gen_4,
    justice_tax_1,
    justice_tax_2,
    justice_tax_3,
    justice_tax_4,
    justice_sub_1,
    justice_sub_2,
    justice_sub_3,
    justice_sub_4,
    justice_open_text,
    justice_open_set,
    .after = start_date
  )

names(wave1)
names(wave3) #they match now

combined_waves <- rbind(wave1, wave3)
combined_waves$ResponseId <- NULL

combined_waves <- combined_waves %>%
  mutate(id = row_number())
combined_waves <- combined_waves %>%
  relocate(id, .after = duration_min)

colSums(is.na(combined_waves)) # (3498 observations) NA's in political_position are very high (304) (all from wave3), in the other columns (like renting, justice_open_text and justice_open_set, the numbers are explained by the fact that columns were imputed in wave1)

#set  seed
#sample(1:1000, 1) #first time I ran it:85
set.seed(85)
subset_combined <- combined_waves[sample(nrow(combined_waves), 300), ]
