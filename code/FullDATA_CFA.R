###############################################################################
## Quant data: CFA with FULL dataset ##
###############################################################################
#load necessary packages
library(lavaan)
library(semPlot)
library(ggcorrplot)
library(moments)

options(scipen = 999) #limit decimals

#
str(combined_waves)

#check if data is normally distributed, as ordinal data with enough categories
# (≥ 5), big datasets and non-skewed distributions
# can be treated as continious according to Rhemtulla et al.,2012)
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
skewness(combined_waves[, justice_cols], na.rm = TRUE)

# skewness between -1 and 1 indicates symmetrical distributions (source)
