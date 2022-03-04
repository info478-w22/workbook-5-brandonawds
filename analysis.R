# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)

demo <- sasxport.get('DEMO_I.XPT')
alco <- sasxport.get('ALQ_I.XPT')

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)

# Wt_sum represents the total US population

## Analysis
# in ALQ151, we want to 2 to be 0, and we want to ignore or disregard 7 and 9 
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# Create a survey design
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

nhanes_mean <- svymean(nhanes_survey, mean)

mean_by_gender <- svyby(~, ~riagender, dclus1, svymean)

drinkers_percentage <- svymean(~alq151, nhanes_survey, na.rm = TRUE)

drinkers_by_gender <- svyby(~alq151, ~riagender, nhanes_survey, svymean)
