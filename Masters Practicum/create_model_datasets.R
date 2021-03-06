setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('custom_functions.R')
print('Hello')
print('Lets Import the tidyverse and create a ClaimsSmall.RDS')

# custom fucntion that downloads and install library if you don't have them, loads it if you do
import_lib(tidyverse)

df <- read_rds("Data/claimsCleanFull.RDS")

# remove OTPT claims associated with ED visits, and duplicate claim numbers. Creates claimsCleanSmall.RDS
df <- clean_claims(df)

# read in the new claimsCleanSmall code testing shortcut
df <- read_rds('Data/claimsCleanSmall.RDS')

# Create chronic condition dataset
print('Assigning chronic conditions to members')
get_chronic_conditions(df)

# add lead service type to claims. Creates claim_lead_age_group.csv
print('Looking into the future to see if a member goes to the ED in the next 3 visits')
get_upcoming_visits(df)

# turn diagnosis code into css_category.
print('Swapping CCS category for diagnosis code')
impute_ccs(df)

# bin member ages
print('Bin member age into groups')
df <- group_ages(df)

# load lead service dataset and body system
df <- df %>% 
  select(CLAIM_NUM,MRN_ALIAS, EPISODE_SEQ, MEMBER_SEX, SERVICE_TYPE, APPROVED_AMT, APPROVED_DAYS)

print('Loading data to be joined')
suppressWarnings(lead<- read_csv("Data/claims_ed_next_3.csv", 
                col_types = cols(AGE_GROUP = col_character(), 
                                 CLAIM_NUM = col_character(), LEAD_EPISODE = col_integer(), 
                                 LEAD_YEAR = col_integer(), MRN_ALIAS = col_character(), 
                                 ED_NEXT_3 = col_logical())))

ccs <- read_csv("Data/claim_ccs_category.csv", 
                col_types = cols(CLAIM_NUM = col_character(), 
                                 CODE_1 = col_character(), CODE_2 = col_character(), 
                                 CODE_3 = col_character(), CODE_4 = col_character(), 
                                 CODE_5 = col_character(), CODE_6 = col_character(), 
                                 CODE_7 = col_character()))

chronic <- suppressWarnings(read_csv("Data/claim_chronic_condition.csv", 
                    col_types = cols(Asthma = col_integer(), 
                                     CLAIM_NUM = col_character(), CLAIM_SEQ = col_integer(), 
                                     COPD = col_integer(), Diabetes = col_integer(), 
                                     HeartFailure = col_integer(), Hypertension = col_integer(), 
                                     MRN_ALIAS = col_character(), MoodDisorder = col_integer(), 
                                     Psychoses = col_integer(), YEAR = col_skip(),
                                     BrainInjury = col_integer(), Dementia = col_integer(), 
                                     Epilepsy = col_integer(), ESRD = col_integer(), 
                                     IHD = col_integer(), SubstanceAbuse = col_integer(),
                                     TIA = col_integer())))
df$CLAIM_NUM <- as.character(df$CLAIM_NUM)


# join together
print('Joining data')
full <- df %>% 
  left_join(lead, by =c('CLAIM_NUM','MRN_ALIAS')) %>% 
  left_join(ccs, by = 'CLAIM_NUM') %>% 
  left_join(chronic, by = c('CLAIM_NUM', 'MRN_ALIAS'))

full <- full %>% 
  select(MRN_ALIAS, MEMBER_SEX, AGE_GROUP, Asthma, BrainInjury, COPD, Dementia,Diabetes, Epilepsy, ESRD,
         HeartFailure,Hypertension, IHD, MoodDisorder,Psychoses,NicotineDependance,SubstanceAbuse,
         TIA, CLAIM_SEQ,EPISODE_SEQ, SERVICE_TYPE,ED_NEXT_3, YEAR, everything())
# save
print('Saving data, this is the last step!')
write_csv(full, 'Data/model_dataset.csv', col_names = TRUE)
print('All done!')
Sys.sleep(5)