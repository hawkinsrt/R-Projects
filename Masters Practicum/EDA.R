library(tidyverse)
library(naniar)


# Read in file
claims <- readRDS("Data/claimsCleanFull.RDS")
# Add column headers
source('DataDictionary.R')
names(claims) <- header
# Glimspe at the structure of claims
glimpse(claims)
# View summary of columns
summary(claims)
# See how many level each factor column has
map(claims, nlevels)
# Number of Patient Types(Inpatient, Outpatient or ED) by Claim Type(Facility or Professional)
claims %>% select(PATIENT_TYPE, CLAIM_TYPE) %>% 
  table()
# Type of Bill Category by Patient Type
claims %>% select(TOB_CATEGORY,PATIENT_TYPE) %>% 
  table()
# Gives a feel of the data, selects the most important columns and will fit on one screen
claims %>%
  select(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ,ED_DISCHARGE_DX_DESC, PREVENTABILITY,ED_NOT_NEEDED_PROP,
         UNCLASSIFIED_ED, PATIENT_TYPE,CLAIM_TYPE,PLACE_OF_SERVICE_DESC, TOB_CATEGORY) %>% 
  arrange(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ) %>% 
  View('ClaimsSub')
# Number of members
claims %>% 
  count(MRN_ALIAS) %>% 
  nrow()
# Diagnosis with an PREVENTABILITY = 1
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP) %>% 
  filter(PREVENTABILITY == 1 & PATIENT_TYPE == 'ED') %>% 
  group_by(ED_DISCHARGE_DX_DESC) %>% 
  summarize(PREVENTABILITY = mean(PREVENTABILITY),
            ED_NOT_NEEDED_PROP = mean(ED_NOT_NEEDED_PROP)) %>% 
  arrange(desc(PREVENTABILITY)) %>% 
  View('Preventable')
# Diagnosis with an ED_NOT_NEEDED_PROP = 1
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP) %>% 
  filter(ED_NOT_NEEDED_PROP == 1 & PATIENT_TYPE == 'ED') %>% 
  group_by(ED_DISCHARGE_DX_DESC) %>% 
  summarize(PREVENTABILITY = mean(PREVENTABILITY),
            ED_NOT_NEEDED_PROP = mean(ED_NOT_NEEDED_PROP)) %>% 
  arrange(desc(PREVENTABILITY)) %>% 
  View('ER Not Needed')
# Diagnosis wjere PREVENTABILITY + ED_NOT_NEEDED_PROP != 1
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP) %>% 
  filter(ED_NOT_NEEDED_PROP + PREVENTABILITY != 1) %>% 
  group_by(ED_DISCHARGE_DX_DESC) %>% 
  summarize(PREVENTABILITY = mean(PREVENTABILITY),
            ED_NOT_NEEDED_PROP = mean(ED_NOT_NEEDED_PROP)) %>% 
  arrange(desc(PREVENTABILITY)) %>% 
  View('Not equal to 1')
# Total amount of money spent on ER claims that were 100% preventable or 100% not er needed
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP, APPROVED_AMT, YEAR) %>% 
  filter(PATIENT_TYPE == 'ED' & (ED_NOT_NEEDED_PROP == 1 | PREVENTABILITY == 1)) %>% 
  group_by(YEAR) %>% 
  summarize(Total_Cost = sum(APPROVED_AMT)) %>% 
  ggplot(aes(YEAR, Total_Cost, fill = YEAR))+
  geom_col()
# Most comman ER diagnosis
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP, APPROVED_AMT, YEAR) %>% 
  filter(PATIENT_TYPE == 'ED' & (ED_NOT_NEEDED_PROP == 1 | PREVENTABILITY == 1)) %>% 
  group_by(ED_DISCHARGE_DX_DESC) %>% 
  summarize(Total_Cases = n()) %>% 
  arrange(desc(Total_Cases)) %>% 
  head(n = 50) %>% 
  ggplot(aes(fct_reorder(ED_DISCHARGE_DX_DESC,Total_Cases), Total_Cases,fill = fct_reorder(ED_DISCHARGE_DX_DESC,Total_Cases)))+
  geom_col()+
  coord_flip()+
  labs(x='Top 50 ED Not Needed ED claims')+
  theme(legend.position="none")

  


