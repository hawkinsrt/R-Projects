library(tidyverse)
library(naniar)
library(scales)

# Read in file
#claims <- readRDS("Data/claimsCleanFull.RDS")
claims <- readRDS("C:/Users/hawkinsrt/Desktop/Data/claimsCleanFull.RDS")
# Glimspe at the structure of claims
glimpse(claims)
# View summary of columns, takes some time to run
summary(claims)
# See how many level each factor column has
claims %>% 
  select_if(is.factor) %>% 
  map(nlevels) %>% 
  unlist()
# Number of Patient Types(Inpatient, Outpatient or ED) by Claim Type(Facility or Professional)
claims %>% select(PATIENT_TYPE, CLAIM_TYPE) %>% 
  table()
# Type of Bill Category by Patient Type
claims %>% select(TOB_CATEGORY,PATIENT_TYPE) %>% 
  table()
# Gives a feel of the data, selects the most important columns and will fit on one screen
claims %>%
  select(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ,APPROVED_DAYS,ED_DISCHARGE_DX_DESC, PREVENTABILITY,ED_NOT_NEEDED_PROP,
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
EDNN %>% 
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
  geom_col()+
  scale_y_continuous(labels = dollar_format())+
  labs(title = 'Cost of Uneeded ER vistis', x = NULL, y = NULL)+
  theme(legend.position="none")
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
  labs(x = 'Diagnosis', title = 'Top 50 ER Not Needed Diagnoses')+
  theme(legend.position="none")
# Age distrbution of Unneeded and prevnetable ER
claims %>% 
  select(PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP, MEMBER_AGE, CLAIM_NUM, MEMBER_SEX) %>%
  filter(PATIENT_TYPE == 'ED' & (ED_NOT_NEEDED_PROP == 1 | PREVENTABILITY == 1)) %>% 
  ggplot(aes(MEMBER_AGE)) +
  geom_histogram(binwidth = 1)+
  facet_wrap(~MEMBER_SEX)+
  labs(title='Age distriction of Preventable or Uneeded ER Visist', x = 'Age', y = NULL)
# Most Unneeded or Preventable ER visitors
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP, APPROVED_AMT, YEAR, MRN_ALIAS) %>% 
  filter(PATIENT_TYPE == 'ED' & (ED_NOT_NEEDED_PROP == 1 | PREVENTABILITY == 1)) %>% 
  group_by(MRN_ALIAS) %>% 
  summarize(Total_Cases = n()) %>% 
  arrange(desc(Total_Cases)) %>% 
  head(n = 50) %>% 
  ggplot(aes(fct_reorder(MRN_ALIAS,Total_Cases), Total_Cases,fill = fct_reorder(MRN_ALIAS,Total_Cases)))+
  geom_col()+
  coord_flip()+
  labs(x = 'Member', title = 'Top 50 Most Frequent Unneeded or Preventable ER Visitors')+
  theme(legend.position="none")
# Most Unneeded or Preventable ER visitors, takes a while to compute
claims %>% 
  select(ED_DISCHARGE_DX_DESC,PATIENT_TYPE, PREVENTABILITY, ED_NOT_NEEDED_PROP, APPROVED_AMT, YEAR, MRN_ALIAS) %>% 
  filter(PATIENT_TYPE == 'ED' & (ED_NOT_NEEDED_PROP == 1 | PREVENTABILITY == 1)) %>% 
  group_by(MRN_ALIAS) %>% 
  summarize(Total_Sum = sum(APPROVED_AMT)) %>% 
  arrange(desc(Total_Sum)) %>% 
  head(n = 50) %>% 
  ggplot(aes(fct_reorder(MRN_ALIAS,Total_Sum), Total_Sum,fill = fct_reorder(MRN_ALIAS,Total_Sum)))+
  geom_col()+
  coord_flip()+
  labs(x = 'Member', title = 'Top 50 Most Clostly Members', subtitle = 'Money spent on uneeded or preventable ER visits')+
  scale_y_continuous(labels = dollar_format())+
  theme(legend.position="none")
# 98D3D44BDEB2 as the most unneeded or prevnetable visits and the most costly
claims %>% 
  select(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ,ED_DISCHARGE_DX_DESC, PREVENTABILITY,ED_NOT_NEEDED_PROP,
         UNCLASSIFIED_ED, PATIENT_TYPE,CLAIM_TYPE,PLACE_OF_SERVICE_DESC, TOB_CATEGORY) %>% 
  filter(MRN_ALIAS == '98D3D44BDEB2',PATIENT_TYPE == 'ED' & (ED_NOT_NEEDED_PROP == 1 | PREVENTABILITY == 1)) %>% 
  arrange(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ) %>% 
  View('#1 ER Lover')

