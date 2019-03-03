library(tidyverse)
df <- readRDS('Data/2013and2014.rds')

# claimsFull <- readRDS("Data/claimsCleanFull.RDS")
# run claimsFull_to_claimsSmall.R to create claimsCleanSmall.RDS
claims <- readRDS('Data/claimsCleanSmall.RDS')

#claimsDubs <- claims %>% 
 # filter(PLACE_OF_SERVICE_DESC == 'EMERGENCY ROOM - HOSPITAL' | SERVICE_TYPE == 'ED')

claims2 <- claims %>% 
  filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC)) %>% 
  filter(CLAIM_NUM %in% df$CLAIM_NUM)

claims3 <- claims2 %>% 
  group_by(MRN_ALIAS) %>% 
  mutate(NEXT_SERVICE = lead(SERVICE_TYPE, n = 1),
         LEAD_EPISODE = lead(EPISODE_SEQ, n = 1),
         LEAD_YEAR = lead(YEAR, n = 1)) %>% 
  select(MRN_ALIAS,SERVICE_TYPE, NEXT_SERVICE,MEMBER_SEX, MEMBER_AGE, 
         CLAIM_SEQ, EPISODE_SEQ, LEAD_EPISODE, YEAR, LEAD_YEAR,everything())

claims4 <- claims3 %>% 
  mutate(AGE_GROUP = case_when(
    MEMBER_AGE <= 2 ~ 'Baby',
    MEMBER_AGE <= 5 ~ 'PreSchool',
    MEMBER_AGE <= 13 ~ 'Child',
    MEMBER_AGE <= 18 ~ 'Teen',
    MEMBER_AGE <= 33 ~ 'Young Adult',
    MEMBER_AGE <= 48 ~ 'Adult',
    MEMBER_AGE <= 64 ~ 'Middle Age',
    MEMBER_AGE <= 78 ~ 'Senior',
    TRUE ~ 'Very Senior')) 

claims5 <- claims4 %>% 
  select(CLAIM_NUM, AGE_GROUP, MEMBER_SEX, SERVICE_TYPE, EPISODE_SEQ, YEAR, MRN_ALIAS)

df2 <-  df %>% 
  left_join(claims5, by = 'CLAIM_NUM')

