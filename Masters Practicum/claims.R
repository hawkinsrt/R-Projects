library(tidyverse)
library(naniar)

# claimsFull <- readRDS("Data/claimsCleanFull.RDS")
# run claimsFull_to_claimsSmall.R to create claimsCleanSmall.RDS
claims <- readRDS('Data/claimsCleanSmall.RDS')

claimsDubs <- claims %>% 
  filter(PLACE_OF_SERVICE_DESC == 'EMERGENCY ROOM - HOSPITAL' | SERVICE_TYPE == 'ED')

claimsSubs2 <- claims %>% 
  filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC)) %>% 
  group_by(MRN_ALIAS) %>% 
  mutate(NEXT_SERVICE = lead(SERVICE_TYPE, n = 1),
         LEAD_EPISODE = lead(EPISODE_SEQ, n = 1),
         LEAD_YEAR = lead(YEAR, n = 1)) %>% 
  select(MRN_ALIAS,SERVICE_TYPE, NEXT_SERVICE,MEMBER_SEX, MEMBER_AGE, 
         CLAIM_SEQ, EPISODE_SEQ, LEAD_EPISODE, YEAR, LEAD_YEAR,everything())



