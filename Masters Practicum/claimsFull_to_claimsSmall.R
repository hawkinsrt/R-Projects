library(tidyverse)
cat("This script reads in the enitre claimsCleanFulll.RDS and removes unnecessary columns to\n",
"save on memory but more importantly it also removes the 6,000 duplicate CLAIM_NUMs that will cause\n",
"problems. It creates a new file in the Data folder called claimsCleanSmall.RDS that can be read\n" ,
"into other scripts to do other mutations(as it contains all the columns we would want and none we don't.")

claimsFull <- readRDS("Data/claimsCleanFull.RDS")

# Select the most important columns
claims <- claimsFull %>%
  select(MRN_ALIAS, MEMBER_SEX,MEMBER_AGE,CLAIM_SEQ,EPISODE_SEQ,YEAR, SERVICE_TYPE,
         PLACE_OF_SERVICE_DESC,CODE_1, CODE_2,  CODE_3, CODE_4, CODE_5,CODE_6, CODE_7,
         ED_DISCHARGE_DX_DESC,PREVENTABILITY,ED_NOT_NEEDED_PROP,
         UNCLASSIFIED_ED, PCP_ID, VENDOR_PROV_ID, CLAIM_TYPE,
         TOB_CATEGORY,APPROVED_DAYS, APPROVED_AMT, CLAIM_NUM) %>%
  filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC)) %>% 
  arrange(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ)

rm(claimsFull)

claims2 <- claims %>% 
  count(CLAIM_NUM) %>% 
  filter(n == 1)

claims3 <- claims2 %>% 
  left_join(claims, by ="CLAIM_NUM")

claims3 <- claims3 %>% 
  select(-n)

saveRDS(claims3,'Data/claimsCleanSmall.RDS')
rm(claims2,claims3)
