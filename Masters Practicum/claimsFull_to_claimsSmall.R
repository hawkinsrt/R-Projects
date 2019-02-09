library(tidyverse)
claimsFull <- readRDS("Data/claimsCleanFull.RDS")

# Select the most important columns
claims <- claimsFull %>%
  select(MRN_ALIAS, MEMBER_SEX,MEMBER_AGE,CLAIM_SEQ,EPISODE_SEQ,YEAR, SERVICE_TYPE,
         PLACE_OF_SERVICE_DESC,CODE_1, CODE_2,  CODE_3, CODE_4, CODE_5,CODE_6, CODE_7,
         ED_DISCHARGE_DX_DESC,PREVENTABILITY,ED_NOT_NEEDED_PROP,
         UNCLASSIFIED_ED, PCP_ID, VENDOR_PROV_ID, CLAIM_TYPE,
         TOB_CATEGORY,APPROVED_DAYS, APPROVED_AMT, CLAIM_NUM) %>%
  arrange(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ)

claims <- claims[unique(claims$CLAIM_NUM), ] %>% 
  arrange(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ)

saveRDS(claims,'Data/claimsCleanSmall.RDS')