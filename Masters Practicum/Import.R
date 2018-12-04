library(tidyverse)
library(naniar)
source("Custom Functions.R")

claims13 <- read_csv('Data/FY13 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims14 <- read_csv('Data/FY14 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims15 <- read_csv('Data/FY15 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims16 <- read_csv('Data/FY16 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims17 <- read_csv('Data/FY17 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims18 <- read_csv('Data/FY18 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))

clean13 <- clean(claims13)
clean14 <- clean(claims14)
clean15 <- clean(claims15)
clean16 <- clean(claims16)
clean17 <- clean(claims17)
clean18 <- clean(claims18)


clean13$YEAR <- 2013 
clean14$YEAR <- 2014 
clean15$YEAR <- 2015 
clean16$YEAR <- 2016 
clean17$YEAR <- 2017 
clean18$YEAR <- 2018 

rm(claims13, claims14,claims15, claims16, claims17, claims18)

saveRDS(clean13, "Data/clean13.RDS")
saveRDS(clean14, "Data/clean14.RDS")
saveRDS(clean15, "Data/clean15.RDS")
saveRDS(clean16, "Data/clean16.RDS")
saveRDS(clean17, "Data/clean17.RDS")
saveRDS(clean18, "Data/clean18.RDS")

# clean13 <- readRDS("Data/clean13.RDS")
# clean14 <- readRDS("Data/clean14.RDS")
# clean15 <- readRDS("Data/clean15.RDS")
# clean16 <- readRDS("Data/clean16.RDS")
# clean17 <- readRDS("Data/clean17.RDS")
# clean18 <- readRDS("Data/clean18.RDS")


claimsCleanFull <- bind_rows(clean13, clean14, clean15, clean16, clean17, clean18)

claimsCleanFull$YEAR <- as.factor(claimsCleanFull$YEAR)

rm(clean13,clean14, clean15, clean16, clean17, clean18)

claimsCleanFull <- claimsCleanFull %>% mutate_if(is.character, as.factor)

names(claimsCleanFull) <- header <- c('MRN_ALIAS','MEMBER_SEX','MEMBER_AGE','CLAIM_NUM','CLAIM_SEQ','EPISODE_SEQ',
                                      'CURR_STATUS','SERVICE_TYPE','MASTER_VENDOR_PROV_ID','VENDOR_PROV_ID','PCP_ID',
                                      'MED_PRAC','CODE_1','CODE_2','CODE_3','CODE_4','CODE_5','CODE_6','CODE_7',
                                      'CODE_8','CODE_9','CODE_10','CODE_11','CODE_12','CODE_13','CODE_14','CODE_15',
                                      'CODE_16','CODE_17','CODE_18','APPROVED_AMT','APPROVED_DAYS','PLACE_OF_SERVICE',
                                      'PLACE_OF_SERVICE_DESC','TYPE_OF_BILL','TOB_CATEGORY','PATIENT_TYPE',
                                      'CLAIM_TYPE','PREVENTABILITY','ED_NOT_NEEDED_PROP','UNCLASSIFIED_ED',
                                      'ED_DISCHARGE_DX_DESC','YEAR')

saveRDS(claims, "C:/Users/hawkinsrt/Desktop/Data/claimsCleanFull.RDS")
