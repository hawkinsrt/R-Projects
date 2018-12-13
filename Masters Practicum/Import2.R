# Use Import2.R to format your clean.RDS files to the correct format so the code in the rest of the project will run without issue.
# Once you do this for the first time and resave the claimsCleanFull.RDS you don't have to run this script again.

clean13 <- readRDS("Data/clean13.RDS")
clean14 <- readRDS("Data/clean14.RDS")
clean15 <- readRDS("Data/clean15.RDS")
clean16 <- readRDS("Data/clean16.RDS")
clean17 <- readRDS("Data/clean17.RDS")
clean18 <- readRDS("Data/clean18.RDS")

clean13$YEAR <- 2013 
clean14$YEAR <- 2014 
clean15$YEAR <- 2015 
clean16$YEAR <- 2016 
clean17$YEAR <- 2017 
clean18$YEAR <- 2018 


claimsCleanFull <- bind_rows(clean13, clean14, clean15, clean16, clean17, clean18)

claimsCleanFull$YEAR <- as.factor(claimsCleanFull$YEAR)

claimsCleanFull <- claimsCleanFull %>% 
                      mutate_at(c("X9","X10","X11"), as.numeric)

rm(clean13,clean14, clean15, clean16, clean17, clean18)

claimsCleanFull <- claimsCleanFull %>% mutate_if(is.character, as.factor)

names(claimsCleanFull) <- header <- c('MRN_ALIAS','MEMBER_SEX','MEMBER_AGE','CLAIM_NUM','CLAIM_SEQ','EPISODE_SEQ',
                                      'CURR_STATUS','REFERRAL_TYPE','MASTER_VENDOR_PROV_ID','VENDOR_PROV_ID','PCP_ID',
                                      'MED_PRAC','CODE_1','CODE_2','CODE_3','CODE_4','CODE_5','CODE_6','CODE_7',
                                      'CODE_8','CODE_9','CODE_10','CODE_11','CODE_12','CODE_13','CODE_14','CODE_15',
                                      'CODE_16','CODE_17','CODE_18','APPROVED_AMT','APPROVED_DAYS','PLACE_OF_SERVICE',
                                      'PLACE_OF_SERVICE_DESC','TYPE_OF_BILL','TOB_CATEGORY','SERVICE_TYPE',
                                      'CLAIM_TYPE','PREVENTABILITY','ED_NOT_NEEDED_PROP','UNCLASSIFIED_ED',
                                      'ED_DISCHARGE_DX_DESC','YEAR')

saveRDS(claimsCleanFull, "Data/claimsCleanFull.RDS")
rm(claimsCleanFull, header)
