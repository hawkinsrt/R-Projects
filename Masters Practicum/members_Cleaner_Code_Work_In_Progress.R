library(tidyverse)

# Import and edit claims data -  Ryan's code
claimsFull <- read_rds("Data/claimsCleanSmall.RDS")

claimsFull<- claimsFull %>% 
  filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC))

all_diag_df <- claimsFull %>% 
  select(starts_with('CODE_'))

all_diag <- c(t(all_diag_df))

all_diag <- data.frame(table(all_diag))
rm(all_diag_df)

# Create vector of ICD-10 'V' codes
icdv <- str_subset(all_diag$all_diag, "[V]\\d{2}\\.") #Grab all codes that start with V
icd10v <-str_subset(icdv, "[X]") #ICD_10 "V" codes include "X"s

icd9 <- str_subset(all_diag$all_diag, "^\\d") #ICD-9 codes start with a number or a E or a V
icd9e <- str_subset(all_diag$all_diag, "[E]\\d{3}") #ICD-9 "E" codes have 3 digits before decimal

icd9Only <- all_diag %>%
  filter((all_diag %in% icd9 | all_diag %in% icd9e | all_diag %in% icdv) & !(all_diag %in% icd10v))

icd9vector <- icd9Only$all_diag

# import diagnosis_final
diag <- read_csv('Data/diagnosis_final.csv')

#==============================================
# unite claim num and episode_seq to get a unique ID. needed for joins and spreads
# select the bare minimum columns for memeory sake, for the same reason take half the years
claimsFull <- claimsFull %>%
  unite(CLAIM_NUM, EPISODE_SEQ, col = ID, sep = '-') %>% 
  select(ID, starts_with('CODE_'),YEAR) %>%
  filter(YEAR %in% c('2013','2014'))

# gather all codes into one column so you only have to mutate once
claimsFull2 <- claimsFull %>% 
  gather(number, DIAG_CODE, starts_with('CODE_')) %>% 
  mutate(ICD_VERSION = ifelse(DIAG_CODE %in% icd9vector, 9,10),
          DIAG_CODE = str_remove_all(DIAG_CODE, pattern = "[[:punct:]]"))

# group by diag code and icd version then join the diagnosis_final dataset to get cci and body system
claimsFull3 <- claimsFull2 %>% 
  group_by(DIAG_CODE, ICD_VERSION) %>% 
  left_join(diag, by = c('DIAG_CODE' = 'DIAG_CODE', 'ICD_VERSION' = 'ICD_VERSION')) %>% 
  ungroup()

# free up some memory
rm(claimsFull)
rm(claimsFull2)

# this is when the spread comes in
# for some reason this throws an error about duplicate rows but ID should be a unique identifier
# I wonder if its because I had to remove the NAs in the gather statement to save memeory and 
# when spreading back out now that those columns don't exsist it gets mad.  
# I used Unite to get a combo body system chronic yes/no in one column just to try and spread SOMETHING
claimsFull4 <- claimsFull3 %>% 
  select(ID, number, CHRONIC_INDICATOR, BODY_SYSTEM) %>% 
  unite(BODY_SYSTEM, CHRONIC_INDICATOR, col = 'CHRONIC_BDSYS', sep = '-') %>% 
  spread(number, CHRONIC_BDSYS)




