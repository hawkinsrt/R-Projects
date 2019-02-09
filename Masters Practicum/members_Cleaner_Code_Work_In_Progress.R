library(tidyverse)
library(naniar)

all_diag_df <- claims %>% 
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
miss_var_summary(diag)
diag$BODY_SYSTEM[is.na(diag$BODY_SYSTEM)] <- 0
unique(diag$BODY_SYSTEM)
#==============================================

# Import and edit claims data -  Ryan's code
claims <- read_rds("Data/claimsCleanSmall.RDS")

claims<- claims %>% 
  filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC)) %>% 
  unite(CLAIM_NUM, EPISODE_SEQ, col = ID, sep = '-') %>% 
  select(ID, starts_with('CODE_'),YEAR) %>% 
  filter(YEAR %in% c('2013','2014'))

# gather all codes into one column so you only have to mutate once
claims2 <- claims %>% 
  gather(number, DIAG_CODE, starts_with('CODE_')) %>% 
  mutate(ICD_VERSION = ifelse(DIAG_CODE %in% icd9vector, 9,10),
          DIAG_CODE = str_remove_all(DIAG_CODE, pattern = "[[:punct:]]"))

rm(claims)

# group by diag code and icd version then join the diagnosis_final dataset to get cci and body system
claims3 <- claims2 %>% 
  #group_by(DIAG_CODE, ICD_VERSION) %>% 
  left_join(diag, by = c('DIAG_CODE' = 'DIAG_CODE', 'ICD_VERSION' = 'ICD_VERSION'))

# free up some memory
rm(claims2)
gc()

claims4 <- claims3 %>% 
  select(ID, number, BODY_SYSTEM) %>% 
  spread(number, BODY_SYSTEM)

miss_var_summary(claims4)
rm(claims3)
rm(claims4)
gc()

