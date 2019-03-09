library(tidyverse)

df <- read_rds("Data/claimsCleanSmall.RDS")

all_diag_df <- df %>% 
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
rm(all_diag,icd9Only,icdv, icd10v,icd9, icd9e)
cat("Preprocessing Complete")

# import diagnosis_final
suppressMessages(suppressWarnings(diag <-read_csv('Data/diagnosis_final.csv')))
diag$BODY_SYSTEM[is.na(diag$BODY_SYSTEM)] <- 0

diag <- diag %>% 
  mutate(TARGET_CONDITION = case_when(
    str_detect(DIAG_CODE, '^F3|^296') ~ 'MoodDisorder',
    str_detect(DIAG_CODE, '^F2|^295') ~ 'Psychoses',
    str_detect(DIAG_CODE, '^J45|^493') ~ 'Asthma',
    str_detect(DIAG_CODE, '^416|^49[012456]|^50[0-5]|^5064|^508[18]|^I27[89]|^J4[0-4]|^J4[67]|^J6[0-7]|^J684|^J70[13]') ~ 'COPD',
    str_detect(DIAG_CODE, '^250|^E1[013]') ~ 'Diabetes',
    str_detect(DIAG_CODE, '^428|^I50') ~ 'HeartFailure',
    str_detect(DIAG_CODE, '^40[1-5]|^I1[0-5]') ~ 'Hypertension'
  )) 


for (i in 1:5){
  
    member <- df %>% 
      count(MRN_ALIAS) %>% 
      select(-n) 
    
    member$group <- rep_len(1:5, length.out=nrow(member))
    
    claims <- df %>% 
      select(MRN_ALIAS, starts_with('CODE_'),YEAR, CLAIM_NUM, CLAIM_SEQ) %>% 
      left_join(member, by = 'MRN_ALIAS') %>% 
      filter(group == i)
  
    
    # gather all codes into one column so you only have to mutate once
    suppressWarnings(claims2 <- claims %>% 
                       gather(number, DIAG_CODE, starts_with('CODE_')) %>% 
                       mutate(ICD_VERSION = ifelse(DIAG_CODE %in% icd9vector, 9,10),
                              DIAG_CODE = str_remove_all(DIAG_CODE, pattern = "[[:punct:]]")))
    
    
    # group by diag code and icd version then join the diagnosis_final dataset to get cci and body system
    claims3 <- claims2 %>% 
      #group_by(DIAG_CODE, ICD_VERSION) %>% 
      left_join(diag, by = c('DIAG_CODE' = 'DIAG_CODE', 'ICD_VERSION' = 'ICD_VERSION'))
    
    # free up some memory
    rm(claims2)
    gc()
      
    # Get only MRN_ALIAS that have a Chronic condition
    claimsC <- claims3 %>% 
      filter(!is.na(TARGET_CONDITION)) %>% 
      count(MRN_ALIAS) %>% 
      select(-n)
      
    claims3$YEAR <- as.numeric(as.character(claims3$YEAR))
    
    claims3 <-  claims3 %>% 
      mutate(YEAR.CC = ifelse(!is.na(TARGET_CONDITION),YEAR, NA))
    
    suppressWarnings(claims4 <- claims3 %>% 
      filter(MRN_ALIAS %in% claimsC$MRN_ALIAS) %>% 
      select(MRN_ALIAS,TARGET_CONDITION, YEAR.CC) %>% 
      group_by(MRN_ALIAS,TARGET_CONDITION) %>% 
      summarise(YEAR.CC = min(YEAR.CC, na.rm = T)) %>% 
      spread(TARGET_CONDITION, YEAR.CC))
    
    rm(claims3)
    
    claims4[claims4 == 'Inf'] <- NA
    
    claims5 <- claims %>% select(MRN_ALIAS, CLAIM_NUM, CLAIM_SEQ, YEAR)
    
    rm(claims)
    
    claims6 <- claims4 %>% 
      left_join(claims5, by = 'MRN_ALIAS') %>% 
      select(-`<NA>`)
    
    claims6$YEAR <- as.numeric(as.character(claims6$YEAR))
    
    rm(claims4,claims5)
    gc()
      
    # loop through body system columns and set to 1 if its before claims YEAR
     for(j in 2:8){
        claims6[,j] <- ifelse(claims6[,j] <= claims6$YEAR, 1, NA)
      }
      
      file <- 'Data/claim_target_condition.csv'
      if(i == 1){
      write_csv(claims6, file, col_names = TRUE)
      }else{
        write_csv(claims6, file, append = T)  
      }
      cat("=====================\n ")
      cat(file, i, 'was save to the Data folder\n')
      
} 
rm(claims6, claimsC, df, diag, member, file, i, icd9vector, j)
