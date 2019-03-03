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
      filter(CHRONIC_INDICATOR == 1) %>% 
      count(MRN_ALIAS) %>% 
      select(-n)
      
    claims3$YEAR <- as.numeric(as.character(claims3$YEAR))
    
    claims3 <-  claims3 %>% 
      mutate(YEAR.CC = ifelse(CHRONIC_INDICATOR == 1,YEAR, NA))
    
    suppressWarnings(claims4 <- claims3 %>% 
      filter(MRN_ALIAS %in% claimsC$MRN_ALIAS) %>% 
      select(MRN_ALIAS,BODY_SYSTEM, YEAR.CC) %>% 
      mutate(BODY_SYSTEM = paste0('BS_', BODY_SYSTEM)) %>%
      group_by(MRN_ALIAS,BODY_SYSTEM) %>% 
      summarise(YEAR.CC = min(YEAR.CC, na.rm = T)) %>% 
      spread(BODY_SYSTEM, YEAR.CC) %>% 
      select(-BS_0, -BS_NA))
    
    rm(claims3)
    
    claims4[claims4 == 'Inf'] <- NA
    
    claims5 <- claims %>% select(MRN_ALIAS, CLAIM_NUM, CLAIM_SEQ, YEAR)
    
    rm(claims)
    
    claims6 <- claims4 %>% 
      left_join(claims5, by = 'MRN_ALIAS')
    
    claims6$YEAR <- as.numeric(as.character(claims6$YEAR))
    
    rm(claims4,claims5)
    gc()
      
    # loop through body system columns and set to 1 if its before claims YEAR
     for(j in 2:19){
        claims6[,j] <- ifelse(claims6[,j] <= claims6$YEAR, 1, NA)
      }
  
      file <- paste0('Data/',i,'of5','_chornic_body_system.rds')
      saveRDS(claims6, file)
      cat("=====================\n ")
      cat(file, 'was save to the Data folder\n')
  
} 

df1 <- readRDS('Data/1of5_chornic_body_system.rds')
df2 <- readRDS('Data/2of5_chornic_body_system.rds')
df3 <- readRDS('Data/3of5_chornic_body_system.rds')
df4 <- readRDS('Data/4of5_chornic_body_system.rds')
df5 <- readRDS('Data/5of5_chornic_body_system.rds')

df6 <- bind_rows(df1,df2,df3,df4,df5)

saveRDS(df6, 'Data/claim_chronic_body_system_full.RDS')

rm(df1,df2,df3,df4,df5,df6)

file.remove('Data/1of5_chornic_body_system.rds')
file.remove('Data/2of5_chornic_body_system.rds')
file.remove('Data/3of5_chornic_body_system.rds')
file.remove('Data/4of5_chornic_body_system.rds')
file.remove('Data/5of5_chornic_body_system.rds')  
