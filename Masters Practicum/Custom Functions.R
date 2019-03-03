clean <- function (claims){ 
  
  df<- claims %>% filter(!is.na(X11))
  df <- unite(df, X13, c(X12, X13))
  
  df2 <- claims %>% filter(is.na(X11))
  df2 <- unite(df2, X42, c(X42, X43))
  
  colnames <- paste0("X",c(1:42))
  
  colnames(df) <- colnames
  colnames(df2) <- colnames
  
  df <- df %>% mutate_all(as.character)
  df2 <- df2 %>% mutate_all(as.character)
  
  claimsClean <- bind_rows(df,df2)
  
  claimsClean <- claimsClean %>% 
                  mutate_at(c("X3","X5","X6","X9", "X10","X11","X31","X32","X39","X40","X41"), as.numeric) %>% 
                  mutate_if(is.character,as.factor)
  return(claimsClean)
  
}

transform_codes <-function(df, year1, year2, method){

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
  cat("================")
  
  # import diagnosis_final
  suppressMessages(suppressWarnings(diag <-read_csv('Data/diagnosis_final.csv')))
  diag$BODY_SYSTEM[is.na(diag$BODY_SYSTEM)] <- 0
  
  years <- c(year1, year2)  
  
  claims<- df %>% 
    select(MRN_ALIAS, starts_with('CODE_'),YEAR, CLAIM_NUM) %>% 
    filter(YEAR %in% years)
  
  cat("=====================")
  
  # gather all codes into one column so you only have to mutate once
  suppressWarnings(claims2 <- claims %>% 
                     gather(number, DIAG_CODE, starts_with('CODE_')) %>% 
                     mutate(ICD_VERSION = ifelse(DIAG_CODE %in% icd9vector, 9,10),
                            DIAG_CODE = str_remove_all(DIAG_CODE, pattern = "[[:punct:]]")))
  

  
  cat("=====================")
  # group by diag code and icd version then join the diagnosis_final dataset to get cci and body system
  claims3 <- claims2 %>% 
    #group_by(DIAG_CODE, ICD_VERSION) %>% 
    left_join(diag, by = c('DIAG_CODE' = 'DIAG_CODE', 'ICD_VERSION' = 'ICD_VERSION'))
  
  # free up some memory
  rm(claims2)
  gc()
  cat("=====================")
  
  if(method == 'chronic'){
  
    claims4 <- claims3 %>% 
      filter(CHRONIC_INDICATOR == 1) %>% 
      select(MRN_ALIAS, BODY_SYSTEM, CHRONIC_INDICATOR,YEAR) %>%
      mutate(BODY_SYSTEM = paste0('BS_', BODY_SYSTEM)) %>% 
      group_by(MRN_ALIAS, BODY_SYSTEM) %>% 
      summarise(CHONIC_INDICATOR = max(CHRONIC_INDICATOR, na.rm = T),
                YEAR = min(as.numeric(as.character(YEAR)))) %>% 
      spread(BODY_SYSTEM, CHONIC_INDICATOR)
  
    claims5 <- claims %>% 
      select(CLAIM_NUM, MRN_ALIAS, YEAR) %>% 
      mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
      inner_join(claims4, by = 'MRN_ALIAS') %>% 
      filter(YEAR.x >= YEAR.y) %>% 
      select(-YEAR.x, -YEAR.y, -MRN_ALIAS)
    
    
    file <- paste0('Data/', year1, 'and', year2,'_chornic_body_system.rds')
    saveRDS(claims5, file)
    cat("=====================\n ")
    cat(file, 'was save to the Data folder\n')
    rm(claims,claims3, claims4, year1, year2, years, file)
    gc()
  }
  else if(method == 'ccs'){
      claims4 <- claims3 %>% 
      select(CLAIM_NUM, number, CCS_CATEGORY) %>% 
      spread(number, CCS_CATEGORY)
    
      file <- paste0('Data/', year1, 'and', year2,'_ccs_category.rds')
      saveRDS(claims4, file)
      cat("=====================\n ")
      cat(file, 'was save to the Data folder\n')
      rm(claims3, claims4, year1, year2, years, file)
      gc()
  }
  
  else if(method == 'body_system'){
      claims4 <- claims3 %>% 
      select(CLAIM_NUM, number, BODY_SYSTEM) %>% 
      spread(number, BODY_SYSTEM)
    
      file <- paste0('Data/', year1, 'and', year2,'_body_system.rds')
      saveRDS(claims4, file)
      cat("=====================\n ")
      cat(file, 'was save to the Data folder\n')
      rm(claims3, claims4, year1, year2, years, file)
      gc()
  }
  else if(method == 'all'){
    claims4 <- claims3 %>% 
      filter(CHRONIC_INDICATOR == 1) %>% 
      select(MRN_ALIAS, BODY_SYSTEM, CHRONIC_INDICATOR,YEAR) %>%
      mutate(BODY_SYSTEM = paste0('BS_', BODY_SYSTEM)) %>% 
      group_by(MRN_ALIAS, BODY_SYSTEM) %>% 
      summarise(CHONIC_INDICATOR = max(CHRONIC_INDICATOR, na.rm = T),
                YEAR = min(as.numeric(as.character(YEAR)))) %>% 
      spread(BODY_SYSTEM, CHONIC_INDICATOR)
    
    claims5 <- claims %>% 
      select(CLAIM_NUM, MRN_ALIAS, YEAR) %>% 
      mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
      inner_join(claims4, by = 'MRN_ALIAS') %>% 
      filter(YEAR.x >= YEAR.y) %>% 
      select(-YEAR.x, -YEAR.y, -MRN_ALIAS)
    
    
    file <- paste0('Data/', year1, 'and', year2,'_chornic_body_system.rds')
    saveRDS(claims5, file)
    cat("=====================\n ")
    cat(file, 'was save to the Data folder\n')
    rm(claims,claims3, claims4,claims5,file)
   
    claims4 <- claims3 %>% 
      select(CLAIM_NUM, number, CCS_CATEGORY) %>% 
      spread(number, CCS_CATEGORY)
    
    file <- paste0('Data/', year1, 'and', year2,'_ccs_category.rds')
    saveRDS(claims4, file)
    cat("=====================\n ")
    cat(file, 'was save to the Data folder\n')
    rm(claims4)
    
    claims4 <- claims3 %>% 
      select(CLAIM_NUM, number, BODY_SYSTEM) %>% 
      spread(number, BODY_SYSTEM)
    
    file <- paste0('Data/', year1, 'and', year2,'_body_system.rds')
    saveRDS(claims4, file)
    cat("=====================\n ")
    cat(file, 'was save to the Data folder\n')
    rm(claims3, claims4, year1, year2, years, file)
    gc()
  }
}

## Writen by Michael Behrend
ImportLib = function(PackageName) {
  # store the package name in a variable
  strLib = deparse(substitute(PackageName))
  # check to see if the package is installed
  if (!(strLib %in% installed.packages()[, 1])){
    # if the package is not installed, then install it
    install.packages(strLib)
    # check again to see if the package is installed
    if (!(strLib %in% available.packages()[, 1])){
      # if the package is not installed, then throw an error
      stop(paste("Package ", strLib, " does not exist."))
      # the program will not continue past this point if the package is not installed
    }
  }
  # if the package IS installed, load it
  suppressWarnings(suppressMessages(library(strLib, character.only = TRUE)))
  
}

createClaimsSmall <- function(){
  claimsFull <- readRDS("Data/claimsCleanFull.RDS")
  # Select the most important columns
  claims <- claimsFull %>%
    select(MRN_ALIAS, MEMBER_SEX,MEMBER_AGE,CLAIM_SEQ,EPISODE_SEQ,YEAR, SERVICE_TYPE,
           PLACE_OF_SERVICE_DESC,CODE_1, CODE_2,  CODE_3, CODE_4, CODE_5,CODE_6, CODE_7,
           ED_DISCHARGE_DX_DESC,PREVENTABILITY,ED_NOT_NEEDED_PROP,
           UNCLASSIFIED_ED, PCP_ID, VENDOR_PROV_ID, CLAIM_TYPE,
           TOB_CATEGORY,APPROVED_DAYS, APPROVED_AMT, CLAIM_NUM) %>%
    filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC))
  
  rm(claimsFull)
  
    claims2 <- claims %>% 
    count(CLAIM_NUM) %>% 
    filter(n == 1)
  
  claims3 <- claims2 %>% 
    left_join(claims, by ="CLAIM_NUM")
  
  claims3 <- claims3 %>% 
    select(-n) %>% 
    arrange(MRN_ALIAS, CLAIM_SEQ, EPISODE_SEQ)
  
  saveRDS(claims3,'Data/claimsCleanSmall.RDS')
  rm(claims2,claims3)
}

transform_claims <- function(df){
  df2 <- df %>% 
    group_by(MRN_ALIAS) %>% 
    mutate(NEXT_SERVICE = lead(SERVICE_TYPE, n = 1),
           LEAD_EPISODE = lead(EPISODE_SEQ, n = 1),
           LEAD_YEAR = lead(YEAR, n = 1)) %>% 
    ungroup() %>% 
    select(CLAIM_NUM,MRN_ALIAS, NEXT_SERVICE, MEMBER_AGE,  LEAD_EPISODE, LEAD_YEAR)

  df3 <- df2 %>% 
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
  
  df4<- df3 %>% 
    select(-MEMBER_AGE)

  saveRDS(df4,'Data/claim_lead_age_group.rds')
  remove(df,df2, df3,df4)
  
}

combine_years <- function(method){
  
  if(method == 'body_system'){
  df1 <- readRDS('Data/2013and2014_body_system.RDS')
  df2 <- readRDS('Data/2015and2016_body_system.RDS')
  df3 <- readRDS('Data/2017and2018_body_system.RDS')
  
  df4 <- bind_rows(df1,df2,df3)
  
  saveRDS(df4, 'Data/claim_body_system_full.R')
  
  rm(df1,df2,df3,df4)}
  else if(method == 'ccs'){
    df1 <- readRDS('Data/2013and2014_ccs_category.RDS')
    df2 <- readRDS('Data/2015and2016_ccs_category.RDS')
    df3 <- readRDS('Data/2017and2018_ccs_category.RDS')
    
    df4 <- bind_rows(df1,df2,df3)
    
    saveRDS(df4, 'Data/claim_ccs_category_full.RDS')
    
    rm(df1,df2,df3,df4)
  }
  else if(method == 'chronic'){
    df1 <- readRDS('Data/2013and2014_chronic_body_system.RDS')
    df2 <- readRDS('Data/2015and2016_chronic_body_system.RDS')
    df3 <- readRDS('Data/2017and2018_chronic_body_system.RDS')
    
    df4 <- bind_rows(df1,df2,df3)
    
    saveRDS(df4, 'Data/claim_chronic_body_system_full.RDS')
    
    rm(df1,df2,df3,df4)
  }
  else if(method == 'all'){
  df1 <- readRDS('Data/2013and2014_body_system.RDS')
  df2 <- readRDS('Data/2015and2016_body_system.RDS')
  df3 <- readRDS('Data/2017and2018_body_system.RDS')
  
  df4 <- bind_rows(df1,df2,df3)
  
  saveRDS(df4, 'Data/claim_body_system_full.RDS')
  
  rm(df1,df2,df3,df4)
  
  df1 <- readRDS('Data/2013and2014_ccs_category.RDS')
  df2 <- readRDS('Data/2015and2016_ccs_category.RDS')
  df3 <- readRDS('Data/2017and2018_ccs_category.RDS')
  
  df4 <- bind_rows(df1,df2,df3)
  
  saveRDS(df4, 'Data/claim_ccs_category_full.R')
  
  rm(df1,df2,df3,df4)
  
  df1 <- readRDS('Data/2013and2014_chronic_body_system.RDS')
  df2 <- readRDS('Data/2015and2016_chronic_body_system.RDS')
  df3 <- readRDS('Data/2017and2018_chronic_body_system.RDS')
  
  df4 <- bind_rows(df1,df2,df3)
  
  saveRDS(df4, 'Data/claim_chronic_body_system_full.R')
  
  rm(df1,df2,df3,df4)
  }
}