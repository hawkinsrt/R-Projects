clean <- function (claims){ 
  # this function was used to clear up original raw dataset
  # will not be used if data is queried from database
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

icd9_vector <- function(df){
  #create a icd9_vector to label diag codes as version 9 or version 10
  #this might be redundant because ccs xwalk as the version number but I like to use
  #version number to join on just to be safe
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
  return(icd9vector)

}
chronic_condition <-function(df){
  
  icd9vector <- icd9_vector(df)
  # import diagnosis_final
  suppressMessages(suppressWarnings(diag <-read_csv('Data/ccs_diag_xwalk.csv')))
  
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
    #split dataset into 5 groups for memory purposes
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
    
    # loop set to 1 if its before claims YEAR
    for(j in 2:8){
      claims6[,j] <- ifelse(claims6[,j] <= claims6$YEAR, 1, NA)
    }
    
    file <- 'Data/claim_target_condition.csv'
    if(i == 1){
      write_csv(claims6, file, col_names = TRUE)
    }else{
      write_csv(claims6, file, append = T)  
    }
    cat(file, i, 'was save to the Data folder\n')
    
  } 
  rm(claims6, claimsC, diag, member, file, i, icd9vector, j)
}


transform_codes <-function(df){
  icd9vector <- icd9_vector(df)
  
  for (i in 1:5){
  
  mem <- df %>% 
    count(MRN_ALIAS) %>% 
    select(-n) 
  
  mem$group <- rep_len(1:5, length.out=nrow(mem))
  
  claims<- df%>% 
    select(MRN_ALIAS, starts_with('CODE_'),YEAR, CLAIM_NUM, CLAIM_SEQ) %>% 
    left_join(mem, by = 'MRN_ALIAS') %>% 
    filter(group ==i)

  # gather all codes into one column so you only have to mutate once
  suppressWarnings(claims2 <- claims %>% 
                     gather(number, DIAG_CODE, starts_with('CODE_')) %>% 
                     mutate(ICD_VERSION = ifelse(DIAG_CODE %in% icd9vector, 9,10),
                            DIAG_CODE = str_remove_all(DIAG_CODE, pattern = "[[:punct:]]")))
  
  suppressMessages(suppressWarnings(diag <-read_csv('Data/ccs_diag_xwalk.csv')))
  # group by diag code and icd version then join the diagnosis_final dataset to get cci and body system
  claims3 <- claims2 %>% 
    #group_by(DIAG_CODE, ICD_VERSION) %>% 
    left_join(diag, by = c('DIAG_CODE' = 'DIAG_CODE', 'ICD_VERSION' = 'ICD_VERSION'))
  
  # free up some memory
  rm(claims2)
  gc()
   
     claims4 <- claims3 %>% 
       select(CLAIM_NUM, number, CCS_CATEGORY) %>% 
       spread(number, CCS_CATEGORY)
     
     file <- paste0('Data/claim_ccs_category.csv')
     
     if(i==1){
     write_csv(claims4, file, col_names = TRUE)
     }else{
       write_csv(claims4, file, append = TRUE)
     }
     cat(file,i, 'was save to the Data folder\n')
     rm(claims3, claims4,  file)
     gc()
  }
  rm(icd9vector)
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
    mutate(NEXT_SERVICE = lead(SERVICE_TYPE, n = 1), #1 visit into the future
           NEXT_SERVICE2 = lead(SERVICE_TYPE, n = 2), #2 visits into the future
           NEXT_SERVICE3 = lead(SERVICE_TYPE, n = 3)) %>% #3 visits into the future
    ungroup() %>%  
    select(CLAIM_NUM,MRN_ALIAS, NEXT_SERVICE, NEXT_SERVICE2, NEXT_SERVICE3,
           MEMBER_AGE) %>% #select only imporatant columns
    filter(!is.na(NEXT_SERVICE3)) %>% 
    mutate(ED_NEXT_3 = ifelse((NEXT_SERVICE == 'ED' | NEXT_SERVICE2 == 'ED' | 
                                NEXT_SERVICE3 == 'ED'), 1, 0)) %>% # if any of next 3 visit is ED then 1
    select(-starts_with('NEXT_SERVICE'))
  
  # create bins for age
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
    select(-MEMBER_AGE) # remove MEMBER_AGE column
  
  write_csv(df4,'Data/claim_lead_age_group.csv', col_names = TRUE) #save to Data folder
  remove(df,df2, df3,df4) #clear up some memory
  
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
  