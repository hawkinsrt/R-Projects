source('custom_functions.R')
import_lib(tidyverse)

claims <- read_rds('Data/claimscleanSmall.RDS')

icd9vector <- icd9_vector(claims)

claims2 <- claims %>% 
  select(CLAIM_NUM, starts_with('CODE_'))

claims2[2:ncol(claims2)] <- lapply(claims2[2:ncol(claims2)], is.na)
claims2[2:ncol(claims2)] <- lapply(claims2[2:ncol(claims2)], as.numeric)

claims <- claims2 %>% 
  mutate(Total = (ncol(claims2)-1) - rowSums(.[2:ncol(claims2)])) %>% 
  select(CLAIM_NUM, Total) %>% 
  left_join(claims, by = 'CLAIM_NUM')

rm(claims2)

claims <- claims  %>% 
  mutate(APPROVED_AMT = round(APPROVED_AMT/Total, digits = 2))

diag <-claims %>%
  select(CODE_1, SERVICE_TYPE, ED_NOT_NEEDED_PROP, PREVENTABILITY,UNCLASSIFIED_ED,
         MEMBER_AGE, MEMBER_SEX, APPROVED_AMT) %>%
  mutate(DIAG_CODE = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
         IS_ED = (SERVICE_TYPE == 'ED'),
         IS_OTPT = (SERVICE_TYPE == 'OTPT'),
         IS_INPT = (SERVICE_TYPE == 'INPT'),
         IS_F = (MEMBER_SEX == 'F'),
         ED_Amount_Approved = ifelse(SERVICE_TYPE == 'ED', APPROVED_AMT, 0),
         ICD_VERSION = ifelse(CODE_1 %in% icd9vector, 9,10)) %>% 
  group_by(DIAG_CODE, ICD_VERSION)%>%
  summarize(ED_NOT_NEEDED_PROP = mean(ED_NOT_NEEDED_PROP, na.rm = TRUE), PREVENTABILITY = mean(PREVENTABILITY,na.rm = TRUE),
            UNCLASSIFIED_ED = mean(UNCLASSIFIED_ED,na.rm = TRUE),NumberOfClaims = n(),
            NumberOfED = sum(IS_ED),
            NumberOfOTPT = sum(IS_OTPT),
            NumberOfINPT = sum(IS_INPT),
            AverageAGE = mean(MEMBER_AGE, na.rm = TRUE),
            Pct_Female = mean(IS_F),
            Amount_Approved = sum(APPROVED_AMT),
            ED_Amount_Approved = sum(ED_Amount_Approved))

diag2 <- claims %>% 
  select(starts_with('CODE_'), -CODE_1, SERVICE_TYPE,MEMBER_AGE, MEMBER_SEX, APPROVED_AMT, Total) %>% 
  gather(starts_with('CODE_'), key = 'KEY', value = 'CODE_1') %>% 
  filter(!is.na(CODE_1)) %>% 
  mutate(DIAG_CODE = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
         IS_ED = (SERVICE_TYPE == 'ED'),
         IS_OTPT = (SERVICE_TYPE == 'OTPT'),
         IS_INPT = (SERVICE_TYPE == 'INPT'),
         IS_F = (MEMBER_SEX == 'F'),
    ED_Amount_Approved = ifelse(SERVICE_TYPE == 'ED', APPROVED_AMT, 0),
    ICD_VERSION = ifelse(CODE_1 %in% icd9vector, 9,10)) %>% 
    group_by(DIAG_CODE, ICD_VERSION)%>%
    summarize(NumberOfClaims = n(),
    NumberOfED = sum(IS_ED),
    NumberOfOTPT = sum(IS_OTPT),
    NumberOfINPT = sum(IS_INPT),
    AverageAGE = mean(MEMBER_AGE, na.rm = TRUE),
    Pct_Female = mean(IS_F), 
    Amount_Approved = sum(APPROVED_AMT),
    ED_Amount_Approved = sum(ED_Amount_Approved))

#Add the the score columns back as NA
diag2$ED_NOT_NEEDED_PROP <- NA 
diag2$PREVENTABILITY <- NA
diag2$UNCLASSIFIED_ED <- NA

#Rearrange columns to match diag
diag2 <- diag2 %>% 
  select(DIAG_CODE,ICD_VERSION,ED_NOT_NEEDED_PROP,PREVENTABILITY,UNCLASSIFIED_ED,everything())

#Do the columns match up
names(diag) == names(diag2)

diag_full <- diag %>% #this summarizes counts where code appears anywhere in claim
  bind_rows(diag2) %>% 
  group_by(DIAG_CODE, ICD_VERSION) %>% 
  summarize(ED_NOT_NEEDED_PROP = mean(ED_NOT_NEEDED_PROP, na.rm = TRUE),
  PREVENTABILITY = mean(PREVENTABILITY,na.rm = TRUE),
  UNCLASSIFIED_ED = mean(UNCLASSIFIED_ED,na.rm = TRUE),
  NumberOfClaims = sum(NumberOfClaims), 
  NumberOfED = sum(NumberOfED),
  NumberOfOTPT = sum(NumberOfOTPT),
  NumberOfINPT = sum(NumberOfINPT),
  AverageAGE = round(mean(AverageAGE, na.rm = TRUE)),
  Pct_Female = round(mean(Pct_Female),digits = 2),
  Amount_Approved_Sum = round(sum(Amount_Approved), digits = 2),
  Amount_Approved_Mean = round(mean(Amount_Approved), digits = 2),
  ED_Amount_Approved = round(sum(ED_Amount_Approved), digits = 2))

# This summarize counts for Primary (Code_1) and Secondary (Code_n+1)
diag <- diag %>% group_by(DIAG_CODE, ICD_VERSION) %>% 
  summarize(P_NumberOfClaims = sum(NumberOfClaims), 
            P_NumberOfED = sum(NumberOfED),
            P_NumberOfOTPT = sum(NumberOfOTPT),
            P_NumberOfINPT = sum(NumberOfINPT)) %>% 
  select(DIAG_CODE, ICD_VERSION,starts_with('P_'))

diag2 <- diag2 %>% group_by(DIAG_CODE, ICD_VERSION) %>% 
  summarize(S_NumberOfClaims = sum(NumberOfClaims), 
            S_NumberOfED = sum(NumberOfED),
            S_NumberOfOTPT = sum(NumberOfOTPT),
            S_NumberOfINPT = sum(NumberOfINPT)) %>% 
  select(DIAG_CODE, ICD_VERSION, starts_with('S_'))

diag_full <- diag_full %>% 
  left_join(diag, by = c('DIAG_CODE', 'ICD_VERSION')) %>% 
  left_join(diag2, by = c('DIAG_CODE', 'ICD_VERSION'))

# Calculate Prevenatable and Not Needed ED visits, only for codes with a Score
code_final <- diag_full %>% 
  mutate(NumberOfED_NN = ifelse(!is.na(ED_NOT_NEEDED_PROP), round(ED_NOT_NEEDED_PROP * NumberOfED), -1),
         NumberOfED_PRV = ifelse(!is.na(PREVENTABILITY), round(PREVENTABILITY * NumberOfED), -1))

# Import CCS crosswalk
cx <- read_csv('Data/ccs_diag_xwalk.csv')

# Join dataset to CCS Crosswalk
code_final2 <- code_final %>% 
  left_join(cx, by = c('DIAG_CODE','ICD_VERSION'))

# Rearrange columns
code_final2 <- code_final2 %>% 
  select(DIAG_CODE,ICD_VERSION,DIAG_CODE_DESC,CCS_CATEGORY, CCS_CATEGORY_DESC,
         everything())

# Determine is diagnosis is considered chronic
code_final3 <- find_chronic_diagnosis(code_final2)

write.csv(code_final3, 'Data/diagnosis_analysis.csv')
