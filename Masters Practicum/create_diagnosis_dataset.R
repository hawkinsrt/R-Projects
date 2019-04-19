library(tidyverse)
source('Custom Functions.R')

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
  mutate(CODE_FULL = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
         IS_ED = (SERVICE_TYPE == 'ED'),
         IS_OTPT = (SERVICE_TYPE == 'OTPT'),
         IS_INPT = (SERVICE_TYPE == 'INPT'),
         IS_F = (MEMBER_SEX == 'F'),
         ED_Amount_Approved = ifelse(SERVICE_TYPE == 'ED', APPROVED_AMT, 0),
         ICD_VERSION = ifelse(CODE_1 %in% icd9vector, 9,10)) %>% 
  group_by(CODE_FULL, ICD_VERSION)%>%
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
  mutate(CODE_FULL = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
         IS_ED = (SERVICE_TYPE == 'ED'),
         IS_OTPT = (SERVICE_TYPE == 'OTPT'),
         IS_INPT = (SERVICE_TYPE == 'INPT'),
         IS_F = (MEMBER_SEX == 'F'),
    ED_Amount_Approved = ifelse(SERVICE_TYPE == 'ED', APPROVED_AMT, 0),
    ICD_VERSION = ifelse(CODE_1 %in% icd9vector, 9,10)) %>% 
    group_by(CODE_FULL, ICD_VERSION)%>%
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
  select(CODE_FULL,ICD_VERSION,ED_NOT_NEEDED_PROP,PREVENTABILITY,UNCLASSIFIED_ED,everything())

#Do the columns match up
names(diag) == names(diag2)

diag_full <- diag %>% 
  bind_rows(diag2) %>% 
  group_by(CODE_FULL, ICD_VERSION) %>% 
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

cx <- read_csv('Data/ccs_diag_xwalk.csv')

names(cx)[names(cx) == 'DIAG_CODE'] <- 'CODE_FULL'

diag3 <- diag_full %>% 
  inner_join(cx)

code_final <- diag_full %>% 
  mutate(NumberOfED_NN = ifelse(!is.na(ED_NOT_NEEDED_PROP), round(ED_NOT_NEEDED_PROP * NumberOfED), -1),
         NumberOfED_PRV = ifelse(!is.na(PREVENTABILITY), round(PREVENTABILITY * NumberOfED), -1))

code_final2 <- code_final %>% 
  left_join(cx, by = c('CODE_FULL' = 'CODE_FULL', 'ICD_VERSION' = 'ICD_VERSION'))

code_final2 <- code_final2 %>% 
  select('DIAG_CODE' = CODE_FULL,ICD_VERSION,DIAG_CODE_DESC,CCS_CATEGORY, CCS_CATEGORY_DESC,
         everything())

code_final3 <- find_chronic_diagnosis(code_final2)

write.csv(code_final3, 'Data/diagnosis_final.csv')