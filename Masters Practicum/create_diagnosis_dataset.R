library(tidyverse)
source('Custom Functions.R')

claims <- read_rds('Data/claimscleanSmall.RDS')

icd9vector <- icd9_vector(claims)

claims <- transform_claims(claims, save = FALSE)

diag <-claims %>%
  select(CODE_1, SERVICE_TYPE, ED_NOT_NEEDED_PROP, PREVENTABILITY,UNCLASSIFIED_ED,
         MEMBER_AGE, MEMBER_SEX, APPROVED_AMT) %>%
  mutate(CODE_FULL = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
         IS_ED = (SERVICE_TYPE == 'ED'),
         IS_OTPT = (SERVICE_TYPE == 'OTPT'),
         IS_INPT = (SERVICE_TYPE == 'INPT'),
         IS_F = (MEMBER_SEX == 'F'),
         ICD_VERSION = ifelse(CODE_1 %in% icd9vector, 9,10)) %>% 
  group_by(CODE_FULL, ICD_VERSION)%>%
  summarize(ED_NOT_NEEDED_PROP = mean(ED_NOT_NEEDED_PROP, na.rm = TRUE), PREVENTABILITY = mean(PREVENTABILITY,na.rm = TRUE),
            UNCLASSIFIED_ED = mean(UNCLASSIFIED_ED,na.rm = TRUE),NumberOfClaims = n(),
            NumberOfED = sum(IS_ED),
            NumberOfOTPT = sum(IS_OTPT),
            NumberOfINPT = sum(IS_INPT),
            AverageAGE = mean(MEMBER_AGE, na.rm = TRUE),
            Pct_Female = mean(IS_F),
            Amount_Approved = sum(APPROVED_AMT))

claims2 <- claims %>% 
  select(CLAIM_NUM, starts_with('CODE_'))

claims2[2:8] <- lapply(claims2[2:8], is.na)
claims2[2:8] <- lapply(claims2[2:8], as.numeric)

claims <- claims2 %>% 
  mutate(Total = 7 - rowSums(.[2:8])) %>% 
  select(CLAIM_NUM, Total) %>% 
  left_join(claims)

rm(claims2)

claims <- claims  %>% 
  mutate(APPROVED_AMT = APPROVED_AMT/Total)

diag2 <- claims %>% 
  select(starts_with('CODE_'), -CODE_1, SERVICE_TYPE,MEMBER_AGE, MEMBER_SEX, APPROVED_AMT) %>% 
  gather(starts_with('CODE_'), key = 'KEY', value = 'CODE_1') %>% 
  filter(!is.na(CODE_1)) %>% 
  select(-KEY) %>%   
  mutate(CODE_FULL = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
    IS_ED = (SERVICE_TYPE == 'ED'),
    IS_OTPT = (SERVICE_TYPE == 'OTPT'),
    IS_INPT = (SERVICE_TYPE == 'INPT'),
    IS_F = (MEMBER_SEX == 'F'),
    ICD_VERSION = ifelse(CODE_1 %in% icd9vector, 9,10)) %>% 
    group_by(CODE_FULL, ICD_VERSION)%>%
    summarize(NumberOfClaims = n(),
    NumberOfED = sum(IS_ED),
    NumberOfOTPT = sum(IS_OTPT),
    NumberOfINPT = sum(IS_INPT),
    AverageAGE = mean(MEMBER_AGE, na.rm = TRUE),
    Pct_Female = mean(IS_F), 
    Amount_Approved = sum(APPROVED_AMT))

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
  Amount_Approved_Mean = round(mean(Amount_Approved), digits = 2))

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

glimpse(code_final2)
summary(code_final2)

code_final3 <- code_final2 %>% 
  mutate(TARGET_CONDITION = case_when(
    str_detect(DIAG_CODE, '^F3|^296') ~ 'MoodDisorder', #Manic, Depressive, Bipolar
    str_detect(DIAG_CODE, '^F2|^295') ~ 'Psychoses',
    str_detect(DIAG_CODE, '^J45|^493') ~ 'Asthma',
    str_detect(DIAG_CODE, '^416|^49[012456]|^50[0-5]|^5064|^508[18]|^I27[89]|^J4[0-4]|^J4[67]|^J6[0-7]|^J684|^J70[13]') ~ 'COPD',
    str_detect(DIAG_CODE, '^250|^E1[013]') ~ 'Diabetes',
    str_detect(DIAG_CODE, '^428|^I50') ~ 'HeartFailure',
    str_detect(DIAG_CODE, '^40[1-5]|^I1[0-5]') ~ 'Hypertension',
    str_detect(DIAG_CODE, '^290|^2941|^3312|^F0[123]') ~ 'Dementia',
    str_detect(DIAG_CODE, '^345|^G40') ~ 'Epilepsy',
    str_detect(DIAG_CODE, '^5856|N186') ~ 'ESRD', # End Stage Renal Disease
    str_detect(DIAG_CODE, '^41[0-4]|I25') ~ 'IHD', # Ischemic Heart Disease
    str_detect(DIAG_CODE, '^85[1-4]|^S06') ~ 'BrainInjury',
    str_detect(DIAG_CODE, '^3623|^430|^431|^433x1|^434x1|^43[56]|^G45[012389]|^H341|^I6[0134]') ~ 'TIA',
    str_detect(DIAG_CODE, '^2652|^291[12356789]|^303[09]|^3050|^3575|^4255|^5353|^571[0_3]|^980|^V113|^F1[0-9]
|^E52|^G621|^I426|^K292|^K70[039]|^T51|^Z502|^Z714|^Z721') ~ 'SubstanceAbuse'
  )) 

write.csv(code_final3, 'Data/diagnosis_final.csv')
