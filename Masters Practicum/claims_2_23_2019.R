library(tidyverse)
df <- readRDS('Data/2013and2014.rds')

# claimsFull <- readRDS("Data/claimsCleanFull.RDS")
# run claimsFull_to_claimsSmall.R to create claimsCleanSmall.RDS
claims <- readRDS('Data/claimsCleanSmall.RDS')

#claimsDubs <- claims %>% 
 # filter(PLACE_OF_SERVICE_DESC == 'EMERGENCY ROOM - HOSPITAL' | SERVICE_TYPE == 'ED')

claims2 <- claims %>% 
  filter(PLACE_OF_SERVICE_DESC != 'EMERGENCY ROOM - HOSPITAL'| is.na(PLACE_OF_SERVICE_DESC)) %>% 
  filter(CLAIM_NUM %in% df$CLAIM_NUM)

claims3 <- claims2 %>% 
  group_by(MRN_ALIAS) %>% 
  mutate(NEXT_SERVICE = lead(SERVICE_TYPE, n = 1),
         LEAD_EPISODE = lead(EPISODE_SEQ, n = 1),
         LEAD_YEAR = lead(YEAR, n = 1)) %>% 
  select(MRN_ALIAS,SERVICE_TYPE, NEXT_SERVICE,MEMBER_SEX, MEMBER_AGE, 
         CLAIM_SEQ, EPISODE_SEQ, LEAD_EPISODE, YEAR, LEAD_YEAR,everything())

claims4 <- claims3 %>% 
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

claims5 <- claims4 %>% 
  select(CLAIM_NUM, AGE_GROUP, MEMBER_SEX, SERVICE_TYPE, EPISODE_SEQ, YEAR, MRN_ALIAS)

df2 <-  df %>% 
  left_join(claims5, by = 'CLAIM_NUM')

#Kyle Add for Chronic_BodySystems: 
#Clean diagnostic codes
all_diag_df <- df2 %>% 
  select(starts_with('CODE_'))

all_diag <- c(t(all_diag_df))

all_diag <- data.frame(table(all_diag))
rm(all_diag_df)
gc()

# Create vector of ICD-10 'V' codes
icdv <- str_subset(all_diag$all_diag, "[V]\\d{2}\\.") #Grab all codes that start with V
icd10v <-str_subset(icdv, "[X]") #ICD_10 "V" codes include "X"s

icd9 <- str_subset(all_diag$all_diag, "^\\d") #ICD-9 codes start with a number or a E or a V
icd9e <- str_subset(all_diag$all_diag, "[E]\\d{3}") #ICD-9 "E" codes have 3 digits before decimal

icd9Only <- all_diag %>%
  filter((all_diag %in% icd9) | (all_diag %in% icd9e) | (all_diag %in% icdv) & !(all_diag %in% icd10v))

icd9vector <- icd9Only$all_diag 
rm(icdv)
rm(icd10v)
rm(icd9)
rm(icd9e)
rm(icd9Only)
gc()

#Using diagnosis file to make Body system and Chronic Condition Dataset
diag <- read_csv("Data/diagnosis_final.csv",
                 col_types = cols(.default = "c"))


diag["CHRONIC_INDICATOR"][is.na(diag["CHRONIC_INDICATOR"])] <- 0
diag["BODY_SYSTEM"][is.na(diag["BODY_SYSTEM"])] <- 0
diag["BODY_SYSTEM_DESC"][diag[,"BODY_SYSTEM"] == 0,] <- "UNSPECIFIED"
diag["CCS_CATEGORY"][is.na(diag["CCS_CATEGORY"])] <- 0
diag["CCS_CATEGORY_DESC"][diag[,"CCS_CATEGORY"] == 0,] <- "UNSPECIFIED"

bodysys <- diag[, c("DIAG_CODE", "ICD_VERSION", "BODY_SYSTEM", "CCS_CATEGORY", "CHRONIC_INDICATOR")]
bodysys$CCS_CATEGORY <- as.factor(as.character(bodysys$CCS_CATEGORY))
bodysys$BODY_SYSTEM <- as.factor(as.character(bodysys$BODY_SYSTEM))
bodysys$CHRONIC_INDICATOR <- as.factor(as.character(bodysys$CHRONIC_INDICATOR))

#Remove Puncuation from df CODE_N and add ICD Version
df3 <- df2 %>%
mutate(DIAG_CODE_1 = str_remove_all(CODE_1, pattern = "[[:punct:]]"),
       ICD_V_1 = ifelse(CODE_1 %in% icd9vector, 9,10),
       DIAG_CODE_2 = str_remove_all(CODE_2, pattern = "[[:punct:]]"),
       ICD_V_2 = ifelse(CODE_2 %in% icd9vector, 9,10),
       DIAG_CODE_3 = str_remove_all(CODE_3, pattern = "[[:punct:]]"),
       ICD_V_3 = ifelse(CODE_3 %in% icd9vector, 9,10),
       DIAG_CODE_4 = str_remove_all(CODE_4, pattern = "[[:punct:]]"),
       ICD_V_4 = ifelse(CODE_4 %in% icd9vector, 9,10),
       DIAG_CODE_5 = str_remove_all(CODE_5, pattern = "[[:punct:]]"),
       ICD_V_5 = ifelse(CODE_5 %in% icd9vector, 9,10),
       DIAG_CODE_6 = str_remove_all(CODE_6, pattern = "[[:punct:]]"),
       ICD_V_6 = ifelse(CODE_6 %in% icd9vector, 9,10),
       DIAG_CODE_7 = str_remove_all(CODE_7, pattern = "[[:punct:]]"),
       ICD_V_7 = ifelse(CODE_7 %in% icd9vector, 9,10))

#Add Chronic and Bodysystem columns for each claim
df4 <- df3 %>%
  mutate(BODY_SYS_1 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_1 = ifelse(paste(DIAG_CODE_1, ICD_V_1, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_2 = ifelse(paste(DIAG_CODE_2, ICD_V_2, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_2 = ifelse(paste(DIAG_CODE_2, ICD_V_2, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_3 = ifelse(paste(DIAG_CODE_3, ICD_V_3, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_3 = ifelse(paste(DIAG_CODE_3, ICD_V_3, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_4 = ifelse(paste(DIAG_CODE_4, ICD_V_4, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_4 = ifelse(paste(DIAG_CODE_4, ICD_V_4, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_5 = ifelse(paste(DIAG_CODE_5, ICD_V_5, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_5 = ifelse(paste(DIAG_CODE_5, ICD_V_5, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_6 = ifelse(paste(DIAG_CODE_6, ICD_V_6, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_6 = ifelse(paste(DIAG_CODE_6, ICD_V_6, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_7 = ifelse(paste(DIAG_CODE_7, ICD_V_7, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_7 = ifelse(paste(DIAG_CODE_7, ICD_V_7, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA))


#Creates a column for each body system (0-18 and 'NONE') based on the claim row stating if there was a chronic condition for that system in the claim
df5 <- df4 %>%
  mutate(
    CHRONIC_BDSYS_0 = ifelse((BODY_SYS_1 == 1 & CHRONIC_1 == 2) | (BODY_SYS_2 == 1 & CHRONIC_2 == 2) | (BODY_SYS_3 == 1 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 1 & CHRONIC_4 == 2) | (BODY_SYS_5 == 1 & CHRONIC_5 == 2) | (BODY_SYS_6 == 1 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 1 & CHRONIC_7 == 2), "CHRONIC_BDSYS_0", NA),
    CHRONIC_BDSYS_1 = ifelse((BODY_SYS_1 == 2 & CHRONIC_1 == 2) | (BODY_SYS_2 == 2 & CHRONIC_2 == 2) | (BODY_SYS_3 == 2 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 2 & CHRONIC_4 == 2) | (BODY_SYS_5 == 2 & CHRONIC_5 == 2) | (BODY_SYS_6 == 2 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 2 & CHRONIC_7 == 2), "CHRONIC_BDSYS_1", NA),
    CHRONIC_BDSYS_2 = ifelse((BODY_SYS_1 == 3 & CHRONIC_1 == 2) | (BODY_SYS_2 == 3 & CHRONIC_2 == 2) | (BODY_SYS_3 == 3 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 3 & CHRONIC_4 == 2) | (BODY_SYS_5 == 3 & CHRONIC_5 == 2) | (BODY_SYS_6 == 3 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 3 & CHRONIC_7 == 2), "CHRONIC_BDSYS_2", NA),
    CHRONIC_BDSYS_3 = ifelse((BODY_SYS_1 == 4 & CHRONIC_1 == 2) | (BODY_SYS_2 == 4 & CHRONIC_2 == 2) | (BODY_SYS_3 == 4 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 4 & CHRONIC_4 == 2) | (BODY_SYS_5 == 4 & CHRONIC_5 == 2) | (BODY_SYS_6 == 4 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 4 & CHRONIC_7 == 2), "CHRONIC_BDSYS_3", NA),
    CHRONIC_BDSYS_4 = ifelse((BODY_SYS_1 == 5 & CHRONIC_1 == 2) | (BODY_SYS_2 == 5 & CHRONIC_2 == 2) | (BODY_SYS_3 == 5 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 5 & CHRONIC_4 == 2) | (BODY_SYS_5 == 5 & CHRONIC_5 == 2) | (BODY_SYS_6 == 5 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 5 & CHRONIC_7 == 2), "CHRONIC_BDSYS_4", NA),
    CHRONIC_BDSYS_5 = ifelse((BODY_SYS_1 == 6 & CHRONIC_1 == 2) | (BODY_SYS_2 == 6 & CHRONIC_2 == 2) | (BODY_SYS_3 == 6 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 6 & CHRONIC_4 == 2) | (BODY_SYS_5 == 6 & CHRONIC_5 == 2) | (BODY_SYS_6 == 6 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 6 & CHRONIC_7 == 2), "CHRONIC_BDSYS_5", NA),
    CHRONIC_BDSYS_6 = ifelse((BODY_SYS_1 == 7 & CHRONIC_1 == 2) | (BODY_SYS_2 == 7 & CHRONIC_2 == 2) | (BODY_SYS_3 == 7 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 7 & CHRONIC_4 == 2) | (BODY_SYS_5 == 7 & CHRONIC_5 == 2) | (BODY_SYS_6 == 7 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 7 & CHRONIC_7 == 2), "CHRONIC_BDSYS_6", NA),
    CHRONIC_BDSYS_7 = ifelse((BODY_SYS_1 == 8 & CHRONIC_1 == 2) | (BODY_SYS_2 == 8 & CHRONIC_2 == 2) | (BODY_SYS_3 == 8 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 8 & CHRONIC_4 == 2) | (BODY_SYS_5 == 8 & CHRONIC_5 == 2) | (BODY_SYS_6 == 8 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 8 & CHRONIC_7 == 2), "CHRONIC_BDSYS_7", NA),
    CHRONIC_BDSYS_8 = ifelse((BODY_SYS_1 == 9 & CHRONIC_1 == 2) | (BODY_SYS_2 == 9 & CHRONIC_2 == 2) | (BODY_SYS_3 == 9 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 9 & CHRONIC_4 == 2) | (BODY_SYS_5 == 9 & CHRONIC_5 == 2) | (BODY_SYS_6 == 9 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 9 & CHRONIC_7 == 2), "CHRONIC_BDSYS_8", NA),
    CHRONIC_BDSYS_9 = ifelse((BODY_SYS_1 == 10 & CHRONIC_1 == 2) | (BODY_SYS_2 == 10 & CHRONIC_2 == 2) | (BODY_SYS_3 == 10 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 10 & CHRONIC_4 == 2) | (BODY_SYS_5 == 10 & CHRONIC_5 == 2) | (BODY_SYS_6 == 10 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 10 & CHRONIC_7 == 2), "CHRONIC_BDSYS_9", NA),
    CHRONIC_BDSYS_10 = ifelse((BODY_SYS_1 == 11 & CHRONIC_1 == 2) | (BODY_SYS_2 == 11 & CHRONIC_2 == 2) | (BODY_SYS_3 == 11 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 11 & CHRONIC_4 == 2) | (BODY_SYS_5 == 11 & CHRONIC_5 == 2) | (BODY_SYS_6 == 11 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 11 & CHRONIC_7 == 2), "CHRONIC_BDSYS_10", NA),
    CHRONIC_BDSYS_11 = ifelse((BODY_SYS_1 == 12 & CHRONIC_1 == 2) | (BODY_SYS_2 == 12 & CHRONIC_2 == 2) | (BODY_SYS_3 == 12 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 12 & CHRONIC_4 == 2) | (BODY_SYS_5 == 12 & CHRONIC_5 == 2) | (BODY_SYS_6 == 12 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 12 & CHRONIC_7 == 2), "CHRONIC_BDSYS_11", NA),
    CHRONIC_BDSYS_12 = ifelse((BODY_SYS_1 == 13 & CHRONIC_1 == 2) | (BODY_SYS_2 == 13 & CHRONIC_2 == 2) | (BODY_SYS_3 == 13 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 13 & CHRONIC_4 == 2) | (BODY_SYS_5 == 13 & CHRONIC_5 == 2) | (BODY_SYS_6 == 13 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 13 & CHRONIC_7 == 2), "CHRONIC_BDSYS_12", NA),
    CHRONIC_BDSYS_13 = ifelse((BODY_SYS_1 == 14 & CHRONIC_1 == 2) | (BODY_SYS_2 == 14 & CHRONIC_2 == 2) | (BODY_SYS_3 == 14 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 14 & CHRONIC_4 == 2) | (BODY_SYS_5 == 14 & CHRONIC_5 == 2) | (BODY_SYS_6 == 14 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 14 & CHRONIC_7 == 2), "CHRONIC_BDSYS_13", NA),
    CHRONIC_BDSYS_14 = ifelse((BODY_SYS_1 == 15 & CHRONIC_1 == 2) | (BODY_SYS_2 == 15 & CHRONIC_2 == 2) | (BODY_SYS_3 == 15 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 15 & CHRONIC_4 == 2) | (BODY_SYS_5 == 15 & CHRONIC_5 == 2) | (BODY_SYS_6 == 15 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 15 & CHRONIC_7 == 2), "CHRONIC_BDSYS_14", NA),
    CHRONIC_BDSYS_15 = ifelse((BODY_SYS_1 == 16 & CHRONIC_1 == 2) | (BODY_SYS_2 == 16 & CHRONIC_2 == 2) | (BODY_SYS_3 == 16 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 16 & CHRONIC_4 == 2) | (BODY_SYS_5 == 16 & CHRONIC_5 == 2) | (BODY_SYS_6 == 16 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 16 & CHRONIC_7 == 2), "CHRONIC_BDSYS_15", NA),
    CHRONIC_BDSYS_16 = ifelse((BODY_SYS_1 == 17 & CHRONIC_1 == 2) | (BODY_SYS_2 == 17 & CHRONIC_2 == 2) | (BODY_SYS_3 == 17 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 17 & CHRONIC_4 == 2) | (BODY_SYS_5 == 17 & CHRONIC_5 == 2) | (BODY_SYS_6 == 17 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 17 & CHRONIC_7 == 2), "CHRONIC_BDSYS_16", NA),
    CHRONIC_BDSYS_17 = ifelse((BODY_SYS_1 == 18 & CHRONIC_1 == 2) | (BODY_SYS_2 == 18 & CHRONIC_2 == 2) | (BODY_SYS_3 == 18 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 18 & CHRONIC_4 == 2) | (BODY_SYS_5 == 18 & CHRONIC_5 == 2) | (BODY_SYS_6 == 18 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 18 & CHRONIC_7 == 2), "CHRONIC_BDSYS_17", NA),
    CHRONIC_BDSYS_18 = ifelse((BODY_SYS_1 == 19 & CHRONIC_1 == 2) | (BODY_SYS_2 == 19 & CHRONIC_2 == 2) | (BODY_SYS_3 == 19 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 19 & CHRONIC_4 == 2) | (BODY_SYS_5 == 19 & CHRONIC_5 == 2) | (BODY_SYS_6 == 19 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 19 & CHRONIC_7 == 2), "CHRONIC_BDSYS_18", NA),
    CHRONIC_BDSYS_NONE = ifelse((BODY_SYS_1 == 20 & CHRONIC_1 == 2) | (BODY_SYS_2 == 20 & CHRONIC_2 == 2) | (BODY_SYS_3 == 20 & CHRONIC_3 == 2) |
                                  (BODY_SYS_4 == 20 & CHRONIC_4 == 2) | (BODY_SYS_5 == 20 & CHRONIC_5 == 2) | (BODY_SYS_6 == 20 & CHRONIC_6 == 2) |
                                  (BODY_SYS_7 == 20 & CHRONIC_7 == 2), "CHRONIC_BDSYS_NONE", NA))

