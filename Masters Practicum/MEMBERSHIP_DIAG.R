# 
# CODE = Diagnosis code as is with decimal
# CODE_FULL = Diagnosis code with decimal removed
# ED_NOT_NEEDED_PROP = Percent of cases where ED is not needed
# PREVENTABILITY = Percent of cases ED would have been prevented with prior intervention
# UNCLASSIFIED_ED = Percent of cases where ED score cannot be classified. IDK this one is weird.*
#   NumberOfClaims = Total count of claims CODE appears in
# NumberOfED = Total count of claims CODE appear in where SERVICE_TYPE == 'ED'
# NumberOfOTPT = Total count of claims CODE appear in where SERVICE_TYPE == 'OTPT'
# NumberOfINPT = Total count of claims CODE appear in where SERVICE_TYPE == 'INPT'
# AverageAGE = Average age of members who get diagnosed with this CODE
# Pct_Female = Percent female who get diagnosed with this CODE
# CODE_DESCRIPTION = ICD 9/10 description
# CHRONIC_INDICATOR = Yes/No/NA is Diagnosis considered chronic
# BODY_SYSTEM = 1-18 Categorization of CODE
# BODY_SYSTEM_DESC = Description of the 1-18 codes
# NumberOfED_NN = NumberOfED * ED_NOT_NEEDED_Prop**
#   NumberOfED_PRV = NumberOfED * PREVENTABILITY**
#   *You can interpret this if UNCLASSIFED_ED == 1 then VP has not created a scores for CODE
# ** If value = -1 then score was NA for CODE.  All CODE_2 - CODE-18*** that never appeared as a CODE_1 will not have a score and thus will have -1s for these columns. Some CODE_1s have NA scores as well but very few < 900
# ***To create this dataset I first gathered all 18 CODEs longways then aggreated until each line is a unique code.

library(dplyr)
library(tidyverse)
library(reshape2)

# Import claims table and clean diagnostic codes
claimsFull <- read_rds("Data/claimsCleanFull.RDS")

all_diag_df <- claimsFull %>% 
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

# Add new un-punctuated codes and ICD version to claims data
claimsFull <- claimsFull %>%
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
         ICD_V_7 = ifelse(CODE_7 %in% icd9vector, 9,10),
         DIAG_CODE_8 = str_remove_all(CODE_8, pattern = "[[:punct:]]"),
         ICD_V_8 = ifelse(CODE_8 %in% icd9vector, 9,10),
         DIAG_CODE_9 = str_remove_all(CODE_9, pattern = "[[:punct:]]"),
         ICD_V_9 = ifelse(CODE_9 %in% icd9vector, 9,10),
         DIAG_CODE_10 = str_remove_all(CODE_10, pattern = "[[:punct:]]"),
         ICD_V_10 = ifelse(CODE_10 %in% icd9vector, 9,10),
         DIAG_CODE_11 = str_remove_all(CODE_11, pattern = "[[:punct:]]"),
         ICD_V_11 = ifelse(CODE_11 %in% icd9vector, 9,10),
         DIAG_CODE_12 = str_remove_all(CODE_12, pattern = "[[:punct:]]"),
         ICD_V_12 = ifelse(CODE_12 %in% icd9vector, 9,10),
         DIAG_CODE_13 = str_remove_all(CODE_13, pattern = "[[:punct:]]"),
         ICD_V_13 = ifelse(CODE_13 %in% icd9vector, 9,10),
         DIAG_CODE_14 = str_remove_all(CODE_14, pattern = "[[:punct:]]"),
         ICD_V_14 = ifelse(CODE_14 %in% icd9vector, 9,10),
         DIAG_CODE_15 = str_remove_all(CODE_15, pattern = "[[:punct:]]"),
         ICD_V_15 = ifelse(CODE_15 %in% icd9vector, 9,10),
         DIAG_CODE_16 = str_remove_all(CODE_16, pattern = "[[:punct:]]"),
         ICD_V_16 = ifelse(CODE_16 %in% icd9vector, 9,10),
         DIAG_CODE_17 = str_remove_all(CODE_17, pattern = "[[:punct:]]"),
         ICD_V_17 = ifelse(CODE_17 %in% icd9vector, 9,10),
         DIAG_CODE_18 = str_remove_all(CODE_18, pattern = "[[:punct:]]"),
         ICD_V_18 = ifelse(CODE_18 %in% icd9vector, 9,10))

# Import and prepare diagnosis data
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


#Create columns based on each CODE_X to indicate which body system the code pertains to
claims <- claimsFull %>%
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
         CHRONIC_7 = ifelse(paste(DIAG_CODE_7, ICD_V_7, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_8 = ifelse(paste(DIAG_CODE_8, ICD_V_8, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_8 = ifelse(paste(DIAG_CODE_8, ICD_V_8, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_9 = ifelse(paste(DIAG_CODE_9, ICD_V_9, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_9 = ifelse(paste(DIAG_CODE_9, ICD_V_9, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_10 = ifelse(paste(DIAG_CODE_10, ICD_V_10, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_10 = ifelse(paste(DIAG_CODE_10, ICD_V_10, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_11 = ifelse(paste(DIAG_CODE_11, ICD_V_11, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_11 = ifelse(paste(DIAG_CODE_11, ICD_V_11, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_12 = ifelse(paste(DIAG_CODE_12, ICD_V_12, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_12 = ifelse(paste(DIAG_CODE_12, ICD_V_12, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_13 = ifelse(paste(DIAG_CODE_13, ICD_V_13, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_13 = ifelse(paste(DIAG_CODE_13, ICD_V_13, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_14 = ifelse(paste(DIAG_CODE_14, ICD_V_14, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_14 = ifelse(paste(DIAG_CODE_14, ICD_V_14, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_15 = ifelse(paste(DIAG_CODE_15, ICD_V_15, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_15 = ifelse(paste(DIAG_CODE_15, ICD_V_15, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_16 = ifelse(paste(DIAG_CODE_16, ICD_V_16, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_16 = ifelse(paste(DIAG_CODE_16, ICD_V_16, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_17 = ifelse(paste(DIAG_CODE_17, ICD_V_17, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_17 = ifelse(paste(DIAG_CODE_17, ICD_V_17, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA),
         BODY_SYS_18 = ifelse(paste(DIAG_CODE_18, ICD_V_18, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$BODY_SYSTEM, NA),
         CHRONIC_18 = ifelse(paste(DIAG_CODE_18, ICD_V_18, sep = "-") %in% paste(bodysys$DIAG_CODE, bodysys$ICD_VERSION, sep = "-"), bodysys$CHRONIC_INDICATOR, NA))
rm(claimsFull)
gc()

#Creates a boolean column for each body system (0-18 and 'NONE') based on the claim row stating if there was a chronic condition for that system in the claim
claims <- claims %>%
  mutate(
    CHRONIC_BDSYS_0 = ifelse((BODY_SYS_1 == 1 & CHRONIC_1 == 2) | (BODY_SYS_2 == 1 & CHRONIC_2 == 2) | (BODY_SYS_3 == 1 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 1 & CHRONIC_4 == 2) | (BODY_SYS_5 == 1 & CHRONIC_5 == 2) | (BODY_SYS_6 == 1 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 1 & CHRONIC_7 == 2) | (BODY_SYS_8 == 1 & CHRONIC_8 == 2) | (BODY_SYS_9 == 1 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 1 & CHRONIC_10 == 2) | (BODY_SYS_11 == 1 & CHRONIC_11 == 2) | (BODY_SYS_12 == 1 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 1 & CHRONIC_13 == 2) | (BODY_SYS_14 == 1 & CHRONIC_14 == 2) | (BODY_SYS_15 == 1 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 1 & CHRONIC_16 == 2) | (BODY_SYS_17 == 1 & CHRONIC_17 == 2) | (BODY_SYS_18 == 1 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_1 = ifelse((BODY_SYS_1 == 2 & CHRONIC_1 == 2) | (BODY_SYS_2 == 2 & CHRONIC_2 == 2) | (BODY_SYS_3 == 2 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 2 & CHRONIC_4 == 2) | (BODY_SYS_5 == 2 & CHRONIC_5 == 2) | (BODY_SYS_6 == 2 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 2 & CHRONIC_7 == 2) | (BODY_SYS_8 == 2 & CHRONIC_8 == 2) | (BODY_SYS_9 == 2 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 2 & CHRONIC_10 == 2) | (BODY_SYS_11 == 2 & CHRONIC_11 == 2) | (BODY_SYS_12 == 2 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 2 & CHRONIC_13 == 2) | (BODY_SYS_14 == 2 & CHRONIC_14 == 2) | (BODY_SYS_15 == 2 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 2 & CHRONIC_16 == 2) | (BODY_SYS_17 == 2 & CHRONIC_17 == 2) | (BODY_SYS_18 == 2 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_2 = ifelse((BODY_SYS_1 == 3 & CHRONIC_1 == 2) | (BODY_SYS_2 == 3 & CHRONIC_2 == 2) | (BODY_SYS_3 == 3 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 3 & CHRONIC_4 == 2) | (BODY_SYS_5 == 3 & CHRONIC_5 == 2) | (BODY_SYS_6 == 3 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 3 & CHRONIC_7 == 2) | (BODY_SYS_8 == 3 & CHRONIC_8 == 2) | (BODY_SYS_9 == 3 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 3 & CHRONIC_10 == 2) | (BODY_SYS_11 == 3 & CHRONIC_11 == 2) | (BODY_SYS_12 == 3 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 3 & CHRONIC_13 == 2) | (BODY_SYS_14 == 3 & CHRONIC_14 == 2) | (BODY_SYS_15 == 3 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 3 & CHRONIC_16 == 2) | (BODY_SYS_17 == 3 & CHRONIC_17 == 2) | (BODY_SYS_18 == 3 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_3 = ifelse((BODY_SYS_1 == 4 & CHRONIC_1 == 2) | (BODY_SYS_2 == 4 & CHRONIC_2 == 2) | (BODY_SYS_3 == 4 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 4 & CHRONIC_4 == 2) | (BODY_SYS_5 == 4 & CHRONIC_5 == 2) | (BODY_SYS_6 == 4 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 4 & CHRONIC_7 == 2) | (BODY_SYS_8 == 4 & CHRONIC_8 == 2) | (BODY_SYS_9 == 4 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 4 & CHRONIC_10 == 2) | (BODY_SYS_11 == 4 & CHRONIC_11 == 2) | (BODY_SYS_12 == 4 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 4 & CHRONIC_13 == 2) | (BODY_SYS_14 == 4 & CHRONIC_14 == 2) | (BODY_SYS_15 == 4 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 4 & CHRONIC_16 == 2) | (BODY_SYS_17 == 4 & CHRONIC_17 == 2) | (BODY_SYS_18 == 4 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_4 = ifelse((BODY_SYS_1 == 5 & CHRONIC_1 == 2) | (BODY_SYS_2 == 5 & CHRONIC_2 == 2) | (BODY_SYS_3 == 5 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 5 & CHRONIC_4 == 2) | (BODY_SYS_5 == 5 & CHRONIC_5 == 2) | (BODY_SYS_6 == 5 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 5 & CHRONIC_7 == 2) | (BODY_SYS_8 == 5 & CHRONIC_8 == 2) | (BODY_SYS_9 == 5 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 5 & CHRONIC_10 == 2) | (BODY_SYS_11 == 5 & CHRONIC_11 == 2) | (BODY_SYS_12 == 5 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 5 & CHRONIC_13 == 2) | (BODY_SYS_14 == 5 & CHRONIC_14 == 2) | (BODY_SYS_15 == 5 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 5 & CHRONIC_16 == 2) | (BODY_SYS_17 == 5 & CHRONIC_17 == 2) | (BODY_SYS_18 == 5 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_5 = ifelse((BODY_SYS_1 == 6 & CHRONIC_1 == 2) | (BODY_SYS_2 == 6 & CHRONIC_2 == 2) | (BODY_SYS_3 == 6 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 6 & CHRONIC_4 == 2) | (BODY_SYS_5 == 6 & CHRONIC_5 == 2) | (BODY_SYS_6 == 6 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 6 & CHRONIC_7 == 2) | (BODY_SYS_8 == 6 & CHRONIC_8 == 2) | (BODY_SYS_9 == 6 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 6 & CHRONIC_10 == 2) | (BODY_SYS_11 == 6 & CHRONIC_11 == 2) | (BODY_SYS_12 == 6 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 6 & CHRONIC_13 == 2) | (BODY_SYS_14 == 6 & CHRONIC_14 == 2) | (BODY_SYS_15 == 6 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 6 & CHRONIC_16 == 2) | (BODY_SYS_17 == 6 & CHRONIC_17 == 2) | (BODY_SYS_18 == 6 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_6 = ifelse((BODY_SYS_1 == 7 & CHRONIC_1 == 2) | (BODY_SYS_2 == 7 & CHRONIC_2 == 2) | (BODY_SYS_3 == 7 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 7 & CHRONIC_4 == 2) | (BODY_SYS_5 == 7 & CHRONIC_5 == 2) | (BODY_SYS_6 == 7 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 7 & CHRONIC_7 == 2) | (BODY_SYS_8 == 7 & CHRONIC_8 == 2) | (BODY_SYS_9 == 7 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 7 & CHRONIC_10 == 2) | (BODY_SYS_11 == 7 & CHRONIC_11 == 2) | (BODY_SYS_12 == 7 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 7 & CHRONIC_13 == 2) | (BODY_SYS_14 == 7 & CHRONIC_14 == 2) | (BODY_SYS_15 == 7 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 7 & CHRONIC_16 == 2) | (BODY_SYS_17 == 7 & CHRONIC_17 == 2) | (BODY_SYS_18 == 7 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_7 = ifelse((BODY_SYS_1 == 8 & CHRONIC_1 == 2) | (BODY_SYS_2 == 8 & CHRONIC_2 == 2) | (BODY_SYS_3 == 8 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 8 & CHRONIC_4 == 2) | (BODY_SYS_5 == 8 & CHRONIC_5 == 2) | (BODY_SYS_6 == 8 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 8 & CHRONIC_7 == 2) | (BODY_SYS_8 == 8 & CHRONIC_8 == 2) | (BODY_SYS_9 == 8 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 8 & CHRONIC_10 == 2) | (BODY_SYS_11 == 8 & CHRONIC_11 == 2) | (BODY_SYS_12 == 8 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 8 & CHRONIC_13 == 2) | (BODY_SYS_14 == 8 & CHRONIC_14 == 2) | (BODY_SYS_15 == 8 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 8 & CHRONIC_16 == 2) | (BODY_SYS_17 == 8 & CHRONIC_17 == 2) | (BODY_SYS_18 == 8 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_8 = ifelse((BODY_SYS_1 == 9 & CHRONIC_1 == 2) | (BODY_SYS_2 == 9 & CHRONIC_2 == 2) | (BODY_SYS_3 == 9 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 9 & CHRONIC_4 == 2) | (BODY_SYS_5 == 9 & CHRONIC_5 == 2) | (BODY_SYS_6 == 9 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 9 & CHRONIC_7 == 2) | (BODY_SYS_8 == 9 & CHRONIC_8 == 2) | (BODY_SYS_9 == 9 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 9 & CHRONIC_10 == 2) | (BODY_SYS_11 == 9 & CHRONIC_11 == 2) | (BODY_SYS_12 == 9 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 9 & CHRONIC_13 == 2) | (BODY_SYS_14 == 9 & CHRONIC_14 == 2) | (BODY_SYS_15 == 9 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 9 & CHRONIC_16 == 2) | (BODY_SYS_17 == 9 & CHRONIC_17 == 2) | (BODY_SYS_18 == 9 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_9 = ifelse((BODY_SYS_1 == 10 & CHRONIC_1 == 2) | (BODY_SYS_2 == 10 & CHRONIC_2 == 2) | (BODY_SYS_3 == 10 & CHRONIC_3 == 2) |
                               (BODY_SYS_4 == 10 & CHRONIC_4 == 2) | (BODY_SYS_5 == 10 & CHRONIC_5 == 2) | (BODY_SYS_6 == 10 & CHRONIC_6 == 2) |
                               (BODY_SYS_7 == 10 & CHRONIC_7 == 2) | (BODY_SYS_8 == 10 & CHRONIC_8 == 2) | (BODY_SYS_9 == 10 & CHRONIC_9 == 2) |
                               (BODY_SYS_10 == 10 & CHRONIC_10 == 2) | (BODY_SYS_11 == 10 & CHRONIC_11 == 2) | (BODY_SYS_12 == 10 & CHRONIC_12 == 2) |
                               (BODY_SYS_13 == 10 & CHRONIC_13 == 2) | (BODY_SYS_14 == 10 & CHRONIC_14 == 2) | (BODY_SYS_15 == 10 & CHRONIC_15 == 2) |
                               (BODY_SYS_16 == 10 & CHRONIC_16 == 2) | (BODY_SYS_17 == 10 & CHRONIC_17 == 2) | (BODY_SYS_18 == 10 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_10 = ifelse((BODY_SYS_1 == 11 & CHRONIC_1 == 2) | (BODY_SYS_2 == 11 & CHRONIC_2 == 2) | (BODY_SYS_3 == 11 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 11 & CHRONIC_4 == 2) | (BODY_SYS_5 == 11 & CHRONIC_5 == 2) | (BODY_SYS_6 == 11 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 11 & CHRONIC_7 == 2) | (BODY_SYS_8 == 11 & CHRONIC_8 == 2) | (BODY_SYS_9 == 11 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 11 & CHRONIC_10 == 2) | (BODY_SYS_11 == 11 & CHRONIC_11 == 2) | (BODY_SYS_12 == 11 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 11 & CHRONIC_13 == 2) | (BODY_SYS_14 == 11 & CHRONIC_14 == 2) | (BODY_SYS_15 == 11 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 11 & CHRONIC_16 == 2) | (BODY_SYS_17 == 11 & CHRONIC_17 == 2) | (BODY_SYS_18 == 11 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_11 = ifelse((BODY_SYS_1 == 12 & CHRONIC_1 == 2) | (BODY_SYS_2 == 12 & CHRONIC_2 == 2) | (BODY_SYS_3 == 12 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 12 & CHRONIC_4 == 2) | (BODY_SYS_5 == 12 & CHRONIC_5 == 2) | (BODY_SYS_6 == 12 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 12 & CHRONIC_7 == 2) | (BODY_SYS_8 == 12 & CHRONIC_8 == 2) | (BODY_SYS_9 == 12 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 12 & CHRONIC_10 == 2) | (BODY_SYS_11 == 12 & CHRONIC_11 == 2) | (BODY_SYS_12 == 12 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 12 & CHRONIC_13 == 2) | (BODY_SYS_14 == 12 & CHRONIC_14 == 2) | (BODY_SYS_15 == 12 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 12 & CHRONIC_16 == 2) | (BODY_SYS_17 == 12 & CHRONIC_17 == 2) | (BODY_SYS_18 == 12 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_12 = ifelse((BODY_SYS_1 == 13 & CHRONIC_1 == 2) | (BODY_SYS_2 == 13 & CHRONIC_2 == 2) | (BODY_SYS_3 == 13 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 13 & CHRONIC_4 == 2) | (BODY_SYS_5 == 13 & CHRONIC_5 == 2) | (BODY_SYS_6 == 13 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 13 & CHRONIC_7 == 2) | (BODY_SYS_8 == 13 & CHRONIC_8 == 2) | (BODY_SYS_9 == 13 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 13 & CHRONIC_10 == 2) | (BODY_SYS_11 == 13 & CHRONIC_11 == 2) | (BODY_SYS_12 == 13 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 13 & CHRONIC_13 == 2) | (BODY_SYS_14 == 13 & CHRONIC_14 == 2) | (BODY_SYS_15 == 13 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 13 & CHRONIC_16 == 2) | (BODY_SYS_17 == 13 & CHRONIC_17 == 2) | (BODY_SYS_18 == 13 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_13 = ifelse((BODY_SYS_1 == 14 & CHRONIC_1 == 2) | (BODY_SYS_2 == 14 & CHRONIC_2 == 2) | (BODY_SYS_3 == 14 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 14 & CHRONIC_4 == 2) | (BODY_SYS_5 == 14 & CHRONIC_5 == 2) | (BODY_SYS_6 == 14 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 14 & CHRONIC_7 == 2) | (BODY_SYS_8 == 14 & CHRONIC_8 == 2) | (BODY_SYS_9 == 14 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 14 & CHRONIC_10 == 2) | (BODY_SYS_11 == 14 & CHRONIC_11 == 2) | (BODY_SYS_12 == 14 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 14 & CHRONIC_13 == 2) | (BODY_SYS_14 == 14 & CHRONIC_14 == 2) | (BODY_SYS_15 == 14 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 14 & CHRONIC_16 == 2) | (BODY_SYS_17 == 14 & CHRONIC_17 == 2) | (BODY_SYS_18 == 14 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_14 = ifelse((BODY_SYS_1 == 15 & CHRONIC_1 == 2) | (BODY_SYS_2 == 15 & CHRONIC_2 == 2) | (BODY_SYS_3 == 15 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 15 & CHRONIC_4 == 2) | (BODY_SYS_5 == 15 & CHRONIC_5 == 2) | (BODY_SYS_6 == 15 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 15 & CHRONIC_7 == 2) | (BODY_SYS_8 == 15 & CHRONIC_8 == 2) | (BODY_SYS_9 == 15 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 15 & CHRONIC_10 == 2) | (BODY_SYS_11 == 15 & CHRONIC_11 == 2) | (BODY_SYS_12 == 15 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 15 & CHRONIC_13 == 2) | (BODY_SYS_14 == 15 & CHRONIC_14 == 2) | (BODY_SYS_15 == 15 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 15 & CHRONIC_16 == 2) | (BODY_SYS_17 == 15 & CHRONIC_17 == 2) | (BODY_SYS_18 == 15 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_15 = ifelse((BODY_SYS_1 == 16 & CHRONIC_1 == 2) | (BODY_SYS_2 == 16 & CHRONIC_2 == 2) | (BODY_SYS_3 == 16 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 16 & CHRONIC_4 == 2) | (BODY_SYS_5 == 16 & CHRONIC_5 == 2) | (BODY_SYS_6 == 16 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 16 & CHRONIC_7 == 2) | (BODY_SYS_8 == 16 & CHRONIC_8 == 2) | (BODY_SYS_9 == 16 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 16 & CHRONIC_10 == 2) | (BODY_SYS_11 == 16 & CHRONIC_11 == 2) | (BODY_SYS_12 == 16 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 16 & CHRONIC_13 == 2) | (BODY_SYS_14 == 16 & CHRONIC_14 == 2) | (BODY_SYS_15 == 16 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 16 & CHRONIC_16 == 2) | (BODY_SYS_17 == 16 & CHRONIC_17 == 2) | (BODY_SYS_18 == 16 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_16 = ifelse((BODY_SYS_1 == 17 & CHRONIC_1 == 2) | (BODY_SYS_2 == 17 & CHRONIC_2 == 2) | (BODY_SYS_3 == 17 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 17 & CHRONIC_4 == 2) | (BODY_SYS_5 == 17 & CHRONIC_5 == 2) | (BODY_SYS_6 == 17 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 17 & CHRONIC_7 == 2) | (BODY_SYS_8 == 17 & CHRONIC_8 == 2) | (BODY_SYS_9 == 17 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 17 & CHRONIC_10 == 2) | (BODY_SYS_11 == 17 & CHRONIC_11 == 2) | (BODY_SYS_12 == 17 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 17 & CHRONIC_13 == 2) | (BODY_SYS_14 == 17 & CHRONIC_14 == 2) | (BODY_SYS_15 == 17 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 17 & CHRONIC_16 == 2) | (BODY_SYS_17 == 17 & CHRONIC_17 == 2) | (BODY_SYS_18 == 17 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_17 = ifelse((BODY_SYS_1 == 18 & CHRONIC_1 == 2) | (BODY_SYS_2 == 18 & CHRONIC_2 == 2) | (BODY_SYS_3 == 18 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 18 & CHRONIC_4 == 2) | (BODY_SYS_5 == 18 & CHRONIC_5 == 2) | (BODY_SYS_6 == 18 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 18 & CHRONIC_7 == 2) | (BODY_SYS_8 == 18 & CHRONIC_8 == 2) | (BODY_SYS_9 == 18 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 18 & CHRONIC_10 == 2) | (BODY_SYS_11 == 18 & CHRONIC_11 == 2) | (BODY_SYS_12 == 18 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 18 & CHRONIC_13 == 2) | (BODY_SYS_14 == 18 & CHRONIC_14 == 2) | (BODY_SYS_15 == 18 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 18 & CHRONIC_16 == 2) | (BODY_SYS_17 == 18 & CHRONIC_17 == 2) | (BODY_SYS_18 == 18 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_18 = ifelse((BODY_SYS_1 == 19 & CHRONIC_1 == 2) | (BODY_SYS_2 == 19 & CHRONIC_2 == 2) | (BODY_SYS_3 == 19 & CHRONIC_3 == 2) |
                                (BODY_SYS_4 == 19 & CHRONIC_4 == 2) | (BODY_SYS_5 == 19 & CHRONIC_5 == 2) | (BODY_SYS_6 == 19 & CHRONIC_6 == 2) |
                                (BODY_SYS_7 == 19 & CHRONIC_7 == 2) | (BODY_SYS_8 == 19 & CHRONIC_8 == 2) | (BODY_SYS_9 == 19 & CHRONIC_9 == 2) |
                                (BODY_SYS_10 == 19 & CHRONIC_10 == 2) | (BODY_SYS_11 == 19 & CHRONIC_11 == 2) | (BODY_SYS_12 == 19 & CHRONIC_12 == 2) |
                                (BODY_SYS_13 == 19 & CHRONIC_13 == 2) | (BODY_SYS_14 == 19 & CHRONIC_14 == 2) | (BODY_SYS_15 == 19 & CHRONIC_15 == 2) |
                                (BODY_SYS_16 == 19 & CHRONIC_16 == 2) | (BODY_SYS_17 == 19 & CHRONIC_17 == 2) | (BODY_SYS_18 == 19 & CHRONIC_18 == 2), 1, 0),
    CHRONIC_BDSYS_NONE = ifelse((BODY_SYS_1 == 20 & CHRONIC_1 == 2) | (BODY_SYS_2 == 20 & CHRONIC_2 == 2) | (BODY_SYS_3 == 20 & CHRONIC_3 == 2) |
                                  (BODY_SYS_4 == 20 & CHRONIC_4 == 2) | (BODY_SYS_5 == 20 & CHRONIC_5 == 2) | (BODY_SYS_6 == 20 & CHRONIC_6 == 2) |
                                  (BODY_SYS_7 == 20 & CHRONIC_7 == 2) | (BODY_SYS_8 == 20 & CHRONIC_8 == 2) | (BODY_SYS_9 == 20 & CHRONIC_9 == 2) |
                                  (BODY_SYS_10 == 20 & CHRONIC_10 == 2) | (BODY_SYS_11 == 20 & CHRONIC_11 == 2) | (BODY_SYS_12 == 20 & CHRONIC_12 == 2) |
                                  (BODY_SYS_13 == 20 & CHRONIC_13 == 2) | (BODY_SYS_14 == 20 & CHRONIC_14 == 2) | (BODY_SYS_15 == 20 & CHRONIC_15 == 2) |
                                  (BODY_SYS_16 == 20 & CHRONIC_16 == 2) | (BODY_SYS_17 == 20 & CHRONIC_17 == 2) | (BODY_SYS_18 == 20 & CHRONIC_18 == 2), 1, 0)
  )

#Turns NA's into 0s in the CHRONIC_BDSYS_ columns, ED_NN column, and turns other columns to factors
claims[,116:135][is.na(claims[,116:135])] <- 0
claims[,80:115] <- lapply(claims[,80:115], factor)
claims$ED_NOT_NEEDED_PROP[is.na(claims$ED_NOT_NEEDED_PROP)] <- 0

#Aggregated dataset based on member, sex, and age bin to identify chronic conditions in each body sytem by member
claims_sub <- claims %>%
  mutate(TARGET = ifelse(SERVICE_TYPE == "ED" & ED_NOT_NEEDED_PROP > 0.9, 1, 0)) %>%
  mutate(AGE_BIN = as.factor(cut(MEMBER_AGE, breaks = seq(0,90, by =10), right = FALSE))) %>%
  group_by(MRN_ALIAS, MEMBER_SEX, AGE_BIN) %>%
  summarize(TARGET = as.factor(max(TARGET)),
            PCP_V70_YEARLY_NONED = as.factor(ifelse((sum(PCP_ID == VENDOR_PROV_ID | (CODE_1 == "V70.0" & SERVICE_TYPE != "ED"))/n_distinct(YEAR))>=1,"Y","N")),
            CHRONIC_SYS_0 = as.factor(max(CHRONIC_BDSYS_0)),
            CHRONIC_SYS_1 = as.factor(max(CHRONIC_BDSYS_1)),
            CHRONIC_SYS_2 = as.factor(max(CHRONIC_BDSYS_2)),
            CHRONIC_SYS_3 = as.factor(max(CHRONIC_BDSYS_3)),
            CHRONIC_SYS_4 = as.factor(max(CHRONIC_BDSYS_4)),
            CHRONIC_SYS_5 = as.factor(max(CHRONIC_BDSYS_5)),
            CHRONIC_SYS_6 = as.factor(max(CHRONIC_BDSYS_6)),
            CHRONIC_SYS_7 = as.factor(max(CHRONIC_BDSYS_7)),
            CHRONIC_SYS_8 = as.factor(max(CHRONIC_BDSYS_8)),
            CHRONIC_SYS_9 = as.factor(max(CHRONIC_BDSYS_9)),
            CHRONIC_SYS_10 = as.factor(max(CHRONIC_BDSYS_10)),
            CHRONIC_SYS_11 = as.factor(max(CHRONIC_BDSYS_11)),
            CHRONIC_SYS_12 = as.factor(max(CHRONIC_BDSYS_12)),
            CHRONIC_SYS_13 = as.factor(max(CHRONIC_BDSYS_13)),
            CHRONIC_SYS_14 = as.factor(max(CHRONIC_BDSYS_14)),
            CHRONIC_SYS_15 = as.factor(max(CHRONIC_BDSYS_15)),
            CHRONIC_SYS_16 = as.factor(max(CHRONIC_BDSYS_16)),
            CHRONIC_SYS_17 = as.factor(max(CHRONIC_BDSYS_17)),
            CHRONIC_SYS_18 = as.factor(max(CHRONIC_BDSYS_18)),
            CHRONIC_SYS_NONE = as.factor(max(CHRONIC_BDSYS_NONE)))

#Only run if don't need claims data anymore:
# rm(claims)
# gc()

saveRDS(claims_sub, file = "CHRON_MBR.rds")





