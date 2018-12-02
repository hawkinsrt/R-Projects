library(tidyverse)
library(naniar)
source("Custom Functions.R")

claims13 <- read_csv('Data/FY13 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims14 <- read_csv('Data/FY14 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims15 <- read_csv('Data/FY15 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims16 <- read_csv('Data/FY16 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims17 <- read_csv('Data/FY17 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))
claims18 <- read_csv('Data/FY18 ClaimHeaderClean.csv', col_names = FALSE, na = c("NULL"))

clean13 <- clean(claims13)
clean14 <- clean(claims14)
clean15 <- clean(claims15)
clean16 <- clean(claims16)
clean17 <- clean(claims17)
clean18 <- clean(claims18)

rm(claims13, claims14,claims15, claims16, claims17, claims18)

saveRDS(clean13, "Data/clean13.RDS")
saveRDS(clean14, "Data/clean14.RDS")
saveRDS(clean15, "Data/clean15.RDS")
saveRDS(clean16, "Data/clean16.RDS")
saveRDS(clean17, "Data/clean17.RDS")
saveRDS(clean18, "Data/clean18.RDS")

# clean13 <- readRDS("Data/clean13.RDS")
# clean14 <- readRDS("Data/clean14.RDS")
# clean15 <- readRDS("Data/clean15.RDS")
# clean16 <- readRDS("Data/clean16.RDS")
# clean17 <- readRDS("Data/clean17.RDS")
# clean18 <- readRDS("Data/clean18.RDS")

claimsCleanFull <- bind_rows(clean13, clean14, clean15, clean16, clean17, clean18)

rm(clean13,clean14, clean15, clean16, clean17, clean18)

claimsCleanFull <- claimsCleanFull %>% mutate_if(is.character, as.factor)

glimpse(claimsCleanFull)
summary(claimsCleanFull)

#saveRDS(claimsCleanFull, "Data/claimsCleanFull.RDS")

sapply(claimsCleanFull, nlevels)



