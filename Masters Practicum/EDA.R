library(tidyverse)
library(naniar)
claims <- readRDS("Data/claimsCleanFull.RDS")
names(claims) <- header
glimpse(claims)
summary(claims)

claims %>% select('member', 'sex', 'age') %>% head()
map(claims, nlevels)

claims %>% select(patient.type, claim.type) %>% 
  table()

claims %>% select(bill.category,patient.type) %>% 
  table()

claimsSub <- claims %>% 
  select(member:status,code.1,place.of.service,bill.category:discharge.desc)
