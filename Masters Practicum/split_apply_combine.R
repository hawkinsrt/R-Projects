# used for testing purposes, faster than rouces claimsFull_to_claimsSmall.R
library(tidyverse)
claims <- read_rds("Data/claimsCleanSmall.RDS")
claims<- read_rds("Data/claimsVerySmall.RDS")
#claims <- slice(claims, 1:1000000)
#saveRDS(claims,'Data/claimsVerySmall.rds')

source('Custom Functions.R')

createClaimsSmall()

transform_claims(claims)

transform_codes(claims, '2013','2014', 'all')
transform_codes(claims, '2015','2016', 'all')
transform_codes(claims, '2017','2018', 'all')

combine_years('body_system')


rm(claims)

df1 <- readRDS('Data/2013and2014_body_system.RDS')
df2 <- readRDS('Data/2015and2016_body_system.RDS')
df3 <- readRDS('Data/2017and2018_body_system.RDS')

df4 <- bind_rows(df1,df2,df3)

saveRDS(df4, 'Data/claim_body_system_full.R')

rm(df1,df2,df3,df4)

df1 <- readRDS('Data/2013and2014_ccs_category.RDS')
df2 <- readRDS('Data/2015and2016_ccs_category.RDS')
df3 <- readRDS('Data/2017and2018_ccs_category.RDS')

df4 <- bind_rows(df1,df2,df3)

saveRDS(df4, 'Data/claim_ccs_category_full.RDS')

rm(df1,df2,df3,df4)


