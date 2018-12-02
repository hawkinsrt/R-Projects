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
                  mutate_at(c("X3","X5","X6","X31","X32","X39","X40","X41"), as.numeric) %>% 
                  mutate_if(is.character,as.factor)
  return(claimsClean)
  
}