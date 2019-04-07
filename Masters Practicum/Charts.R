library(tidyverse)
claims<- read_csv("Data/claims_ccs_as_code_with_lead_service.csv")

small <- claims %>% 
  select(MRN_ALIAS, MEMBER_SEX, AGE_GROUP, SERVICE_TYPE, Asthma, COPD, Diabetes,HeartFailure, 
         Hypertension, Psychoses, MoodDisorder, CLAIM_SEQ)

pivot <- small %>% gather(Condition, value, Asthma, COPD, Diabetes, HeartFailure,
                          Hypertension, Psychoses, MoodDisorder)

pivot %>% filter(!is.na(value)) %>% #Remove NAs
  group_by(Condition) %>% # Group by each Chronic Condition
  summarize(Total_Cases = n()) %>% # Count how many cases for each Condition
  ggplot(aes(fct_reorder(Condition,Total_Cases), Total_Cases, #Order by X axis
         fill = fct_reorder(Condition, Total_Cases))) + # Colar by X axis
  geom_col()+ # Bar chart
  geom_text(aes(label= paste0(round(Total_Cases/1000000), ' M')),vjust = 2)+ #Add labels to column
  labs(x = 'Chronic Condition', y = 'Total Visits',  #labels for tile and axis
       title = 'Total Visits with a Prevously Diagnosed Common Chronic Condition (Millions)')+
  theme(legend.position = 'none', axis.text.y = element_blank() ) #Remove legend and y axis


pivot %>% filter(!is.na(value) & SERVICE_TYPE == 'ED') %>% #Remove NAs and Only ED visits
  group_by(Condition) %>% # Group by each Chronic Condition
  summarize(Total_Cases = n()) %>% # Count how many cases for each Condition
  ggplot(aes(fct_reorder(Condition,Total_Cases), Total_Cases, #Order by X axis
             fill = fct_reorder(Condition, Total_Cases))) + # Colar by X axis
  geom_col()+ # Bar chart
  geom_text(aes(label= format(Total_Cases, big.mark = ','),vjust = 2))+ #Add labels to column
  labs(x = 'Chronic Condition', y = 'Total Visits',  #labels for tile and axis
       title = 'Total ED Visits with a Prevously Diagnosed Common Chronic Condition',
       fill = 'Chronic Condition')+
  theme(legend.position = 'none', axis.text.y = element_blank())+ #Remove legend and y axis
  theme_void()+
  theme(legend.position = c(.1, .8), 
        legend.justification = c(0, 1),
        legend.text=element_text(size=12),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))

pivot %>% filter(!is.na(value)) %>% 
  group_by(MRN_ALIAS, Condition) %>% 
  summarise(value = max(value)) %>%
  group_by(Condition) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(fct_reorder(Condition, Total), Total,
             fill = fct_reorder(Condition, Total)))+
  geom_col()+
  geom_text(aes(label= format(Total,big.mark = ',')), vjust = 2, size = 5)+ #Add labels to column
  labs(x = 'Chronic Condition', y = 'Number of Members',  #labels for tile and axis
       title = 'Members with a Prevously Diagnosed Common Chronic Condition',
       fill = 'Chronic Condition')+
  theme_void()+
  theme(legend.position = c(.1, .8), 
       legend.justification = c(0, 1),
       legend.text=element_text(size=12),
       legend.title = element_text(size = 14),
       plot.title = element_text(hjust = 0.5, size = 16))

write.csv(pivot, 'test.csv')
