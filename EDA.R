library(readr)
library(dplyr)
library(ggplot2)
Crime_Data <- read_csv("Crime_Data.csv")
View(Crime_Data)

# Group by Precinct
df2 <- Crime_Data %>% 
  group_by(Precinct) %>% 
  summarise(sum_crime=sum(Crime_Count),.groups = 'drop') %>%
  as.data.frame()

#View the results
df2 


ggplot(data = df2, aes(x = Precinct, y = sum_crime)) +
  geom_bar(stat = "identity")
