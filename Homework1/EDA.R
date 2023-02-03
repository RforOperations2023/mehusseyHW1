library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

foodprod <- read_csv("foodprod.csv")
View(foodprod)

colnames(foodprod)[colnames(foodprod) == "Food product"] <- "product"
colnames(foodprod)[colnames(foodprod) == 
                     "Greenhouse gas emissions per 1000kcal"] <- "greenhouse_gas"
colnames(foodprod)[colnames(foodprod) == 
                     "Land use per 1000kcal"] <- "land_use"

food <- subset(foodprod, foodprod$Total_emissions > 18) #filtering to just be top 5
#so that it is more interactive for the user, and also because the top 5 have no missing values
View(food)
write.csv(food, "food.csv") 

