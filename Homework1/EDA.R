library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(RColorBrewer)

foodprod <- read_csv("foodprod.csv")
View(foodprod)
str(foodprod)

colnames(foodprod)[colnames(foodprod) == "Food product"] <- "product"
#making it one word so it is easier for coding

food <- foodprod[complete.cases(foodprod), ] #filtering out rows that have NAs 
str(food)
#clearing NAs so that the user can only look at food products that have all data
#Shows only 24 instead of 43 foods
write.csv(food, "food.csv") #writing my csv here so I can just call the clean one in the app
foodtop10 <- food[order(food$Total_emissions, decreasing = TRUE), ][1:10, ]
#after exploring with graphs, I decided to filter even more to the top 10 food products by total emissions so the graphs are not so busy they are unreadable to the reader
write.csv(foodtop10, "foodtop10.csv")
#TESTING OUT DIFFERENT GRAPHS FOR APP
ggplot(foodtop10, aes(x = Greenhouse_gas_kcal, y = Land_use_kcal, color = product)) +
  geom_point() +
  xlab("Greenhouse Gas Emissions (per 100kcal)") +
  ylab("Land Use Emissions (per 100kcal)") +
  ggtitle("Greenhouse Gas Emissions vs Land Use Emissions by Food Production")

ggplot(foodtop10, aes(x = Eutrophying_emissions_kcal, y = Land_use_kcal, color = product)) +
  geom_point() +
  xlab("Eutrophying Emissions (per 100kcal") +
  ylab("Land Use Emissions (per 100kcal)") +
  ggtitle("Eutrophying Emissions vs Land Use Emissions by Food Production")
#histograms data is too spread out

#bar plot
ggplot(data = foodtop10, aes(x = product, y = Total_emissions)) +
  geom_bar(stat = "identity") +
  xlab("Food Product") +
  ylab("Emissions") +
  ggtitle("Contribution of Food Products to Total Emissions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = "#1f77b4")

#pie chart
foodprod_sum <- aggregate(foodtop10$Total_emissions, by = list(foodtop10$product), sum)
names(foodprod_sum) <- c("product", "Total_emissions")

ggplot(data = foodprod_sum, aes(x = "", y = Total_emissions, fill = product)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Total Emissions by Food Product") +
  xlab("") +
  ylab("Emissions") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Food Product"))

#there are too many colors in this pie chart

#Stacked bar plot
ggplot(data = foodtop10, aes(x = product, y = Total_emissions, fill = product)) +
  geom_bar(stat = "identity") +
  xlab("Food Product") +
  ylab("Emissions") +
  ggtitle("Contribution of Food Products to Emissions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")


