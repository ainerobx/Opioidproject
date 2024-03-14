#Creating line graph for opioid  and alcohol related deaths over time 1990-2019 in the USA
install.packages("dplyr")
install.packages("here")
install.packages("tidyverse")
install.packages("readr")
install.packages("gganimate")
install.packages("plotly")

library(here)
library(tidyverse)
library(readr)
library(gganimate)
library(plotly)

usdrugdeaths <- read.csv(here('alldrugsus.csv'))


#deleting unnessecary columns

columns_to_delete <- c("measure_id", "measure_name","cause_id", "metric_id","metric_name","age_id","sex_id","sex_name","age_name","upper","lower", "location_name")
usdrugdeaths <- usdrugdeaths[, !(names(usdrugdeaths) %in% columns_to_delete)]

#renaming columns

##val change to rate_deaths 
colnames(usdrugdeaths)[colnames(usdrugdeaths)=="val"] <-"rate_death"

#make wide data
usdrugdeaths<- usdrugdeaths %>%
  pivot_wider(names_from = cause_name, values_from = rate_death)

#change column names
colnames(usdrugdeaths)[colnames(usdrugdeaths)=="Alcohol use disorders"] <-"Alcohol"
colnames(usdrugdeaths)[colnames(usdrugdeaths)=="Opioid use disorders"] <-"Opioid"
colnames(usdrugdeaths)[colnames(usdrugdeaths)=="Cocaine use disorders"] <-"Cocaine"
colnames(usdrugdeaths)[colnames(usdrugdeaths)=="Amphetamine use disorders"] <-"Amphetamine"

#make line graph with all 4 drugs
drugdeaths <- ggplot(usdrugdeaths, aes(x = year)) +
  geom_line(aes(y = Alcohol, color = factor("Alcohol")), linetype = "solid", size = 1.65) +
  geom_line(aes(y = Opioid, color = factor("Opioid")), linetype = "solid", size = 1.65) +
  geom_line(aes(y = Cocaine, color = factor("Cocaine")), linetype = "solid", size = 1.65) +
  geom_line(aes(y = Amphetamine, color = factor("Amphetamine")), linetype = "solid", size = 1.65) +
  scale_color_manual(name = "Cause", values = c("Alcohol" = "dodgerblue", "Opioid" = "firebrick", "Cocaine"= "darkgreen", "Amphetamine"="purple")) +
  labs(title = "Drug-related Deaths in the US", 
       subtitle =  "Data from 1990-2019",
       caption = "Source: Institute for Health Metrics and Evaluation",
       x = "Year",
       y = "Death Rate per 100,000",
       color = "Cause") +
  scale_y_continuous(limits = c(0, max(usdrugdeaths$Alcohol, usdrugdeaths$Opioid, usdrugdeaths$Cocaine, usdrugdeaths$Amphetamine) + 1)) +
  theme_minimal() +
  theme(plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.caption = element_text(face = "italic", hjust = 0)) +
  theme(axis.title.x = element_text(color = "#0099f9", size = 16, face = "bold"),
        axis.title.y = element_text(color = "#0099f9", size = 16, face = "italic"))

print(drugdeaths)

# Convert ggplot to plotly
interactive_plot <- ggplotly(drugdeaths)

# View the interactive plot (can be exported as HTML)
interactive_plot

