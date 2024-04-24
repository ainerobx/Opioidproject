#install relevant packages
install.packages("here")
install.packages("tidyverse")
install.packages("readr")
install.packages("ggplot2")
#load up relevant packages
library(here)
library(tidyverse)
library(readr)
library(ggplot2)

# read and open data
## save data file as df

df <- read.csv(here('rateofdeath.csv'))

#exploring data
head(df)
summary(df)
str(df)

#deleting unnessecary columns

columns_to_delete <- c("measure_id", "measure_name","cause_id", "metric_id","cause_name","metric_name","age_id","sex_id","sex_name","age_name","upper","lower")
df <- df[, !(names(df) %in% columns_to_delete)]


#renaming collumns

##location_name change to state
colnames(df)[colnames(df) == "location_name"] <- "state"

##val change to number_deaths 
colnames(df)[colnames(df)=="val"] <-"death_rate"

#change to lower case
df$state <- tolower(df$state)


df <- df %>% mutate(number_deaths = as.numeric(death_rate))


#try to make data wide and plot the maps this way 
df$year <- factor(df$year)
df_wide <- pivot_wider(data = df,
                       id_cols = state,
                       names_from = year,
                       values_from =death_rate,
                       names_prefix = "death_rate_")
colnames(df_wide)
head(df_wide)


#subset data set by year
##1990
desired_year1990 <- 1990
year1990 <- df[which(df$year == desired_year1990),]

##1991
desired_year1991 <- 1991
year1991 <- df[which(df$year == desired_year1991),]

##1992
desired_year1992 <- 1992
year1992 <- df[which(df$year == desired_year1992),]

##1993
desired_year1993 <- 1993
year1993 <- df[which(df$year == desired_year1993),]

##1994
desired_year1994 <- 1994
year1994 <- df[which(df$year == desired_year1994),]

##1995
desired_year1995 <- 1995
year1995 <- df[which(df$year == desired_year1995),]

##1996
desired_year1996 <- 1996
year1996 <- df[which(df$year == desired_year1996),]

##1997
desired_year1997 <- 1997
year1997 <- df[which(df$year == desired_year1997),]

##1998
desired_year1998 <- 1998
year1998 <- df[which(df$year == desired_year1998),]

##1999
desired_year1999 <- 1999
year1999 <- df[which(df$year == desired_year1999),]

##2000
desired_year2000 <- 2000
year2000 <- df[which(df$year == desired_year2000),]

##2001
desired_year2001 <- 2001
year2001 <- df[which(df$year == desired_year2001),]

##2002
desired_year2002 <- 2002
year2002 <- df[which(df$year == desired_year2002),]

##2003
desired_year2003 <- 2003
year2003 <- df[which(df$year == desired_year2003),]

##2004
desired_year2004 <- 2004
year2004 <- df[which(df$year == desired_year2004),]

##2005
desired_year2005 <- 2005
year2005 <- df[which(df$year == desired_year2005),]

##2006
desired_year2006 <- 2006
year2006 <- df[which(df$year == desired_year2006),]

##2007
desired_year2007 <- 2007
year2007 <- df[which(df$year == desired_year2007),]

##2008
desired_year2008 <- 2008
year2008 <- df[which(df$year == desired_year2008),]

##2009
desired_year2009 <- 2009
year2009 <- df[which(df$year == desired_year2009),]

##2010
desired_year2010 <- 2010
year2010 <- df[which(df$year == desired_year2010),]

##2011
desired_year2011 <- 2011
year2011 <- df[which(df$year == desired_year2011),]

##2012
desired_year2012 <- 2012
year2012 <- df[which(df$year == desired_year2012),]

##2013
desired_year2013 <- 2013
year2013 <- df[which(df$year == desired_year2013),]

##2014
desired_year2014 <- 2014
year2014 <- df[which(df$year == desired_year2014),]

##2015
desired_year2015 <- 2015
year2015 <- df[which(df$year == desired_year2015),]

##2016
desired_year2016 <- 2016
year2016 <- df[which(df$year == desired_year2016),]

##2017
desired_year2017 <- 2017
year2017 <- df[which(df$year == desired_year2017),]

##2018
desired_year2018 <- 2018
year2018 <- df[which(df$year == desired_year2018),]

##2019
desired_year2019 <- 2019
year2019 <- df[which(df$year == desired_year2019),]

#or make a loop for simplified coding

# Create an empty list to store subsets for each year
year_list <- vector("list", length = 30)  

# Loop through each year and subset the data
for (year in 1990:2019) {
  desired_year <- df[df$year == year, ]
  year_list[[year - 1989]] <- desired_year  # Store the subset in the list
}



# could make maps for every year, however these would all have their individual 
#scales for death rate and therefore the colour changing would only represent change for that specific year and not across the years  
#this is just an example for 1991 and the process of creating a map of the us

library(maps)
us_states <- map_data("state")
head(us_states)

#change region to state in map data
colnames(us_states)[colnames(us_states) == "region"] <- "state"

#create map for 1990
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))

p + geom_polygon(fill = "white", color = "black")


p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = state))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

#align map to correct latitude and longitude
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = state))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)



#join opioid death data with map data
map1990 <- left_join(year1990, us_states)

#plot opioid death on map data for 1990
p1990 <- ggplot(data = map1990,
            aes(x = long, y = lat,
                group = group, fill = death_rate))

p1990 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = "white", high = "darkred", na.value = "white", name = "Death Rate")



#make a loop for all death maps 1990-2019
#keep death rate scale constant through all years to show true change in death rate

overall_scale_limits <- range(df$death_rate, na.rm = TRUE)

generate_and_save_map <- function(current_year) {
  current_data <- df %>% filter(year == current_year)
  map_data <- left_join(current_data, us_states, by = "state")
  
  p <- ggplot(data = map_data,
              aes(x = long, y = lat,
                  group = group, fill = death_rate)) +
    geom_polygon(color = "black", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient(low = "white", high = "red", na.value = "yellow", 
                        name = "Death Rate", limits = overall_scale_limits) +
    ggtitle(paste("US Opioid-Use Related Death Rate - Year", current_year)) +
    labs(subtitle = "Institute for Health Metrics and Evaluation") +
    theme(plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14),
          panel.grid.major = element_blank(),  # Remove major gridlines
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent"))

  
  # Save the plot as an image
  ggsave(filename = paste("opioid_death_map_", current_year, ".png", sep = ""), plot = p)
}


# Generate and save frames for each year
lapply(1990:2019, generate_and_save_map)



##make a gif of opioid deaths 1990-2019

# Install and load the magick package
library(magick)

# Get the list of PNG files in the current directory
file_names <- list.files(pattern = "\\.png$", full.names = TRUE)

# Read the PNG files into R as magick images
images <- image_read(file_names)

# Create the animation with a frame rate of 24 frames per second
animation <- image_animate(images, fps = 5)

# Specify the output format as GIF when writing the animation
animation_file <- "output.gif"
image_write(animation, animation_file, format = "gif")


#using an animated plot using gganimate
install.packages("gganimate")
install.packages("gifski")
install.packages("av")

library(gganimate)
library(gifski)
library(av)


##
#make an interactive plot where you can adjust the year 
#load up relevant packages
library(shiny)
library(shinyWidgets)

map_changing <- fluidPage(
  titlePanel("US Opioid-Use Related Death Rate"),
  
  # Create a slider input for selecting the year
  sliderTextInput("year", "Select Year:",
                  choices = as.character(1990:2019), selected = "1990"),
  
  # Display the plot
  plotOutput("map")
)

server <- function(input, output) {
  # Generate the plot based on the selected year
  output$map <- renderPlot({
    current_data <- df %>% filter(year == input$year)
    map_data <- left_join(current_data, us_states, by = "state")
    
    ggplot(data = map_data,
           aes(x = long, y = lat,
               group = group, fill = death_rate)) +
      geom_polygon(color = "black", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_gradient(low = "white", high = "red", na.value = "yellow", 
                          name = "Death Rate", limits = overall_scale_limits) +
      ggtitle(paste("US Opioid-Use Related Death Rate - Year", input$year)) +
      labs(subtitle = "Institute for Health Metrics and Evaluation") +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"))
  })
}

shinyApp(map_changing, server)

##

library(shiny)
library(shinyWidgets)
library(plotly)

map_changing <- fluidPage(
  titlePanel("US Opioid-Use Related Death Rate"),
  
  # Create a slider input for selecting the year
  sliderTextInput("year", "Select Year:",
                  choices = as.character(1990:2019), selected = "1990"),
  
  # Display the plot
  plotlyOutput("map")
)

server <- function(input, output) {
  # Generate the plot based on the selected year
  output$map <- renderPlotly({
    current_data <- df %>% filter(year == input$year)
    map_data <- left_join(current_data, us_states, by = "state")
    
    p <- ggplot(data = map_data,
           aes(x = long, y = lat,
               group = group, fill = death_rate, text = paste("State: ", state, "<br>Death Rate: ", death_rate))) +
      geom_polygon(color = "black", size = 0.1) +
      coord_map() +
      scale_fill_gradient(low = "white", high = "darkred", na.value = "yellow", 
                          name = "Death Rate", limits = overall_scale_limits) +
      ggtitle(paste("US Opioid-Use Related Death Rate - Year", input$year)) +
      labs(subtitle = "Institute for Health Metrics and Evaluation") +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"))
    
    ggplotly(p) %>% layout(geo = list(projection = list(type = 'albers usa'))) 
  })
}

  

shinyApp(map_changing, server)


#saving the code here so i can try knit without the shiny


#make an interactive plot where you can adjust the year 
#load up relevant packages
library(shiny)
library(shinyWidgets)

map_changing <- fluidPage(
  titlePanel("US Opioid-Use Related Death Rate"),
  
  # Create a slider input for selecting the year
  sliderTextInput("year", "Select Year:",
                  choices = as.character(1990:2019), selected = "1990"),
  
  # Display the plot
  plotOutput("map")
)


overall_scale_limits <- range(df$death_rate, na.rm = TRUE)

server <- function(input, output) {
  # Generate the plot based on the selected year
  output$map <- renderPlot({
    current_data <- df %>% filter(year == input$year)
    map_data <- left_join(current_data, us_states, by = "state")
    
    ggplot(data = map_data,
           aes(x = long, y = lat,
               group = group, fill = death_rate)) +
      geom_polygon(color = "black", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_gradient(low = "white", high = "darkred", na.value = "yellow", 
                          name = "Death Rate", limits = overall_scale_limits) +
      ggtitle(paste("US Opioid-Use Related Death Rate - Year", input$year)) +
      labs(subtitle = "Institute for Health Metrics and Evaluation") +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"))
  })
}


shinyApp(map_changing, server)

shinyApp(ui = map_changing, server = server)



# Make interactive map where you can see the stats for each state when you hover over them 


library(plotly)

map_hover <- fluidPage(
  titlePanel("US Opioid-Use Related Death Rate"),
  
  # Create a slider input for selecting the year
  sliderTextInput("year", "Select Year:",
                  choices = as.character(1990:2019), selected = "1990"),
  
  # Display the plot
  plotlyOutput("map")
)

server <- function(input, output) {
  # Generate the plot based on the selected year
  output$map <- renderPlotly({
    current_data <- df %>% filter(year == input$year)
    map_data <- left_join(current_data, us_states, by = "state")
    
    p <- ggplot(data = map_data,
                aes(x = long, y = lat,
                    group = group, fill = death_rate, text = paste("State: ", state, "<br>Death Rate: ", death_rate))) +
      geom_polygon(color = "black", size = 0.1) +
      coord_map() +
      scale_fill_gradient(low = "white", high = "darkred", na.value = "yellow", 
                          name = "Death Rate", limits = overall_scale_limits) +
      ggtitle(paste("US Opioid-Use Related Death Rate - Year", input$year)) +
      labs(subtitle = "Institute for Health Metrics and Evaluation") +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"))
    
    ggplotly(p) %>% layout(geo = list(projection = list(type = 'albers usa'))) 
  })
}



print(shinyApp(map_hover, server))




SAVE

#make an interactive plot where you can adjust the year 
#load up relevant packages
library(shiny)
library(shinyWidgets)

map_changing <- fluidPage(
  titlePanel("US Opioid-Use Related Death Rate"),
  
  # Create a slider input for selecting the year
  sliderTextInput("year", "Select Year:",
                  choices = as.character(1990:2019), selected = "1990"),
  
  # Display the plot
  plotOutput("map")
)

server <- function(input, output) {
  # Generate the plot based on the selected year
  output$map <- renderPlot({
    current_data <- df %>% filter(year == input$year)
    map_data <- left_join(current_data, us_states, by = "state")
    
    ggplot(data = map_data,
           aes(x = long, y = lat,
               group = group, fill = death_rate)) +
      geom_polygon(color = "black", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_gradient(low = "white", high = "red", na.value = "yellow", 
                          name = "Death Rate", limits = overall_scale_limits) +
      ggtitle(paste("US Opioid-Use Related Death Rate - Year", input$year)) +
      labs(subtitle = "Institute for Health Metrics and Evaluation") +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"))
  })
}

shinyApp(map_changing, server)

#Combine shiny and plotly to make an interactive map where you can hover over the states to see a value and also go through year by year

library(shiny)
library(shinyWidgets)
library(plotly)

map_changing <- fluidPage(
  titlePanel("US Opioid-Use Related Death Rate"),
  
  # Create a slider input for selecting the year
  sliderTextInput("year", "Select Year:",
                  choices = as.character(1990:2019), selected = "1990"),
  
  # Display the plot
  plotlyOutput("map")
)

server <- function(input, output) {
  # Generate the plot based on the selected year
  output$map <- renderPlotly({
    current_data <- df %>% filter(year == input$year)
    map_data <- left_join(current_data, us_states, by = "state")
    
    p <- ggplot(data = map_data,
                aes(x = long, y = lat,
                    group = group, fill = death_rate, text = paste("State: ", state, "<br>Death Rate: ", death_rate))) +
      geom_polygon(color = "black", size = 0.1) +
      coord_map() +
      scale_fill_gradient(low = "white", high = "darkred", na.value = "yellow", 
                          name = "Death Rate", limits = overall_scale_limits) +
      ggtitle(paste("US Opioid-Use Related Death Rate - Year", input$year)) +
      labs(subtitle = "Institute for Health Metrics and Evaluation") +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"))
    
    ggplotly(p) %>% layout(geo = list(projection = list(type = 'albers usa'))) 
  })
}



shinyApp(map_changing, server)





