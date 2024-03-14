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


# could make maps for every year, however these would all have their individual 
#scales for death rate and therefore the colour changing would only represent change for that specific year and not across the years  

library(maps)
us_states <- map_data("state")
head(us_states)

#change region to state in map data
colnames(us_states)[colnames(us_states) == "region"] <- "state"

#create map for 1990
p1990 <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))

p1990 + geom_polygon(fill = "white", color = "black")


p1990 <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = state))

p1990 + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

#align map to correct latitude and longitude
p1990 <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = state))

p1990 + geom_polygon(color = "gray90", size = 0.1) +
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
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient(low = "white", high = "red", na.value = "yellow", 
                        name = "Death Rate", limits = overall_scale_limits) +
    ggtitle(paste("Opioid Death Map - Year", current_year))
  
  # Save the plot as an image
  ggsave(filename = paste("opioid_death_map_", current_year, ".png", sep = ""), plot = p)
}

# Generate and save frames for each year
lapply(1990:2019, generate_and_save_map)

##make a gif of opioid deaths 1990-2019

# Install and load the magick package
install.packages("magick")
library(magick)

# Get the list of PNG files in the current directory
file_names <- list.files(pattern = "\\.png$", full.names = TRUE)

# Read the PNG files into R as magick images
images <- image_read(file_names)

# Create the animation with a frame rate of 24 frames per second
animation <- image_animate(images, fps = 10)

# Specify the output format as GIF when writing the animation
animation_file <- "output.gif"
image_write(animation, animation_file, format = "gif")
