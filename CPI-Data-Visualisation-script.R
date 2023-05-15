# install and load tidyverse library
install.packages("tidyverse")
library(tidyverse)

# install and load readr to use read_csv function
install.packages("readr")
library(readr)

# use read_csv function to load in data file
raw_data<-read_csv("C:\\Users\\olive\\OneDrive\\Documents\\CPI Data Visualisation Project\\CPI by country by year csv.csv")

# raw_data delineates the data frame containing all data to be used for the project, before data wrangling
# install and load dplyr and tidyr for data wrangling
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

# want to plot world avg score per year, and uk yearly score
# need to create values for worldavg and europeavg
worldavg <- colMeans (raw_data[sapply(raw_data, is.numeric)], na.rm = TRUE)

# first, filter the original dataframe into a new set only including european countries
europeavg <- filter(raw_data, Country %in% c("Russia", 
                                             "Germany",
                                             "UK",
                                             "France",
                                             "Belarus",
                                             "Bulgaria",
                                             "Norway",
                                             "Bosnia and Herzegovina",
                                             "Slovenia",
                                             "Luxembourg",
                                             "Italy",
                                             "Spain",
                                             "Ukraine",
                                             "Poland",
                                             "Austria",
                                             "Denmark",
                                             "Ireland",
                                             "Albania",
                                             "Latvia",
                                             "Malta",
                                             "Romania",
                                             "Netherlands",
                                             "Belgium",
                                             "Czechia",
                                             "Serbia",
                                             "Finland",
                                             "Croatia",
                                             "Lithuania",
                                             "Estonia",
                                             "Iceland",
                                             "Greece", 
                                             "Portugal",
                                             "Sweden",
                                             "Hungary",
                                             "Switzerland",
                                             "Slovakia",
                                             "Moldova",
                                             "North Macedonia",
                                             "Montenegro"))

# then, use the colMeans function to find the averages of the europeavg set and redefine europeavg as this value set
europeavg <- colMeans(europeavg[sapply(europeavg, is.numeric)], na.rm = TRUE)

# create new dataframes for europeavg and world avg
Worldavgdata <- bind_rows(worldavg)
Worldavgdata <- cbind(Worldavgdata, Country="World Average")
Worldavgdata <- Worldavgdata[, c("Country", names(Worldavgdata)[-which(names(Worldavgdata)=="Country")])]
Europeavgdata <- bind_rows(europeavg)
Europeavgdata <- cbind(Europeavgdata, Country="European Average")
Europeavgdata <- Europeavgdata[, c("Country", names(Europeavgdata)[-which(names(Europeavgdata)=="Country")])]

# uk score filter
uk_score <- filter(raw_data, Country == 'UK')

# combine data frames
plotdata <- bind_rows(uk_score, Europeavgdata, Worldavgdata)
tidyplotdata <- plotdata %>%
  pivot_longer(cols=-c("Country"),
               names_to = "Year",
               values_to = "CPI_score")

# now need to plot
ggplot(tidyplotdata, aes(x = Year, y = CPI_score, colour = Country, group=Country))+ # x/y axis labels and line group
  geom_point(size = 4, alpha = 0.3)+ # dot size and transparency
  geom_path(linewidth = 1)+ # specifies line width and dot grouping
  theme_gray()+ # theme
  labs(title = "Percieved levels of corruption in the UK over time, compared to \n World Average and European Average",
       subtitle = "Plot of Corruption Perception Index (CPI) scores by year 2012 - 2022")+ #labels
  theme (plot.title = element_text(face =  "bold"))+ # make title bold
  scale_y_reverse()