# title: Visualization Project
# Author details: Author: M. Haseeb Hassan, Contact details: hacceebhassan@gmail.com
# Copyright statement: Copyrights @ hacceebhassan

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(ggthemes)

# File for Hours and Temp Graph

raw_data <- read_excel("data/file.xlsx")
# raw_data <- raw_data %>%
#  replace(., is.na(.), 0)

# Order of the Graph
# Turn your 'treatment' column into a character vector
raw_data$Virus <- as.character(raw_data$Virus)
# Then turn it back into a factor with the levels in the correct order
raw_data$Virus <- factor(raw_data$Virus, levels = unique(raw_data$Virus))


# Axis Tricks
# Labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v')
ylim.hours <- c(0, 3)
ylim.temp <- c(0, 22)
b <- diff(ylim.hours) / diff(ylim.temp)
a <- b * (ylim.hours[1] - ylim.temp[1])


# Color Pallete
cbbPalette <- c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#CCCCCC", "#CCCCCC")

# Plot_1
ggplot() +
  ggtitle("Title of the Chart") +
  geom_bar(data = raw_data, aes(fill = Specie_Name, y = Hours, x = Virus), color = "black", position = "dodge", stat = "identity", width = 0.5) +
  scale_fill_manual(values = cbbPalette) +
  geom_point(data = raw_data, aes(fill = Specie_Name, y = a + Temp * b, x = Virus), position = position_dodge(width = 0.5), stat = "identity", size = 2, color = "red") +
  # scale_colour_manual(values=cbbPalette) +
  scale_x_discrete(name = "Virus") +
  scale_y_continuous("Hours", limits = c(0, 3), sec.axis = sec_axis(~ (. - a) / b, name = "Temperature", breaks = seq(0, 35, 5))) +
  # scale_y_continuous("Hours", sec.axis = sec_axis(~ (. - 0), name = "Temperature", breaks = seq(0, 25, 5))) +
  theme_classic() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.title = element_blank(),
    legend.position = "none"
  )



# Axis Trick
ylim.logg <- c(0, 10)
ylim.salinity <- c(0, 40)
d <- diff(ylim.logg) / diff(ylim.salinity)
c <- b * (ylim.logg[1] - ylim.salinity[1])

# Plot_2
ggplot() +
  ggtitle("Title of the Chart") +
  geom_bar(data = raw_data, aes(fill = Specie_Name, y = `Log10 IVL`, x = Virus), color = "black", position = "dodge", stat = "identity", width = 0.5) +
  scale_fill_manual(values = cbbPalette) +
  geom_point(data = raw_data, aes(fill = Specie_Name, y = c + Salinity * d, x = Virus), position = position_dodge(width = 0.5), stat = "identity", size = 2, color = "blue") +
  # scale_colour_manual(values=cbbPalette) +
  scale_x_discrete(name = "Virus") +
  scale_y_continuous("Log10 IVL", limits = c(0, 12), breaks = seq(0, 12, 3), sec.axis = sec_axis(~ (. - b) * 4, name = "Salinity")) +
  # scale_y_continuous("Hours", sec.axis = sec_axis(~ (. - 0), name = "Temperature", breaks = seq(0, 25, 5))) +
  theme_classic() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    legend.title = element_blank(),
    legend.position = "none"
  )