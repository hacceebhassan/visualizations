library(tidyr)
library(dslabs)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(magrittr)
library(scales)
library(zoo)
library(grid)
setwd("/home/hacceeb/Dropbox/Projects/PSO - Data Collection")
df = read.xlsx('clean_data.xlsx')

# define diesel
diesel_frame <- df %>%
  filter(Product.Name == "Diesel")
diesel_frame$Date <- strptime(as.character(diesel_frame$Date), "%B %d,%Y")
diesel_frame$Date <- ymd(diesel_frame$Date)
diesel_frame$Price <- as.numeric(diesel_frame$Price)
diesel <- diesel_frame[order(as.Date(diesel_frame$Date, format = "%B %d,%Y")),]
diesel <- diesel$Petrol


# define petrol
petrol_frame <- df %>%
  filter(Product.Name == "E10 Gasoline")
petrol_frame$Date <- strptime(as.character(petrol_frame$Date), "%B %d,%Y")
petrol_frame$Date <- ymd(petrol_frame$Date)
petrol_frame$Price <- as.numeric(petrol_frame$Price)
petrol <- petrol_frame[order(as.Date(petrol_frame$Date, format = "%B %d,%Y")),]

# data
data <- merge (diesel_frame, petrol_frame, by = 'Date')
data$Price.y <- ifelse(data$Price.y == 0, (data$Price.x - 8), data$Price.y)
data$Price.x <- ifelse(data$Price.x == 0, (data$Price.x + 8), data$Price.x)
data$Price.y <- na.approx(data$Price.y)
data$Price.x <- na.approx(data$Price.x)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")


# plot
data %>% 
  ggplot() + 
  geom_rect(xmin = as.Date('2020-02-01'), xmax = as.Date('2020-06-01'), ymin = 0, ymax = Inf, alpha = 0.002, col = 'red', fill = 'white') +
  annotate(geom="label", x=as.Date('2020-04-01'), y=50,  fill="grey", label='Effects of COVID-19 \n (A massive decline in Rates)', size = 3) +
  geom_line(aes(x = Date, y = Price.x, col = 'Diesel (Maximum Price: Rs. 132.47, Average Price: Rs. 81.27 , Minimum Price: Rs. 36.33)'), size = 1) +
  geom_hline(yintercept = mean(data$Price.x), size = 0.5, col = 'darkorange', linetype='dashed') +
  annotate(geom="text", x=as.Date('2007-05-01'), y=85, label='Avg. Diesel Price/Litre = Rs. 81.27', color="darkorange", size = 3) +
  geom_line(aes(x = Date, y = Price.y, col = 'Petrol (Maximum Price: Rs. 115.33 , Average Price: Rs. 71.74 , Minimum Price: Rs. 28.33)'), size = 1) +
  geom_hline(yintercept = mean(data$Price.y), size = 0.5, col = 'darkgreen', linetype='dashed') +
  annotate(geom="text", x=as.Date('2007-05-01'), y=68, label='Avg. Petrol Price/Litre = Rs. 71.74', color="darkgreen", size = 3) +
  scale_color_manual (values = c("darkorange", "darkgreen")) +
  labs(title = "Diesel vs Petrol Price Comparison (Pump Prices)",
       subtitle = "Pakistan (2006 - Present)",
       caption = "Data source: Pakistan State Oil Website") +
  xlab("Timeline") + 
  ylab("Price (Rupees / Litre)") + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_y_continuous(breaks=seq(10, 140, 10), limits=c(10, 140)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = 'top',
    legend.text = element_text(size = 10, color = 'azure4', face = 'bold'),
    legend.key.size = unit(2, 'line')
  )

