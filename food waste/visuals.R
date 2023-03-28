library(tidyverse)
library(ggplot2)
library(directlabels)
library(geomtextpath)
library(ggrepel)
library(treemap)
library(treemapify)
library(ggplotify)
library(lubridate)
library(dplyr)
library(tools)


# notes:
#   
# 1) table inputs: sector and year
# 2) plot-1 inputs: sector and year
# 3) plot-2 inputs: food type and year
# 4) plot-3 inputs: year


data <- read_csv('ReFED_US_Food_Surplus.csv')

data <- data %>%
  mutate(year =as.Date(year, format="%Y"))


data$year = year(year)


  mutate(year = as.numeric(format(Date.Local,'%Y')))


summary(data$year)

class(data$year)



#plot-1: bar chart of tons_waste by sector , facet wrap by year
data_plot1 <- data %>%
  group_by(sector, year) %>%
  summarise_at(vars(tons_waste), list(tons_waste_mean = mean))


ggplot(data_plot1, aes(x = year, y = tons_waste_mean, fill = sector)) + 
  geom_bar(stat = 'identity') + theme_classic()


#plot-2: bar chart of tons_waste by food_type
data_plot2 <- data %>%
  group_by(food_type, year) %>%
  summarise_at(vars(tons_waste), list(food_type_mean = mean))

# ggplot(data_plot2, aes(x = year, y = food_type_mean)) + 
#   geom_bar(stat = 'identity') + facet_wrap(~food_type, ncol = 4)
# 
# 
# ggplot(data_plot2, aes(x=year, y=food_type_mean, group = food_type, colour = food_type)) + geom_line() + 
#   scale_colour_discrete(guide = 'none') +
#   scale_x_discrete(expand=c(0, 1)) +
#   geom_dl(aes(label = food_type), method = list(dl.combine("last.points")), cex = 0.8) 
# 
# 
# 
# ggplot(data_plot2, aes(x=year, y=food_type_mean, group = food_type, colour = food_type)) + geom_line() + 
#   geom_label_repel(aes(label = label),
#                    nudge_x = 1,
#                    na.rm = TRUE)


ggplot(data_plot2, aes(x=year, y=food_type_mean, group = food_type, colour = food_type)) + geom_line() + 
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand = expansion(mult = c(0, 0.3))) +
  geom_dl(aes(label = food_type), method = list(dl.combine("last.points")), cex = 0.8) + theme_classic()





#plot-3: violin plot of tons_surplus by sector

data_plot3 <- data %>%
  group_by(sector, year) %>%
  summarise_at(vars(tons_surplus), list(tons_surplus_mean = mean))


data_plot3 %>%
  filter(year == 2010) %>%
  ggplot(aes(fill = sector, area = tons_surplus_mean, label = sector)) + geom_treemap() + geom_treemap_text(colour ="white", place = "centre")






# area chart 

data_foodtype <- data %>%
  group_by(food_type, year) %>%
  summarise_at(vars(tons_waste), list(food_type_mean = mean))

unique(data_area$food_type)



data_foodtype %>%
  filter(year == 2010) %>%
  plot_ly(x = ~food_type, y = ~food_type_mean, hovertemplate = "Year %{year} <br> Food Waste %{food_type_mean} kilotonnes <extra></extra>", type = 'bar') %>% 
  layout(title = "Food Waste by Product Type",
                      xaxis = list(title = "", tickangle = -90),
                      yaxis = list(title = ""))




data_foodtype %>%
  filter(year == 2010) %>%
  plot_ly(x = ~food_type, y = ~food_type_mean, type = 'bar',
          hovertemplate = ~paste("<b>Year:</b>", year, "<br><b>Food Waste<b>", comma(food_type_mean,digits=0), "kilotonnes")) %>% 
  layout(title = "Food Waste by Product Type",
         xaxis = list(title = "", tickangle = -90),
         yaxis = list(title = ""))








#donut chart

data_surplus <- data %>%
  group_by(sector, year) %>%
  summarise_at((tons_surplus), list(tons_surplus_mean = mean))


#use the above data subset and further filter it according to user inputs
data_surplus <- data_surplus %>%
  filter(year == 2010) %>%
  filter(sector == "Farm")
  plot_ly(labels = ~sector, values = ~tons_surplus_mean) %>% add_pie(hole = 0.6) %>%
  layout(title = 'Surplus Food Produced by Sector')


