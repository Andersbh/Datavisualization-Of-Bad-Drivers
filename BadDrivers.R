library(readr)
library(GGally)
library(tidyverse)

urlfile = "https://raw.githubusercontent.com/fivethirtyeight/data/master/bad-drivers/bad-drivers.csv"

data = read.csv(url(urlfile))


data$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents <- with(data, Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents/100)

data$New <- with(data, Number.of.drivers.involved.in.fatal.collisions.per.billion.miles * Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents)


#Distributions
state <-  data %>% plot_ly(x = ~Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, 
                         y = ~State,
                         name = "Total Number of drivers involved in fatal collisions per billion miles",
                         type = "bar", 
                         width = 1500, height = 1000,
                         marker = list(color = "lightsteelblue", 
                                       line = list(color = "black", 
                                                   width = 1)))
state <- state %>% add_trace(x = ~New,
                             name = "Number of drivers involved in fatal collisions per billion miles without previous accidents",
                             marker = list(color = "steelblue"))
state <- state %>% layout(title = 'Number of drivers involved in fatal collisions per billion miles',
                          xaxis = list(
                            title = "",
                            tickfont = list(
                              size = 12,
                              color = 'rgb(107, 107, 107)')),
                          yaxis = list(
                            #categoryorder='total ascending',
                            title = "State",
                            titlefont = list(size = 16),
                            tickfont = list(
                              size = 12,
                              color = 'rgb(107, 107, 107)')),
                          bargap = 0.25,
                          barmode = 'overlay',
                          plot_bgcolor = "whitesmoke")

#ADD PERCENTAGE TO EACH BAR
#CHANGE THE COLOR 
#ADD TITLE AND SOURCE LINE
#CHANGE LEGENDS
#MAYBE CHANGE BACKGROUND

#Linked plot ideas
#Grouped bar chart (Speeding & Alcohol-Impaired) link with Number of drivers involved
grouped_bar <- data %>% plot_ly(x = ~State, 
                                y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding, 
                                type = 'bar', 
                                name = 'Percentage of drivers who were speeding',
                                marker = list(color = "steelblue", 
                                              line = list(color = "black", 
                                                          width = 0.5)))
grouped_bar <- grouped_bar %>% add_trace(y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired, 
                                         name = 'Percentage of drivers who were alcohol-impaired',
                                         marker = list(color = "salmon", 
                                                       line = list(color = "black", 
                                                                   width = 0.5)))
grouped_bar <- grouped_bar %>% layout(title = "Speeding VS Alcohol-impaired",
                                      xaxis = list(
                                        title = "",
                                        tickfont = list(
                                          size = 12,
                                          color = 'rgb(107, 107, 107)')),
                                      yaxis = list(
                                        title = "Percentage",
                                        titlefont = list(size = 16),
                                        tickfont = list(
                                          size = 12,
                                          color = 'rgb(107, 107, 107)')),
                                      barmode = 'group', bargap = 0.25, bargroupgap = 0.1,
                                      plot_bgcolor = "whitesmoke")
#CREATE RADIO BUTTON SO IT IS POSSIBLE TO ONLY LOOK AT ON VARIABLE AND THEN SWITCH BACK



#Violin plot
violin <- data %>% plot_ly(y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,
                        type = 'violin',
                        name = "Percentage of drivers who were speeding",
                        box = list(
                          visible = T
                        ),
                        meanline = list(
                          visible = T))
violin <- violin %>% add_trace(y = ~Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,
                             type = 'violin',
                             name = "Percentage of drivers who were alcohol-impaired",
                             box = list(
                               visible = T
                             ),
                             meanline = list(
                               visible = T))
violin <- violin %>% layout(title = "Distribution of the percentage of drivers who were speeding and alcohol-impaired",
                                      xaxis = list(
                                        title = "",
                                        tickfont = list(
                                          size = 12,
                                          color = 'rgb(107, 107, 107)')),
                                      yaxis = list(
                                        title = "Percentage",
                                        titlefont = list(size = 16),
                                        tickfont = list(
                                          size = 12,
                                          color = 'rgb(107, 107, 107)')),
                                      plot_bgcolor = "whitesmoke")


data <- data %>% mutate(num_speed= Number.of.drivers.involved.in.fatal.collisions.per.billion.miles * (Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding/100))

data <- data %>% mutate(num_alcohol= Number.of.drivers.involved.in.fatal.collisions.per.billion.miles * (Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired/100))



speed_alcohol <- data %>% plot_ly(x = ~num_speed, 
                                  y = ~num_alcohol, 
                                  text = ~State, 
                                  type = 'scatter',
                                  marker = list(color = "steelblue"))
speed_alcohol <- speed_alcohol %>% layout(title = 'Correlation between drivers who were speeding and alcohol-impaired',
                      xaxis = list(
                        title = "Number of drivers who were speeding",
                        titlefont = list(size = 14),
                        tickfont = list(
                          size = 12,
                          color = 'rgb(107, 107, 107)')),
                      yaxis = list(
                        title = "Number of drivers who were alcohol-impaired",
                        titlefont = list(size = 14),
                        tickfont = list(
                          size = 12,
                          color = 'rgb(107, 107, 107)')),
                      plot_bgcolor = "whitesmoke")



pie <- data %>% plot_ly(labels = ~State, values = ~Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, type = 'pie')
pie <- pie %>% layout(title = 'Number of drivers involved in fatal collisions per billion miles',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


write.csv(data,"C:\\Users\\andershedegaard\\Datavisualisering\\baddrivers.csv", row.names = FALSE)

