library(tidyverse)
library(data.table)
library(ggthemes)
library(readxl)
library(scales)
library(formattable)
library(stringr)


data <- as.data.frame(read_excel("DATA+for+Sept18+SWDchallenge.xlsx"))
setnames(data, old=c( "X__1","X__2"), new=c("2000", "2016"))

data2 <- data[-c(1,2,9), ]
setnames(data2, old=c("DATA TO GRAPH"), new=c("Geographic Region"))

data3 <- data2 %>%
  gather(key = "year", value = "Marketshare", 2:3) %>%
  mutate(PercentMarketShare = percent(Marketshare, digits=1))



ggplot(data3, aes(x= as.factor(year), y = PercentMarketShare, group = `Geographic Region`, color = `Geographic Region`)) + 
  geom_point(size = 2) +
  geom_line(size = 1) +
  theme_minimal(base_size = 12) +
  ylab("Percent of Market Share")+
  xlab("Year") +
  ggtitle("Global Tourism Trends from 2000 to 2016", subtitle = str_wrap("Since 2000, Asia Pacific region has increased in market share, whereas the European region has had a similar decrease in market share", width = 100)) +
labs(caption = "Source: World Travel and Tourism Council") +
  scale_color_manual(breaks = c("Asia Pacific", "Europe", "Latin America", "Middle East", "North America", "Other"),
                     values=c("#2f76eb", "#dd544a", "#909090", "#909090", "#909090", "#909090", "#909090")) +
  #Asia Pacific Labels
   geom_text(data = subset(data3, year == 2016 & `Geographic Region`== "Asia Pacific"), aes(x = as.factor(year), y = PercentMarketShare, label = str_wrap(paste0(`Geographic Region`, " market share increased to ", PercentMarketShare), width = 30), hjust = -.05, vjust = -0.125)) + 
  theme(legend.position = "none") +
  #2000 Label
  geom_text(data = subset(data3, year == 2000 & `Geographic Region`== "Asia Pacific"), aes(x = as.factor(year), y = PercentMarketShare, label = PercentMarketShare), hjust = 1.25) + theme( legend.position = "none") +
  #Add label for Europe
  geom_text(data = subset(data3, year == 2016 & `Geographic Region`== "Europe"), aes(x = as.factor(year), y = PercentMarketShare, label = str_wrap(paste0(`Geographic Region`, " market share decreased to ", PercentMarketShare), width = 25), hjust = -.05, vjust = -0.025)) + 
  #200 Label
  geom_text(data = subset(data3, year == 2000 & `Geographic Region`== "Europe"), aes(x = as.factor(year), y = PercentMarketShare, label = PercentMarketShare), hjust = 1.25) + theme( legend.position = "none")+
  #Add Label for Latin America
  geom_text(data = subset(data3, year == 2016 & `Geographic Region`== "Latin America"), aes(x = as.factor(year), y = PercentMarketShare, label = paste0(`Geographic Region`, " ", PercentMarketShare)), hjust = -.05) + 
  #2000 Label  
    geom_text(data = subset(data3, year == 2000 & `Geographic Region`== "Latin America"), aes(x = as.factor(year), y = PercentMarketShare, label = PercentMarketShare), hjust = 1.25) + theme( legend.position = "none")+
  #Add Label for Middle East
  geom_text(data = subset(data3, year == 2016 & `Geographic Region`== "Middle East"), aes(x = as.factor(year), y = PercentMarketShare, label = paste0(`Geographic Region`, "  ", PercentMarketShare)), hjust = -.05) +
    #2000 Labele
    geom_text(data = subset(data3, year == 2000 & `Geographic Region`== "Middle East"), aes(x = as.factor(year), y = PercentMarketShare, label = PercentMarketShare), hjust = 1.25) + theme( legend.position = "none")+
    #Add Label for North America
    geom_text(data = subset(data3, year == 2016 & `Geographic Region`== "North America"), aes(x = as.factor(year), y = PercentMarketShare, label = paste0(`Geographic Region`, "  ", PercentMarketShare)), hjust = -.05)+
    #2000
      geom_text(data = subset(data3, year == 2000 & `Geographic Region`== "North America"), aes(x = as.factor(year), y = PercentMarketShare, label = PercentMarketShare), hjust = 1.25) + theme( legend.position = "none")+
    
  #Add Label for Other
  geom_text(data = subset(data3, year == 2016 & `Geographic Region`== "Other"), aes(x = as.factor(year), y = PercentMarketShare, label = paste0(`Geographic Region`, "  ", PercentMarketShare)), hjust = -.05)+
    #2000 Label
    geom_text(data = subset(data3, year == 2000 & `Geographic Region`== "Other"), aes(x = as.factor(year), y = PercentMarketShare, label = PercentMarketShare), hjust = 1.25) + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 14))
 
  
   
