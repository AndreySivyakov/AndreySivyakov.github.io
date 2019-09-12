# install.packages('ggthemes')
# install.packages('ggplot2')
library(ggthemes)
library(ggplot2)

data <- read.csv('Economist_Assignment_Data.csv')
data <- data[, -1]

Country.Labels <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                    "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                    "India", "Italy", "China", "South Africa", "Spane",
                    "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                    "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                    "New Zealand", "Singapore")

ggplot(data, aes(CPI, HDI)) + 
  geom_point(size = 5, shape = 1, aes(color = Region)) + 
  theme_economist_white() +
  geom_smooth(method = 'lm', formula = y ~ log(x), se = F, color = 'red', size = 1.2) +
  geom_text(aes(label = Country), color = "gray20", 
            data = subset(data, Country %in% Country.Labels),check_overlap = T) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(color="black", size=12, face ="bold"), 
        legend.text = element_text(color="black", size=10), 
        legend.position = c(0.923,0.155), 
        axis.text.x = element_text(color="grey20", size=10, face="italic"),
        axis.text.y = element_text(color="grey20", size=10, face="italic"),  
        axis.title.x = element_text(color="grey20", size=10, face="italic"),
        axis.title.y = element_text(color="grey20", size=10, face="italic"),
        plot.caption=element_text(hjust=0.5, vjust=0.5)) + 
  ggtitle('Corruption and Human Development') +
  labs(x = "Corruption Perception Index, 2011 (10=least corrupt)",
       y = "Human Development Index, 2011 (1=best)") +
  scale_x_continuous(breaks = c(1:10), limits = c(1,10)) + 
  scale_y_continuous(breaks = seq(0.2,1,0.1), limits = c(0.2,1))
