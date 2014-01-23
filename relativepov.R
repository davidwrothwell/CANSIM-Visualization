
## trying to run a graph of LIM only

setwd("~/GitHub/CANSIM-Visualization")


#subset
relativepov <-subset(data2,data2$Line=="Low income measure after tax")

summary(relativepov$Line)

#1. What is the overall canadian poverty trend over time relative LIM measure?
ggplot(data=subset(relativepov,relativepov$Geography=="Canada"&relativepov$Statistic=="Percentage of persons in low income"&
                     relativepov$Population=="All persons"),
       aes(x=Year,y=Value/100,colour=Line,group=Line))+ #Year on x-axis, pov rate/100 on y-axis and each line gets a different colour
  ylab("Poverty Rate")+ggtitle("Canadian Poverty Measures")+
  geom_line()+ #All elements of geom_line taken from ggplot aesthetics arguments
  geom_point()+ #All elements of geom_point taken from ggplot aesthetics arguemnts
  scale_y_continuous(breaks=seq(0,max(relativepov$Value,na.rm=TRUE),.01),labels=percent)+
  #Give a tick to every 1% pov rate on the y axis and convert pov rates (0<x<1) into percentages
  theme(legend.direction="horizontal",legend.position="top",axis.text.x=element_text(angle=45))+
  #Legend reads left to right on top of graph and angle x-axis ticks to 45 degrees
  scale_x_continuous(breaks=seq(min(relativepov$Year,na.rm=TRUE),max(relativepov$Year,na.rm=TRUE),2))
#Give a tick to every 2 years on the x-axis, starting from the minimum Year to the maximum Year


## scale_y_continuous(breaks=seq(0,max(relativepov$Value,na.rm=TRUE),.01),labels=percent)+
