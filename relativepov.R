
## trying to run a graph of LIM only

setwd("~/GitHub/CANSIM-Visualization")

library(xtable)
library(ggplot2)
library(reshape)
library(stringr)
library(scales)
library(Hmisc)
library(grid)
fileUrl <- "http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/02020802-eng.zip"
temp <- tempfile()
download.file(fileUrl, temp)
data.test <- read.csv(unz(temp, "02020802-eng.csv"))
unlink(temp)
###Above I have grabbed the data directly from the website
data.test<-data.test[,c(-7,-6)] #Dropping "vector" columns
colnames(data.test)<-c("Year","Geography","Line","Statistic","Population","Value") #labelling

geo<-c("Atlantic provinces","Newfoundland and Labrador", #Creating vector for geographies of interest
       "New Brunswick","Nova Scotia","Prince Edward Island",
       "Prairie provinces","Alberta","Manitoba","Saskatchewan",
       "British Columbia","Ontario","Quebec","Canada")
population<-c("Females","Males","Persons under 18 years", #Creating vector for populations of interest
              "Persons 18 to 64 years","Persons 65 years and over",
              "Persons in economic families",
              "Persons under 18 years in female lone-parent families",
              "Persons under 18 years in two-parent families",
              "Unattached individuals","All persons")
data2<-subset(data.test,!is.na(data.test$Population)&!is.na(data.test$Geography)& #Drop NA values
                (data.test$Geography %in% geo)) #Keep all rows where Geography column matches

#Small data cleaning...reordering and renaming certain factor levels
data2$Value<-as.numeric(as.character(data2$Value)) #Change value from factor to numeric
#Below I am reordering the factor levels of Geography and Population since I can't do it within Hmisc functions
data2$Geography<-ordered(data2$Geography,levels=c("Atlantic provinces","Newfoundland and Labrador",
                                                  "New Brunswick","Nova Scotia","Prince Edward Island",
                                                  "Prairie provinces","Alberta","Manitoba","Saskatchewan",
                                                  "British Columbia","Ontario","Quebec","Canada"))
data2$Population<-ordered(data2$Population,levels=c("Females","Males","Persons under 18 years",
                                                    "Persons 18 to 64 years","Persons 65 years and over",
                                                    "Persons in economic families","Persons under 18 years in female lone-parent families","Persons under 18 years in two-parent families","Unattached individuals","All persons",
                                                    "Females (x 1,000)","Males (x 1,000)","Persons under 18 years (x 1,000)",
                                                    "Persons 18 to 64 years (x 1,000)","Persons 65 years and over (x 1,000)",
                                                    "Persons in economic families (x 1,000)",
                                                    "Persons under 18 years in female lone-parent families (x 1,000)",
                                                    "Persons under 18 years in two-parent families (x 1,000)",
                                                    "Unattached individuals (x 1,000)","All persons (x 1,000)")
)
data2$Population<-revalue(data2$Population,c("Persons under 18 years in female lone-parent families"="Child in single mother families","Persons under 18 years in two-parent families"="Child in two-parent families"))
#End of data cleaning


#subset
relativepov <-subset(data2,data2$Line=="Low income measure after tax")

summary(relativepov$Line)
summary(relativepov$Statistic)
summary(relativepov$Population)

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
