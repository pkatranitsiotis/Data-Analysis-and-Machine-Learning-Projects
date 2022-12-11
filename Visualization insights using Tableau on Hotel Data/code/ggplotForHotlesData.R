#### installation of the library for ggplot ####
install.packages("tidyverse")
library(tidyverse)

#### enter dataset in R ####
library(readr)

#the Hotels' bookings dataset was retrieved from Kaggle
hb <- read_csv("hotel_bookings.csv")
hb<-as.data.frame(hb)
View(hb)
attach(hb)

#observe whether the variables are categorized correctly to measured and factored or not
str(hb)
hb$is_canceled<-as.factor(is_canceled)
hb$is_repeated_guest<-as.factor(is_repeated_guest)

##### plot - Reservation Status ####

#preparation of plot data
temp<-factor(is_canceled,labels = c("Not Canceled","Canceled"))

ggplot(data=hb,
       aes(
         x=hotel,
         y=prop.table(stat(count)),
         fill=factor(temp),
         label=scales::percent(prop.table(stat(count)))
       ))+
 geom_bar(position = position_dodge())+
  geom_text(stat="count",position = position_dodge(1),vjust=-0.3,hjust=0.5)+
  scale_fill_brewer(palette="Paired")+
 scale_y_continuous(labels = scales::percent)+
 labs(title = "Reservation Status in each hotel", x="Hotel", y="Percentage", fill="Reservation Status", levels=c("Not","Yes"))

##### plot - Country ####
install.packages("choroplethr")
install.packages("choroplethrMaps")
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
data(country.map,package = "choroplethrMaps")
View(country.map)


countrydf<-as.data.frame(country.map)
temp.map<-as.data.frame(temp.map)
for (i in 1:1511) {
  for (k in 1:10586) {
    if (countrydf[k,12]==temp.map[i,1]) temp.map[i,1]<-countrydf[k,66]
    next
  }
}

temp.plot<-temp.map %>%
  rename(region=country,
         value=stays_in_week_nights)
country_choropleth(temp.plot)

for (i in 1:119390) {
  j<-which(countrydf[,12]==hb[i,14])
  hb[i,14]<-countrydf[j]
}

for (i in 1:119390) {
  for (j in 1:33) {
    if (countrydf[j,12]==hb[i,14]) hb[i,14]<-countrydf[j,66]
    break
  }
}
countrydf[j,66]

install.packages('devtools')
devtools::install_github(UrbanInstitute/urbnmapr)
household_data <- left_join(hb, country) 


  ggplot(hb,aes(hotel,country, fill = stays_in_week_nights)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

##### plot - Avarage Daily Rate for each costumer type based on booked meal ####

#creating a temporary variable in order present the average value of adr
temp1 = aggregate(list(adr = hb$adr), list(meal = factor(hb$meal),customer_type=hb$customer_type), mean)

#creation of graph with all costumer types simultanusely
ggplot(temp1,
       aes(x=meal,
           y= adr, fill=customer_type)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(adr)),position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Paired")+
  labs(title="Average Daily Rate for customers' types based on booked meal",
       y = "Average Daily Rate",
       x="Meal",
       fill="Customer Type")

#creation of graph for each costumer type (separately)
ggplot(temp1,
       aes(x=meal,
          y= adr, fill=customer_type)) +
  geom_bar(stat="identity",
           position = position_dodge()) +
  geom_text(aes(label=round(adr)),position = position_dodge(width=0.9),hjust=0.5,vjust=1)+
  scale_fill_brewer(palette="Paired")+
  labs(title="Average Daily Rate for each customer type base on booked meal",
       y = "Average Daily Rate",
       x="Meal",
       fill="Customer Type")

######## plot - Cancellation Rate per year ######
library(dplyr)
library(ggpubr)

#preparation of plot data
temp2<-hb %>%
  group_by(is_canceled,arrival_date_year,arrival_date_month) %>%
  summarize(n=n())
temp2<-filter(temp2,is_canceled==1)
head(temp2)

###2015#
line.plot.data.2015<-filter(temp2, arrival_date_year==2015)
head(line.plot.data.2015)
p2015<-ggplot(line.plot.data.2015,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Cancellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="Cancellations", title = "2015")

###2016
line.plot.data.2016<-filter(temp2, arrival_date_year==2016)
head(line.plot.data.2016)
p2016<-ggplot(line.plot.data.2016,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Cancellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="Cancellations", title = "2016")

###2017
line.plot.data.2017<-filter(temp2, arrival_date_year==2017)
p2017<-ggplot(line.plot.data.2017,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Cancellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="Cancellations", title = "2017")

##creation of line plot
ggarrange(p2015,p2016,p2017,ncol=1,nrow=3,labels="Cancellation Rate per year",hjust=-1,common.legend = TRUE,legend = "right")

#######For each hotel
temp3<-hb %>%
  group_by(is_canceled,arrival_date_year,arrival_date_month,hotel) %>%
  summarize(n=n())
temp3<-filter(temp3,is_canceled==1)
head(temp3)

###2015 for resort hotel#
line.plot.data.2015.rh<-filter(temp3,hotel=="Resort Hotel" & arrival_date_year==2015)
head(line.plot.data.2015.rh)
p2015.rh<-ggplot(line.plot.data.2015.rh,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Cancellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="Resort Hotel", title = "2015")


###2016 for resort hotel#
line.plot.data.2016.rh<-filter(temp3,hotel=="Resort Hotel" & arrival_date_year==2016)
head(line.plot.data.2016.rh)
p2016.rh<-ggplot(line.plot.data.2016.rh,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Canellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="Resort Hotel", title = "2016")

###2017 for resort hotel#
line.plot.data.2017.rh<-filter(temp3,hotel=="Resort Hotel" & arrival_date_year==2017)
head(line.plot.data.2017.rh)
p2017.rh<-ggplot(line.plot.data.2017.rh,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Canellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="Resort Hotel", title = "2017")


###2015 for City hotel#
line.plot.data.2015.ch<-filter(temp3,hotel=="City Hotel" & arrival_date_year==2015)
head(line.plot.data.2015.ch)
p2015.ch<-ggplot(line.plot.data.2015.ch,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Canellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="City Hotel", title = "2015")


###2016 for City hotel#
line.plot.data.2016.ch<-filter(temp3,hotel=="City Hotel" & arrival_date_year==2016)
head(line.plot.data.2016.ch)
p2016.ch<-ggplot(line.plot.data.2016.ch,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Canellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="City Hotel", title = "2016")

###2017 for City hotel#
line.plot.data.2017.ch<-filter(temp3,hotel=="City Hotel" & arrival_date_year==2017)
head(line.plot.data.2017.ch)
p2017.ch<-ggplot(line.plot.data.2017.ch,aes(
  x=arrival_date_month,
  y=n,
  col=n,
  group=1
))+
  scale_colour_gradient(low="#33CC99",high="red",name="Total Canellations")+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=n,vjust=0,hjust=1),colour="black")+
  labs(x="Arrival Month", y="City Hotel", title = "2017")

#creation of the line plot for each hotel (per year)
p1<-ggarrange(p2015.rh,p2016.rh,p2017.rh,ncol=1,nrow=3)
p2<-ggarrange(p2015.ch,p2016.ch,p2017.ch,ncol=1,nrow=3)
ggarrange(p2015.rh,p2015.ch,p2016.rh,p2016.ch,p2017.rh,p2017.ch,ncol=1,nrow=6,labels ="Cancellation Rate per year for each hotel",common.legend = TRUE,legend = "right")


##### plot - Average Daily Rate for Repeated Guests #####

#preparation of plot data
boxplot.data<-group_by(hb,reserved_room_type,is_repeated_guest,hotel) %>%
  summarise(avg.adr=mean(adr))
head(boxplot.data)

#creation of plot for City Hotel
boxplot.data.ch<-filter(boxplot.data,hotel=="City Hotel")
bp1<-ggplot(boxplot.data.ch,
       aes(
         factor(is_repeated_guest,labels=c("No","Yes")),
         avg.adr,
         color=reserved_room_type))+
  geom_jitter(size=2.5,position=position_dodge(0))+
  guides(col=guide_legend("Room Type"))+
  geom_boxplot(fill="#6699CC",alpha=0.6,colour="black",size=0.8,width=0.4)+
  labs(x="Repeated Guest",y="City Hotel",title = "Average Daily Rate for Repeated Guests")

#creation of plot for Resort Hotel
boxplot.data.rh<-filter(boxplot.data,hotel=="Resort Hotel")
bp2<-ggplot(boxplot.data.rh,
       aes(
         factor(is_repeated_guest,labels=c("No","Yes")),
         avg.adr,
         color=reserved_room_type))+
  geom_jitter(size=2.5,position=position_dodge(0))+
  guides(col=guide_legend("Room Type"))+
  geom_boxplot(fill="#6699CC",alpha=0.6,colour="black",size=0.8,width=0.4)+
  labs(x="Repeated Guest",y="Resort Hotel")

#creation of aggregated graph
ggarrange(bp1,bp2,nrow = 2,ncol=1,common.legend = TRUE,legend = "right")

##########
ggplot(data=hb)+
  geom_sf(aes(fill=country))+
  scale_fill_viridis_c(option="plasma")

require(maps)
require(viridis)
library(mapdata)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill=hb$stays_in_week_nights, colour = "white")
View(world_map)
jp <- ggplot2::map_data('world2')

temp.boxplot<-hb %>%
  group_by(reserved_room_type,is_repeated_guest,adr) %>%
  summarise(n=sum(adr))
View(temp.boxplot)

boxplot.data<-group_by(hb,reserved_room_type,is_repeated_guest) %>%
  summarise(avg.adr=mean(adr))
View(boxplot.data)

boxplot.data$is_repeated_guest<-as.factor(boxplot.data$is_repeated_guest)

ggplot(boxplot.data,
       aes(factor(is_repeated_guest,labels=c("No","Yes")),avg.adr,color=reserved_room_type))+
  geom_jitter(size=2.5,width = 0.5, height = 0.5)+
  guides(col=guide_legend("Room Type"))+
  geom_boxplot(fill="#6699CC",alpha=0.8,colour="black",size=0.8)+
  labs(x="Repeated Guest",y="Average Daily Rate",title="Average Daily Rate for Repeated Guests")


ggplot(hb,aes(
  x=stays_in_week_nights,
  y=previous_cancellations
))+
  geom_point()
