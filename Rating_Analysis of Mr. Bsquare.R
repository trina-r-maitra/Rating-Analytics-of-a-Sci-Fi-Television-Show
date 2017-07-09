### NBC Data Challenge


#### Install Required Packages

install.packages("dplyr") # Data Munging/Manipulation
library(dplyr)

install.packages("ggplot2") # Plots
library(ggplot2)

install.packages("lubridate") # Date/Time Munging
library(lubridate)

install.packages("scales") # Plot breaks in GGplot
library(scales)

install.packages("lmtest") # Granger Causality Test
library(lmtest)

install.packages("tseries") # ADF Test for stationarity
library(tseries)

####################################################################################################
### IMPORT DATA 
####################################################################################################

### Read Data - Ratings by minute for each episode of Mr. Bl@ck.s.bot
mxm=read.csv("C:\\MSBAPM\\NBC\\mxm.csv") %>% mutate(Date=as.Date(Date,format="%Y-%m-%d"),
                                                   telecast_time= hms(as.character(Time)))

### Read Data - Overall program ratings with competitive networks

overall_ratings=read.csv("C:\\MSBAPM\\NBC\\seasonal_telecast_ratings.csv") %>%
  mutate(time_slot=as.POSIXct(as.character(time_slot), format="%Y-%m-%d %H:%M:%S"),
         telecast_date=as.Date(time_slot), 
         telecast_hour=factor(ifelse(hour(time_slot)==19,"7 PM",
                       ifelse(hour(time_slot)==20,"8 PM",
                       ifelse(hour(time_slot)==21,"9 PM","10 PM"))), 
                       levels=c("10 PM", "9 PM", "8 PM", "7 PM"),ordered=TRUE))
  

####################################################################################################
### DATA MUNGING FOR EXPLORATORY ANALYSIS & PLOTS 
####################################################################################################

# Show Time Change

show_time_change=mxm %>% mutate(show_time_pm=hour(telecast_time)) %>% 
  group_by(Date)%>% select(Date, show_time_pm) %>% unique()

# From 2016-09-21 onwards the Show has been moved to 8:00 PM to 9:00 PM slot


### Data Munging for Exploratory Plots
mxm_trend=mxm %>% group_by(Date) %>% summarize(median_rating=round(median(Rating,na.rm=TRUE),2),
                                               mean_rating=round(mean(Rating,na.rm=TRUE),2),
                                               median_loss_perc=round(median(Total_Loss_perc,na.rm=TRUE),2),
                                               mean_loss_perc=round(mean(Total_Loss_perc,na.rm=TRUE),2),
                                               total_ad_minutes=sum(Minute_In_Commercial))


### Save Theme setting for the Exploratory Plots
plot_theme=theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(size=14,face="bold"),
                 plot.title = element_text(size=18,face="bold"),
                 legend.title = element_blank(),
                 legend.text=element_text(size=12,face="bold"))

plot_text_lines= list(geom_vline(xintercept = 24,linetype=1, size=1,color="red"),
  geom_text(aes(x=15,y=2.0), label="Season 1", size=5, color="red"),
  geom_text(aes(x=30,y=2.0), label="Season 2", size=5,color="red"),
  geom_vline(xintercept = 25,linetype=1, size=1,color="blue"),
  geom_text(aes(x=30,y=2.2), label="8PM - 9PM slot", size=5,color="blue"),
  geom_text(aes(x=15,y=2.2), label="10PM - 11PM slot", size=5,color="blue"))

### Grouping hour long episode of Mr. BB in 4 groups of 15 minutes each

episode_time_segments=mxm %>% mutate(minute_group=ifelse(minute(telecast_time)<15, "0-15", 
                                                         ifelse(minute(telecast_time)<30, "15-30", 
                                                                ifelse(minute(telecast_time)<45, "30-45","45-60")))) %>%
  mutate(minute_group=as.factor(minute_group))%>%
  group_by(Date, minute_group) %>% 
  summarize(total_ad_minutes=sum(Minute_In_Commercial),
            mean_rating=round(mean(Rating,na.rm=TRUE),2),
            mean_loss_perc=round(mean(Total_Loss_perc,na.rm=TRUE),2)) 

####################################################################################################
### STATISTICAL TESTS
####################################################################################################

### Hypothesis Tests

seasons=mxm %>% mutate(season=ifelse(Date <='2016-05-23',"1","2"),
                       time_slot=ifelse(hour(telecast_time)==10,"10PM-11PM","8PM-9PM"))

# Claim 1: Average Rating of Season 1 is higher than Average Rating of Season 2
# Null Hypothesis - Average Rating of Season 1 is same as Average Rating of Season 2
# Alternate Hypothesis - Average Rating of Season 1 is higher than Average Rating of Season 2

t.test(Rating~season, data=seasons, alternative=c("greater"))

# Claim 2: Average Viewership Loss of Season 1 is lesser than Average Viewership Loss of Season 2
# Null Hypothesis - Average Viewership Loss of Season 1 is same as Average Viewership Loss of Season 2
# Alternate Hypothesis - Average Viewership Loss of Season 1 is lesser than Average Viewership Loss of Season 2

t.test(Total_Loss_perc~season, data=seasons, alternative=c("less"))

# Claim 3: Changing the show timing to 8PM-9PM from 10PM-11PM has increased rating, on average.
# Null Hypothesis - Average Rating of 10PM-11PM slot is same as Average Rating of 8PM-9PM slot
# Alternate Hypothesis - Average Rating of 10PM-11PM slot is higher than Average Rating of 8PM-9PM slot

t.test(Rating~time_slot, data=seasons, alternative=c("greater"))


# Claim 4: Total Ad minutes has decreased in Season 2 because of poor ratings
# Null Hypothesis - Average commercial minutes in Season 1 & 2 are same
# Alternate Hypothesis - Average commercial minutes in Season 1 > Average commercial minutes in season 2

ad=mxm_trend %>% mutate(season=ifelse(Date <='2016-05-23',"1","2"))
t.test(total_ad_minutes~season, data=ad, alternative=c("greater"))

# Granger Causality Test between Viewership Loss and Total Commercial Minutes 
# during first 15 minutes

# 0-15 minute group
group1=episode_time_segments %>% filter(minute_group=="0-15")

#Check whether the series are stationary

adf.test(diff(diff(group1$total_ad_minutes)))
adf.test(diff(group1$mean_loss_perc))


#H0 : Total Ad Minutes does not cause Viewership Loss
#HA : Total Ad Minutes causes Viewership Loss

grangertest(diff(diff(mean_loss_perc))~diff(diff(total_ad_minutes)),order=1,data=group1)


### Correlation between Rating and Total Commercial Minutes
cor(mxm_trend$mean_rating,mxm_trend$total_ad_minutes)


####################################################################################################
### PLOTS
####################################################################################################

### Plot 1
# Ratings Trend Plot

ggplot(data=mxm_trend)+
  geom_line(aes(x=as.factor(Date),y=median_rating,group=1,color="Median Rating"),size=2)+
  geom_line(aes(x=as.factor(Date),y=mean_rating,group=1,color="Mean Rating"),size=2)+
  plot_theme +
  xlab("Telecast Date")+ ylab(" ")+
  ggtitle("Rating Trend of Mr. Bl@ck.s.bot across Season 1 & 2")+ plot_text_lines+
  geom_vline(xintercept = 14,linetype=2, size=1,color="black")
 

### Plot 2
# Total Loss Perc Trend

loss_per_trend_plot=ggplot(data=mxm_trend)+
  geom_line(aes(x=as.factor(Date),y=median_loss_perc,group=1,color="Median Loss Per"),size=2)+
  geom_line(aes(x=as.factor(Date),y=mean_loss_perc,group=1,color="Mean Loss Per"),size=2)+
  plot_theme+
  xlab("Telecast Date")+ ylab(" ")+
  ggtitle("Viewership Loss % of Mr. Bl@ck.s.bot across Season 1 & 2")+plot_text_lines
  

### Plot 3
# Total ads during the Minute groups

commercial_trend_plot=ggplot(data=episode_time_segments,aes(x=as.factor(Date),y=total_ad_minutes,
                                                            fill=minute_group))+
  geom_bar(stat="identity",size=2)+facet_grid(minute_group~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(size=12, face="bold"))+
  xlab("Telecast Date")+ ylab("Total Commercial Minutes")+
  ggtitle("Commercial Trend of Mr. Bl@ck.s.bot across the minute groups")+
  geom_vline(xintercept = 24,linetype=1, size=1,color="red")+
  geom_text(aes(x=15,y=9), label="Season 1", size=4, color="red")+
  geom_text(aes(x=30,y=9), label="Season 2", size=4,color="red")+
  geom_vline(xintercept = 25,linetype=1, size=1,color="blue")+
  geom_text(aes(x=30,y=11), label="8PM - 9PM slot", size=4,color="blue")+
  geom_text(aes(x=15,y=11), label="10PM - 11PM slot", size=4,color="blue")


### Plot 4
# Average Rating Trend during the Minute groups

avg_rating_trend_plot=ggplot(data=episode_time_segments,
                            aes(x=as.factor(Date),y=mean_rating,group=minute_group,color=minute_group))+
  geom_line(size=2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"))+
  xlab("Telecast Date")+ ylab("Average Rating")+
  ggtitle("Average Rating of Mr. Bl@ck.s.bot across the minute groups")+
  geom_vline(xintercept = 24,linetype=1, size=1,color="red")+
  geom_text(aes(x=15,y=2.5), label="Season 1", size=4, color="red")+
  geom_text(aes(x=30,y=2.5), label="Season 2", size=4,color="red")+
  geom_vline(xintercept = 25,linetype=1, size=1,color="blue")+
  geom_text(aes(x=30,y=3), label="8PM - 9PM slot", size=4,color="blue")+
  geom_text(aes(x=15,y=3), label="10PM - 11PM slot", size=4,color="blue")



### Plot 5
# Viewership Loss Trend during the Minute groups

viewership_loss_trend_plot=ggplot(data=episode_time_segments,
                             aes(x=as.factor(Date),y=mean_loss_perc,group=minute_group,color=minute_group))+
  geom_line(size=2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"))+
  xlab("Telecast Date")+ ylab("Average Viewership Loss")+
  ggtitle("Average Viewership Loss of Mr. Bl@ck.s.bot across the minute groups")+
  geom_vline(xintercept = 24,linetype=1, size=1,color="red")+
  geom_text(aes(x=15,y=4), label="Season 1", size=4, color="red")+
  geom_text(aes(x=30,y=4), label="Season 2", size=4,color="red")+
  geom_vline(xintercept = 25,linetype=1, size=1,color="blue")+
  geom_text(aes(x=30,y=5), label="8PM - 9PM slot", size=4,color="blue")+
  geom_text(aes(x=15,y=5), label="10PM - 11PM slot", size=4,color="blue")

####################################################################################################
### OverAll Ratings - DATA MUNGING & EXPLORATORY PLOTS
####################################################################################################

### Assuming Mr. Bl@ck.s.bot belongs to Other(Sci Fi) category

overall_ratings_genre=overall_ratings %>% 
  mutate(genre_group=ifelse(genre=="Other", "Other-Mr.BB", "Anime+Comedy+Drama"))


### First Time Show and not season premier show

first_time_show=overall_ratings_genre %>% filter(show_repeat==0 & show_premier==0) %>%
  group_by(telecast_date,telecast_hour,network,genre_group) %>% summarize(mean_rating=mean(rating*100))
                                                                    
ggplot(data=first_time_show,aes(x=telecast_date,y=mean_rating, group=genre_group,color=genre_group))+
  geom_line()+
  facet_grid(telecast_hour~network)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=12, face="bold"))+
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))+
  xlab("Telecast Date")+ ylab("Average Rating")+
  ggtitle("Average Rating across Networks & Telecast Hour for new & non premier shows")


### Repeat and non-season premier show
 
repeat_time_show=overall_ratings_genre %>% filter(show_repeat==1 & show_premier==0) %>%
  group_by(telecast_date,telecast_hour,network,genre_group) %>% summarize(mean_rating=mean(rating))

ggplot(data=repeat_time_show,aes(x=telecast_date,y=mean_rating, group=genre_group,color=genre_group))+geom_line()+
  facet_grid(telecast_hour~network)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=12, face="bold"))+
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))+
  xlab("Telecast Date")+ ylab("Average Rating")+
  ggtitle("Average Rating Trend across Networks & Telecast Hour for repeat & non premier shows")

### New & Season premier show

new_premier_show=overall_ratings_genre %>% filter(show_repeat==0 & show_premier==1) %>%
  group_by(telecast_date,telecast_hour,network,genre_group) %>% summarize(mean_rating=mean(rating))

ggplot(data=new_premier_show,aes(x=telecast_date,y=mean_rating, group=genre_group,color=genre_group))+geom_line()+
  facet_grid(telecast_hour~network)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=12, face="bold"))+
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))+
  xlab("Telecast Date")+ ylab("Average Rating")+
  ggtitle("Average Rating Trend across Networks & Telecast Hour for new premier shows")


### Repeat & Season premier show

repeat_premier_show=overall_ratings_genre %>% filter(show_repeat==1 & show_premier==1) %>%
  group_by(telecast_date,telecast_hour,network,genre_group) %>% summarize(mean_rating=mean(rating))

ggplot(data=repeat_premier_show,aes(x=telecast_date,y=mean_rating, group=genre_group,color=genre_group))+geom_line()+
  facet_grid(telecast_hour~network)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=12,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        plot.title = element_text(size=18,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=12, face="bold"))+
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))+
  xlab("Telecast Date")+ ylab("Average Rating")+
  ggtitle("Average Rating Trend across Networks & Telecast Hour for repeat premier shows")



