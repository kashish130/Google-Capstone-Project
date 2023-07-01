#Importing Datasets
daily_activity<- read.csv("C:\\Users\\user\\Desktop\\Fitabase Data\\dailyActivity_merged.csv")
weight<- read.csv("C:\\Users\\user\\Desktop\\Fitabase Data\\weightLogInfo_merged.csv")
daily_sleep<- read.csv("C:\\Users\\user\\Desktop\\Fitabase Data\\sleepDay_merged.csv")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(skimr)
library(scales)
library(janitor)
head(daily_activity)
head(weight)
head(daily_sleep)
glimpse(daily_activity)
glimpse(weight)
glimpse(daily_sleep)
daily_activity_new<- daily_activity%>%
  filter(TotalSteps!=0)
weight_new<- weight%>%
  separate(Date,into = c("Date","Time"),sep = " ")
daily_sleep_new<- daily_sleep%>%
  separate(SleepDay,into = c("Date","Time"),sep = " ")
n_distinct(daily_activity_new$Id)
n_distinct(daily_sleep_new$Id)
n_distinct(weight_new$Id)
nrow(daily_activity_new)
nrow(daily_sleep_new)
nrow(weight_new)
nrow(unique(daily_activity_new))
nrow(unique(daily_sleep_new))
nrow(unique(weight_new))
daily_sleep_unique<- unique(daily_sleep_new)
skim_without_charts(daily_activity_new)
skim_without_charts(daily_sleep_unique)
skim_without_charts(weight_new)
daily_activity_final <- daily_activity_new %>% 
  select(Id, ActivityDate, TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>% 
  rename(Date = ActivityDate)
weight_final <- weight_new %>% 
  select(Id, Date, BMI, WeightPounds, IsManualReport)
daily_sleep_final <- daily_sleep_unique %>% 
  select(Id, Date, TotalMinutesAsleep, TotalTimeInBed)
VeryActiveMin <- sum(daily_activity_final$VeryActiveMinutes)
FairlyActiveMin <- sum(daily_activity_final$FairlyActiveMinutes)
LightlyActiveMin <- sum(daily_activity_final$LightlyActiveMinutes)
SedentaryMin <- sum(daily_activity_final$SedentaryMinutes)
TotalMin <- VeryActiveMin + FairlyActiveMin + LightlyActiveMin + SedentaryMin
slices <- c(VeryActiveMin,FairlyActiveMin,LightlyActiveMin,SedentaryMin)
lbls <- c("VeryActive","FairlyActive","LightlyActive","Sedentary")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%", sep="")
pie(slices, labels = lbls,col = rainbow(length(lbls)),  main = "Percentage of Activity in Minutes")
ggplot(daily_activity_final)+ geom_point(mapping=aes(x=TotalSteps,y=Calories),color="Red")+ geom_smooth(mapping = aes(x=TotalSteps,y=Calories))+labs(title = "Relationship between Total Steps and Calories Burned")
combined_data <- merge(daily_activity_new, daily_sleep_unique, by=c("Id"))
combined_data$user_steps <- " "
combined_data_grouped <- combined_data %>% 
  group_by (Id) %>% 
  summarize(average_totalsteps = mean(TotalSteps),
            average_totalcalories = mean(Calories),
            average_totaldistance = mean(TotalDistance),
            average_minutesasleep = mean(TotalMinutesAsleep, na.rm = TRUE)) %>% 
  mutate(user_steps = case_when(
    average_totalsteps >= 10000 ~ "Highly Active/Active",
    average_totalsteps >= 7500 & average_totalsteps < 10000 ~ "Somewhat Active",
    average_totalsteps >= 5000 & average_totalsteps < 7500 ~ "Low Active",
    average_totalsteps < 5000 ~ "Sedentary"))
combined_data <- subset(combined_data, select = -user_steps)
combined_data_grouped <- merge(combined_data, combined_data_grouped, by= c("Id"))
combined_data_grouped$user_steps <- factor(combined_data_grouped$user_steps, levels = c("Sedentary", "Low Active", "Somewhat Active", "Highly Active/Active"))
ggplot(combined_data_grouped, aes(user_steps, TotalMinutesAsleep))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = Calories, color = Calories))+
  labs(title = "Activity Level vs Daily Sleep Minutes", x = "Activity Level", y = "Daily Sleep Minutes", fill= "Activity Level", color= "Daily Calories Burned", caption= "Data Source: 
Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")
ggplot(combined_data_grouped, aes(user_steps, TotalSteps))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = Calories, color = Calories))+
  labs(title = "Activity Level vs Daily Steps", x = "Activity Level", y = "Daily Steps", fill= "Activity Level", size= "", color= "Daily Calories Burned", caption= "Data Source: Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")
