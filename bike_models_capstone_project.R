# Install/Load Necessary Packages for Project ----------------------------------

install.packages("tidyverse")
library(tidyverse)
library(data.table)
library(lubridate)

# Laying a path to the files, importing multiple files at once as csv, have to also check all files are in the correct format.

bike_data1 <- list.files(path = "Google Data Analystics Course/Course 8 Cape Stone Project/bike_trip_data/", pattern = ".csv") 

# Concat the directory to the file names

bike_data1 <- str_c("Google Data Analystics Course/Course 8 Cape Stone Project/bike_trip_data/", bike_data1)

# Provide name to each element of the vector, this also acts as a way to name and count the data for each month

names(bike_data1) <- c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")


# applying a function to each element of the vector, saved as data frame, all data aggregated successfully

total_data <- map_df(.x = bike_data1, .f = read_csv, .id = "data_source")


# Creating a ride_length column, fixing the fact that end_at and start_at would regularly be reversed causing negative travel times to occur

ride_length_added <- total_data %>%
  mutate(total_data, ride_length_sec = ended_at - started_at) %>% 
  mutate(total_data, number = as.numeric(ride_length_sec)) %>% 
  mutate(total_data, adjusted = abs(number)) %>% 
  mutate(total_data, ride_time = seconds_to_period(adjusted))

# Selecting only the cleaned data

cleaned_data<- ride_length_added %>%
  select("data_source","ride_id", "rideable_type","started_at", "ended_at", "start_station_name",
         "start_station_id",   "end_station_name",   "end_station_id",     "start_lat",         "start_lng",     "end_lat",           
         "end_lng", "member_casual","adjusted",  "ride_time" )

# Adding a day_of_the_week column, all of the data needed for analysis have been collected

day_of_the_week <- weekdays(as.Date(total_data$started_at))

all_data <- data.frame(day_of_the_week, cleaned_data)

#now checking or duplicates in the data using ride_id variable

unique(all_data[c("ride_id")]) # matches total number of rows, there are no duplicates

# some calculations with the data, mean, max, median, had to work with seconds then convert to day format, then had to limit the mean to 2 decimal places.

calc <- all_data %>% 
  summarize(mean_rd = format(round(seconds_to_period(mean(adjusted)),2), nsmall = 2), max_rd = seconds_to_period(max(adjusted)), median_rd = seconds_to_period(median(adjusted)))

write.csv(calc, "calc")

# Gather a list of No: of rides during the each month and each weekday. Have to save as dataframe or ggplot wont work, and sort in descending order

most_ride_month <- as.data.frame(sort(table(all_data$data_source)))

most_ride_day <- as.data.frame(sort(table(all_data$day_of_the_week)))

# Creating some visuals from all of the data, monthly
# manually reordered it so that it goes chronologically has to be in the same pipe for the reorder to work, Also have to manually color the individual bars.

month_data <- most_ride_month %>% 
  mutate(Var1 = fct_relevel(Var1,"Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")) %>% 
  ggplot(aes(x=Var1, y=Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("steelblue","steelblue","steelblue","steelblue","steelblue","red3","red3","red3","red3","red3","red3", "steelblue")) +
  labs(title = "Summer Months Fun!", subtitle = "There is a surge in rides over the summer months", x = "Months of the Year", y = "Number of Rides (2e+05 = 200,000)", caption=paste0("Data from: ", "December 2020", " to ", "November 2021")) +
  annotate("segment", x = 4, xend = 6, y = 450000, yend = 750000, color = "black", size = 2, arrow = arrow()) +
  guides(fill = guide_legend(title = "Months"))

# Creating visuals for weekdays

day_data <-  most_ride_day %>% 
  mutate(Var1 = fct_relevel(Var1,"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("steelblue","steelblue","steelblue","steelblue","red3","red3","red3")) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Weekend Package Opportunity", subtitle = "Surge in Rides over the Weekend", x = "Weekdays", y = "Number of Rides", caption=paste0("Data from: ", "December 2020", " to ", "November 2021")) +
  annotate("label", x=6, y=650000, label="Opportunity", color = "black", fontface = "bold", size = 5.5) +
  annotate("segment", x = 4.6, xend = 7.4, y = 600000, yend = 600000, size = 1.2, arrow = arrow(ends = "both", angle = 90, length = unit(.3,"cm"))) +
  guides(fill = guide_legend(title = "Weekdays"))


day_data
month_data

# Further investigation into the data for casual and members

unique(all_data[c("member_casual")]) # check for unique values, checking for no errors

# creating data frames for casual and members. Performing initial calculations. tend to go on for longer, probably for fun, 

casual_riders <- all_data %>% 
  filter(member_casual == "casual")

calc2 <- casual_riders %>% 
  summarize(mean_cs = format(round(seconds_to_period(mean(adjusted)),2), nsmall = 2), max_cs = seconds_to_period(max(adjusted)), median_cs = seconds_to_period(median(adjusted)))

member_riders <- all_data %>% 
  filter(member_casual == "member")

calc3 <- member_riders %>% 
  summarize(mean_mm = format(round(seconds_to_period(mean(adjusted)),2), nsmall = 2), max_mm = seconds_to_period(max(adjusted)), median_mm = seconds_to_period(median(adjusted)))

View(calc2)

# casual vs member calculation dodge bar chart

column1 <- c(rep("Median", 2), rep("Mean", 2), rep("Max", 2))
column2 <- gl(2, 1, 6, labels=c("Casual", "Member"))
column3 <- c(16.1, 9.72, 33.08, 16.61, 55.94, 29.05)

d <- data.frame(column1=column1, column2=column2, column3=column3)

mmm <- ggplot(d, aes(x=column1, y=column3, fill=column2)) + geom_bar(position=position_dodge(),stat="identity") +
  labs(title = "Casual Riders Have More Fun!", x = "", y = "Minutes Ridden", caption=paste0("Max scaled 1:1000")) +
  annotate("text", x=2.3, y=50, label="Members want to enjoy their routine", color = "black", fontface = "bold", size = 4.5) +
  guides(fill = guide_legend(title = "Casual/Member"))

mmm

# pie chart casual vs members in all recorded rides

colours = c("red", "blue")
cs_mm <- c(2489347, 2989749)
percentage <- c("casual", "member")
pct <- round(cs_mm/sum(cs_mm)*100)
percentage <- paste(percentage, pct)
percentage <- paste(percentage,"%",sep=" ")
pie <- pie(cs_mm, labels = percentage, main = "percentage of members vs casual", col = c("red3", "steelblue"))
pie

# creating some more visuals for casual vs members on Weekdays

most_ride_day_cs <- as.data.frame(sort(table(casual_riders$day_of_the_week)))

most_ride_day_mm <- as.data.frame(sort(table(member_riders$day_of_the_week)))


day_data_cs <-  most_ride_day_cs %>% 
  mutate(Var1 = fct_relevel(Var1,"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("steelblue","steelblue","steelblue","steelblue","red3","red3","red3")) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Weekend Surge for Casual Riders", x = "Weekdays", y = "Number of Rides (2e+05 =200,000)", caption=paste0("Data from: ", "December 2020", " to ", "November 2021")) +
  annotate("segment", x = 4, xend = 6, y = 400000, yend = 650000, color = "black", size = 2, arrow = arrow())

day_data_cs <- day_data_cs + guides(fill = guide_legend(title = "Weekdays"))

day_data_cs

day_data_mm <-  most_ride_day_mm %>% 
  mutate(Var1 = fct_relevel(Var1,"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("steelblue","steelblue","steelblue","steelblue","red3","red3","red3")) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Gradual Dip For Members", x = "Weekdays", y = "Number of Rides (2e+05 =200,000)", caption=paste0("Data from: ", "December 2020", " to ", "November 2021")) +
  annotate("segment", x = 3, xend = 7, y = 500000, yend = 450000, color = "black", size = 2, arrow = arrow())

day_data_mm <- day_data_mm + guides(fill = guide_legend(title = "Weekdays"))

day_data_mm

# Exported the day data to excel to create a line graph for comparison

write.csv(most_ride_day_cs, "day.csv")
write.csv(most_ride_day_mm, "day1.csv")

# creating some visuals for casual vs members for the Year
# direct comparison of the two

final_plot <- all_data %>% 
  mutate(data_source = fct_relevel(data_source,"Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")) %>% 
  ggplot(aes(x = data_source, fill = member_casual)) +
  geom_bar(stat = "count") +
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 55)) +
  labs(title = "Casual vs Member ", subtitle = "Casual have higher peaks and drops compared to members", x = "", y = "Number of Rides", caption=paste0("Data from: ", "December 2020", " to ", "November 2021")) +
  annotate("segment", x = 6.6, xend = 11.4, y = 390000, yend = 390000, size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.3,"cm"))) +
  annotate("segment", x = 1, xend = 3, y = 50000, yend = 50000, size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.3,"cm")))

final_plot

# 45% of all riders have been casual. Summary, casual riders value fun, warm weather and the weekends. going on longer rides on average and median. While members are more consistent.going to work, they use it as a mode of transport
# I would advise the marketing team target the a weekend and summer deal. We can have them as bi-annual members or weekend member that can create a sales funnel for them to become annual members
