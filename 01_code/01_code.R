#INITIALISING

install.packages(c("readr", "tidyverse", "ggplot2","lubridate", "zoo", "RcppRoll", "MASS", "brant", "fastDummies","car"))
library(car)
library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(RcppRoll)
library(MASS)
library(brant)
library(fastDummies)

data <- read_csv("https://github.com/Derek-Jones/renzo-pomodoro/raw/master/all-tasks.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#CLEANING DATA

#Removing outliers and NA values in the estimate column


estimateQ1 <- quantile(data$estimate, 0.25, na.rm = TRUE)
estimateQ3 <- quantile(data$estimate, 0.75, na.rm = TRUE)

estimateIQR <- estimateQ3 - estimateQ1

data <- data %>%
  filter(estimate >= estimateQ1 - 2 * estimateIQR,
         estimate <= estimateQ3 + 2 * estimateIQR)

actualQ1 <- quantile(data$actual, 0.25, na.rm = TRUE)
actualQ3 <- quantile(data$actual, 0.75, na.rm = TRUE)

actualIQR <- actualQ3 - actualQ1

data <- data %>%
  filter(actual >= actualQ1 - 2 * actualIQR &
           actual <= actualQ3 + 2 * actualIQR | is.na(actual))

#Removing the 12 tasks estimated to take 0.5 pomodoros, and the 22 estimated to
#take 0.

data <- data %>%
  filter(estimate > 0.5)


#After this date ("2018-10-17") he stops marking any tasks as done.
data <- data[1:max(which(data$date == "2018-10-16")),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#BASIC INFORMATION

#Information about the dates in the dataset
unique_dates <- unique(data$date)
unique_date_count <- length(unique_dates)
earliest_date <- min(data$date, na.rm = TRUE)
latest_date <- max(data$date, na.rm = TRUE)

#Number of tasks for each year
date_years <- as.numeric(format(data$date, "%Y"))
table(date_years)

#Number of unique task descriptions
unique_tasks <- unique(data$X.words)

#Number of tags
all_tags <- trimws(unlist(strsplit(data$X.words, split = ",")))
unique_tags <- unique(all_tags)

#Overall correlation between estimated and actual
est_act_corr <- cor(data$estimate, data$actual, use="complete.obs", method="pearson")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#OVERALL ACCURACY AND BIAS

#Creating a new column to show over/underestimation
data$difference <- data$estimate - data$actual

#An error of 1 is a lot more when the estimate was 1 than when the estimate
# was 9, so using log values to create a column for error proportional to 
#the estimate, and which is symmetrical around 0.

#The 933 actual column values of 0 are causing infinite error values, so I'm
#replacing them all with 0.5, unless they weren't actually done, in which case
#I'm replacing them with NA values. 

data <- data %>%
  mutate(actual = if_else(actual == 0, 0.5, actual)) %>%
  mutate(estimate = if_else(estimate == 0, 0.5, estimate))

data <- mutate(data, log_error = log(estimate)-log(actual))

data <- mutate(data, proportional_error = ifelse(log_error>=1, 2.71828^log_error, -1/(2.71828^log_error)))

#overall bias (under/overestimation)
mean(data$difference, na.rm = TRUE)
mean(data$log_error,na.rm=TRUE)

#overall bias, but subsetting data for tasks marked as done

data %>%
  subset(DONE == 1) %>%
  summarize(mean_value = mean(log_error, na.rm = TRUE)) %>%
  pull(mean_value)

#Creating a year column to group data
data$year <- format(data$date, "%Y")

#Finding the mean of over/estimation for each year across the dataset
bias_by_year <- data %>%
  group_by(year) %>%
  summarise(mean_value = mean(log_error, na.rm = TRUE),
            n = n(),
            sd = sd(log_error, na.rm = TRUE),
            se = sd / sqrt(n),
            ci = 1.96 * se)

bias_by_year$year <- as.numeric(as.character(bias_by_year$year))


ggplot(bias_by_year, aes(x=year,y=mean_value))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = mean_value - ci, ymax = mean_value + ci, width = 0.2))+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x="Year",y="Mean Log Difference")+
  scale_x_continuous(breaks=2009:2019)+
  ggtitle("Under/Overestimation Error Over Each Year of the Dataset: All Tasks")


#and now filtering also for Done tasks
data %>%
  subset(DONE == 1) %>%
  group_by(year) %>%
  summarize(mean_value = mean(log_error, na.rm = TRUE))


bias_by_year_done <- data %>%
  subset(DONE == 1) %>%
  group_by(year) %>%
  summarise(mean_value = mean(log_error, na.rm = TRUE),
            n = n(),
            sd = sd(log_error, na.rm = TRUE),
            se = sd / sqrt(n),
            ci = 1.96 * se)

bias_by_year_done$year <- as.numeric(as.character(bias_by_year_done$year))


ggplot(bias_by_year_done, aes(x=year,y=mean_value))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = mean_value - ci, ymax = mean_value + ci, width = 0.2))+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  scale_x_continuous(breaks=2009:2019)+
  labs(x="Year",y="Mean of Log Difference")+
  ggtitle("Under/Overestimation Error Over Each Year of the Dataset: Core Tasks")


#Creating a column to show the month of each task
data$yearMonth <- format(data$date, "%Y-%m")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CODING DIFFERENT TYPES OF TASK

#Different types:
#Done: planned and completed on the same day
#Eventually Done: planned but completed a day or more later
#Never Done: Planned but not completed 
#Maybe Done: not marked as done, but given an 'actual' number of pomodoros taken
#Maybe Eventually done: as above, but a day or more later.

#To deduce which type a task is, I need three pieces of information:
#1. Whether the task was planned and done on the previous working day
#2. Whether the task was planned but *not* done on previous day
#3. Whether the task was also planned the next day (done or not)

#So I'm going to create three new columns to indicate these and then
# figure out the Done type of each row 


#original order of the columns to help keep track of chronological order
data <- mutate(data, original_order = row_number())

all_dates <- data$date
all_dates <- unique(all_dates)
all_dates <- sort(all_dates)

find_previous_date <- function(current_date) {
  position <- which(all_dates == current_date)
  
  if (position - 1 != 0) {
    return(all_dates[position - 1])
  } else {
    return(NA)
  }
}

#Creating a column of the previous working day (i.e. accounting for weekends
# and holidays) next to each task
data <- data %>%
  mutate(previous_date = as.Date(purrr::map_dbl(date, find_previous_date), origin = "1970-01-01"))

#Same but for next working day
find_next_date <- function(current_date) {
  position <- which(all_dates == current_date)
  
  if (position + 1 != 0) {
    return(all_dates[position + 1])
  } else {
    return(NA)
  }
}

data <- data %>%
  mutate(next_date = as.Date(purrr::map_dbl(date, find_next_date), origin = "1970-01-01"))

data <- arrange(data, description, date)

#1. Column to indicate if task done on previous working day 
data <- mutate(data, done_previous_day = ifelse(lag(date) == previous_date & lag(DONE) == 1 & lag(description)==description, 1, 0))

#The same task may be done multiple times on the same day, so this line corrects
#incorrect 0's resulting from the fact that the above line of code assumes that 
#lag(date) will be yesterday or earlier
data <- mutate(data, done_previous_day = ifelse(lag(date)==date & lag(done_previous_day)==1 & lag(description)==description, 1,done_previous_day))

#2. Task not done on previous day 
data <- mutate(data, not_done_previous_day = ifelse(lag(date) == previous_date & lag(DONE) == 0 & lag(description)==description, 1, 0))

#3. Task also next day (done or not irrelevant)
data <- mutate(data, task_next_day = ifelse(lead(date) == next_date & lead(description)==description, 1,0))


#Now, creating columns for the five types of Done:
#1. Only: if not: task not done on previous day AND if task done
data <- mutate(data, Done_Only = ifelse(not_done_previous_day == 0 & DONE == 1,1,0))
#2. Never: if not task also appears next day AND if task not done AND task doesn’t have an actual number
data <- mutate(data, Done_Never = ifelse(task_next_day==0 & DONE==0 & is.na(actual)==TRUE,1,0))
#3. Maybe Only: if marked as not done AND has an actual number of pomodoros AND not identical task not done on previous day
data <- mutate(data, Done_Maybe_Only = ifelse(DONE==0 & is.na(actual)==FALSE & not_done_previous_day==0,1,0))
#4. Maybe eventually: if marked as not done AND has an actual number of pomodoros AND identical task not done on previous day
data <- mutate(data, Done_Maybe_Eventually = ifelse(DONE==0 & is.na(actual)==FALSE & not_done_previous_day ==1,1,0))
#5.Eventually: if task not done on previous day AND task done
data <- mutate(data, Done_Eventually = ifelse(not_done_previous_day==1 & DONE==1,1,0))

#Done Totals
data %>%
  dplyr::select("Done_Only","Done_Never","Done_Maybe_Only","Done_Maybe_Eventually","Done_Eventually") %>%
  colSums(na.rm = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#EXAMINING BIAS OVER TIME

#Finding the mean of over/underestimation for each year & month across the dataset
estimatesByMonth <- data %>%
  group_by(yearMonth) %>%
  summarize(mean_value = mean(log_error, na.rm = TRUE))

#and again filtering also for Done tasks
estimatesByMonthDone <- data %>%
  subset(DONE == 1) %>%
  group_by(yearMonth) %>%
  summarize(mean_value = mean(log_error, na.rm = TRUE))

#And now plotting estimation over time
#Starting with all values (i.e. Done and Maybe Done)

allMonths <- unique(data$yearMonth)

byMonthdf <- data.frame(months= allMonths, x = 1:length(allMonths), y= estimatesByMonth)

monthPlot <- ggplot(byMonthdf, aes(x=x, y=y.mean_value)) +
  geom_line() +
  geom_point() +
  labs(x = "Months", y = "Mean Time Over/Underestimation")+
  ggtitle("Over/Underestimation Over Time - All Rows")

januaryPositions <- c(10, 22, 34, 46, 58, 70, 82, 94, 106, 118)
yearLabels <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

monthPlot + scale_x_continuous(breaks = januaryPositions,
                               labels = yearLabels)

#and now plotting only Done tasks

allMonthsDone <- unique(subset(data, DONE == 1)$yearMonth)

byMonthDonedf <- data.frame(months= allMonthsDone, x = 1:length(allMonthsDone), y= estimatesByMonthDone$mean_value)

byMonthDonedf$x = 1:length(allMonths)

monthDonePlot <- ggplot(byMonthDonedf, aes(x=x, y=y)) +
  geom_line() +
  geom_point() +
  labs(x = "Months", y = "Mean Over/Underestimation")+
  ggtitle("Over/Underestimation Over Time - Core Tasks")

monthDonePlot + scale_x_continuous(breaks = januaryPositions,
                                   labels = yearLabels)

data$rolling_error_mean <- roll_mean(data$log_error, n = 50, fill = NA, na.rm = TRUE, align = "center")


ggplot(data, aes(x = date, y = rolling_error_mean)) +
  geom_point() +
  labs(x = "Months", y = "Time Over/Underestimation")+
  ggtitle("Mean Time Over/Underestimation - 50 Task Rolling Average")


#now filtering for Done only 

df_Done_Only <- data %>%
  subset(Done_Only == 1)
df_Done_Only$rolling_error_mean <- roll_mean(df_Done_Only$log_error, n = 50, fill = NA, na.rm = TRUE, align = "center")

ggplot(df_Done_Only, aes(x = date, y = rolling_error_mean)) +
  geom_point() +
  labs(x = "Months", y = "Time Over/Underestimation")+
  ggtitle("Over/Underestimation, Core Tasks - 50 Task Rolling Average")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOES OVER/UNDERESTIMATION BIAS VARY ACCORDING TO TASK TYPE?

data %>%
  group_by(Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually) %>%
  summarise(mean_error = mean(log_error, na.rm = TRUE))

#mean actual for the different dones
data %>%
  group_by(Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually) %>%
  summarise(mean_actual = mean(actual, na.rm = TRUE))

#mean estimate for the different dones
Done_Estimate_Means <- data %>%
  group_by(Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually, Done_Never) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE),std_estimate = sd(estimate, na.rm = TRUE))

Done_Estimate_Means[,3:7]

#Let's look at estimates over time according to the type of Done

#Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually, Done_Never

#I've already made df_Done_Only

df_Done_Eventually <- data %>%
  subset(Done_Eventually==1)
df_Done_Eventually$rolling_error_mean <- roll_mean(df_Done_Eventually$log_error, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Maybe_Only <- data %>%
  subset(Done_Maybe_Only==1)
df_Done_Maybe_Only$rolling_error_mean <- roll_mean(df_Done_Maybe_Only$log_error, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Maybe_Eventually <- data %>%
  subset(Done_Maybe_Eventually==1)
df_Done_Maybe_Eventually$rolling_error_mean <- roll_mean(df_Done_Maybe_Eventually$log_error, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Never <- data %>%
  subset(Done_Never==1)
df_Done_Never$rolling_error_mean <- roll_mean(df_Done_Never$log_error, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Only$Status <- "Done Only"
df_Done_Eventually$Status <- "Done Eventually"
df_Done_Maybe_Only$Status <- "Done Maybe Only"
df_Done_Maybe_Eventually$Status <- "Done Maybe Eventually"
df_Done_Never$Status <- "Done Never"

#Creating a status column to give Done type.
data <- data %>%
  gather(key = "Status", value = "value", 
         Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually, Done_Never) %>%
  dplyr::filter(value == 1) %>%
  dplyr::select(-value)

#Summary table for Done categories over time
#month
Done_by_month <- data %>%
  group_by(yearMonth, Status) %>%
  summarise(frequency = n())

#year
Done_by_year <- data %>%
  group_by(year, Status) %>%
  summarise(frequency = n())

Done_by_year$year <- as.numeric(Done_by_year$year)

#Creating numbers for the x axis
unique_months = unique(Done_by_month$yearMonth)

Done_by_month <- mutate(Done_by_month, monthNumber = match(as.character(yearMonth), unique_months))

# Frequency of Done categories over months
ggplot(Done_by_month, aes(x = monthNumber, y = frequency, color = Status)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = januaryPositions,labels = yearLabels)+
  labs(x = "Date", y = "Frequency", color = "Category") +
  ggtitle("Frequency of Done Categories Over Time") +
  theme_minimal()

#over years
#Also calculating a total tasks value to plot
Done_by_year_total <- Done_by_year %>%
  group_by(year) %>%
  summarise(total_frequency = sum(frequency))

Done_by_year_total$year <- as.numeric(Done_by_year_total$year)

ggplot(Done_by_year, aes(x = year, y = frequency, color = Status)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2009:2018)+
  labs(x = "Year", y = "Frequency", color = "Category") +
  ggtitle("Frequency of Done Categories Over Time") +
  geom_line(data = Done_by_year_total, aes(x = year, y = total_frequency, color = 'Total'), size = 1.2)

#Same graph, but plotting the proportions of the different done categories
Done_by_month <- Done_by_month %>%
  group_by(monthNumber) %>%
  mutate(proportion = frequency/sum(frequency)) %>%
  ungroup()

#by Year
Done_by_year <- Done_by_year %>%
  group_by(year) %>%
  mutate(proportion = frequency/sum(frequency)) %>%
  ungroup()

ggplot(Done_by_year, aes(x = year, y = proportion, color = Status)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Proportion", color = "Category") +
  ggtitle("Proportions of Done Categories Over Time")


#Rolling Estimation Bias Averages

ggplot() +
  geom_point(data = df_Done_Only, aes(x = date, y = rolling_error_mean, color = Status)) +
  geom_point(data = df_Done_Eventually, aes(x = date, y = rolling_error_mean, color = Status)) +
  geom_point(data = df_Done_Maybe_Only, aes(x = date, y = rolling_error_mean, color = Status)) +
  geom_point(data = df_Done_Maybe_Eventually, aes(x = date, y = rolling_error_mean, color = Status)) +
  labs(x = "Months", y = "Time Over/Underestimation") +
  ggtitle("Mean Time Over/Underestimation - 50 Task Rolling Average") +
  scale_color_manual(values = c("Done Only" = "green", "Done Eventually" = "yellow", "Done Maybe Only" = "orange", "Done Maybe Eventually" = "purple"))

data_ignore_never <- data %>% filter(Status != "Done_Never")

#getting the boxplots to show up in a certain order
doneOrder <- c("Done_Only", "Done_Eventually", "Done_Maybe_Only", "Done_Maybe_Eventually", "Done_Never")
data$Status <- factor(data$Status, levels = doneOrder)
data_ignore_never$Status <- factor(data_ignore_never$Status, levels = doneOrder)

ggplot(data_ignore_never, aes(x = Status, y = log_error)) +
  geom_boxplot() +
  labs(x = "Done Category", y = "Over/Underestimation") +
  ggtitle("Over/Underestimation by Done Category")

ggplot(data_ignore_never, aes(x = Status, y = actual)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 8)) +
  labs(x = "Done Category", y = "Actual Sessions Taken") +
  ggtitle("Actual Sessions Taken by Done Category")

ggplot(data, aes(x = Status, y = estimate)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 8)) +
  labs(x = "Done Category", y = "Estimated Sessions Taken") +
  ggtitle("Estimated Sessions Taken by Done Category")

#_________________________________________________________________________
#ARE LONGER TASKS LESS PRONE TO THE PLANNING FALLACY?

#Plotting Estimation Bias over task length estimate
estimate_by_length <- data %>%
  group_by(estimate) %>%
  summarise(
    mean_diff = mean(log_error, na.rm = TRUE),
    n = n(),
    sd_diff = sd(log_error, na.rm = TRUE),
    se_diff = sd_diff / sqrt(n),
    ci_diff = 1.96 * se_diff
  )


estimate_by_length_plot <- ggplot(estimate_by_length, aes(x = estimate, y = mean_diff)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_diff - ci_diff, ymax = mean_diff + ci_diff), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Task Length Estimate", y = "Mean Under/Over-Estimation", 
       title = "Mean Log Error by Task Length Estimate - All Tasks")

estimate_by_length_plot + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))


#Does this trend obtain consistently across different task types?

#Core tasks (Done Only & Done Eventually)
df_Done_Only_Eventually <- data %>%
  subset(Status=="Done_Only" | Status == "Done_Eventually")

estimate_by_length_only_eventually <- df_Done_Only_Eventually %>%
  group_by(estimate) %>%
  summarise(
    mean_diff = mean(log_error, na.rm = TRUE),
    n = n(),
    sd_diff = sd(log_error, na.rm = TRUE),
    se_diff = sd_diff / sqrt(n),
    ci_diff = 1.96 * se_diff
  )

estimate_by_length_Only_Eventually_plot <- ggplot(estimate_by_length_only_eventually, aes(x = estimate, y = mean_diff)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_diff - ci_diff, ymax = mean_diff + ci_diff), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Task Length Estimate", y = "Mean Under/Over-Estimation", 
       title = "Mean Log Error by Task Length Estimate - Core Tasks")

estimate_by_length_Only_Eventually_plot + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#Maybe Tasks

df_maybes<- data %>%
  subset(Status=="Done_Maybe_Only" | Status == "Done_Maybe_Eventually")

estimate_by_length_maybe <- df_maybes %>%
  group_by(estimate) %>%
  summarise(
    mean_diff = mean(log_error, na.rm = TRUE),
    n = n(),
    sd_diff = sd(log_error, na.rm = TRUE),
    se_diff = sd_diff / sqrt(n),
    ci_diff = 1.96 * se_diff
  )

estimate_by_length_maybes_plot <- ggplot(estimate_by_length_maybe, aes(x = estimate, y = mean_diff)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_diff - ci_diff, ymax = mean_diff + ci_diff), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Task Length Estimate", y = "Mean Under/Over-Estimation", 
       title = "Mean Log Error by Task Length Estimate - Maybe Tasks")

estimate_by_length_maybes_plot  + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#Yes - same trend regardless of task type.

#Mean absolute error over estimate

absError_by_length <- data %>%
  group_by(estimate) %>%
  summarise(
    mean_err = mean(abs(log_error), na.rm = TRUE),
    n = n(),
    sd_err = sd(abs(log_error), na.rm = TRUE),
    se_err = sd_err / sqrt(n),
    ci_err = 1.96 * se_err
  )

estimate_by_length_plot <- ggplot(absError_by_length, aes(x = estimate, y = mean_err)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_err - ci_err, ymax = mean_err + ci_err), width = 0.2)+
  labs(x = "Task Length Estimate", y = "Mean Absolute Error", 
       title = "Mean Absolute Log Error by Task Length Estimate - All Tasks")

estimate_by_length_plot + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DATA TRANSFORMATION FOR MULTIPLE REGRESSIONS

#creating running total column for each task
data <- data %>% arrange(original_order)

data <- data %>%
  group_by(description) %>%
  mutate(running_total_tasks = cumsum(!is.na(description))) %>%
  ungroup()

#column for number of tasks planned in a day, as well as some other
  #mean information about each day.

data <- data %>%
  group_by(date) %>%
  mutate(n_planned = n()) %>%
  mutate(day_done_proportion = mean(DONE)) %>%
  mutate(mean_estimate_day = mean(estimate)) %>%
  mutate(mean_bias_day = mean(log_error, na.rm = TRUE)) %>%
  ungroup()

# column for the number of times each task was completed in the preceding weeks

calculate_task_frequency <- function(data, date_range_col) {
  return(sapply(seq_along(data$date), function(i) {
    sum(data$description == data$description[i] & 
          data$date >= data[[date_range_col]][i] & 
          (data$date < data$date[i] | 
             (data$date == data$date[i] & data$original_order < data$original_order[i]))
    )
  }))
}

data$date_minus_2weeks <- data$date - 14
data$date_minus_4weeks <- data$date - 28
data$date_minus_6weeks <- data$date - 42
data$date_minus_8weeks <- data$date - 56
data$date_minus_10weeks <- data$date - 70
data$date_minus_12weeks <- data$date - 84
data$date_minus_14weeks <- data$date - 98
data$date_minus_16weeks <- data$date - 112
data$date_minus_18weeks <- data$date - 126
data$date_minus_20weeks <- data$date - 140
data$date_minus_22weeks <- data$date - 154
data$date_minus_24weeks <- data$date - 168
data$date_minus_26weeks <- data$date - 182
data$date_minus_28weeks <- data$date - 196
data$date_minus_30weeks <- data$date - 210
data$date_minus_32weeks <- data$date - 224



data$task_frequency_2weeks <- calculate_task_frequency(data, "date_minus_2weeks")
data$task_frequency_4weeks <- calculate_task_frequency(data, "date_minus_4weeks")
data$task_frequency_6weeks <- calculate_task_frequency(data, "date_minus_6weeks")
data$task_frequency_8weeks <- calculate_task_frequency(data, "date_minus_8weeks")
data$task_frequency_10weeks <- calculate_task_frequency(data, "date_minus_10weeks")
data$task_frequency_12weeks <- calculate_task_frequency(data, "date_minus_12weeks")
data$task_frequency_14weeks <- calculate_task_frequency(data, "date_minus_14weeks")
data$task_frequency_16weeks <- calculate_task_frequency(data, "date_minus_16weeks")
data$task_frequency_18weeks <- calculate_task_frequency(data, "date_minus_18weeks")
data$task_frequency_20weeks <- calculate_task_frequency(data, "date_minus_20weeks")
data$task_frequency_22weeks <- calculate_task_frequency(data, "date_minus_22weeks")
data$task_frequency_24weeks <- calculate_task_frequency(data, "date_minus_24weeks")
data$task_frequency_26weeks <- calculate_task_frequency(data, "date_minus_26weeks")
data$task_frequency_28weeks <- calculate_task_frequency(data, "date_minus_28weeks")
data$task_frequency_30weeks <- calculate_task_frequency(data, "date_minus_30weeks")
data$task_frequency_32weeks <- calculate_task_frequency(data, "date_minus_32weeks")

#Maybe status
data$maybe_status <- grepl(pattern = "Maybe", x= data$Status)

#Core tasks
done_data <- data %>%
  subset(Status == "Done_Only" | Status == "Done_Eventually" | Status == "Done_Never")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#WHAT'S THE TIME HORIZON FOR THE INFLUENCE OF TASK EXPERIENCE?

no_time_model <- lm(abs(log_error) ~ estimate + original_order + maybe_status, data = data)

weeks2model <- lm(abs(log_error) ~ estimate + original_order + maybe_status + task_frequency_2weeks, data = data)

weeks4model <- lm(abs(log_error) ~ estimate + original_order + maybe_status + task_frequency_4weeks, data = data)

weeks6model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_6weeks, data = data)

weeks8model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_8weeks, data = data)

weeks10model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_10weeks, data = data)

weeks12model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_12weeks, data = data)

weeks14model<- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_14weeks, data = data)

weeks16model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_16weeks, data = data)

weeks18model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_18weeks, data = data)

weeks20model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_20weeks, data = data)

weeks22model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_22weeks, data = data)

weeks24model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_24weeks, data = data)

weeks26model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_26weeks, data = data)

weeks28model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_28weeks, data = data)

weeks30model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_30weeks, data = data)

weeks32model <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  task_frequency_32weeks, data = data)

totaltimemodel <- lm(abs(log_error) ~ estimate + original_order + maybe_status +  running_total_tasks, data = data)

models <- list(no_time_model, weeks2model, weeks4model, weeks6model, weeks8model, weeks10model, weeks12model, weeks14model,weeks16model,weeks18model,weeks20model,weeks22model, weeks24model,weeks26model,weeks28model,weeks30model,weeks32model,totaltimemodel)

adj_r_squared_values <- lapply(models, function(x) summary(x)$adj.r.squared)
week_values=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34)
week_labels = c("0","2","4","6","8","10","12","14","16","18","20","22","24","26","28","30","32", "All")

rsq_over_weeks <- data.frame(
  r_sq = unlist(adj_r_squared_values),
  weeks = week_values,
  labels = week_labels
)

last_two_points <- tail(rsq_over_weeks, 2)

ggplot(rsq_over_weeks[1:17,], aes(x = weeks, y = r_sq)) +
  geom_point() +
  geom_line() +
  geom_line(data = last_two_points, linetype = "longdash") + 
  geom_point(data = last_two_points) + 
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34),labels = week_labels)+
  labs(x = "Task Experience over x previous weeks", y = "Adjusted R-Squared Value")+
  ggtitle("Variance in Absolute Error Explained by Task Exprience")

rsq_over_weeks[which(rsq_over_weeks$r_sq == max(rsq_over_weeks$r_sq)),]
#22 weeks prior

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#is there an effect of having more tasks planned? e.g. if he has a busy day maybe he
#works harder? 

planned_tasks_model <- lm(log_error ~ estimate + original_order + maybe_status + task_frequency_22weeks + n_planned, data = data)

planned_tasks_model_done <- lm(log_error ~ estimate + original_order + task_frequency_22weeks + n_planned, data = done_data)

#yes! with more tasks planned, he's more prone to overestimation.
#This could be either because he's working harder, or because when there are 
# more tasks it's because he's being more granular and precise in his delineation of tasks. 

n_planned_plot <- data %>%
  group_by(n_planned) %>%
  summarise(error = mean(log_error, na.rm= TRUE),
            n = n(),
            sd = sd(log_error, na.rm = TRUE),
            se = sd / sqrt(n),
            ci = 1.96 * se,
            proportion_done = mean(day_done_proportion)
  )

ggplot(data = n_planned_plot, aes(x= n_planned,y= error)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = error - ci, ymax = error + ci), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Number of Tasks Planned", y = "Log Error")+
  ggtitle("Mean Log Error Over Number of Planned Tasks in a Day")

ggplot(data = n_planned_plot, aes(x= n_planned,y= proportion_done)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line() +
  geom_point() +  
  labs(x = "Number of Tasks Planned", y = "Mean Proportion of Tasks Done")+
  ggtitle("Mean Proportion of Tasks Completed Over Number of Planned Tasks in a Day")

daily_mean_bias_data <- data[!(is.na(data$mean_bias_day)),]
cor.test(daily_mean_bias_data$mean_bias_day, daily_mean_bias_data$n_planned)

#is there an interaction between time and estimate?
#i.e. does he get disproportionately better at over/underestimation over time?
time_estimate_interaction_model <- lm(log_error ~ estimate + original_order + maybe_status  + task_frequency_22weeks + n_planned + original_order*estimate, data = data)
abs_time_estimate_interaction_model <- lm(abs(log_error) ~ estimate + original_order + maybe_status  + task_frequency_22weeks + n_planned + original_order*estimate, data = data)
#no.
#But interestingly the significance of the original order disappears entirely
#when I include that interaction term. 

#is there an interaction between estimate and task frequency? 
#there should be, since if he's improving the slope of the task length-bias
# association should flatten somewhat. 
estimate_freq_interaction_model <- lm(log_error ~ estimate + original_order + maybe_status  + task_frequency_22weeks  + n_planned + task_frequency_22weeks*estimate, data = data)
abs_estimate_freq_interaction_model <- lm(abs(log_error) ~ estimate + original_order + maybe_status  + task_frequency_22weeks  + n_planned + task_frequency_22weeks*estimate, data = data)
#yes!

#I want to see what happens to both estimate and actual with increasing task 
#experience, controlling for n_planned, and maybe status

estimate_experience_model <- lm(estimate ~ maybe_status  + task_frequency_22weeks + n_planned, data = data)

actual_experience_model <- lm(actual ~ estimate + maybe_status  + task_frequency_22weeks + n_planned, data = data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Predicting whether a task will be done or not, rather than the degree of bias

done_probability_model <- glm(DONE ~ estimate + original_order + maybe_status + task_frequency_22weeks + n_planned, data = data, family = binomial)
null_model <- glm(DONE ~ 1, data = data, family = binomial)
anova(null_model, done_probability_model, test = "Chisq")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Effect of estimate on Done probability

done_prob_plot <- data %>%
  group_by(estimate) %>%
  summarise(done_prob = mean(DONE),
            n = n(),
            prob_sd = sd(DONE, na.rm = TRUE),
            prob_se = prob_sd / sqrt(n),
            prob_ci = 1.96 * prob_se) %>%
  ungroup()

ggplot(done_prob_plot, aes(x=estimate,y=done_prob))+
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10) +
  geom_errorbar(aes(ymin = done_prob - prob_ci, ymax = done_prob + prob_ci), width = 0.2)+
  labs(x = "Task Length Estimate", y = "Probability of Task Completion") +
  ggtitle("Probability of Task Completion by Task Length Estimate")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CUMULATIVE ERROR CURVES

all_dates <- data.frame(date = seq(min(data$date), max(data$date), by = "day"))

#All data

#Overestimation

overestimation_data_tasks <- data %>%
  filter(difference >= 0)

overestimation_data_tasks$cumul <- cumsum(abs(overestimation_data_tasks$log_error))
overestimation_data_tasks$cumul <- overestimation_data_tasks$cumul/(max(overestimation_data_tasks$cumul))

overestimation_data_tasks <- overestimation_data_tasks %>%
  mutate(task_number = 1:nrow(overestimation_data_tasks))

ggplot(overestimation_data_tasks, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Total Task Number", y = "Culumative Proportion of Overestimation Error")+
  ggtitle("Cumulative Overestimation Error Over Tasks - All Rows")


#Underestimation

underestimation_data_tasks <- data %>%
  filter(difference <= 0)

underestimation_data_tasks$cumul <- cumsum(abs(underestimation_data_tasks$log_error))
underestimation_data_tasks$cumul <- underestimation_data_tasks$cumul/(max(underestimation_data_tasks$cumul))

underestimation_data_tasks <- underestimation_data_tasks %>%
  mutate(task_number = 1:nrow(underestimation_data_tasks))

ggplot(underestimation_data_tasks, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Total Task Number", y = "Culumative Proportion of Overestimation Error")+
  ggtitle("Cumulative Underestimation Error Over Tasks - All Rows")

#Absolute Error
abs_error_data_tasks <- data %>%
  filter(!is.na(difference))

abs_error_data_tasks$cumul <- cumsum(abs(abs_error_data_tasks$log_error))
abs_error_data_tasks$cumul <- abs_error_data_tasks$cumul/(max(abs_error_data_tasks$cumul))

abs_error_data_tasks <- abs_error_data_tasks %>%
  mutate(task_number = 1:nrow(abs_error_data_tasks))

ggplot(abs_error_data_tasks, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Total Task Number", y = "Culumative Proportion of Error")+
  ggtitle("Cumulative Error Over Tasks - All Rows")


#Core Tasks
#Overestimation

overestimation_data_tasks_core <- done_data %>%
  filter(difference >= 0)

overestimation_data_tasks_core$cumul <- cumsum(abs(overestimation_data_tasks_core$log_error))
overestimation_data_tasks_core$cumul <- overestimation_data_tasks_core$cumul/(max(overestimation_data_tasks_core$cumul))

overestimation_data_tasks_core <- overestimation_data_tasks_core %>%
  mutate(task_number = 1:nrow(overestimation_data_tasks_core))

ggplot(overestimation_data_tasks_core, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Total Task Number", y = "Culumative Proportion of Overestimation Error")+
  ggtitle("Cumulative Overestimation Error Over Tasks - Core Tasks")


#Underestimation

underestimation_data_tasks_core <- done_data %>%
  filter(difference <= 0)

underestimation_data_tasks_core$cumul <- cumsum(abs(underestimation_data_tasks_core$log_error))
underestimation_data_tasks_core$cumul <- underestimation_data_tasks_core$cumul/(max(underestimation_data_tasks_core$cumul))

underestimation_data_tasks_core <- underestimation_data_tasks_core %>%
  mutate(task_number = 1:nrow(underestimation_data_tasks_core))

ggplot(underestimation_data_tasks_core, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Total Task Number", y = "Culumative Proportion of Underestimation Error")+
  ggtitle("Cumulative Underestimation Error Over Tasks - Core Tasks")


#Absolute Error

abs_error_tasks_core <- done_data %>%
  filter(!is.na(difference))

abs_error_tasks_core$cumul <- cumsum(abs(abs_error_tasks_core$log_error))
abs_error_tasks_core$cumul <- abs_error_tasks_core$cumul/(max(abs_error_tasks_core$cumul))

abs_error_tasks_core <- abs_error_tasks_core %>%
  mutate(task_number = 1:nrow(abs_error_tasks_core))

ggplot(abs_error_tasks_core, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Total Task Number", y = "Culumative Proportion of Error")+
  ggtitle("Cumulative Error Over Tasks - Core Tasks")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ROLLING MEAN ERROR

data$rolling_error <- roll_mean(abs(data$log_error), n = 500, fill = NA, na.rm = TRUE, align = "center")

#Over Time
ggplot(data, aes(x = date, y = rolling_error)) +
  geom_line() +
  labs(x = "Months", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error Over Time")

#Over Tasks

ggplot(data, aes(x = as.numeric(row.names(data)), y = rolling_error)) +
  geom_line() +
  labs(x = "Total Task Number", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error - Over Tasks")


#Core tasks
done_data$rolling_error <- roll_mean(abs(done_data$log_error), n = 500, fill = NA, na.rm = TRUE, align = "center")

#Over Time
ggplot(done_data, aes(x = date, y = rolling_error)) +
  geom_line() +
  labs(x = "Months", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error Over Time - Core Tasks")

#Over Tasks

ggplot(done_data, aes(x = as.numeric(row.names(done_data)), y = rolling_error)) +
  geom_line() +
  labs(x = "Total Task Number", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error Over Tasks - Core Tasks")

#Do his task length estimates change over time?

data$rolling_estimate <- roll_mean(data$estimate, n = 500, fill = NA, na.rm = TRUE, align = "center")

ggplot(data, aes(x = as.numeric(row.names(data)), y = rolling_estimate)) +
  geom_line() +
  labs(x = "Total Task Number", y = "Mean of Task Length Estimate")+
  ggtitle("All Data: Rolling Mean Task Length Estimate Over Tasks")


done_data$rolling_estimate <- roll_mean(done_data$estimate, n = 500, fill = NA, na.rm = TRUE, align = "center")

ggplot(done_data, aes(x = as.numeric(row.names(done_data)), y = rolling_estimate)) +
  geom_line() + 
  labs(x = "Total Task Number", y = "Mean of Task Length Estimate")+
  ggtitle("Core Tasks: Rolling Mean Task Length Estimates Over Tasks")

#yes - so his overall error increased over tasks, but this seems to be 
# partly because his estimated task length also increased over time. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROPORTION OF TASKS COMPLETED - ROLLING AVERAGE

data$rolling_error_done <- roll_mean(data$DONE, n = 500, fill = NA, na.rm = TRUE, align = "center")

done_proportion_over_time_model <- lm(day_done_proportion ~ original_order, data = data)
done_proportion_over_time_model_done <- lm(day_done_proportion ~ original_order, data = done_data)

ggplot(data, aes(x = as.numeric(row.names(data)), y = rolling_error_done)) +
  geom_line() +
  labs(x = "Total Task Number", y = "Mean Proportion of Tasks Completed")+
  ggtitle("Rolling Mean Proportion of Tasks Completed")

#Core Tasks

done_data$rolling_error_done <- roll_mean(done_data$DONE, n = 500, fill = NA, na.rm = TRUE, align = "center")

ggplot(done_data, aes(x = as.numeric(row.names(done_data)), y = rolling_error_done)) +
  geom_line() +
  labs(x = "Total Task Number", y = "Mean Proportion of Tasks Completed")+
  ggtitle("Rolling Mean Proportion of Tasks Completed - Core Tasks")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Looking at tags

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IDENTIFYING MOST COMMON TAGS
#Though I don't end up using them in the final model. 

data$tags <- strsplit(data$X.words, ",")

unnested_data <- data %>%
  unnest(tags)

tag_list <- as.list(unnested_data$tags)
all_tags <- unlist(tag_list)
tagsdf <- data.frame(table(all_tags))
tagsdf <- arrange(tagsdf,desc(Freq))

top_tags <- as.vector(tagsdf[1:9,"all_tags"])

# create a new column where tags not in top 9 are labelled as 'other'
unnested_data <- unnested_data %>%
  mutate(tag = if_else(tags %in% top_tags, tags, "other"))

unnested_data <- fastDummies::dummy_cols(unnested_data, select_columns = "tag", remove_selected_columns = TRUE)

#deleting all the @ symbols in column names
names(unnested_data) <- gsub("@", "", names(unnested_data))

#removing duplicate values rows, summing tag columns.
tag_data <- unnested_data %>%
  group_by(original_order) %>%
  summarise(across(starts_with("tag_"), ~sum(.x, na.rm = TRUE)))

tag_data <- tag_data %>%
  mutate(tag_other = if_else(tag_other != 0, 1, tag_other))

#deleting all tag cols in unnested data
unnested_data <- unnested_data[ , !grepl( "^tag_", names( unnested_data ) ) ]

#deleting all duplicate original_order values
unnested_data <- unnested_data %>%
  distinct(original_order, .keep_all = TRUE)

data <- left_join(unnested_data, tag_data, by = "original_order")

#which of the main tags predict error 
# + tag_bbc + tag_book + tag_ask + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other
individual_tags_model <- lm(log_error ~  tag_bbc + tag_book + tag_ask + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other, data = data)
abs_individual_tags_model <- lm(abs(log_error) ~  tag_bbc + tag_book + tag_ask + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other, data = data)

#running total for tags
data <- data %>%
  group_by(tags) %>%
  mutate(running_total_tags = cumsum(!is.na(tags))) %>%
  ungroup()

#tag frequency
calculate_tag_frequency <- function(data, date_range_col) {
  return(sapply(seq_along(data$date), function(i) {
    sum(data$tags == data$tags[i] & 
          data$date >= data[[date_range_col]][i] & 
          (data$date < data$date[i] | 
             (data$date == data$date[i] & data$original_order < data$original_order[i]))
    )
  }))
}

data$tag_frequency_2weeks <- calculate_tag_frequency(data, "date_minus_2weeks")
data$tag_frequency_4weeks <- calculate_tag_frequency(data, "date_minus_4weeks")
data$tag_frequency_6weeks <- calculate_tag_frequency(data, "date_minus_6weeks")
data$tag_frequency_8weeks <- calculate_tag_frequency(data, "date_minus_8weeks")
data$tag_frequency_10weeks <- calculate_tag_frequency(data, "date_minus_10weeks")
data$tag_frequency_12weeks <- calculate_tag_frequency(data, "date_minus_12weeks")
data$tag_frequency_14weeks <- calculate_tag_frequency(data, "date_minus_14weeks")
data$tag_frequency_16weeks <- calculate_tag_frequency(data, "date_minus_16weeks")
data$tag_frequency_18weeks <- calculate_tag_frequency(data, "date_minus_18weeks")
data$tag_frequency_20weeks <- calculate_tag_frequency(data, "date_minus_20weeks")

#Time Horizon analysis but with tags

tag_no_time_model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status , data = data)

tag_weeks2model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_2weeks, data = data)

tag_weeks4model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_4weeks, data = data)

tag_weeks6model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_6weeks, data = data)

tag_weeks8model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_8weeks, data = data)

tag_weeks10model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_10weeks, data = data)

tag_weeks12model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_12weeks, data = data)

tag_weeks14model<- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_14weeks, data = data)

tag_weeks16model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status  +  tag_frequency_16weeks, data = data)

tag_weeks18model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_18weeks, data = data)

tag_weeks20model <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + tag_frequency_20weeks, data = data)

tag_totaltimemodel <- lm(abs(log_error) ~ task_frequency_22weeks + estimate + original_order + maybe_status   + running_total_tags, data = data)

tag_models <- list(tag_no_time_model, tag_weeks2model, tag_weeks4model, tag_weeks6model, tag_weeks8model, tag_weeks10model, tag_weeks12model, tag_weeks14model,tag_weeks16model,tag_weeks18model,tag_weeks20model, tag_totaltimemodel)

#What’s the time horizon for the influence of experience with a certain tag?

tag_adj_r_squared_values <- lapply(tag_models, function(x) summary(x)$adj.r.squared)

tag_week_values=c(0,2,4,6,8,10,12,14,16,18,20,22)
tag_week_labels = c("0","2","4","6","8","10","12","14","16","18","20","All")

tag_rsq_over_weeks <- data.frame(
  r_sq = unlist(tag_adj_r_squared_values),
  weeks = tag_week_values,
  labels = tag_week_labels
)

tag_last_two_points <- tail(tag_rsq_over_weeks, 2)

ggplot(tag_rsq_over_weeks[1:11,], aes(x = weeks, y = r_sq)) +
  geom_point() +
  geom_line() +
  geom_line(data = tag_last_two_points, linetype = "longdash") + 
  geom_point(data = tag_last_two_points) + 
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34),labels = week_labels)+
  labs(x = "Task Experience over x previous weeks", y = "Adjusted R-Squared Value")+
  ggtitle("Variance in Absolute Error Explained by Task Exprience")

anova(tag_no_time_model, tag_totaltimemodel)
#no better

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Number of actual pomodoro sessions each day over the dataset - rolling mean

actual_daily_plot <- data %>%
  group_by(date) %>%
  summarise(daily_actual = sum(actual, na.rm=TRUE)) %>%
  ungroup()

actual_daily_plot <- actual_daily_plot %>%
  mutate(rolling_actual = roll_mean(daily_actual, n=100, fill=NA,na.rm=TRUE,align="center"))%>%
  ungroup()

#Number of estimated pomodoro sessions each day over the dataset - rolling mean

actual_estimate_plot <- data %>%
  group_by(date) %>%
  summarise(daily_estimate = sum(estimate, na.rm=TRUE)) %>%
  ungroup()

actual_estimate_plot <- actual_estimate_plot %>%
  mutate(rolling_estimate = roll_mean(daily_estimate, n=100, fill=NA,na.rm=TRUE,align="center"))%>%
  ungroup()

daily_actual_estimate <- left_join(actual_estimate_plot, actual_daily_plot, by = "date")

#making a proportion column

daily_actual_estimate <- daily_actual_estimate %>%
  mutate(mean_proportion = (rolling_actual/rolling_estimate)*20)

daily_actual_estimate <- daily_actual_estimate %>% 
  tidyr::pivot_longer(cols = c("rolling_estimate", "rolling_actual", "mean_proportion"), 
                      names_to = "estimate_actual", 
                      values_to = "value")

ggplot(daily_actual_estimate, aes(x = date, y = value, color = estimate_actual)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0,20, by = 2),sec.axis = sec_axis(~.*0.05, name = "Actual/Estimate Proportion", breaks = seq(0, 1, by = 0.1))) +
  scale_color_manual(values = c("rolling_estimate" = "deepskyblue", "rolling_actual" = "coral1", "mean_proportion" = "black"),
                     labels = c("rolling_estimate" = "Mean Daily Estimate", 
                                "rolling_actual" = "Mean Daily Actual", 
                                "mean_proportion" = "Actual/Estimate Proportion")) +
  labs(color = "Legend", x = "Date", y="Estimate & Actual Values")+
  ggtitle("Rolling Mean Daily Estimate, Actual, and Actual/Estimate Proportion - All Rows")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Does daily mean estimate correlate with actual for that day?

data <- data %>%
  group_by(date) %>%
  mutate(daily_actual_total = sum(actual)) %>%
  ungroup()

unique_date_data <- data %>%
  distinct(date, .keep_all= TRUE)

cor.test(unique_date_data$daily_actual_total, unique_date_data$mean_estimate_day)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#final models for predicting error
final_model<- lm(log_error ~ estimate + original_order + maybe_status + task_frequency_22weeks + n_planned, data = data)
abs_final_model<- lm(abs(log_error) ~ estimate + original_order + maybe_status + task_frequency_22weeks + n_planned, data = data)

#Checking assumptions
plot(final_model)

#Independence
durbinWatsonTest(final_model)

#multicolinearity
vif(final_model)
