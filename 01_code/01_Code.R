#INITIALISING

install.packages(c("readr", "tidyverse", "ggplot2","lubridate", "zoo", "RcppRoll", "MASS", "brant", "fastDummies"))

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

#Removing the 5 tasks estimated to take 0.5 pomodoros, and the 35 estimated to
  #take 0.

data <- data %>%
  filter(estimate > 0.5)

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

#overall bias (under/overestimation)
mean(data$difference, na.rm = TRUE)

#overall bias, but subsetting data for tasks marked as done

data %>%
  subset(DONE == 1) %>%
  summarize(mean_value = mean(difference, na.rm = TRUE)) %>%
  pull(mean_value)

#Creating a year column to group data
data$year <- format(data$date, "%Y")

#Finding the mean of over/estimation for each year across the dataset
data %>%
  group_by(year) %>%
  summarize(mean_value = mean(difference, na.rm = TRUE))

#and now filtering also for Done tasks
data %>%
  subset(DONE == 1) %>%
  group_by(year) %>%
  summarize(mean_value = mean(difference, na.rm = TRUE))

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
  summarize(mean_value = mean(difference, na.rm = TRUE))

#and again filtering also for Done tasks
estimatesByMonthDone <- data %>%
  subset(DONE == 1) %>%
  group_by(yearMonth) %>%
  summarize(mean_value = mean(difference, na.rm = TRUE))

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

byMonthDonedfMissing <- data.frame(months= allMonthsDone, x = 1:length(allMonthsDone), y= estimatesByMonthDone$mean_value)

#Two months are missing from near the end of the dataset when I exclude maybe
  #done tasks, so I'm adding in the missing months here
newRows <- data.frame(months = c("2018-11","2018-12"),x = c(1,1), y= c(NA,NA))

byMonthDonedf <- rbind(byMonthDonedfMissing[1:115,], newRows, byMonthDonedfMissing[116:117,])

byMonthDonedf$x = 1:length(allMonths)

monthDonePlot <- ggplot(byMonthDonedf, aes(x=x, y=y)) +
  geom_line() +
  geom_point() +
  labs(x = "Months", y = "Mean Over/Underestimation")+
  ggtitle("Over/Underestimation Over Time - Core Tasks")

monthDonePlot + scale_x_continuous(breaks = januaryPositions,
                               labels = yearLabels)

data$rolling_difference_mean <- roll_mean(data$difference, n = 50, fill = NA, na.rm = TRUE, align = "center")


ggplot(data, aes(x = date, y = rolling_difference_mean)) +
  geom_point() +
  labs(x = "Months", y = "Time Over/Underestimation")+
  ggtitle("Mean Time Over/Underestimation - 50 Task Rolling Average")


#now filtering for Done only 

df_Done_Only <- data %>%
  subset(Done_Only == 1)
df_Done_Only$rolling_difference_mean <- roll_mean(df_Done_Only$difference, n = 50, fill = NA, na.rm = TRUE, align = "center")

ggplot(df_Done_Only, aes(x = date, y = rolling_difference_mean)) +
  geom_point() +
  labs(x = "Months", y = "Time Over/Underestimation")+
  ggtitle("Over/Underestimation, Core Tasks - 50 Task Rolling Average")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IDENTIFYING MOST COMMON TAGS

data$tags <- strsplit(data$X.words, ",")

data <- data %>%
  unnest(tags)


tag_list <- as.list(data$tags)
all_tags <- unlist(tag_list)
tagsdf <- data.frame(table(all_tags))
tagsdf <- arrange(tagsdf,desc(Freq))

top_tags <- as.vector(tagsdf[1:9,"all_tags"])

# create a new column where tags not in top 9 are labelled as 'other'
data <- data %>%
  mutate(tag = if_else(tags %in% top_tags, tags, "other"))

data <- fastDummies::dummy_cols(data, select_columns = "tag", remove_selected_columns = TRUE)

names(data) <- gsub("@", "", names(data))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOES OVER/UNDERESTIMATION BIAS VARY ACCORDING TO TASK TYPE?

data %>%
  group_by(Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))

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
df_Done_Eventually$rolling_difference_mean <- roll_mean(df_Done_Eventually$difference, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Maybe_Only <- data %>%
  subset(Done_Maybe_Only==1)
df_Done_Maybe_Only$rolling_difference_mean <- roll_mean(df_Done_Maybe_Only$difference, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Maybe_Eventually <- data %>%
  subset(Done_Maybe_Eventually==1)
df_Done_Maybe_Eventually$rolling_difference_mean <- roll_mean(df_Done_Maybe_Eventually$difference, n = 50, fill = NA, na.rm = TRUE, align = "center")

df_Done_Never <- data %>%
  subset(Done_Never==1)
df_Done_Never$rolling_difference_mean <- roll_mean(df_Done_Never$difference, n = 50, fill = NA, na.rm = TRUE, align = "center")

#Done_Only, Done_Eventually, Done_Maybe_Only, Done_Maybe_Eventually, Done_Never

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
Done_by_month <- data %>%
  group_by(yearMonth, Status) %>%
  summarise(frequency = n())

#Creating numbers for the x axis
unique_months = unique(Done_by_month$yearMonth)

Done_by_month <- mutate(Done_by_month, monthNumber = match(as.character(yearMonth), unique_months))

# Frequency of Done categories over time
Done_over_time_plot <- ggplot(Done_by_month, aes(x = monthNumber, y = frequency, color = Status)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Frequency", color = "Category") +
  ggtitle("Frequency of Done Categories Over Time") +
  theme_minimal()

Done_over_time_plot  + scale_x_continuous(breaks = januaryPositions,
                               labels = yearLabels)

#Same graph, but plotting the proportions of the different done categories
Done_by_month <- Done_by_month %>%
  group_by(monthNumber) %>%
  mutate(proportion = frequency/sum(frequency)) %>%
  ungroup()

Done_over_time_plot_prop <- ggplot(Done_by_month, aes(x = monthNumber, y = proportion, color = Status)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Frequency", color = "Category") +
  ggtitle("Proportions of Done Categories Over Time") +
  theme_minimal()

Done_over_time_plot_prop  + scale_x_continuous(breaks = januaryPositions,
                                          labels = yearLabels)

#Rolling Bias Averages

ggplot() +
  geom_point(data = df_Done_Only, aes(x = date, y = rolling_difference_mean, color = Status)) +
  geom_point(data = df_Done_Eventually, aes(x = date, y = rolling_difference_mean, color = Status)) +
  geom_point(data = df_Done_Maybe_Only, aes(x = date, y = rolling_difference_mean, color = Status)) +
  geom_point(data = df_Done_Maybe_Eventually, aes(x = date, y = rolling_difference_mean, color = Status)) +
  labs(x = "Months", y = "Time Over/Underestimation") +
  ggtitle("Mean Time Over/Underestimation - 50 Task Rolling Average") +
  scale_color_manual(values = c("Done Only" = "green", "Done Eventually" = "yellow", "Done Maybe Only" = "orange", "Done Maybe Eventually" = "purple"))

data_ignore_never <- data %>% filter(Status != "Done_Never")

#getting the boxplots to show up in a certain order
doneOrder <- c("Done_Only", "Done_Eventually", "Done_Maybe_Only", "Done_Maybe_Eventually", "Done_Never")
data$Status <- factor(data$Status, levels = doneOrder)
data_ignore_never$Status <- factor(data_ignore_never$Status, levels = doneOrder)

ggplot(data_ignore_never, aes(x = Status, y = difference)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-6, 8)) +
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

#An error of 1 is a lot more when the estimate was 1 than when the estimate
# was 9, so creating a column for error proportional to the estimate. 

data <- data %>%
  mutate(proportional_error = difference/estimate)


#Plotting Estimation Bias over task length estimate
estimate_by_length <- data %>%
  group_by(estimate) %>%
  summarise(
    mean_diff = mean(proportional_error, na.rm = TRUE),
    n = n(),
    sd_diff = sd(proportional_error, na.rm = TRUE),
    se_diff = sd_diff / sqrt(n),
    ci_diff = 1.96 * se_diff
  )

estimate_by_length_plot <- ggplot(estimate_by_length, aes(x = estimate, y = mean_diff)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_diff - ci_diff, ymax = mean_diff + ci_diff), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Task Length Estimate", y = "Mean Under/Over-Estimation", 
       title = "Mean Proportional Error by Task Length Estimate - All Tasks")

estimate_by_length_plot + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#Does this trend obtain consistently across different task types?

#Core tasks (Done Only & Done Eventually)
df_Done_Only_Eventually <- data %>%
  subset(Status=="Done_Only" | Status == "Done_Eventually")

estimate_by_length_only_eventually <- df_Done_Only_Eventually %>%
  group_by(estimate) %>%
  summarise(
    mean_diff = mean(proportional_error, na.rm = TRUE),
    n = n(),
    sd_diff = sd(proportional_error, na.rm = TRUE),
    se_diff = sd_diff / sqrt(n),
    ci_diff = 1.96 * se_diff
  )

estimate_by_length_Only_Eventually_plot <- ggplot(estimate_by_length_only_eventually, aes(x = estimate, y = mean_diff)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_diff - ci_diff, ymax = mean_diff + ci_diff), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Task Length Estimate", y = "Mean Under/Over-Estimation", 
       title = "Mean Proportional Error by Task Length Estimate - Core Tasks")

estimate_by_length_Only_Eventually_plot + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#Maybe Tasks

df_maybes<- data %>%
  subset(Status=="Done_Maybe_Only" | Status == "Done_Maybe_Eventually")

estimate_by_length_maybe <- df_maybes %>%
  group_by(estimate) %>%
  summarise(
    mean_diff = mean(proportional_error, na.rm = TRUE),
    n = n(),
    sd_diff = sd(proportional_error, na.rm = TRUE),
    se_diff = sd_diff / sqrt(n),
    ci_diff = 1.96 * se_diff
  )

estimate_by_length_maybes_plot <- ggplot(estimate_by_length_maybe, aes(x = estimate, y = mean_diff)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean_diff - ci_diff, ymax = mean_diff + ci_diff), width = 0.2)+
  geom_hline(yintercept = 0, linetype="longdash", color = "red")+
  labs(x = "Task Length Estimate", y = "Mean Under/Over-Estimation", 
       title = "Mean Proportional Error by Task Length Estimate - Maybe Tasks")

estimate_by_length_maybes_plot  + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#Yes - same trend regardless of task type.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DATA TRANSFORMATION FOR MULTIPLE REGRESSIONS

#creating running total column for each task
data <- data %>% arrange(original_order)

data <- data %>%
  group_by(description) %>%
  mutate(running_total_tasks = cumsum(!is.na(description))) %>%
  ungroup()

#TODO hmm, I should really do a running total for tags as well, to see whether
  #that's a better predictor than the unique task descriptions...
#Also maybe an interesting near/far transfer question.

data <- data %>%
  group_by(tags) %>%
  mutate(running_total_tags = cumsum(!is.na(tags))) %>%
  ungroup()


#Task Number

# Create a date range of 1 month and 3 months for each task's completion date
data$date_minus_1month <- data$date - 30
data$date_minus_2months <- data$date - 60
data$date_minus_3months <- data$date - 90
data$date_minus_4months <- data$date - 120
data$date_minus_5months <- data$date - 150
data$date_minus_6months <- data$date - 180


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

data$task_frequency_2weeks <- calculate_task_frequency(data, "date_minus_2weeks")
data$task_frequency_4weeks <- calculate_task_frequency(data, "date_minus_4weeks")
data$task_frequency_6weeks <- calculate_task_frequency(data, "date_minus_6weeks")
data$task_frequency_8weeks <- calculate_task_frequency(data, "date_minus_8weeks")
data$task_frequency_10weeks <- calculate_task_frequency(data, "date_minus_10weeks")
data$task_frequency_12weeks <- calculate_task_frequency(data, "date_minus_12weeks")

#Maybe status
data$maybe_status <- grepl(pattern = "Maybe", x= data$Status)

#Core tasks
done_data <- data %>%
  subset(Status == "Done_Only" | Status == "Done_Eventually" | Status == "Done_Never")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#WHAT'S THE TIME HORIZON FOR THE INFLUENCE OF TASK EXPERIENCE?
#MULTIPLE REGRESSIONS

#TODO could also include running_total_tasks to include effect of total previous experience
  # maybe some effect on top of recency effect

no_time_model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other, data = data)

weeks2model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_2weeks, data = data)

weeks4model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_4weeks, data = data)

weeks6model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_6weeks, data = data)

weeks8model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_8weeks, data = data)

weeks10model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_10weeks, data = data)

weeks12model <- lm(abs(proportional_error) ~ estimate + original_order + maybe_status + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_12weeks, data = data)

#What’s the time horizon for the influence of experience?
AIC(no_time_model, weeks2model, weeks4model, weeks6model, weeks8model, weeks10model, weeks12model)
BIC(no_time_model, weeks2model, weeks4model, weeks6model, weeks8model, weeks10model, weeks12model)

anova(no_time_model, weeks2model)
anova(weeks2model, weeks4model) #not sure why this doesn't give an F or p value!
#etc.

#Could try a stepwise ANOVA, but this seems to leave most of the week predictors in
#so I'm not sure what to make of that. 

#Core Tasks

core_no_time_model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other, data = done_data)

core_weeks2model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_2weeks, data = done_data)

core_weeks4model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_4weeks, data = done_data)

core_weeks6model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_6weeks, data = done_data)

core_weeks8model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_8weeks, data = done_data)

core_weeks10model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_10weeks, data = done_data)

core_weeks12model <- lm(abs(proportional_error) ~ estimate + original_order + tag_bbc + tag_book + tag_craft + tag_meeting + tag_planning + tag_riskapp + tag_teamly + tag_uswitch + tag_wgsn + tag_other  + task_frequency_12weeks, data = done_data)


#Time Horizon
AIC(core_no_time_model, core_weeks2model, core_weeks4model, core_weeks6model, core_weeks8model, core_weeks10model, core_weeks12model)
BIC(core_no_time_model, core_weeks2model, core_weeks4model, core_weeks6model, core_weeks8model, core_weeks10model, core_weeks12model)

anova(core_no_time_model, core_weeks2model)
#etc.

#TODO is there an interaction between time and estimate? Maybe he gets disproportionately better at over/underestimation


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CUMULATIVE ERROR CURVES

all_dates <- data.frame(date = seq(min(data$date), max(data$date), by = "day"))

#All data

#Overestimation

overestimation_data_tasks <- data %>%
  filter(difference >= 0)

overestimation_data_tasks$cumul <- cumsum(abs(overestimation_data_tasks$proportional_error))
overestimation_data_tasks$cumul <- overestimation_data_tasks$cumul/(max(overestimation_data_tasks$cumul))

overestimation_data_tasks <- overestimation_data_tasks %>%
  mutate(task_number = 1:nrow(overestimation_data_tasks))

ggplot(overestimation_data_tasks, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Tasks", y = "Culumative Proportion of Overestimation Error")+
  ggtitle("Cumulative Overestimation Error Over Tasks - All Data")


#Underestimation

underestimation_data_tasks <- data %>%
  filter(difference <= 0)

underestimation_data_tasks$cumul <- cumsum(abs(underestimation_data_tasks$proportional_error))
underestimation_data_tasks$cumul <- underestimation_data_tasks$cumul/(max(underestimation_data_tasks$cumul))

underestimation_data_tasks <- underestimation_data_tasks %>%
  mutate(task_number = 1:nrow(underestimation_data_tasks))

ggplot(underestimation_data_tasks, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Tasks", y = "Culumative Proportion of Overestimation Error")+
  ggtitle("Cumulative Underestimation Error Over Tasks - All Data")

#Absolute Error
abs_error_data_tasks <- data %>%
  filter(!is.na(difference))

abs_error_data_tasks$cumul <- cumsum(abs(abs_error_data_tasks$proportional_error))
abs_error_data_tasks$cumul <- abs_error_data_tasks$cumul/(max(abs_error_data_tasks$cumul))

abs_error_data_tasks <- abs_error_data_tasks %>%
  mutate(task_number = 1:nrow(abs_error_data_tasks))

ggplot(abs_error_data_tasks, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Tasks", y = "Culumative Proportion of Error")+
  ggtitle("Cumulative Error Over Tasks - All Data")


#Core Tasks
#Overestimation

overestimation_data_tasks_core <- done_data %>%
  filter(difference >= 0)

overestimation_data_tasks_core$cumul <- cumsum(abs(overestimation_data_tasks_core$proportional_error))
overestimation_data_tasks_core$cumul <- overestimation_data_tasks_core$cumul/(max(overestimation_data_tasks_core$cumul))

overestimation_data_tasks_core <- overestimation_data_tasks_core %>%
  mutate(task_number = 1:nrow(overestimation_data_tasks_core))

ggplot(overestimation_data_tasks_core, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Tasks", y = "Culumative Proportion of Overestimation Error")+
  ggtitle("Cumulative Overestimation Error Over Tasks - Core Tasks")


#Underestimation

underestimation_data_tasks_core <- done_data %>%
  filter(difference <= 0)

underestimation_data_tasks_core$cumul <- cumsum(abs(underestimation_data_tasks_core$proportional_error))
underestimation_data_tasks_core$cumul <- underestimation_data_tasks_core$cumul/(max(underestimation_data_tasks_core$cumul))

underestimation_data_tasks_core <- underestimation_data_tasks_core %>%
  mutate(task_number = 1:nrow(underestimation_data_tasks_core))

ggplot(underestimation_data_tasks_core, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Tasks", y = "Culumative Proportion of Underestimation Error")+
  ggtitle("Cumulative Underestimation Error Over Tasks - Core Tasks")



#Absolute Error

abs_error_tasks_core <- done_data %>%
  filter(!is.na(difference))

abs_error_tasks_core$cumul <- cumsum(abs(abs_error_tasks_core$proportional_error))
abs_error_tasks_core$cumul <- abs_error_tasks_core$cumul/(max(abs_error_tasks_core$cumul))

abs_error_tasks_core <- abs_error_tasks_core %>%
  mutate(task_number = 1:nrow(abs_error_tasks_core))

ggplot(abs_error_tasks_core, aes(x = task_number, y = cumul)) +
  geom_point() +
  geom_segment(aes(x = min(0), y = 0, xend = max(task_number), yend = 1)) +
  labs(x = "Tasks", y = "Culumative Proportion of Error")+
  ggtitle("Cumulative Error Over Tasks - Core Tasks")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ROLLING MEAN ERROR

data$rolling_error <- roll_mean(abs(data$proportional_error), n = 500, fill = NA, na.rm = TRUE, align = "center")

#Over Time
ggplot(data, aes(x = date, y = rolling_error)) +
  geom_line() +
  labs(x = "Months", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error Over Time")

#Over Tasks

ggplot(data, aes(x = as.numeric(row.names(data)), y = rolling_error)) +
  geom_line() +
  labs(x = "Tasks", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Asbolute Error - Over Tasks")


#Core tasks
done_data$rolling_error <- roll_mean(abs(done_data$proportional_error), n = 500, fill = NA, na.rm = TRUE, align = "center")

#Over Time
ggplot(done_data, aes(x = date, y = rolling_error)) +
  geom_line() +
  labs(x = "Months", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error Over Time - Core Tasks")

#Over Tasks

ggplot(done_data, aes(x = as.numeric(row.names(done_data)), y = rolling_error)) +
  geom_line() +
  labs(x = "Tasks", y = "Mean of Absolute Estimation Error")+
  ggtitle("Rolling Mean Absolute Error Over Tasks - Core Tasks")

#Do his task length estimates change over time?

data$rolling_estimate <- roll_mean(data$estimate, n = 500, fill = NA, na.rm = TRUE, align = "center")

ggplot(data, aes(x = as.numeric(row.names(data)), y = rolling_estimate)) +
  geom_line() +
  labs(x = "Tasks", y = "Mean of Task Length Estimate")+
  ggtitle("All Data: Mean Task Length Estimate Over Tasks")


done_data$rolling_estimate <- roll_mean(done_data$estimate, n = 500, fill = NA, na.rm = TRUE, align = "center")
 
ggplot(done_data, aes(x = as.numeric(row.names(done_data)), y = rolling_estimate)) +
  geom_line() +
  labs(x = "Tasks", y = "Mean of Task Length Estimate")+
  ggtitle("Core Tasks: Mean Task Length Estimates Over Tasks")

#yes - so his overall error increased over tasks, but this seems to be 
  #at least partly because his estimated task length also increased over time. 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROPORTION OF TASKS COMPLETED - ROLLING AVERAGE

data_end_cut <- data[1:max(which(data$date == "2018-10-16")),]
#After this date ("2018-10-17") he stops marking any tasks as done.

#No Planning Data
data_end_cut$rolling_error_done <- roll_mean(data_end_cut$DONE, n = 500, fill = NA, na.rm = TRUE, align = "center")

ggplot(data_end_cut, aes(x = as.numeric(row.names(data_end_cut)), y = rolling_error_done)) +
  geom_line() +
  labs(x = "Tasks", y = "Mean Proportion of Tasks Completed")+
  ggtitle("Rolling Mean Proportion of Tasks Completed")


#Core Tasks
done_data_end_cut <- done_data[1:max(which(done_data$date == "2018-10-16")),]

done_data_end_cut$rolling_error_done <- roll_mean(done_data_end_cut$DONE, n = 500, fill = NA, na.rm = TRUE, align = "center")

ggplot(done_data_end_cut, aes(x = as.numeric(row.names(done_data_end_cut)), y = rolling_error_done)) +
  geom_line() +
  labs(x = "Tasks", y = "Mean Proportion of Tasks Completed")+
  ggtitle("Rolling Mean Proportion of Tasks Completed - Core Tasks")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Next

#I wonder whether there's any relation between word_cnt (which I haven't used
# at all) and accuracy. Maybe longer description = clearer intentions = 
# more accuracy? Or something? Should take a look. 
