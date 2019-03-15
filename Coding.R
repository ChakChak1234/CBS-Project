# Load libraries
library(plyr)
library(ggplot2)
library(scales)
library(cowplot)

# Load data from csv
unis_data <- read.csv(file = "unis.csv", header = TRUE)
views_data <- read.csv(file = "views.csv", header = TRUE)

# Remove unnecessary columns
unis_data <- subset(unis_data, select = -X)
views_data <- subset(views_data, select = -X)

# Convert factor into datetime
unis_data$ft_difftime <- difftime(unis_data$ft_end, unis_data$ft_start, units="days")

# Merge dataframes together 
data_complete <- join(unis_data, views_data, type = "inner") # Completed data only
data_full <- join(unis_data, views_data, type = "full") # all data

# 1. We have free trials of different lengths. What conclusions can you draw about the success 
# of the various trial lengths? Please evaluate free trial signup counts and any other metrics 
# using any techniques you choose. Do you have enough information to gauge conversion rate from 
# free trial to paid? If so, is there any bias your estimate?

# Preliminary exploratory analyses
table(as.numeric(data_complete$ft_difftime))
boxplot(as.numeric(data_complete$ft_difftime),
        main = "Free Trial Duration", xlab = "Free Trial", ylab = "Duration of Free Trial (in Days)")
hist(as.numeric(data_complete$ft_difftime),
     main = "Free Trial Duration", xlab = "Length of Free Trial (in Days)", ylab = "Frequency of Free Trial")
summary(as.numeric(data_complete$ft_difftime))

# Divide complete data by total data
length(data_complete$tve_user_id)/length(data_full$tve_user_id)

# Convert difftime to numeric
data_complete$ft_difftime <- as.numeric(data_complete$ft_difftime)

# Proportion of success from 7-day trial
sum(data_complete$ft_difftime <= 7 )/length(data_complete$tve_user_id)

# Proportion of sucess from 30-day trial
sum(data_complete$ft_difftime >= 29 & data_complete$ft_difftime <= 30)/length(data_complete$tve_user_id)

# 7 day and 30 day trials appear to have the highest conversion rate (5.6%), with the average trial length
# being 18 days and the median trial length being 7 days. However, there's not enough information
# to gauge an unbiased conversion rate from free trial to paid simply to due the large portion of
# missing data. I arrived at this conclusion by dividing the IDs with complete information by
# the complete number of IDs. I attempted to partition whether the view event came before the trial
# period (vice versa) by looking at the dates, but it wasn't a completely viable solution.

# 2. From time to time we have signup events. Please flag any abnormal signup activity and use 
# the internet to try and identify any drivers.

# Convert column from factor to date variable
data_complete$signup_date <- as.Date(data_complete$ft_start)

# Plot graph and store into object for later use
plot_1 <- ggplot(data_complete, aes(x=signup_date)) + geom_histogram(binwidth=30, colour="white") +
  scale_x_date(labels = date_format("%Y-%b-%d"),
               breaks = seq(min(data_complete$signup_date)-5, max(data_complete$signup_date)+5, 30),
               limits = c(as.Date("2017-01-01"), as.Date("2017-09-18"))) +
  ylab("Frequency") + xlab("Year and Month") + ggtitle("Sign-Up Date") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Assuming free trial start date is the signup date, a histogram was used to see the monthly frequency
# of signups. The histogram result indicates that, on average, there's about 5000 sign ups, though
# signups increased 5-fold in the month of May, and then a sharp then gradual decrease back to the
# normal monthly signup of 5000 subscribers. Internet research suggests that amazon prime during
# and before May, 2017 introduced two interesting features that may or may not have caused the spike
# in subscribers: CBS-All Access Showtime without ads; and the Amazon Prime Channel now functions
# as a TV channel. These two products could have most likely precipated increased viewership.

# 3. Is there any seasonality in signup and viewing events?

# Convert column
data_complete$view_date <- as.Date(data_complete$dt)

# Plot graph and store into object for later use
plot_2 <- ggplot(data_complete, aes(x=view_date)) + geom_histogram(binwidth=30, colour="white") +
  scale_x_date(labels = date_format("%Y-%b-%d"),
               breaks = seq(min(data_complete$view_date)-5, max(data_complete$view_date)+5, 30),
               limits = c(as.Date("2017-01-01"), as.Date("2017-09-18"))) +
  ylab("Frequency") + xlab("Year and Month") + ggtitle("Viewing Date") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot signup and viewing events for seasonality
plot_grid(plot_1, plot_2, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))

# Viewing signup and viewing event plots indicates an interesting trend. Before May, 2017 each month
# suggests there's about 5000 subscribes signing up and viewing events. During and after May, there's
# a huge spike of subscribers, though there's drastically more people signing up than viewing. It should
# , however, be noted that the dataset with user IDs in signup and viewing were used, not the completed
# dataset.

# 4. Can you draw any insights into viewing behavior?
data_complete$title_id <- as.factor(data_complete$title_id)

# Histogram of viewing behavior (by minute)
plot_3 <- ggplot(data=data_complete, aes(x=seconds/60)) + geom_histogram() +
  ylab("Frequency") + xlab("By Minute") + ggtitle("When Users Ended Episode") 
plot_4 <- ggplot(data=data_complete, aes(x=runtime/60)) + geom_histogram() +
  ylab("Frequency") + xlab("By Minute") + ggtitle("Episode Duration")

# Summary of viewing behavior (by minute)
summary(data_complete$seconds/60)
summary(data_complete$runtime/60)

# Exploratory Analysis
# Correlation between seconds and runtime, regardless of viewing date and title_id
plot_5 <- ggplot(data_complete, aes(x=seconds, y=runtime)) + geom_point(size=1, shape=21, fill="white") + 
  xlab("Seconds") + ylab("Runtime") + ggtitle("Seconds vs Runtime") 
# correlation test
cor.test(data_complete$seconds, data_complete$runtime)

plot_grid(plot_3, plot_4, plot_5, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))

# The average view of each title is about 60 minutes, regardless of how long each episode. The distribution
# of runtime is normally distributed, which makes sense, while the actual viewing of each episode is
# positvely skewed, indicating that most people stop watching the episode before it ends. A scatterplot
# was visualized to explore possible relationship between when the subscriber stopped watching the title
# and the title's length. Results indicate a positive correlation (r = 0.413) between the two variables,
# though it should be cautioned that runtime is always greater than when the episode stopped, as previously
# suggested.

