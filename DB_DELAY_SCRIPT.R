install.packages("readr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggridges")     
install.packages("ggbeeswarm")   
install.packages("viridis")     
install.packages("patchwork")   
install.packages("hexbin")       


library(readr)
library(lubridate) 
library(dplyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggridges)
library(ggbeeswarm)
library(viridis)
library(patchwork)
library(hexbin)


# Data Preparation or Exploration 

db_data <- read_csv("~/Desktop/DB_PROJECT/DBtrainrides.csv", n_max = 2029894)

#Checking structure and Summary 
str(db_data)        # types of each column
summary(db_data)    # summary stats
colnames(db_data)   # column names

#Handling missing values 
colSums(is.na(db_data))

#Removing Outliers Delays over 24 hours (1440 minutes) are unrealistic and should be removed.
db_data <- subset(db_data, 
                  arrival_delay_m < 1440 & departure_delay_m < 1440)

#Feature Engineering (Time-Based Variables)

db_data$hour <- hour(db_data$arrival_plan)
db_data$weekday <- wday(db_data$arrival_plan, label = TRUE)
db_data$date <- as.Date(db_data$arrival_plan)

# Peak-hour flag
db_data$peak_hour <- ifelse(db_data$hour %in% c(7:9, 16:19), "Peak", "Off-Peak")

#Converting to Factors Some variables should be categorical (factors) for analysis.
db_data$category <- as.factor(db_data$category)
db_data$arrival_delay_check <- as.factor(db_data$arrival_delay_check)
db_data$departure_delay_check <- as.factor(db_data$departure_delay_check)
db_data$weekday <- as.factor(db_data$weekday)
db_data$peak_hour <- as.factor(db_data$peak_hour)

#Distribution of Station CategoriesHow many records are from big hubs vs. small stations?
table(db_data$category)

#Delay Flag DistributionHow many trains are “officially delayed” (>6 min)?
table(db_data$arrival_delay_check)
table(db_data$departure_delay_check)

#Basic Correlation between arrival & departure delays
cor(db_data$arrival_delay_m, db_data$departure_delay_m, use="complete.obs")

#Counts by State or City (to see regional coverage)
table(db_data$state)[1:10]  # first 10 states



#Final Verification 
colSums(is.na(db_data))                    # no missing values
range(db_data$arrival_delay_m, na.rm=TRUE) # reasonable delay values
range(db_data$departure_delay_m, na.rm=TRUE)
head(db_data)                              # quick preview




#--------------------Exploratory Data Analysis (EDA)----------------------------


#SUMMARY 
summary(db_data$arrival_delay_m)
summary(db_data$departure_delay_m)

#---styling shortcut for all the plots to make it consistent.
theme_set(theme_minimal(base_size = 12))


# This is to make a copy of the dataset for visualization
db_vis <- db_data

# Clip extreme values just by replacing them
db_vis$arr_clip <- ifelse(db_vis$arrival_delay_m < -10, -10,
                          ifelse(db_vis$arrival_delay_m > 120, 120,
                                 db_vis$arrival_delay_m))

db_vis$dep_clip <- ifelse(db_vis$departure_delay_m < -10, -10,
                          ifelse(db_vis$departure_delay_m > 120, 120,
                                 db_vis$departure_delay_m))
#-------------------------------------------------------------------------------


# What do train delays look like overall? hence we do a histogram 
ggplot(db_vis, aes(x = arr_clip)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Arrival Delays (All Trains)",
       x = "Arrival delay (minutes, clipped)",
       y = "Number of trains") +
  coord_cartesian(xlim = c(0, 60)) +
  theme_minimal()

# “Do some weekdays have systematically worse delays? Hence we do the boxplot 
ggplot(db_vis, aes(x = weekday, y = arr_clip)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Arrival delays by weekday",
       x = "Weekday", y = "Arrival delay (minutes, clipped)") +
  theme_minimal()


# How are delays distributed across weekdays not for just the medians?” Violin plot of arrival delays by weekday
ggplot(db_vis, aes(x = weekday, y = arr_clip)) +
  geom_violin(fill = "lightgreen") +
  labs(title = "Arrival delays by weekday",
       x = "Weekday", y = "Arrival delay (minutes)") +
  theme_minimal()
#--------------------------------------------------------------------------------

#“When during the week do delays tend to be worst?”

# Calculate mean arrival delay by weekday and hour
heat_df <- aggregate(arrival_delay_m ~ weekday + hour, data = db_vis, FUN = mean, na.rm = TRUE)

# Heatmap plot
ggplot(heat_df, aes(x = hour, y = weekday, fill = arrival_delay_m)) +
  geom_tile() +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Average Arrival Delay by Weekday and Hour",
       x = "Hour of Day",
       y = "Weekday",
       fill = "Mean Delay (min)") +
  theme_minimal()

#--------------------------------------------------------------------------------
# “Do trains experience more delays during peak commuter hours lets say between 7–9am, 4–7pm compared to off-peak times?
ggplot(db_vis, aes(x = peak_hour, y = arr_clip, fill = peak_hour)) +
geom_violin(trim = FALSE, alpha = 0.6) +
geom_boxplot(width = 0.15, fill = "white") +
labs(title = "Peak vs Off-Peak: Arrival Delay Distribution",
     x = "", y = "Arrival delay (minutes, clipped)") +
theme_minimal()
#--------------------------------------------------------------------------------

#“Do departure delays lead to arrival delays or vice versa ?”
#Hexbin: arrival vs departure delays 

p_hex <- ggplot(db_vis, aes(dep_clip, arr_clip)) +
geom_hex(bins = 40) +
scale_fill_viridis(option = "D", name = "count") +
coord_equal() +
labs(title = "Joint distribution: departure vs arrival delays",
x = "departure delay (min, clipped)", y = "arrival delay (min, clipped)")
print(p_hex)

#--------------------------------------------------------------------------------
  
#“Do bigger station categories have higher average arrival delays?”
# Mean arrival delay by station category by calculate the average arrival delay for each station category.
cat_df <- aggregate(arrival_delay_m ~ category, data = db_data, FUN = function(x) mean(x, na.rm = TRUE))

# Order categories by mean delay hence to reorder the categories so when we plot, the bars appear from smallest mean delay to largest
cat_df <- cat_df[order(cat_df$arrival_delay_m), ]
cat_df$category <- factor(cat_df$category, levels = cat_df$category)

# 3) Bar chart
p_cat <- ggplot(cat_df, aes(x = category, y = arrival_delay_m)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Mean Arrival Delay by Station Category",
       x = "Station Category", y = "Mean delay (minutes)") +
  theme_minimal()

print(cat_df)
print(p_cat)
#--------------------------------------------------------------------------------

#“What is the probability of crossing DB’s 6-minute official delay threshold, and does this actually differ between peak and off-peak times?”
ggplot(db_vis, aes(x = arrival_delay_m, color = peak_hour)) +
stat_ecdf() +
geom_vline(xintercept = 6, linetype = "dashed", color = "red") +
labs(title = "Probability of Delay (Peak vs Off-Peak)",
     x = "Arrival delay (minutes)", y = "Cumulative probability") +
theme_minimal()

#--------------------------------------------------------------------------------

# ---- Hypothesis testing---- 

# Hypothesis 1: Station Size (ANOVA) Arrival delay by station category

# H₀ (null): Mean arrival delays are equal across all station categories.

# H₁ (alt): At least one station category has a different mean arrival delay.

anova_model <- aov(arrival_delay_m ~ category, data = db_data)
summary(anova_model)
# Tukey's Honest Significant Difference Test
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# Optional: plot the confidence intervals
plot(tukey_results)

#--------------------------------------------------------------------------------

# Hypothesis 2: T-Test 

# H₀ (null): Mean delays are the same in peak vs off-peak hours.

# H₁ (alt): Mean delays are higher during peak hours.

# Two-sample t-test: peak vs off-peak
t_test_result <- t.test(arrival_delay_m ~ peak_hour, data = db_data)

print(t_test_result)

#--------------------------------------------------------------------------------

# Hypothesis 3: ANOVA

# H₀ (null): Average delays are equal across weekdays (Mon–Sun).

# H₁ (alt): At least one weekday has a different average delay.


anova_weekday <- aov(arrival_delay_m ~ weekday, data = db_data)
summary(anova_weekday)

tukey_weekday <- TukeyHSD(anova_weekday)
print(tukey_weekday)
plot(tukey_weekday)

#--------------------------------------------------------------------------------

# Hypothesis 4: Correlation

# H₀ (null): Arrival delays and departure delays are unrelated (correlation = 0; mean difference = 0).

# H₁ (alt): Arrival delays strongly predict departure delays (positive correlation > 0).

# Correlation between arrival and departure delays
cor_result <- cor.test(db_data$arrival_delay_m, db_data$departure_delay_m, use = "complete.obs")
print(cor_result)

#--------------------------------------------------------------------------------

# Hypothesis 5

# H₀ (null): Probability of delay > 6 min is independent of station category, weekday, and time of day.

# H₁ (alt): Probability of delay > 6 min depends on these factors.

# Make sure it's coded as factor (Delayed vs OnTime)
db_data$arrival_delay_check <- factor(db_data$arrival_delay_m > 6, 
                                      levels = c(FALSE, TRUE), 
                                      labels = c("OnTime", "Delayed"))
table(db_data$arrival_delay_check)


# Chi-square: Delay vs Weekday
chisq_weekday <- chisq.test(table(db_data$arrival_delay_check, db_data$weekday))
chisq_weekday

# Station Catagory 
chisq_category <- chisq.test(table(db_data$arrival_delay_check, db_data$category))
chisq_category

# peak/off-peak
chisq_peak <- chisq.test(table(db_data$arrival_delay_check, db_data$peak_hour))
chisq_peak
