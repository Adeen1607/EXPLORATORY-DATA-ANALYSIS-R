##################################################
### PROG8435   Data Analysis Mathematics, Algorithms and Modeling                 
##################################################
# Title:
# ASSIGNMENT-1 Exploratory / Descriptive Analysis
##################################################
##################################################
# Written by {MOHAMMED ADEEN, SHAIK}
# ID: 8969152
#
##################################################

# Load necessary libraries
library(ggplot2)

#set work directory
setwd("C:/Users/Adeen/Desktop/R scripts/Assignment 1")

# Load the data from a CSV file
data <- read.csv(file = "PROG8435_Assign_Explore_24S.csv", header = TRUE)

# Display the first few rows of the dataset to verify
head(data)

## TASK 1: Summary Table:

# 1.a summary table to show the total income by each category of marital status.
Summary_table <- aggregate(income ~ m.status, data = data,FUN= sum, na.rm = TRUE)
print(Summary_table)
# 1.b highest total income

# Fetching the row with the highest income in the summary table
highest_total_income_status <- Summary_table$m.status[which.max(Summary_table$income)]

# This will print the value of m.status with the highest total income
print(highest_total_income_status)  

# 2.a. Calculate the mean age of respondents born in Asia.
mean_age_asia <- mean(subset(data, nation == "Asia")$age, na.rm = TRUE)
mean_age_asia <- round(mean_age_asia, 2)
print(mean_age_asia)

# 2.b. Calculate the mean age of respondents born in Asia weighted by the number of children they have.
weighted_mean_age_asia <- with(subset(data, nation == "Asia"), weighted.mean(age, n.child, na.rm = TRUE))
weighted_mean_age_asia <- round(weighted_mean_age_asia, 2)
print(weighted_mean_age_asia)

# 3.a  table to show the mean score on the political:
comparison_table<-aggregate(score ~ gender, data = data, FUN = mean)
print(comparison_table)
# 3.b who has the highest mean score? :
highest_score<-comparison_table$gender[which.max(comparison_table$score)]
print(highest_score)

# 4.  Calculate the 34th and 63rd percentiles of percentage of time taken on the test
percentiles <- quantile(data$time1, probs = c(0.34, 0.63))
print(percentiles)

## TASK 2 : ORGANIZING DATA

# 1. Pie chart

# a. Create a pie chart showing the number of respondents by Political Affiliation
pie_table <- table(data$political)
pie(pie_table, main = "Respondents by Political Affiliation")

# b. Which Political Affiliation contains the most respondents?
most_respondents <- names(pie_table)[which.max(pie_table)]
print(most_respondents)

# c. Which Political Affiliation has the fewest respondents?
fewest_respondents <- names(pie_table)[which.min(pie_table)]
print(fewest_respondents)

# 2. Summary Table

# a. Create a table that shows the percentage of respondents from each Region in the Treatment group

total_respondents_by_region <- table(data$nation)
treat_counts_by_region <- aggregate(data$group == "treat", by = list(data$nation), FUN = sum)
treat_counts_by_region$Percentage_Treat_Respondents <- (treat_counts_by_region$x / total_respondents_by_region[treat_counts_by_region$Group.1]) * 100

#renaming the column names for better understanding of the summary table
colnames(treat_counts_by_region) <- c("Region", "Number_of_respondents", "Percentage_of_respondents")

print(treat_counts_by_region)

# b. Which region has the highest percentage of people in the Treatment group?
highest_percentage_region <- treat_counts_by_region$Region[which.max(treat_counts_by_region$Percentage_of_respondents)]
print(highest_percentage_region)
# c. Which region has the lowest percentage of people in the Treatment group?
lowest_percentage_region <- treat_counts_by_region$Region[which.min(treat_counts_by_region$Percentage_of_respondents)]
print(lowest_percentage_region)

# 3. Barchart
 
# a. Create a bar chart showing the mean Standardized Test Score on the Political Awareness Test for each Region(nation)
mean_scr_by_region <- aggregate(scr~nation, data = data, FUN = mean)
ggplot(mean_scr_by_region, aes(x = nation, y = scr)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean standardized Test score by Region", x = "Region", y = "Mean standardized test score")+
  coord_cartesian(ylim = c(1.0, 1.2))

# b.  Which Region has the lowest mean score?
lowest_mean_scr<-mean_scr_by_region$nation[which.min(mean_scr_by_region$scr)]
print(lowest_mean_scr)

# c.  Which Region has the highest mean score?
highest_mean_scr<-mean_scr_by_region$nation[which.max(mean_scr_by_region$scr)]
print(highest_mean_scr)
# 4 Histogram

# a. Create a histogram with 5 bins showing the distribution of the percentage of household income going to food.
ggplot(data, aes(x = food*100)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +  
  labs(title = "Distribution of percentage of household Income to food", x = "Percentage of Income to food", y = "Frequency")

# b. Which range of values has the highest frequency?
# Answered in word document by referring to histogram.

# 5 Boxplot

# a. Create a sequence of box plotsshowing the distribution of income separated by marital status
ggplot(data, aes(x = income, y = m.status)) +
  geom_boxplot() +
  labs(title = "Distribution of Income by marital status", x = "Income", y = "Marital status")
# b,c,d answered in word documentation by referring to the boxplot.

# 6 Scatterplot

# a. Histogram of Income
ggplot(data, aes(x = income)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black") +  
  labs(title = "Distribution of Income", x = "Income", y = "Frequency")

# b. Histogram for Standardized score (scr)
ggplot(data, aes(x = scr)) +
  geom_histogram(binwidth = 0.5,fill = "skyblue", color = "black") +
  labs(title = "Distribution of Standardized score", x = "Standardized score", y = "Frequency")

# c. Create a scatter plot showing the relationship between the income and standardized score
ggplot(data, aes(x = income, y = scr)) +
  geom_point() +
  labs(title = "Relationship between income and standardized score", x = "income", y = "Std. score")

# d.conclusions in word doc.

# e. correlation efficient
correlation_coefficient <- cor(data$income, data$scr)
print(correlation_coefficient)
