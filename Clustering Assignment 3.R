##################################################
### PROG8435   Data Analysis Mathematics, Algorithms and Modeling                 
##################################################
# Title:
# ASSIGNMENT-3 Unsupervised Learning: K-Means Clustering
##################################################
##################################################
# Written by {MOHAMMED ADEEN, SHAIK}
# ID: 8969152
#
##################################################


# Load necessary libraries
library(ggplot2)
library("pastecs") # For statistical analysis
library("lattice") # For improved graphical representations

#set work directory
setwd("C:/Users/Adeen/Desktop/R scripts/Assignment 3")

# Load the data from the file
df <- read.csv(file = "PROG8435-24S-Assign03.txt", header = TRUE)

# Display the first few rows of the dataset to verify
head(df)

## TASK 1: Data Transformation and Descriptive Analysis

# 1.Rename all variables with my initials (MS) appended
names(df) <- paste0(names(df), "_MS")
head(df,5)

# 2. Create graphical summaries of the data

# Generating boxplots for each variable to identify outliers and overall distribution
for(col in names(df)) {
  print( ggplot(df, aes_string(x = "factor(1)", y = col)) +
    geom_boxplot() +
    labs(y = col, title = paste("Boxplot of", col)) +
    theme_light())  # Using a light theme for a clean look
}

# Generating histograms for each variable to visualize the frequency distribution
for(col2 in names(df)) {
  print( ggplot(df, aes_string(x = col2)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(x = col2, y = "Frequency", title = paste("Histogram of", col2)) +
    theme_minimal())  # Minimal theme for a modern look
  
}

# 3. standardize all variables
df_standardized <- as.data.frame(scale(df))
head(df_standardized,4)
# TASK 2. K means clustering

# 1. creating segementation/clustering for k
df_cluster <- select(df_standardized, Milk_MS, Froz_MS )

# 2. Creating wss plots
wss_values <- sapply(2:7, function(k){
  kmeans(df_cluster, centers = k, nstart = 20)$tot.withinss
})

# Plotting the WSS values to visualize the 'elbow' with k = 2,3,4,5,6,7
plot(2:7, wss_values, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Total within-cluster sum of squares")


# Task 3: Evaluation of Clusters

# 4.1: clusters for k=4, k-1 = 3 , k+1 = 5

# k = 4
set.seed(123)  # Ensuring reproducibility
k_optimal <- 4
kmeans_result <- kmeans(df_cluster, centers = k_optimal, nstart = 20)

# Creating a scatter plot to visualize the clusters formed
ggplot(df_cluster, aes(x = Milk_MS, y = Froz_MS)) +
  geom_point(aes(color = as.factor(kmeans_result$cluster))) +  
  scale_color_manual(values = rainbow(k_optimal)) + 
  labs(title = "Cluster visualization with k=4", x = "Milk Products", y = "Frozen Products")


# k-1 = 3
set.seed(123)  # Ensuring reproducibility
k_optimal <- 3
kmeans_result_k2 <- kmeans(df_cluster, centers = k_optimal, nstart = 20)

# Creating a scatter plot to visualize the clusters formed
ggplot(df_cluster, aes(x = Milk_MS, y = Froz_MS)) +
  geom_point(aes(color = as.factor(kmeans_result_k2$cluster))) +  # Coloring points by cluster
  scale_color_manual(values = rainbow(k_optimal)) +  # Using different colors for each cluster
  labs(title = "Cluster visualization with k-1 = 3", x = "Milk Products", y = "Frozen Products")

# k+1 = 5
set.seed(123)  # Ensuring reproducibility
k_optimal <- 5
kmeans_result_k3 <- kmeans(df_cluster, centers = k_optimal, nstart = 20)

# Creating a scatter plot to visualize the clusters formed
ggplot(df_cluster, aes(x = Milk_MS, y = Froz_MS)) +
  geom_point(aes(color = as.factor(kmeans_result_k3$cluster))) +  # Coloring points by cluster
  scale_color_manual(values = rainbow(k_optimal)) +  # Using different colors for each cluster
  labs(title = "Cluster visualization with k+1 = 5", x = "Milk Products", y = "Frozen Products")

# 4.2 : I conclude that the set with k=4 best describe the data

set.seed(123)  # Ensuring reproducibility
k_optimal <- 4
kmeans_result <- kmeans(df_cluster, centers = k_optimal, nstart = 20)
 
# 4.3 : Summary table for the chosen set

summary_table <- df_cluster %>%
  mutate(cluster = kmeans_result$cluster) %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    Milk_MS_mean = mean(Milk_MS),
    Milk_MS_sd = sd(Milk_MS),
    Froz_MS_mean = mean(Froz_MS),
    Froz_MS_sd = sd(Froz_MS)
  )

# Print the summary table
print(summary_table)

# 4.4 : Creating suitable names for the clusters

centers <- data.frame(kmeans_result$centers)
centers$cluster <- factor(1:k_optimal)

cluster_labels <- data.frame(
  cluster = factor(1:k_optimal),
  label = c(" Low Milk / Low forzen ", "High Milk / Low frozen", "Low Milk/ High Frozen", "High Milk / High Frozen"))

# Merge cluster centers with labels
centers <- merge(centers, cluster_labels, by = "cluster") 

# scatterplot of the cluster chart after naming them:
ggplot(df_cluster, aes(x = Milk_MS, y = Froz_MS)) +
  geom_point(aes(color = as.factor(kmeans_result$cluster))) +
  scale_color_manual(values = rainbow(k_optimal)) +
  geom_text(data = centers, aes(x = Milk_MS, y = Froz_MS, label = label), vjust = -1, hjust = 1) +  # Adding labels
  labs(title = "Cluster visualization with k=4", x = "Milk Products", y = "Frozen Products") +
  theme_minimal()