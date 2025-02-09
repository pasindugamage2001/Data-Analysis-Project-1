# Clear workspace
ls()
rm(list = ls())


# Load the data set
Sleep_data = read.csv(file = "sleeptime_prediction_dataset.csv", header = TRUE, sep = ",")

# Dimension of the data set
dim(Sleep_data)

# Names of the variables 
names(Sleep_data)

# Check the classes of variables in the data set
sapply(Sleep_data, class)

###########################################################################

# Finding missing values 
missing_values = sum(is.na(Sleep_data))
paste("No of missing values in the dataset:", missing_values)

# Finding duplicate values 
duplicated_val = sum(duplicated(Sleep_data) == TRUE)
paste("No of duplicated data in the dataset:", duplicated_val)

# Note: Variables are mutually independent 

## Data Filtering 

# Remove the responses where total number of hours calculated for each daily activity is greater than 24 
# List of time-related columns
time_columns = c("WorkoutTime", "ReadingTime", "PhoneTime", "WorkHours", "RelaxationTime", "SleepTime")

# Calculate the sum of each row for the selected columns
TotalTime = rowSums(Sleep_data[, time_columns], na.rm = TRUE)

# Filter rows where the total time is less than or equal to 24 hours
Data = Sleep_data[TotalTime <= 24, ]
dim(Data)
summary(Data)

# Detecting outliers  
outliers = function(data) {
  data = na.omit(data)  # Remove NAs before finding outliers
  return(length(boxplot(data, plot = FALSE)$out))
}

paste("No of outliers in the workout time column:", outliers(Data$WorkoutTime))
paste("No of outliers in the reading time column:", outliers(Data$ReadingTime))
paste("No of outliers in the phone time column:", outliers(Data$PhoneTime))
paste("No of outliers in the work hours column:", outliers(Data$WorkHours))
paste("No of outliers in the caffeine intake column:", outliers(Data$CaffeineIntake))
paste("No of outliers in the relaxation time column:", outliers(Data$RelaxationTime))
paste("No of outliers in the sleep time column:", outliers(Data$SleepTime))

# Only sleep time column has outliers

## Feature Engineering 

# Categorize sleep time
breaks = c(0, 7, 9, Inf)
labels = c("Short Sleep", "Optimal Sleep", "Long Sleep")

data = Data %>%
  mutate(SleepPattern = cut(Data$SleepTime, breaks = breaks, labels = labels, include.lowest = TRUE))
print(data)
summary(data)

##############################################################################

# Split the data set into training and testing 
set.seed(123)
index = sample(1:nrow(data), 0.2 * nrow(data))

# Test data set
test_Data = data[index, ]
paste(dim(test_Data)[1], "of data are used as test set")

# Train data set
train_Data = data[-index, ]
paste(dim(train_Data)[1], "of data are used as train set")

############################################################################

# Time allocation for each activity 

# Calculate average time for each activity
avg_time = colMeans(train_Data[, c("WorkHours", "WorkoutTime", "RelaxationTime", "ReadingTime", "PhoneTime", "SleepTime")])

# Convert to a data frame
avg_time_df = data.frame(Activity = names(avg_time), Time = avg_time)

# Pie chart
library(ggplot2)

ggplot(avg_time_df, aes(x = "", y = Time, fill = Activity)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    title = "Average Time Allocation Across Activities",
    fill = "Activity"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Summary statistics for sleep time
summary(train_Data$SleepTime)
sd(train_Data$SleepTime)

# Histogram of sleep time 
ggplot(train_Data, aes(x = SleepTime)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black", position = "dodge", alpha = 0.7) +
  labs(
    title = "Histogram of Sleep Time",
    x = "Sleep Time",
    y = "Count"
  ) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Density plot of Sleep Time 
ggplot(train_Data, aes(x = SleepTime)) +
  geom_density(binwidth = 0.5, fill = "lightblue", color = "black", position = "dodge", alpha = 0.7) +
  labs(
    title = "Density Plot of Sleep Time",
    x = "Sleep Time",
  ) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Boxplot of Sleep Data
boxplot(train_Data$SleepTime, col = "#FF3", main = "Boxplot Of Sleep Time")

# Scatterplot of Workout Time vs SleepTime
library(gridExtra)

p1 = ggplot(train_Data, aes(x = WorkoutTime, y = SleepTime)) +
  geom_point(color = "#F8C2DA", alpha = 0.7) +
  labs(
    x = "Workout Time (hours)",
    y = "Sleep Time (hours)"
  ) + geom_smooth(method = lm, color = "#DC166B")
theme_minimal()

# Scatterplot of Reading Time vs SleepTime
p2 = ggplot(train_Data, aes(x = ReadingTime, y = SleepTime)) +
  geom_point(color = "#AFEEEE", alpha = 0.7) +
  labs(
    x = "Reading Time (hours)",
    y = "Sleep Time (hours)"
  ) + geom_smooth(method = lm, color = "#6495ED")
theme_minimal()

# Scatterplot of Relaxation Time vs SleepTime
p3 = ggplot(train_Data, aes(x = RelaxationTime, y = SleepTime)) +
  geom_point(color = "#FFEBCD", alpha = 0.7) +
  labs(
    x = "Relaxation Time (hours)",
    y = "Sleep Time (hours)"
  ) + geom_smooth(method = lm, color = "#CD853F")

grid.arrange(p1, p2, p3, nrow = 1)

# Scatterplot of PhoneTime vs SleepTime
p4 = ggplot(train_Data, aes(x = PhoneTime, y = SleepTime)) +
  geom_point(color = "#90EE90", alpha = 0.7) +
  labs(
    x = "Phone Time (hours)",
    y = "Sleep Time (hours)"
  ) + geom_smooth(method = lm, color = "#228B22")
theme_minimal()

# Scatterplot of Caffeine Intake vs SleepTime
p5 = ggplot(train_Data, aes(x = CaffeineIntake, y = SleepTime)) +
  geom_point(color = "#D8BFD8", alpha = 0.7) +
  labs(
    x = "Caffeine Intake (mg)",
    y = "Sleep Time (hours)"
  ) + geom_smooth(method = lm, color = "#BA55D3")
theme_minimal()

# Scatterplot of Work Hours vs SleepTime
p6 = ggplot(train_Data, aes(x = WorkHours, y = SleepTime)) +
  geom_point(color = "#ADD8E6", alpha = 0.7) +
  labs(
    x = "Work Hours (hours)",
    y = "Sleep Time (hours)"
  ) + geom_smooth(method = lm, color = "#0000CD")
theme_minimal()

grid.arrange(p4, p5, p6, nrow = 1)

# Boxplot for predictors vs sleep pattern 
predictors = c("WorkHours", "PhoneTime", "ReadingTime", "CaffeineIntake", "RelaxationTime", "WorkoutTime")

par(mfrow = c(2, 3))

for (var in predictors) {
  plot(train_Data$SleepPattern, train_Data[[var]], col = "purple",
       pch = 16, main = paste("Sleep Pattern vs .", var),
       xlab = "Sleep Pattern", ylab = var)
}

# Compute correlation matrix
cor_matrix <- cor(train_Data[, c("WorkHours", "WorkoutTime", "RelaxationTime", "ReadingTime", "PhoneTime", "CaffeineIntake", "SleepTime")], use = "complete.obs")

# Melt the correlation matrix for visualization
library(reshape2)
melted_cor <- melt(cor_matrix)

# Create the heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "darkblue", high = "white", mid = "red",
                       midpoint = 0, limit = c(-1, 1), space = "Lab") +
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", fill = "Correlation")

# Principal Component Analysis (PCA)

# Select predictor variables (excluding response variable "SleepTime")
predictor_columns <- c("WorkHours", "WorkoutTime", "RelaxationTime", "ReadingTime", "PhoneTime", "CaffeineIntake")

# Perform PCA
pca_result <- prcomp(train_Data[, predictor_columns], center = TRUE, scale. = TRUE)

# Print summary of PCA
summary(pca_result)

# Contribution of variables to PCs
library(factoextra)
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("blue", "red"))

# 2D Score Plot for PCA
pca_scores <- as.data.frame(pca_result$x)

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ggtitle("2D Score Plot") +
  xlab("PC1") +
  ylab("PC2")
