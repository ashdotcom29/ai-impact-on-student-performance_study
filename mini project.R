# Load Libraries
library(dplyr)
library(ggplot2)


# Import Dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)

head(data)
str(data)
summary(data)


# Data Cleaning
data <- na.omit(data)

data$Primary_AI_Tool <- as.factor(data$Primary_AI_Tool)
data$Main_Usage_Case <- as.factor(data$Main_Usage_Case)
data$AI_Ethics_Concern <- as.factor(data$AI_Ethics_Concern)


# Feature Creation
data$GPA_Improvement <- data$GPA_Post_AI - data$GPA_Baseline

data$Usage_Level <- cut(data$Task_Frequency_Daily,
                        breaks = c(0, 3, 6, 10),
                        labels = c("Low", "Medium", "High"))


# Descriptive Statistics
mean(data$GPA_Baseline)
mean(data$GPA_Post_AI)
mean(data$GPA_Improvement)

data %>%
  group_by(Usage_Level) %>%
  summarise(avg_improvement = mean(GPA_Improvement))


# Visualization

gpa_data <- data.frame(
  Type = c("Before AI", "After AI"),
  GPA = c(mean(data$GPA_Baseline), mean(data$GPA_Post_AI))
)

ggplot(gpa_data, aes(x = Type, y = GPA)) +
  geom_bar(stat = "identity") +
  ggtitle("GPA Before vs After AI")

ggplot(data, aes(x = Task_Frequency_Daily, y = GPA_Improvement)) +
  geom_point() +
  ggtitle("AI Usage vs GPA Improvement")

ggplot(data, aes(x = Primary_AI_Tool, y = GPA_Improvement)) +
  geom_boxplot() +
  ggtitle("GPA Improvement by AI Tool")


# Correlation
cor(data$Task_Frequency_Daily, data$GPA_Improvement)


# Regression Model
model <- lm(GPA_Post_AI ~ GPA_Baseline + Task_Frequency_Daily + Time_Saved_Hours_Weekly, data = data)

summary(model)
coef(model)


# Prediction
data$Predicted_GPA <- predict(model, newdata = data)


# Save Data
write.csv(data, "cleaned_ai_data.csv", row.names = FALSE)

#print cleaned data
print(data)
