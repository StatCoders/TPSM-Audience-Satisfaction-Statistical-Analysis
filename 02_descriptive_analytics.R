#Load cleaned dataset
data <- read.csv("dataset/cleaned_audience_data.csv")
print(paste("Total observations loaded:", nrow(data)))


#Create Quota Sampling Table to reduce bias
quota_table <- data.frame(
  Age_Group = c("Teen (13-19 years)", "Young Adult (20-29 years)", 
                "Adult (30-44 years)", "Middle-Aged Adult (45-59 years)", 
                "Senior (60 years and above)"),
  Quota_Percent = c(10.3, 17.3, 25.9, 23.8, 22.7),
  Quota_Count = c(51, 84, 126, 117, 111)
)
print("QUOTA SAMPLING TABLE (Proportionate Method)")
print(quota_table)


#Compare Quota vs Observed Counts
observed_counts <- table(data$Age_Group)
observed_df <- data.frame(
  Age_Group = names(observed_counts),
  Observed_Count = as.numeric(observed_counts)
)
comparison <- merge(quota_table, observed_df, by = "Age_Group", all.x = TRUE)
print("QUOTA vs OBSERVED COUNTS")
print(comparison)


# Ensure Observed_Count is in exactly the same order as quota_table
observed_ordered <- comparison$Observed_Count[match(quota_table$Age_Group, comparison$Age_Group)]

# Create matrix for barplot
barplot_data <- rbind(quota_table$Quota_Count, observed_ordered)
rownames(barplot_data) <- c("Quota", "Observed")
colnames(barplot_data) <- quota_table$Age_Group

# Plot
barplot(barplot_data, beside = TRUE,
        main = "Quota vs Observed Sample by Age Group",
        ylab = "Number of Respondents",
        col = c("skyblue", "orange"),
        legend = rownames(barplot_data),
        args.legend = list(x = "topright"))

mtext("Proportionate Quota Sampling (Sri Lanka 2025 mid-year estimates)", 
      side = 3, line = 0.5)


#Overall Descriptive Statistics for Satisfaction_Score
summary_satisfaction <- summary(data$Satisfaction_Score)
print("OVERALL SATISFACTION SCORE (Summary)")
print(summary_satisfaction)
sd_satisfaction <- sd(data$Satisfaction_Score)
print(paste("Standard Deviation of Satisfaction Score:", round(sd_satisfaction, 3)))


#Proportion of Repeat Viewers
repeat_prop <- prop.table(table(data$Repeat_Viewer))
print("PROPORTION OF REPEAT VIEWERS")
print(round(repeat_prop * 100, 2))



#Mean Satisfaction by Repeat_Viewer
mean_by_repeat <- tapply(data$Satisfaction_Score, data$Repeat_Viewer, mean)
print("MEAN SATISFACTION SCORE BY REPEAT VIEWER")
print(round(mean_by_repeat, 3))



#Box Plot - Satisfaction by Repeat Viewer
boxplot(Satisfaction_Score ~ Repeat_Viewer, data = data,
        main = "Satisfaction Score by Repeat Viewing Behaviour",
        ylab = "Satisfaction Score (1-5)",
        col = c("lightgreen", "lightcoral"),
        xlab = "Repeat Viewer (Yes/No)")
means <- tapply(data$Satisfaction_Score, data$Repeat_Viewer, mean)
points(1:2, means, pch = 19, col = "red", cex = 1.5)



#Crosstab - Satisfaction Level vs Repeat_Viewer
data$Satisfaction_Level <- cut(data$Satisfaction_Score, 
                               breaks = c(0, 2.5, 3.5, 5), 
                               labels = c("Low", "Medium", "High"))

crosstab <- table(data$Satisfaction_Level, data$Repeat_Viewer)
print("CROSS-TAB: SATISFACTION LEVEL vs REPEAT VIEWER")
print(crosstab)
print(round(prop.table(crosstab, margin = 1) * 100, 2))



#Mean Satisfaction by Age Group + Bar Plot
mean_by_age <- tapply(data$Satisfaction_Score, data$Age_Group, mean)
print(" MEAN SATISFACTION BY AGE GROUP")
print(round(mean_by_age, 3))

barplot(mean_by_age, main = "Average Satisfaction Score by Age Group",
        ylab = "Mean Satisfaction Score", col = "steelblue",
        las = 2)




