#Load the dataset
data <- read.csv("dataset/cleaned_audience_data.csv")
print(paste("Total observations loaded:", nrow(data)))


#Two sample t-test : Mean Satisfaction Score by Repeat Viewer
t_test_result <- t.test(Satisfaction_Score ~ Repeat_Viewer, data = data, var.equal = TRUE)
print(" TWO-SAMPLE T-TEST: Satisfaction Score by Repeat Viewer")
print(t_test_result)


#Check assumptions for t-test (Levene’s test -> equal variance)
library(car)
levene_test <- leveneTest(Satisfaction_Score ~ Repeat_Viewer, data = data)
print("LEVENE'S TEST FOR EQUAL VARIANCES")
print(levene_test)


#Two sample t-test : Welch's version [As variances were not equal]
t_test_result <- t.test(Satisfaction_Score ~ Repeat_Viewer, data = data, var.equal = FALSE)
print(" TWO-SAMPLE T-TEST: Welch's Version")
print(t_test_result)


#Chi-square test – Association between Satisfaction Level and Repeat Viewer

# Create Satisfaction_Level column (required for chi-square test)
data$Satisfaction_Level <- cut(data$Satisfaction_Score, 
                               breaks = c(0, 2.5, 3.5, 5), 
                               labels = c("Low", "Medium", "High"))

chisq_result <- chisq.test(table(data$Satisfaction_Level, data$Repeat_Viewer))
print("CHI-SQUARE TEST: Satisfaction Level vs Repeat Viewer")
print(chisq_result)



#Proportion test – Proportion of Repeat Viewers
prop_result <- prop.test(x = table(data$Repeat_Viewer)[2], n = nrow(data), p = 0.5, alternative = "greater")
print("ONE-PROPORTION Z-TEST: Proportion of Repeat Viewers > 50%")
print(prop_result)



#One-way ANOVA – Satisfaction Score across Age Groups
anova_result <- aov(Satisfaction_Score ~ Age_Group, data = data)
print("ONE-WAY ANOVA: Satisfaction Score by Age Group")
summary(anova_result)



#Post-hoc analysis (Tukey’s HSD) after ANOVA
posthoc <- TukeyHSD(anova_result)
print("POST-HOC Analysis")
print(posthoc)


#Summary of all inferential results
print("Inferential Analaysis Summary")
cat("1. Two-sample t-test (Welch) p-value   : ", round(t_test_result$p.value, 4), "\n")
cat("2. Chi-square test p-value             : ", round(chisq_result$p.value, 4), "\n")
cat("3. One-proportion test p-value         : ", round(prop_result$p.value, 4), "\n")
cat("4. ANOVA p-value                       : ", round(summary(anova_result)[[1]][["Pr(>F)"]][1], 4), "\n")


# === As P values are extremely small it is not good to show as p=0 therefore,

# Summary of all inferential results 
print("=== INFERENTIAL ANALYSIS SUMMARY ===")

cat("1. Two-sample t-test (Welch) p-value     : ", 
    ifelse(t_test_result$p.value < 0.001, "< 0.001", round(t_test_result$p.value, 4)), "\n")

cat("2. Chi-square test p-value                : ", 
    ifelse(chisq_result$p.value < 0.001, "< 0.001", round(chisq_result$p.value, 4)), "\n")

cat("3. One-proportion test p-value           : ", 
    ifelse(prop_result$p.value < 0.001, "< 0.001", round(prop_result$p.value, 4)), "\n")

cat("4. ANOVA p-value                         : ", 
    ifelse(summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.001, "< 0.001", 
           round(summary(anova_result)[[1]][["Pr(>F)"]][1], 4)), "\n")
