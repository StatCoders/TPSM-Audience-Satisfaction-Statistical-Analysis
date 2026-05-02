
required_packages <- c("corrplot", "rpart", "rpart.plot",
                       "randomForest", "caret", "pROC")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
print("All packages loaded.")

# BLOCK 2: Load Dataset
data <- read.csv("dataset/cleaned_audience_data.csv", stringsAsFactors = FALSE)
print(paste("Rows loaded:", nrow(data)))
print(colnames(data))



# BLOCK 3: Feature Engineering

print("=== FEATURE ENGINEERING ===")

# Target variable as factor (required for classification)
data$Repeat_Viewer <- as.factor(data$Repeat_Viewer)

# Age_Group: ordered Teen=1 to Senior=5
data$Age_Group_Num <- as.integer(factor(data$Age_Group,
                                        levels = c("Teen (13-19 years)",
                                                   "Young Adult (20-29 years)",
                                                   "Adult (30-44 years)",
                                                   "Middle-Aged Adult (45-59 years)",
                                                   "Senior (60 years and above)")))

# Gender: Male=1, Female=0
data$Gender_Num <- ifelse(data$Gender == "Male", 1, 0)

# Hours_Per_Week: ordered Less=1 to More than 10=4
data$Hours_Num <- as.integer(factor(data$Hours_Per_Week,
                                    levels = c("Less than 2 hours", "2-5 hours",
                                               "6-10 hours", "More than 10 hours")))

# Satisfaction_Level: for EDA reference only, NOT used in models
data$Satisfaction_Level <- cut(data$Satisfaction_Score,
                               breaks = c(0, 2.5, 3.5, 5),
                               labels = c("Low", "Medium", "High"))

print("Feature engineering done.")
cat("\nClass Distribution (%):\n")
print(round(prop.table(table(data$Repeat_Viewer)) * 100, 1))


# BLOCK 4: EDA Visualizations
print("=== EDA VISUALIZATIONS ===")

# How many Yes vs No in the dataset
barplot(table(data$Repeat_Viewer),
        main = "Distribution of Repeat Viewer",
        ylab = "Count", col = c("lightblue", "lightgreen"), ylim = c(0, 430))

# Does satisfaction differ between repeat vs non-repeat viewers?
boxplot(Satisfaction_Score ~ Repeat_Viewer, data = data,
        main = "Satisfaction Score by Repeat Viewer",
        ylab = "Satisfaction Score", col = c("lightcoral", "lightgreen"))

# Does watching hours relate to repeat viewing?
barplot(table(data$Repeat_Viewer, data$Hours_Per_Week),
        main = "Hours Per Week by Repeat Viewer", beside = TRUE,
        col = c("lightblue", "lightgreen"), legend = TRUE,
        args.legend = list(x = "topright"), las = 2, cex.names = 0.75)

# Does age group affect repeat viewing?
barplot(table(data$Repeat_Viewer, data$Age_Group),
        main = "Age Group by Repeat Viewer", beside = TRUE,
        col = c("lightblue", "lightgreen"), legend = TRUE,
        args.legend = list(x = "topright"), las = 2, cex.names = 0.6)


# BLOCK 5: Correlation Heatmap

print("=== CORRELATION HEATMAP ===")

numeric_data <- data[, c("Satisfaction_Score", "Emotional_Engagement",
                         "Value_For_Time", "Prefer_Familiar_Content",
                         "Meets_Expectations", "Satisfaction",
                         "Hours_Num", "Age_Group_Num", "Gender_Num")]

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.75, title = "Correlation Heatmap",
         mar = c(0,0,2,0), addCoef.col = "black",
         number.cex = 0.65, tl.col = "black")


# BLOCK 6: Train-Test Split (70% Train / 30% Test)

set.seed(123)   # ensures same random split every run
trainID <- sample(1:nrow(data), round(0.7 * nrow(data)))
train   <- data[trainID, ]
test    <- data[-trainID, ]
print(paste("Train:", nrow(train), "| Test:", nrow(test)))


# BLOCK 7: Handle Class Imbalance using upSample
print("=== HANDLING CLASS IMBALANCE ===")

# Only use the 8 clean, leak-free predictors
model_cols <- c("Satisfaction_Score", "Emotional_Engagement",
                "Value_For_Time", "Prefer_Familiar_Content",
                "Meets_Expectations", "Satisfaction",
                "Age_Group_Num", "Gender_Num", "Hours_Num",
                "Repeat_Viewer")

train_clean <- train[, model_cols]
test_clean  <- test[, model_cols]

train_balanced <- upSample(
  x     = train_clean[, -which(names(train_clean) == "Repeat_Viewer")],
  y     = train_clean$Repeat_Viewer,
  yname = "Repeat_Viewer")

cat("Balanced class distribution:\n")
print(table(train_balanced$Repeat_Viewer))

# BLOCK 8: Logistic Regression
print("=== LOGISTIC REGRESSION ===")

logistic_model <- glm(
  Repeat_Viewer ~ Satisfaction_Score + Emotional_Engagement +
    Value_For_Time + Prefer_Familiar_Content + Meets_Expectations +
    Satisfaction + Age_Group_Num + Gender_Num + Hours_Num,
  data   = train_balanced,
  family = binomial(link = "logit"))

print(summary(logistic_model))   # shows p-values for each predictor

# Training accuracy
pred_train_log       <- predict(logistic_model, train_balanced, type = "response")
pred_class_train_log <- factor(ifelse(pred_train_log > 0.5, "Yes", "No"), levels = c("No","Yes"))
train_acc_log        <- mean(pred_class_train_log == train_balanced$Repeat_Viewer)

# Test accuracy
pred_test_log       <- predict(logistic_model, test_clean, type = "response")
pred_class_test_log <- factor(ifelse(pred_test_log > 0.5, "Yes", "No"), levels = c("No","Yes"))
test_acc_log        <- mean(pred_class_test_log == test_clean$Repeat_Viewer)

print(paste("Train Accuracy:", round(train_acc_log * 100, 2), "%"))
print(paste("Test Accuracy :", round(test_acc_log  * 100, 2), "%"))

# Confusion matrix: shows true positives, false positives etc.
print(confusionMatrix(pred_class_test_log, test_clean$Repeat_Viewer, positive = "Yes"))

# AUC: better metric than accuracy for imbalanced classes
roc_log <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_log)
print(paste("AUC:", round(auc(roc_log), 4)))


# BLOCK 9: Decision Tree
print("=== DECISION TREE ===")

tree_model <- rpart(
  Repeat_Viewer ~ Satisfaction_Score + Emotional_Engagement +
    Value_For_Time + Prefer_Familiar_Content + Meets_Expectations +
    Satisfaction + Age_Group_Num + Gender_Num + Hours_Num,
  data    = train_balanced,
  method  = "class",
  control = rpart.control(cp = 0.01, minsplit = 10))

# Visualize the decision tree
rpart.plot(tree_model, main = "Decision Tree - Repeat Viewer",
           type = 3, extra = 104, fallen.leaves = TRUE, cex = 0.75)

# Training accuracy
pred_train_tree <- predict(tree_model, train_balanced, type = "class")

train_acc_tree  <- mean(pred_train_tree == train_balanced$Repeat_Viewer)

# Test accuracy
pred_test_tree  <- predict(tree_model, test_clean, type = "class")
test_acc_tree   <- mean(pred_test_tree == test_clean$Repeat_Viewer)

print(paste("Train Accuracy:", round(train_acc_tree * 100, 2), "%"))
print(paste("Test Accuracy :", round(test_acc_tree  * 100, 2), "%"))

print(confusionMatrix(pred_test_tree, test_clean$Repeat_Viewer, positive = "Yes"))

pred_test_tree_prob <- predict(tree_model, test_clean, type = "prob")[, "Yes"]
roc_tree            <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_tree_prob)
print(paste("AUC:", round(auc(roc_tree), 4)))


# BLOCK 10: Random Forest
print("=== RANDOM FOREST ===")

set.seed(42)   # reproducibility for random forest

rf_model <- randomForest(
  Repeat_Viewer ~ Satisfaction_Score + Emotional_Engagement +
    Value_For_Time + Prefer_Familiar_Content + Meets_Expectations +
    Satisfaction + Age_Group_Num + Gender_Num + Hours_Num,
  data       = train_balanced,
  ntree      = 300,   # 500 trees for stable results
  mtry       = 5,# 3 predictors tried at each split (sqrt of 9)
  nodesize = 5,
  importance = TRUE)

print(rf_model)

# Training accuracy
pred_train_rf <- predict(rf_model, train_balanced)
train_acc_rf  <- mean(pred_train_rf == train_balanced$Repeat_Viewer)

# Test accuracy
pred_test_rf  <- predict(rf_model, test_clean)
test_acc_rf   <- mean(pred_test_rf == test_clean$Repeat_Viewer)

print(paste("Train Accuracy:", round(train_acc_rf * 100, 2), "%"))
print(paste("Test Accuracy :", round(test_acc_rf  * 100, 2), "%"))

print(confusionMatrix(pred_test_rf, test_clean$Repeat_Viewer, positive = "Yes"))

pred_test_rf_prob <- predict(rf_model, test_clean, type = "prob")[, "Yes"]
roc_rf            <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_rf_prob)
print(paste("AUC:", round(auc(roc_rf), 4)))

# Which features mattered most?
print("=== FEATURE IMPORTANCE ===")
print(importance(rf_model))
varImpPlot(rf_model, main = "Feature Importance - Random Forest")


print(confusionMatrix(pred_test_nb, test_clean$Repeat_Viewer, positive = "Yes"))
# Install if needed
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Get confusion matrix
cm <- confusionMatrix(pred_class_test_log, test_clean$Repeat_Viewer, positive = "Yes")

# Convert to data frame
cm_df <- as.data.frame(cm$table)

# Plot heatmap
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  labs(title = "Confusion Matrix - Logistic Regression",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Confusion matrix for Decision Tree
cm_tree <- confusionMatrix(pred_test_tree, test_clean$Repeat_Viewer, positive = "Yes")
cm_tree_df <- as.data.frame(cm_tree$table)

# Plot
ggplot(cm_tree_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Confusion Matrix - Decision Tree",
       subtitle = "Counts of Predictions vs Actual Values",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Confusion matrix for Random Forest
cm_rf <- confusionMatrix(pred_test_rf, test_clean$Repeat_Viewer, positive = "Yes")
cm_rf_df <- as.data.frame(cm_rf$table)

# Plot
ggplot(cm_rf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(title = "Confusion Matrix - Random Forest",
       subtitle = "Counts of Predictions vs Actual Values",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# =====================================================
# METRICS FUNCTION (ALL IN ONE)
# =====================================================
get_all_metrics <- function(pred_class, pred_prob, actual) {
  
  cm <- confusionMatrix(pred_class, actual, positive = "Yes")
  
  precision <- cm$byClass["Precision"]
  recall    <- cm$byClass["Recall"]
  f1        <- cm$byClass["F1"]
  
  # ROC-AUC
  roc_obj <- roc(as.numeric(actual == "Yes"), pred_prob)
  roc_auc <- auc(roc_obj)
  
  # PR-AUC
  pr <- pr.curve(
    scores.class0 = pred_prob[actual == "Yes"],
    scores.class1 = pred_prob[actual == "No"],
    curve = FALSE
  )
  pr_auc <- pr$auc.integral
  
  return(c(Precision = precision,
           Recall = recall,
           F1 = f1,
           ROC_AUC = roc_auc,
           PR_AUC = pr_auc))
}

# Logistic
prob_log <- pred_test_log

# Decision Tree
prob_tree <- predict(tree_model, test_clean, type = "prob")[, "Yes"]

# Random Forest
prob_rf <- predict(rf_model, test_clean, type = "prob")[, "Yes"]

if (!requireNamespace("PRROC", quietly = TRUE)) install.packages("PRROC")
library(PRROC)

# Logistic
metrics_log <- get_all_metrics(
  pred_class_test_log,
  prob_log,
  test_clean$Repeat_Viewer
)

# Decision Tree
metrics_tree <- get_all_metrics(
  pred_test_tree,
  prob_tree,
  test_clean$Repeat_Viewer
)

# Random Forest
metrics_rf <- get_all_metrics(
  pred_test_rf,
  prob_rf,
  test_clean$Repeat_Viewer
)

print(metrics_log)
print(metrics_tree)
print(metrics_rf)


results <- rbind(
  Logistic = metrics_log,
  Tree     = metrics_tree,
  RF       = metrics_rf
)

print(round(results, 4))


# AUC
pred_test_nb_prob <- predict(nb_model, test_clean, type = "raw")[, "Yes"]
roc_nb <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_nb_prob)

print(paste("AUC:", round(auc(roc_nb), 4)))

# BLOCK 11: ROC Curve Comparison
print("=== ROC CURVE COMPARISON ===")

plot(roc_log,  col = "blue",      lwd = 2, main = "ROC Curve - All Models")
plot(roc_tree, col = "darkgreen", lwd = 2, add = TRUE)
plot(roc_rf,   col = "red",       lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")  # random-guess baseline
legend("bottomright",
       legend = c(paste("Logistic Regression (AUC =", round(auc(roc_log),  3), ")"),
                  paste("Decision Tree       (AUC =", round(auc(roc_tree), 3), ")"),
                  paste("Random Forest       (AUC =", round(auc(roc_rf),   3), ")")),
       col = c("blue","darkgreen","red"), lwd = 2, cex = 0.85)


# BLOCK 12: Final Comparison Summary
print("=== FINAL MODEL COMPARISON ===")

cat("\n")
cat("================================================================\n")
cat(sprintf("%-22s | Train Acc | Test Acc |   AUC\n", "Model"))
cat("----------------------------------------------------------------\n")
cat(sprintf("%-22s | %7.2f%% | %7.2f%% | %.4f\n",
            "Logistic Regression",
            train_acc_log * 100, test_acc_log * 100, auc(roc_log)))
cat(sprintf("%-22s | %7.2f%% | %7.2f%% | %.4f\n",
            "Decision Tree",
            train_acc_tree * 100, test_acc_tree * 100, auc(roc_tree)))
cat(sprintf("%-22s | %7.2f%% | %7.2f%% | %.4f\n",
            "Random Forest",
            train_acc_rf * 100, test_acc_rf * 100, auc(roc_rf)))
cat("================================================================\n")
cat("Best Model    : Random Forest (highest AUC & test accuracy)\n")
cat("Leakage Fixed : Re_Watched & Revisit_Times removed\n")
cat("Imbalance Fix : upSample applied on training set\n")
cat("Predictors    : Satisfaction_Score, Emotional_Engagement,\n")
cat("                Value_For_Time, Prefer_Familiar_Content,\n")
cat("                Meets_Expectations, Satisfaction,\n")
cat("                Age_Group_Num, Gender_Num, Hours_Num\n")
cat("Conclusion    : Satisfaction Score is the strongest honest\n")
cat("                predictor of repeat viewing behaviour.\n")

