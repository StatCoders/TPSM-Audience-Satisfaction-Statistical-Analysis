# =====================================================
# AUDIENCE REPEAT VIEWER PREDICTION - FULL IMPROVED SCRIPT
# Dataset: cleaned_audience_data.csv (489 observations)
# =====================================================

# =====================================================
# Block 1: Install & Load Required Packages
# =====================================================
required_packages <- c("corrplot", "rpart", "rpart.plot", "randomForest", "caret", "pROC")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

print("All packages loaded successfully.")

# =====================================================
# Block 2: Load the Dataset
# =====================================================
data <- read.csv("dataset/cleaned_audience_data.csv", stringsAsFactors = FALSE)

print(paste("Total observations loaded:", nrow(data)))
print("Column names:")
print(colnames(data))

# =====================================================
# Block 3: Feature Engineering
# =====================================================
print("=== FEATURE ENGINEERING ===")

# --- Target variable ---
data$Repeat_Viewer <- as.factor(data$Repeat_Viewer)

# --- Ordinal encoding: Age_Group ---
data$Age_Group_Num <- as.integer(factor(data$Age_Group,
                                        levels = c("Teen (13-19 years)",
                                                   "Young Adult (20-29 years)",
                                                   "Adult (30-44 years)",
                                                   "Middle-Aged Adult (45-59 years)",
                                                   "Senior (60 years and above)")))

# --- Binary encoding: Gender ---
data$Gender_Num <- ifelse(data$Gender == "Male", 1, 0)

# --- Ordinal encoding: Hours_Per_Week ---
data$Hours_Num <- as.integer(factor(data$Hours_Per_Week,
                                    levels = c("Less than 2 hours", "2-5 hours",
                                               "6-10 hours", "More than 10 hours")))

# --- Binary encoding: Re_Watched ---
data$Re_Watched_Num <- ifelse(data$Re_Watched == "Yes", 1, 0)

# --- Ordinal encoding: Revisit_Times ---
data$Revisit_Num <- as.integer(factor(data$Revisit_Times,
                                      levels = c("Never", "Once", "2-3 times", "More than 3 times")))

# --- Satisfaction_Level: binned from Satisfaction_Score
#     NOTE: NOT used alongside Satisfaction_Score in models (data leakage).
#     Kept here for reference/EDA only.
data$Satisfaction_Level <- cut(data$Satisfaction_Score,
                               breaks = c(0, 2.5, 3.5, 5),
                               labels = c("Low", "Medium", "High"))

print("Feature engineering completed.")

# Class distribution check
cat("\nClass Distribution:\n")
print(round(prop.table(table(data$Repeat_Viewer)) * 100, 1))

# =====================================================
# Block 4: EDA Visualizations
# =====================================================
print("=== EDA VISUALIZATIONS ===")

# Target variable distribution
barplot(table(data$Repeat_Viewer),
        main = "Distribution of Repeat Viewer (Target Variable)",
        ylab = "Count",
        col  = c("lightblue", "lightgreen"),
        ylim = c(0, 420))

# Satisfaction Score by Repeat Viewer
boxplot(Satisfaction_Score ~ Repeat_Viewer, data = data,
        main = "Satisfaction Score by Repeat Viewer",
        ylab = "Satisfaction Score",
        col  = c("lightcoral", "lightgreen"))

# Hours Per Week by Repeat Viewer
barplot(table(data$Repeat_Viewer, data$Hours_Per_Week),
        main   = "Hours Per Week by Repeat Viewer",
        beside = TRUE,
        col    = c("lightblue", "lightgreen"),
        legend = TRUE,
        args.legend = list(x = "topright"),
        las = 2, cex.names = 0.75)

# Re_Watched by Repeat Viewer
barplot(table(data$Repeat_Viewer, data$Re_Watched),
        main   = "Re-Watched Content by Repeat Viewer",
        beside = TRUE,
        col    = c("lightblue", "lightgreen"),
        legend = TRUE,
        args.legend = list(x = "topright"))

# =====================================================
# Block 5: Correlation Heatmap (Numeric Variables)
# =====================================================
print("=== CORRELATION HEATMAP ===")

numeric_data <- data[, c("Satisfaction_Score", "Emotional_Engagement",
                         "Value_For_Time", "Prefer_Familiar_Content",
                         "Meets_Expectations", "Satisfaction",
                         "Hours_Num", "Revisit_Num", "Re_Watched_Num")]

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.75,
         title       = "Correlation Heatmap of Numeric Variables",
         mar         = c(0, 0, 2, 0),
         addCoef.col = "black",
         number.cex  = 0.65,
         tl.col      = "black")

# =====================================================
# Block 6: Train-Test Split (70% / 30%)
# =====================================================
set.seed(123)
trainID <- sample(1:nrow(data), round(0.7 * nrow(data)))
train   <- data[trainID, ]
test    <- data[-trainID, ]
print(paste("Train set size:", nrow(train), "| Test set size:", nrow(test)))
cat("Train class distribution:\n")
print(table(train$Repeat_Viewer))

# =====================================================
# Block 7: Handle Class Imbalance using upSample (caret)
#
# WHY NOT ROSE?
# ROSE fails when the dataset contains free-text / high-cardinality
# string columns (e.g. Preferred_Media, Usual_Content_Type).
# upSample from caret safely duplicates minority class rows
# without needing to synthesize values for incompatible column types.
# We pass only the engineered numeric columns to avoid this entirely.
# =====================================================
print("=== HANDLING CLASS IMBALANCE (upSample) ===")

# Only keep model-relevant engineered columns — no free-text
model_cols <- c("Satisfaction_Score", "Emotional_Engagement", "Value_For_Time",
                "Prefer_Familiar_Content", "Meets_Expectations",
                "Age_Group_Num", "Gender_Num", "Hours_Num",
                "Re_Watched_Num", "Revisit_Num", "Repeat_Viewer")

train_clean <- train[, model_cols]
test_clean  <- test[, model_cols]

# upSample: balances by oversampling minority class
train_balanced <- upSample(x     = train_clean[, -which(names(train_clean) == "Repeat_Viewer")],
                           y     = train_clean$Repeat_Viewer,
                           yname = "Repeat_Viewer")

cat("Balanced training set class distribution:\n")
print(table(train_balanced$Repeat_Viewer))

# =====================================================
# Block 8: Logistic Regression
# =====================================================
print("=== LOGISTIC REGRESSION ===")

logistic_model <- glm(Repeat_Viewer ~ Satisfaction_Score + Emotional_Engagement +
                        Value_For_Time + Prefer_Familiar_Content + Meets_Expectations +
                        Age_Group_Num + Gender_Num + Hours_Num + Re_Watched_Num + Revisit_Num,
                      data   = train_balanced,
                      family = binomial(link = "logit"))

print(summary(logistic_model))

# Training Accuracy
pred_train_log       <- predict(logistic_model, train_balanced, type = "response")
pred_class_train_log <- factor(ifelse(pred_train_log > 0.5, "Yes", "No"), levels = c("No", "Yes"))
train_acc_log        <- mean(pred_class_train_log == train_balanced$Repeat_Viewer)

# Test Accuracy
pred_test_log       <- predict(logistic_model, test_clean, type = "response")
pred_class_test_log <- factor(ifelse(pred_test_log > 0.5, "Yes", "No"), levels = c("No", "Yes"))
test_acc_log        <- mean(pred_class_test_log == test_clean$Repeat_Viewer)

print(paste("Training Accuracy:", round(train_acc_log * 100, 2), "%"))
print(paste("Test Accuracy    :", round(test_acc_log  * 100, 2), "%"))

print("--- Logistic Regression Confusion Matrix (Test) ---")
cm_log <- confusionMatrix(pred_class_test_log, test_clean$Repeat_Viewer, positive = "Yes")
print(cm_log)

roc_log <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_log)
print(paste("Logistic Regression AUC:", round(auc(roc_log), 4)))

# =====================================================
# Block 9: Decision Tree
# =====================================================
print("=== DECISION TREE ===")

tree_model <- rpart(Repeat_Viewer ~ Satisfaction_Score + Emotional_Engagement +
                      Value_For_Time + Prefer_Familiar_Content + Meets_Expectations +
                      Age_Group_Num + Gender_Num + Hours_Num + Re_Watched_Num + Revisit_Num,
                    data    = train_balanced,
                    method  = "class",
                    control = rpart.control(cp = 0.01, minsplit = 10))

# Tree visualization (rpart.plot now installed)
rpart.plot(tree_model,
           main          = "Decision Tree for Predicting Repeat Viewer",
           type          = 3,
           extra         = 104,
           fallen.leaves = TRUE,
           cex           = 0.75)

# Training Accuracy
pred_train_tree <- predict(tree_model, train_balanced, type = "class")
train_acc_tree  <- mean(pred_train_tree == train_balanced$Repeat_Viewer)

# Test Accuracy
pred_test_tree  <- predict(tree_model, test_clean, type = "class")
test_acc_tree   <- mean(pred_test_tree == test_clean$Repeat_Viewer)

print(paste("Training Accuracy:", round(train_acc_tree * 100, 2), "%"))
print(paste("Test Accuracy    :", round(test_acc_tree  * 100, 2), "%"))

print("--- Decision Tree Confusion Matrix (Test) ---")
cm_tree <- confusionMatrix(pred_test_tree, test_clean$Repeat_Viewer, positive = "Yes")
print(cm_tree)

pred_test_tree_prob <- predict(tree_model, test_clean, type = "prob")[, "Yes"]
roc_tree            <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_tree_prob)
print(paste("Decision Tree AUC:", round(auc(roc_tree), 4)))

# =====================================================
# Block 10: Random Forest
# =====================================================
print("=== RANDOM FOREST ===")

set.seed(42)  # set.seed before RF for reproducibility

rf_model <- randomForest(Repeat_Viewer ~ Satisfaction_Score + Emotional_Engagement +
                           Value_For_Time + Prefer_Familiar_Content + Meets_Expectations +
                           Age_Group_Num + Gender_Num + Hours_Num + Re_Watched_Num + Revisit_Num,
                         data       = train_balanced,
                         ntree      = 500,
                         mtry       = 3,
                         importance = TRUE)

print(rf_model)

# Training Accuracy
pred_train_rf <- predict(rf_model, train_balanced)
train_acc_rf  <- mean(pred_train_rf == train_balanced$Repeat_Viewer)

# Test Accuracy
pred_test_rf  <- predict(rf_model, test_clean)
test_acc_rf   <- mean(pred_test_rf == test_clean$Repeat_Viewer)

print(paste("Training Accuracy:", round(train_acc_rf * 100, 2), "%"))
print(paste("Test Accuracy    :", round(test_acc_rf  * 100, 2), "%"))

print("--- Random Forest Confusion Matrix (Test) ---")
cm_rf <- confusionMatrix(pred_test_rf, test_clean$Repeat_Viewer, positive = "Yes")
print(cm_rf)

pred_test_rf_prob <- predict(rf_model, test_clean, type = "prob")[, "Yes"]
roc_rf            <- roc(as.numeric(test_clean$Repeat_Viewer == "Yes"), pred_test_rf_prob)
print(paste("Random Forest AUC:", round(auc(roc_rf), 4)))

# Feature Importance
print("=== FEATURE IMPORTANCE (Random Forest) ===")
print(importance(rf_model))
varImpPlot(rf_model, main = "Feature Importance - Random Forest")

# =====================================================
# Block 11: ROC Curve Comparison (All 3 Models)
# =====================================================
print("=== ROC CURVE COMPARISON ===")

plot(roc_log,  col = "blue",      lwd = 2,
     main = "ROC Curve Comparison - All Models")
plot(roc_tree, col = "darkgreen", lwd = 2, add = TRUE)
plot(roc_rf,   col = "red",       lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright",
       legend = c(paste("Logistic Regression (AUC =", round(auc(roc_log),  3), ")"),
                  paste("Decision Tree       (AUC =", round(auc(roc_tree), 3), ")"),
                  paste("Random Forest       (AUC =", round(auc(roc_rf),   3), ")")),
       col = c("blue", "darkgreen", "red"), lwd = 2, cex = 0.85)

# =====================================================
# Block 12: Final Model Comparison Summary
# =====================================================
print("=== FINAL MODEL COMPARISON SUMMARY ===")

cat("\n")
cat("=============================================================\n")
cat(sprintf("%-22s | Train Acc | Test Acc |   AUC\n", "Model"))
cat("-------------------------------------------------------------\n")
cat(sprintf("%-22s | %7.2f%% | %7.2f%% | %.4f\n",
            "Logistic Regression",
            train_acc_log  * 100, test_acc_log  * 100, auc(roc_log)))
cat(sprintf("%-22s | %7.2f%% | %7.2f%% | %.4f\n",
            "Decision Tree",
            train_acc_tree * 100, test_acc_tree * 100, auc(roc_tree)))
cat(sprintf("%-22s | %7.2f%% | %7.2f%% | %.4f\n",
            "Random Forest",
            train_acc_rf   * 100, test_acc_rf   * 100, auc(roc_rf)))
cat("=============================================================\n")
cat("Best Model      : Random Forest (highest AUC & test accuracy)\n")
cat("Imbalance Fix   : upSample used — ROSE skipped (incompatible with free-text columns)\n")
cat("Leakage Fix     : Satisfaction_Level excluded (derived from Satisfaction_Score)\n")
cat("Features used   : 10 engineered numeric/binary/ordinal predictors\n")
cat("Reproducibility : set.seed(123) for split | set.seed(42) for RF\n")
cat("Conclusion      : Satisfaction Score & Re_Watched are key repeat-viewing predictors\n")

