#Installing Packages
install.packages("readxl") 

# Step 1: Load Libraries
library(readxl)   # To read Excel files
library(dplyr)    # For data manipulation
library(tidyr)    # For data cleaning

cat("Step 1 Completed: Libraries loaded successfully\n")


# Step 2: Load the Dataset
data <- read_excel("dataset/Audience Satisfaction and Repeat Viewing  (Responses).xlsx", 
                   sheet = "Form Responses 1")

cat("Step 2 Completed: Dataset loaded successfully\n")
cat("Rows:", nrow(data), " | Columns:", ncol(data), "\n")


# Step 3: Inspect the Data
print(head(data, 3))      
print(str(data))


# Step 4: Clean Column Names
colnames(data) <- make.names(colnames(data), unique = TRUE)

cat("Step 4 Completed: Column names cleaned\n")
print(colnames(data))


# Step 5: Rename Columns with Short & Clear Names
data <- data %>%
  rename(
    Age_Group = What.is.your.age.group.,
    Gender = Select.Your.Gender,
    Preferred_Media = Which.type.of.media.do.you.prefer.most.for.entertainment.,
    Hours_Per_Week = How.many.hours.per.week.do.you.spend.on.entertainment.content.,
    Satisfaction = I.am.generally.satisfied.with.the.entertainment.content.I.consume.,
    Meets_Expectations = The.content.I.consume.meets.my.expectations.,
    Emotional_Engagement = I.feel.emotionally.engaged.with.the.content.I.watch.or.listen.to.,
    Value_For_Time = The.entertainment.content.provides.good.value.for.my.time.,
    Re_Watched = Have.you.re.watched.or.re.listened.to.any.entertainment.content.that.you.enjoyed.,
    Revisit_Times = How.many.times.do.you.typically.revisit.the.same.entertainment.content.within.a.few.months.,
    Prefer_Familiar_Content = I.prefer.re.experiencing.familiar.content.rather.than.trying.new.content.,
    Usual_Content_Type = What.type.of.content.do.you.usually.re.watch.or.re.listen.to.
  )

cat("Step 5 Completed: Columns renamed for easy analysis\n")


# Step 6: Convert Data Types
# Convert Likert scale questions (1-5) to numeric
likert_vars <- c("Satisfaction", "Meets_Expectations", "Emotional_Engagement", 
                 "Value_For_Time", "Prefer_Familiar_Content")
data[likert_vars] <- lapply(data[likert_vars], as.numeric)

# Convert categorical variables to factors
data$Age_Group <- factor(data$Age_Group)
data$Gender <- factor(data$Gender)
data$Re_Watched <- factor(data$Re_Watched, levels = c("Yes", "No"))

cat("Step 6 Completed: Data types converted\n")


# Step 7: Handle Missing / Invalid Values
data$Usual_Content_Type[is.na(data$Usual_Content_Type) | 
                    data$Usual_Content_Type %in% c("N", "", "I dont...")] <- "Not Specified"

cat("Step 7 Completed: Missing values handled\n")


# Step 8: Check Age Distribution (Related to Quota Sampling)
cat("\n--- Age Distribution (Quota Sampling Reference) ---\n")
age_dist <- data %>%
  group_by(Age_Group) %>%
  summarise(Count = n(), 
            Percentage = round(n()/nrow(data)*100, 1))

print(age_dist)

# Add note about sampling method
data <- data %>%
  mutate(Sampling_Method = "Quota Sampling (Age Group Proportion)")

cat("Step 8 Completed: Age distribution checked for quota sampling\n")


# Step 9: Create Derived Variables
data <- data %>%
  mutate(
    Satisfaction_Score = rowMeans(select(., Satisfaction, Meets_Expectations, 
                                         Emotional_Engagement, Value_For_Time), 
                                  na.rm = TRUE),
    
    Repeat_Viewer = ifelse(Revisit_Times %in% c("Once", "2-3 times", "More than 3 times"), 
                           "Yes", "No"),
    Repeat_Viewer = factor(Repeat_Viewer)
  )

cat("Step 9 Completed: Derived variables created\n")


# Step 10: Remove Unwanted Columns
data <- data %>%
  select(-Timestamp, -Sampling_Method)

cat(" Step 10 Completed: Timestamp and Sampling_Method columns removed\n")
cat("Remaining Columns:", ncol(data), "\n")


#Step 11
#Age category labels are having - (dash). Therefore below step is used to clean
#the group labels

data <- data %>%
  mutate(Age_Group = case_when(
    grepl("Teen", Age_Group, ignore.case = TRUE) ~ "Teen (13-19 years)",
    grepl("Young Adult", Age_Group, ignore.case = TRUE) ~ "Young Adult (20-29 years)",
    grepl("Adult \\(30", Age_Group, ignore.case = TRUE) ~ "Adult (30-44 years)",
    grepl("Middle", Age_Group, ignore.case = TRUE) ~ "Middle-Aged Adult (45-59 years)",
    grepl("Senior", Age_Group, ignore.case = TRUE) ~ "Senior (60 years and above)",
    TRUE ~ as.character(Age_Group)
  ))

cat("✅ Age_Group labels cleaned successfully\n")
print(unique(data$Age_Group))   



# Step 12: Save Cleaned Dataset with Proper Encoding
write.csv(data, "dataset/cleaned_audience_data.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8") 

cat("\nCleaned dataset saved successfully with UTF-8 encoding!\n")