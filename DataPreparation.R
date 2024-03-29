source("DataCollection.R")

# Converts dataset values into R native values
convert_types <- function(dataset) {
  print("Convert into appropiate types")
  logical_fields <- c("school_support", "family_support", "extra_paid_classes", "activities", "nursery_school", "higher_ed", "internet_access", "romantic_relationship")
  unordered_factor_fields <- c("school", "sex", "address_type", "parent_status", "mother_job", "father_job", "school_choice_reason", "guardian")
  
  print("Converting boolean like types into logicals")
  dataset[logical_fields] <- lapply(dataset[logical_fields], function(x) x == "yes")
  
  
  print("Converting pure string fields to factors when applicable")
  dataset[unordered_factor_fields] <- lapply(dataset[unordered_factor_fields], as.factor)
  dataset$family_size <- ordered(dataset$family_size, levels = c("Greater than 3", "Less than or equal to 3"))
  
  education_levels <- c("higher education", "secondary education", "5th to 9th grade", "primary education (4th grade)", "none")
  dataset$mother_education <- ordered(dataset$mother_education, levels = education_levels)
  dataset$father_education <- ordered(dataset$father_education, levels = education_levels)
  
  dataset$travel_time <- ordered(dataset$travel_time, levels = c(">1 hour", "30 min. to 1 hour", "15 to 30 min.", "<15 min."))
  dataset$study_time <- ordered(dataset$study_time, levels = c(">10 hours", "5 to 10 hours", "2 to 5 hours", "<2 hours"))

  return (dataset)
}

# Swaps rows to order by data type
swap_rows <- function(dataset) {
  print("Swap rows for tydiness")
  names <- names(dataset)
  names[16] <- "romantic_relationship"
  names[24] <- "class_failures"
  dataset[, names]
  return (dataset)
}

#Retrieves the outliers (uses Boxplot_outliers function)
find_outliers <- function(dataset){
  print("Searching for outliers")
  important_numeric_fields <- c("age", "absences", "class_failures", "weekday_alcohol", "weekend_alcohol", "health", "grade_1", "grade_2", "final_grade")
  outliers_list <- lapply(dataset[important_numeric_fields], boxplot_outliers)
  return(dataset)
}

#Uses Boxplot to return a list with the outliers
boxplot_outliers <- function(column) {
  outliers <- boxplot(column, plot = FALSE)$out
  names(outliers) <- names(column)
  return(outliers)
}

#Adds 2% NA values to the data set in random positions in column health
generate_na_values <- function(dataset) {
  print("Adding artificial NA values")
  set.seed(0)
  na_index <- sample(nrow(dataset), 0.02 * nrow(dataset))
  dataset[na_index, "health"] <- NA
  return(dataset)
}

#Replace NA values with the mean of the column
na_imputation <- function(dataset){
  print("Filling NA values")
  dataset$health[is.na(dataset$health)] <- round(mean(dataset$health, na.rm = TRUE))
  return(dataset)
}

# Adds normalized_health column to the dataset (range [0,1])
normal_column <- function(dataset){
  print("Adding normalized health column")
  dataset$normalized_health <- (dataset$health - min(dataset$health))/(max(dataset$health)-min(dataset$health))
  return(dataset)
}

# Adds week_alcohol column to the dataset (average between weekend and weekday alcohol)
combined_column <- function(dataset){
  print("Adding combined alcohol column")
  dataset$week_alcohol <- (dataset$weekday_alcohol + dataset$weekend_alcohol)/2
  return(dataset)
}

get_grade_letter <- function(percentage) {
  if (percentage >= 90) {
    return("A")
  } else if (percentage >= 80) {
    return("B")
  } else if (percentage >= 70) {
    return("C")
  } else if (percentage >= 60) {
    return("D")
  } else {
    return("F")
  }
}

categorical_column <- function(dataset){
  print("Adding categorical column")
  dataset$final_grade_letter <- lapply(dataset$final_grade * 100/20, get_grade_letter)
  dataset$final_grade_letter <- factor(dataset$final_grade_letter, levels = c("A", "B", "C", "D", "F"))
  return(dataset)
}

# Removes unused column
remove_excess_rows <- function(dataset) {
  print("Removing uneeded rows")
  dataset$student_id <- NULL
  return(dataset)
}

prepare_data <- function(df) {
  df <- remove_excess_rows(df)
  df <- convert_types(df)
  df <- swap_rows(df)
  df <- find_outliers(df)
  df <- generate_na_values(df)
  df <- na_imputation(df)
  df <- normal_column(df)
  df <- combined_column(df)
  df <- categorical_column(df)
  return (df)
}

print("Math dataset")
math_df <- prepare_data(math_df)
print("Portuguese dataset")
portuguese_df <- prepare_data(portuguese_df)
print("Both datasets")
both_df <- prepare_data(both_df)

save.image("datasets.RData")