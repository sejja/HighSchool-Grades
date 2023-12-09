convert_types <- function(dataset) {
  logical_fields <- c("school_support", "family_support", "extra_paid_classes", "activities", "nursery_school", "higher_ed", "internet_access", "romantic_relationship")
  factor_fields <- c("school", "sex", "address_type", "family_size", "parent_status", "mother_education", "father_education", "mother_job", "father_job", "school_choice_reason", "guardian", "travel_time", "study_time")
  
  print("Converting boolean like types into logicals")
  dataset[logical_fields] <- lapply(dataset[logical_fields], function(x) x == "yes")
  
  
  print("Converting pure string fields to factors when applicable")
  dataset[factor_fields] <- lapply(dataset[factor_fields], as.factor)

  return (dataset)
}

swap_rows <- function(dataset) {
  names <- names(dataset)
  names[16] <- "romantic_relationship"
  names[24] <- "class_failures"
  dataset[, names]
  return (dataset)
}

find_outliers <- function(dataset){
  important_numeric_fields <- c("absences","age","weekday_alcohol","weekend_alcohol","health")
  outliers_list <- lapply(dataset[important_numeric_fields], boxplot_outliers)
  # Hay que ver cuales quitamos y cuales dejamos --------------------------------------------------------------------------------
}

boxplot_outliers <- function(column) {
  outliers <- boxplot(column, plot = FALSE)$out
  names(outliers) <- names(column)
  return(outliers)
}

normal_column <- function(dataset){
  dataset <- cbind(dataset, normalized_health = ((dataset$health - min(dataset$health))/(max(dataset$health)-min(dataset$health))))
}

combined_column <- function(dataset){
  dataset <- cbind(dataset, week_alcohol = (dataset$weekday_alcohol + dataset$weekend_alcohol)/2)
}

remove_excess_rows <- function(dataset) {
  dataset$student_id <- NULL
}

prepare_data <- function(df) {
  print("Data Set Read Succesfully!")
  print("Removing uneeded rows")
  remove_excess_rows(df)
  print("Convert into appropiate types")
  df <- convert_types(df)
  print("Swap rows for tydiness")
  df <- swap_rows(df)
  print("Searching outliers")
  find_outliers(df)
  print("Adding normalized health column")
  normal_column(df)
  combined_column(df)
  return (df)
}
