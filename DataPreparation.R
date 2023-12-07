open_dataset <- function(path) {
  return (read.csv(path))
}

convert_types <- function(dataset) {
  logical_fields <- c("school_support", "family_support", "extra_paid_classes", "activities", "nursery_school", "higher_ed", "internet_access", "romantic_relationship")
  numeric_fields <- c("family_relationship", "free_time", "social", "weekday_alcochol", "weekend_alcohol", "health", "abstences", "grade_1", "grade_2", "final_grade")
  factor_fields <- c("school", "sex", "address_type", "family_size", "parent_status", "mother_education", "father_education", "mother_job", "father_job", "school_choice_reason", "guardian", "travel_time", "study_time")
  
  print("Converting boolean like types into logicals")
  
  for(field in logical_fields)
    dataset[[field]] <- ifelse(dataset[[field]] == "yes", T, F)
  
  print("Converting pure string fields to factors when applicable")
  
  for(field in factor_fields)
    dataset[[field]] <- as.factor(dataset[[field]])
}

prepare_data <- function(path) {
  surveillance <- open_dataset(path)
  print("Data Set Read Succesfully!")
  print("Convertin tinto appropiate types")
  convert_types(surveillance) 
  return (surveillance)
}