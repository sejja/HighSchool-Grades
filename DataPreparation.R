open_dataset <- function(path) {
  return (read.csv(path))
}

convert_types <- function(dataset) {
  logical_fields <- c("school_support", "family_support", "extra_paid_classes", "activities", "nursery_school", "higher_ed", "internet_access", "romantic_relationship")
  factor_fields <- c("school", "sex", "address_type", "family_size", "parent_status", "mother_education", "father_education", "mother_job", "father_job", "school_choice_reason", "guardian", "travel_time", "study_time")
  
  print("Converting boolean like types into logicals")
  
  for(field in logical_fields)
    dataset[[field]] <- ifelse(dataset[[field]] == "yes", T, F)
  
  print("Converting pure string fields to factors when applicable")
  
  for(field in factor_fields)
    dataset[[field]] <- as.factor(dataset[[field]])

  return (dataset)
}

swap_rows <- function(dataset) {
  names <- names(dataset)
  names[16] <- "romantic_relationship"
  names[24] <- "class_failures"
  dataset[, names]
  return (dataset)
}

prepare_data <- function(path) {
  surveillance <- open_dataset(path)
  print("Data Set Read Succesfully!")
  print("Convert into appropiate types")
  surveillance <- convert_types(surveillance)
  print("Swap rows for tydiness")
  surveillance <- swap_rows(surveillance)
  return (surveillance)
}
