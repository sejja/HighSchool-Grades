open_dataset <- function(path) {
  return (read.csv(path))
}

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

prepare_data <- function(path) {
  surveillance <- open_dataset(path)
  print("Data Set Read Succesfully!")
  print("Convert into appropiate types")
  surveillance <- convert_types(surveillance)
  print("Swap rows for tydiness")
  surveillance <- swap_rows(surveillance)
  return (surveillance)
}
