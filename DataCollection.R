open_dataset <- function(path) {
  return(read.csv(path)) #The default values work just fine
}

math_df <- open_dataset("dataset/student_math_clean.csv")
portuguese_df <- open_dataset("dataset/student_portuguese_clean.csv")
both_df <- rbind(math_df, portuguese_df)

important_columns <-
  c(
    "school",
    "sex",
    "age",
    "family_size",
    "parent_status",
    "study_time",
    "internet_access",
    "free_time",
    "social",
    "absences",
    "final_grade"
  )

print(summary(both_df[important_columns]))
