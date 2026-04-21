library( tidyverse )


rf_class <- read_csv("rf_class_predictions.csv")

rf_ranger <- read_csv("final_rangder_random_forest_prediction.csv")

log_step_risk <- read_csv("log_step_risk_predictions.csv")

rf_class = rf_class %>% 
  select(student_id,predicted_bully_high)

dim(rf_class)
dim(rf_ranger)
dim(log_step_risk)

#merge (by student_id) rf_class, rf_ranger, and log_step_risk into one dataset 
#called student_test_data.csv 
lambda_student_predictions <- rf_class %>% 
  left_join(rf_ranger, by = "student_id") %>% 
  left_join(log_step_risk, by = "student_id")

dim(lambda_student_predictions)

write.csv(lambda_student_predictions, "lambda_student_predictions.csv", 
          row.names = FALSE)

source( "check_predictions_function.R" )
check_prediction_file_format( "lambda_student_predictions.csv" )


