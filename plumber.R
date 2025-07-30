#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#Reading in the data
diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

#Selecting response and 3 chosen explanatory variables
#Converting categorical variables from numeric variables to factor variables
diabetes_select <- diabetes_data |>
  rename("diabetes_resp" = Diabetes_binary) |>
  rename("bmi" = BMI) |>
  rename("phys_activity" = PhysActivity) |>
  rename("age" = Age) |>
  mutate(diabetes_resp = factor(diabetes_resp, levels = c(0, 1), labels = c("No diabetes", "Prediabetes or diabetes"))) |>
  mutate(phys_activity = factor(phys_activity, levels = c(0, 1), labels = c("No physical activity", "Physical activity"))) |>
  mutate(age = factor(age, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), labels = c("Age 18-24", "Age 25-29", "Age 30-34", "Age 35-39", "Age 40-44", "Age 45-49", "Age 50-54", "Age 55-59", "Age 60-64", "Age 65-69", "Age 70-74", "Age 75-79", "Age 80 or older"))) |>
  mutate(HvyAlcoholConsump = as.factor(HvyAlcoholConsump)) |>
  mutate(AnyHealthcare = as.factor(AnyHealthcare))

#Fitting the model using logistic regression
LR1_fit <- glm(diabetes_resp ~ phys_activity + bmi + age + HvyAlcoholConsump + AnyHealthcare, data = diabetes_select, family = binomial)

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Creating predictions using the best model
#* @param bmi BMI (range from 0 to 100)
#* @param phys_activity Physical activity in the last 30 days (either 1 = yes or 0 = no)
#* @param age age group (range from 1 to 13)
#* @param HvyAlcoholConsump heavy alcohol consumption (either 1 = yes or 0 = no)
#* @param AnyHealthcare any healthcare coverage (either 1 = yes or 0 = no)
#* @get /pred
function(bmi, phys_activity, age, HvyAlcoholConsump, AnyHealthcare) {
    predictor_data <- tibble(as.numeric(bmi), 
                             as.factor(phys_activity), 
                             as.factor(age), 
                             as.factor(HvyAlcoholConsump),
                             as.factor(AnyHealthcare))
    
    model_prediction <- predict(LR1_fit, predictor_data)
    
    list(model_prediction = as.character(model_prediction))
}

#* Displaying author and GitHub information
#* @get /info
function() {
    "Makenna Meyer, GitHub URL: https://mkmeyer.github.io/ST558-Project-3/EDA.html"
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
