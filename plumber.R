#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

# Reading in the data
#diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# Fitting the best model


#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Creating predictions using the best model
#* @param msg The message to echo
#* @get /pred
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Displaying author and GitHub information
#* @serializer html
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
