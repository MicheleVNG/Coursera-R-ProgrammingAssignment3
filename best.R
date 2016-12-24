## Task 1 - Finding the best hospital in a state
## This function takes "state" and "outcome" as input, and outputs the
## best Hospital in the given state for a certain condition (heart attack, 
## heart failure, or pneumonia).

best <- function(state, outcome) {
	# Read outcome data
	data <- read.csv(file.path("data","outcome-of-care-measures.csv"),
			 stringsAsFactors = FALSE,
			 na.strings = "Not Available")
	
	# Check that state and outcome are valid
	if(sum(data$State == state) == 0) {
		stop("invalid state")
	}
	if(outcome != "heart attack"  &&
	   outcome != "heart failure" &&
	   outcome != "pneumonia") {
		stop("invalid outcome")
	}
	
	# Set the right index based on outcome
	if(outcome == "heart attack") column_index <- 11
	if(outcome == "heart failure") column_index <- 17
	if(outcome == "pneumonia") column_index <- 23
	
	relevant_columns <- c(2, 7, column_index)
	filtered_data <- data[, relevant_columns]
	names(filtered_data) <- c("Hospital.Name", "State", "Outcome")
	
	# Select only rows that match the state and are valid
	filtered_data <- filtered_data[filtered_data$State == state, ]
	filtered_data <- filtered_data[complete.cases(filtered_data), ]
	
	# Order by outcome and hospital name
	filtered_data <- filtered_data[ order(filtered_data$Outcome,
					      filtered_data$Hospital.Name), ]
	
	best <- filtered_data[1, "Hospital.Name"]
	best
}