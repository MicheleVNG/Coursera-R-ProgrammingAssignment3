## Task 3 - Ranking hospitals in all states
## This function takes "output" and a number (or "best" / "worst") as input,
## and outputs a data frame with the hospital names of the hospitals that
## have rank = num in the respective states.

rankall <- function(outcome, num = "best") {
	# Read outcome data
	data <- read.csv(file.path("data","outcome-of-care-measures.csv"),
			 stringsAsFactors = FALSE,
			 na.strings = "Not Available")
	
	# Check that outcome and num are valid
	if(outcome != "heart attack"  &&
	   outcome != "heart failure" &&
	   outcome != "pneumonia") {
		stop("invalid outcome")
	}
	if(num != "best" &&
	   num != "worst" &&
	   !is.numeric(num)) {
		stop("invalid value for 'num'")
	}
	
	# Set the right index based on outcome
	if(outcome == "heart attack") column_index <- 11
	else if(outcome == "heart failure") column_index <- 17
	else if(outcome == "pneumonia") column_index <- 23
	
	relevant_columns <- c(2, 7, column_index)
	filtered_data <- data[, relevant_columns]
	names(filtered_data) <- c("Hospital.Name", "State", "Outcome")
	
	# Order by state, then outcome, then hospital name
	filtered_data <- filtered_data[ order(filtered_data$State,
					      filtered_data$Outcome,
					      filtered_data$Hospital.Name), ]
	
	# Group by state
	state_rankings <- split(filtered_data[, c(1, 3)], filtered_data[, 2])
	
	# Extract a list of the hospitals matching "num"
	matching <- lapply(state_rankings,
			   function(x) {
			   	if(is.numeric(num)) {
			   		return(x[num, 1])
			   	} else if(num == "best") {
			   		return(x[1, 1])
			   	} else if(num == "worst") {
			   		valid <- x[complete.cases(x), ]
			   		return(valid[nrow(valid), 1])
			   	}
			   })
	
	# Build the data frame
	matching.v <- unlist(matching)	# transform "matching" in a vector
	df <- data.frame(hospital = matching.v,
			 state    = names(matching.v))
	df
}