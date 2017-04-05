best <- function(state, outcome) {
    # Setup the possible outcomes
    possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")

    # Read in the outcome of care measures file
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Get all of the state values
    stateValues <- outcomeData[outcomeData$State == state,]

    # Validate that the state exists in the dataset
    if (nrow(stateValues) == 0) {
        stop("invalid state")
    }

    # Get which outcome is being selected
    selectedOutcome <- possibleOutcomes[outcome]

    # Validate that the outcome is one of the possible outcomes
    print(selectedOutcome)
}