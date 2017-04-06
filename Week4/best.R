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
    outcomeLocation <- match(outcome, possibleOutcomes)
    
    # Validate that the outcome is one of the possible outcomes
    if (is.na(outcomeLocation)) {
        stop("invalid outcome")
    }

    selectedOutcome <- possibleOutcomes[outcomeLocation]

    if (selectedOutcome == "heart attack") {
        lowest <- stateValues[which(as.numeric(stateValues$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == min(as.numeric(stateValues$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE)),]
    } else if (selectedOutcome == "heart failure") {
        lowest <- stateValues[which(as.numeric(stateValues$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == min(as.numeric(stateValues$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE)),]
    } else {
        lowest <- stateValues[which(as.numeric(stateValues$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == min(as.numeric(stateValues$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE)),]
    }

    ordered <- lowest[order(lowest$Hospital.Name),]
    print(ordered[1,]$Hospital.Name)
}