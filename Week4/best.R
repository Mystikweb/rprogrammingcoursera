best <- function(state, outcome) {
    # Read in the outcome of care measures file
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Setup the possible outcomes and the base columns required for testing the data
    possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
    baseColumns <- c("State", "Hospital.Name")

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

    # Get the selected outcome
    selectedOutcome <- possibleOutcomes[outcomeLocation]

    # Determine the column that is to be tested on
    if (selectedOutcome == "heart attack") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (selectedOutcome == "heart failure") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }

    # Get a subset of the required columns for testing 
    # and convert the test column to numeric
    baseColumns <- c(baseColumns, columnName)
    subbed <- stateValues[, baseColumns]
    subbed[, columnName] <- as.numeric(subbed[, columnName])

    # Get the min value of the subsets filter column
    # and order it by the hostpital name
    lowest <- subbed[which(subbed[, columnName] == min(subbed[, columnName], na.rm = TRUE)),]
    ordered <- lowest[order(lowest$Hospital.Name),]

    return(ordered[1,]$Hospital.Name)
}