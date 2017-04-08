rankhospital <- function(state, outcome, num = "best") {
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
    subbed[,columnName] <- as.numeric(subbed[,columnName])

    # Filter out any NA values and order then based on the filter column and hospital name
    filtered <- subbed[!is.na(subbed[, columnName]),]
    ordered <- filtered[order(filtered[,columnName], filtered[,"Hospital.Name"]),]

    # Check that if the num parameter is a numeric value
    # and that it isn't greater than the number of hostpitals
    if (is.numeric(num)) {
        if (num > nrow(ordered)) {
            return(NA)
        } else {
            return(ordered[num, ]$Hospital.Name)
        }
    } else {
        if (num == "best") {
            return(ordered[1,]$Hospital.Name)
        } else if (num == "worst") {
            return(ordered[nrow(ordered),]$Hospital.Name)
        } else {
            stop("invalid num")
        }
    }
}