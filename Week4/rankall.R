rankall <- function(outcome, num = "best") {
    # Read in the outcome of care measures file
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Get a unique vector of all states in the filtered data and sort them
    stateList <- unique(outcomeData$State)
    stateList <- sort(stateList)

    # Setup the possible outcomes and the base columns required for testing the data
    possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
    possibleCharNums <- c("best", "worst")
    baseColumns <- c("Hospital.Name", "State")

    # Get which outcome is being selected
    outcomeLocation <- match(outcome, possibleOutcomes)

    # Validate that the outcome is one of the possible outcomes
    if (is.na(outcomeLocation)) {
        stop("invalid outcome")
    }

    if (!is.numeric(num) && is.na(match(num, possibleCharNums))) {
        stop("invalid num")
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
    baseColumns <- c(baseColumns, columnName)
    subbed <- outcomeData[, baseColumns]

    # Convert the filter column to numeric
    subbed[, columnName] <- as.numeric(subbed[, columnName])

    # Create an empty vector to hold the hospital names
    hospitals <- vector()
    for (stateName in stateList) {
        # Get all of the test values for the given state
        stateValues <- subbed[subbed$State == stateName,]

        # If there are no values then just add a NA to the hospitals
        if (nrow(stateValues) == 0) {
            hospitals <- c(hospitals, "<NA>")
        } else {
            # Filter the NA values from the values for the state and order by the filter column and hospital name
            filtered <- stateValues[!is.na(stateValues[, columnName]),]
            ordered <- filtered[order(filtered[, columnName], filtered[, "Hospital.Name"]),]

            if (is.numeric(num)) {
                # If a numeric parameter is given use either an NA for an out of bounds selection
                # or the hostpital name at that value in the ordered data
                if (num > nrow(ordered)) {
                    hospitals <- c(hospitals, "<NA>")
                } else {
                    hospitals <- c(hospitals, ordered[num,]$Hospital.Name)
                }
            } else {
                # If the value is best then take the top hospital name else use the lowest
                if (num == "best") {
                    hospitals <- c(hospitals, ordered[1,]$Hospital.Name)
                } else {
                    hospitals <- c(hospitals, ordered[nrow(ordered),]$Hospital.Name)
                }
            }
        }
    }

    # Create a data frame from the hostpital names and the state list
    results <- data.frame("hospital" = hospitals, "state" = stateList)
    return(results)
}