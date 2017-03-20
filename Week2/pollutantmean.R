library(stringr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # create the path to read the files from
    # specifies the "" seperator so that the combined pieces have no spaces
    directory_path <- paste("./", directory, "/", sep = "")

    for (item in id) {
        file_name <- paste(str_pad(item, width = 3, side = "left", pad = "0"), ".csv", sep = "")
        read_path <- paste(directory_path, file_name, sep = "")

        if (exists("pollution_data")) {
            temp_data <- read.csv(read_path)
            pollution_data <- rbind(pollution_data, temp_data)
            rm(temp_data)
        } else {
            pollution_data <- read.csv(read_path)
        }
    }

    pollutant_data <- pollution_data[pollutant]

    mean(pollutant_data[!is.na(pollutant_data)])
}