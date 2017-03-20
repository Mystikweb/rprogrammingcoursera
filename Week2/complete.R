library(stringr)

complete <- function(directory, id = 1:332) {
    directory_path <- paste("./", directory, "/", sep = "")

    result <- data.frame(id = numeric(0), nobs = numeric(0))

    for (item in id) {
        file_name <- paste(str_pad(item, width = 3, side = "left", pad = "0"), ".csv", sep = "")
        read_path <- paste(directory_path, file_name, sep = "")

        monitor_data <- read.csv(read_path)

        id_result <- data.frame(id = item, nobs = sum(complete.cases(monitor_data)))

        result <- rbind(result, id_result)
    }

    result
}