library(stringr)

corr <- function(directory, threshold = 0) {
    result <- vector()

    complete_result <- complete(directory)

    threshold_list <- complete_result[complete_result$nobs > threshold,]
    file_list <- threshold_list$id

    directory_path <- paste("./", directory, "/", sep = "")
    for (item in file_list) {
        file_name <- paste(str_pad(item, width = 3, side = "left", pad = "0"), ".csv", sep = "")
        read_path <- paste(directory_path, file_name, sep = "")

        raw_monitor_data <- read.csv(read_path)
        monitor_data <- raw_monitor_data[complete.cases(raw_monitor_data),]

        result <- c(result, cor(monitor_data$nitrate, monitor_data$sulfate))
    }

    return(result)
}