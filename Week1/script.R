data <- read.csv("hw1_data.csv")

print(names(data))

print(data[1:2,])

print(nrow(data))

print(tail(data, 2))

print(data[[47, "Ozone"]])

print(sum(is.na(data$Ozone)))

non_ozone_na_values <- data[!is.na(data$Ozone),]
print(mean(non_ozone_na_values$Ozone))

good_values <- data[complete.cases(data),]
ozone_temp_values <- good_values[good_values$Ozone > 31 & good_values$Temp > 90,]
print(mean(ozone_temp_values$Solar.R))

month_six_values <- data[data$Month == 6,]
print(mean(month_six_values$Temp))

month_five_values <- good_values[good_values$Month == 5,]
print(max(month_five_values$Ozone))
