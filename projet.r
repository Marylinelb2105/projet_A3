

data <- stat_acc_V3

data <- data[!is.null(data$id_code_insee) & !is.null(data$an_nais) & !is.null(data$age) & !is.null(data$place), ]
data <- data[!is.na(data$id_code_insee) & !is.na(data$an_nais) & !is.na(data$age) & !is.na(data$place), ]

data$Num_Acc <- as.numeric(data$Num_Acc)
data$id_usa <- as.numeric(data$id_usa)
data$id_code_insee <- as.numeric(data$id_code_insee)
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
data$an_nais <- as.numeric(data$an_nais)
data$age <- as.numeric(data$age)
data$place <- as.numeric(data$place)
data$ville <- as.character(data$ville)
data$date <- as.Date(data$date)

head(data)