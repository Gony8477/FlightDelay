train_data <- read.csv("trainFinal.csv")
test_data <- read.csv("testFinal.csv")

# Train Attributes
train_data$ID <- as.character(train_data$ID)
train_data$FlightNum <- as.factor(train_data$FlightNum)
train_data$DayofMonth <- as.factor(train_data$DayofMonth)

# Test Attributes
test_data$ID <- as.character(test_data$ID)
test_data$FlightNum <- as.factor(test_data$FlightNum)
test_data$DayofMonth <- as.factor(test_data$DayofMonth)

# Null of ActualElapsedTime
train_data$ActualElapsedTime[is.na(train_data$ActualElapsedTime)] <- median(train_data$ActualElapsedTime, na.rm=TRUE)
test_data$ActualElapsedTime[is.na(test_data$ActualElapsedTime)] <- median(test_data$ActualElapsedTime, na.rm=TRUE)

# Null of AirTime
train_data$AirTime[is.na(train_data$AirTime)] <- median(train_data$AirTime, na.rm=TRUE)
test_data$AirTime[is.na(test_data$AirTime)] <- median(test_data$AirTime, na.rm=TRUE)

#Null of TaxiIn
train_data$TaxiIn[is.na(train_data$TaxiIn)] <- median(train_data$TaxiIn, na.rm=TRUE)
test_data$TaxiIn[is.na(test_data$TaxiIn)] <- median(test_data$TaxiIn, na.rm=TRUE)

# Train 5가지 Delay 빈값 = 0
train_data$CarrierDelay[is.na(train_data$CarrierDelay)] <- 0
train_data$WeatherDelay[is.na(train_data$WeatherDelay)] <- 0
train_data$NASDelay[is.na(train_data$NASDelay)] <- 0
train_data$SecurityDelay[is.na(train_data$SecurityDelay)] <- 0
train_data$LateAircraftDelay[is.na(train_data$LateAircraftDelay)] <- 0

# Test 5가지 Delay 빈값 = 0
test_data$CarrierDelay[is.na(test_data$CarrierDelay)] <- 0
test_data$WeatherDelay[is.na(test_data$WeatherDelay)] <- 0
test_data$NASDelay[is.na(test_data$NASDelay)] <- 0
test_data$SecurityDelay[is.na(test_data$SecurityDelay)] <- 0
test_data$LateAircraftDelay[is.na(test_data$LateAircraftDelay)] <- 0

library(adabag)
ada_boo <- boosting(TotalDelay ~ DayofMonth + FlightNum + ActualElapsedTime + CRSElapsedTime + AirTime + Distance + TaxiOut + TaxiIn + CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay, data = train_data, boos=TRUE, mfinal = 6)
ada_pre <- predict(ada_boo, test_data)
ada_pre

ada_df <- data.frame(test_data$ID[1:298], ada_pre)
names(ada_df) <- c("ID", "TotalDelay")
write.csv(ada_df, "AdaBoost.csv", row.names=FALSE)