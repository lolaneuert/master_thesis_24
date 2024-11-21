

getwd()
library(dplyr)   
library(stats)
library(factoextra)
library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(viridis)
library(patchwork)
library(car)
library(fpp)
library(forecast)
library(pracma)
library(zoo)

# load datasets
sample_data <- read_excel("data/all_measurements.xlsx", sheet = "All")

bailer_data <- read_excel("data/all_measurements.xlsx", sheet = "bailer_data")

dna_data <- read_excel("data/all_measurements.xlsx", sheet = "DNA")
data_PC07 <- read_excel("data/all_measurements.xlsx", sheet = "PC07")
data_PC12 <- read_excel("data/all_measurements.xlsx", sheet = "PC12")
data_PC16 <- read_excel("data/all_measurements.xlsx", sheet = "PC16")


PC03_kit <- read_excel("data/all_measurements.xlsx", sheet = "PC03-kit")
PC14_kit <- read_excel("data/all_measurements.xlsx", sheet = "PC14-kit")
PC15_kit <- read_excel("data/all_measurements.xlsx", sheet = "PC15-kit")


PC03_fluo <- read_excel("data/all_measurements.xlsx", sheet = "PC03-aquaread")
PC14_fluo <- read_excel("data/all_measurements.xlsx", sheet = "PC14-fluo")
PC15_fluo <- read_excel("data/all_measurements.xlsx", sheet = "PC15-aquaread")


################ calculate total mass recovery

auc_values <- data.frame(tracer = c("KUM1", "KUM2", "KUM3", "Uranine", "Tinopal"), PC03 = c(1:5), PC14 = c(1:5), PC15 = c(1:5), PC07 = c(1:5), PC12 = c(1:5), PC16 = c(1:5))
auc_values$sum <- rowSums(auc_values[,2:7] )
auc_values
auc_values[c(4:5),c(2:7)] <- 0

##for KUM1
#Pc03
kum1_PC03 <- dna_data[c(1:13),c(3,7)]
kum1_PC03$nr <- c(1:13)
integrate(approxfun(kum1_PC03$nr, kum1_PC03$KUM1norm), 1, 13) # 0 with absolute error < 0
auc_values[1,2] <- 0
#PC14
kum1_PC14 <- dna_data[c(14:28),c(3,7)]
kum1_PC14$nr <- c(1:15)
integrate(approxfun(kum1_PC14$nr, kum1_PC14$KUM1norm), 1, 15) # 0 with absolute error < 0
auc_values[1,3] <- 0
#PC15
kum1_PC15 <- dna_data[c(29:38),c(3,7)]
kum1_PC15$nr <- c(1:10)
integrate(approxfun(kum1_PC15$nr, kum1_PC15$KUM1norm), 1, 10) 
auc_values[1,4] <- 1.196048e-09
#PC07
kum1_PC07 <- bailer_data[c(32:36),c(1,2)]
kum1_PC07$nr <- c(1:5)
integrate(approxfun(kum1_PC07$nr, kum1_PC07$value), 1, 5) 
auc_values[1,5] <- 1.356766e-07
#PC12
kum1_PC12 <- bailer_data[c(37:41),c(1,2)]
kum1_PC12$nr <- c(1:5)
integrate(approxfun(kum1_PC12$nr, kum1_PC12$value), 1, 5) 
auc_values[1,6] <- 8.143341e-08
#PC16
kum1_PC16 <- bailer_data[c(42:46),c(1,2)]
kum1_PC16$nr <- c(1:5)
integrate(approxfun(kum1_PC16$nr, kum1_PC16$value), 1, 5) 
auc_values[1,7] <- 6.996927e-08


##for KUM2
#Pc03
kum2_PC03 <- dna_data[c(1:13),c(3,8)]
kum2_PC03$nr <- c(1:13)
integrate(approxfun(kum2_PC03$nr, kum2_PC03$KUM2norm), 1, 13)
auc_values[2,2] <- 4.838172e-09 
#PC14
kum2_PC14 <- dna_data[c(14:28),c(3,8)]
kum2_PC14$nr <- c(1:15)
integrate(approxfun(kum2_PC14$nr, kum2_PC14$KUM2norm), 1, 15) 
auc_values[2,3] <- 1.731857e-08
#PC15
kum2_PC15 <- dna_data[c(29:38),c(3,8)]
kum2_PC15$nr <- c(1:10)
integrate(approxfun(kum2_PC15$nr, kum2_PC15$KUM2norm), 1, 10) 
auc_values[2,4] <- 1.584276e-08

#PC07
kum2_PC07 <- bailer_data[c(47:51),c(1,2)]
kum2_PC07$nr <- c(1:5)
integrate(approxfun(kum2_PC07$nr, kum2_PC07$value), 1, 5) 
auc_values[2,5] <- 3.094369e-08
#PC12
kum2_PC12 <- bailer_data[c(52:56),c(1,2)]
kum2_PC12$nr <- c(1:5)
integrate(approxfun(kum2_PC12$nr, kum2_PC12$value), 1, 5) 
auc_values[2,6] <- 1.431438e-08
#PC16
kum2_PC16 <- bailer_data[c(57:61),c(1,2)]
kum2_PC16$nr <- c(1:5)
integrate(approxfun(kum2_PC16$nr, kum2_PC16$value), 1, 5) 
auc_values[2,7] <- 1.501617e-08

##for KUM3
#PC03
kum3_PC03 <- dna_data[c(1:13),c(3,9)]
kum3_PC03$nr <- c(1:13)
integrate(approxfun(kum3_PC03$nr, kum3_PC03$KUM3norm), 1, 13)
auc_values[3,2] <- 3.668653e-07
#PC14
kum3_PC14 <- dna_data[c(14:28),c(3,9)]
kum3_PC14$nr <- c(1:15)
integrate(approxfun(kum3_PC14$nr, kum3_PC14$KUM3norm), 1, 15) 
auc_values[3,3] <- 3.114871e-07
#PC15
kum3_PC15 <- dna_data[c(29:38),c(3,9)]
kum3_PC15$nr <- c(1:10)
integrate(approxfun(kum3_PC15$nr, kum3_PC15$KUM3norm), 1, 10) 
auc_values[3,4] <- 7.10404e-07

#PC07
kum3_PC07 <- bailer_data[c(62:66),c(1,2)]
kum3_PC07$nr <- c(1:5)
integrate(approxfun(kum3_PC07$nr, kum3_PC07$value), 1, 5) 
auc_values[3,5] <- 1.565135e-08
#PC12
kum3_PC12 <- bailer_data[c(67:71),c(1,2)]
kum3_PC12$nr <- c(1:5)
integrate(approxfun(kum3_PC12$nr, kum3_PC12$value), 1, 5) 
auc_values[3,6] <- 5.020158e-08
#PC16
kum3_PC16 <- bailer_data[c(72:76),c(1,2)]
kum3_PC16$nr <- c(1:5)
integrate(approxfun(kum3_PC16$nr, kum3_PC16$value), 1, 5) 
auc_values[3,7] <-0

##for Uranine continuous
#Pc03
#PC14
#PC15

## for Uranine kit
#Pc03
#PC14
#PC15
#PC07
#PC12
#PC16

##for Tinopal continuous
#PC14
ggplot(PC14_fluo, aes(x = date_time, y = value, color = type)) +
  geom_line() +
  labs(title = "Fluorescence in PC14", x = "Date", y = "Tracer (ppb)") +
  theme_minimal()

tinopal_PC14 <- PC14_fluo[c(1:1790), c(1,2)]
tinopal_PC14$time_diff <- c(0, diff(tinopal_PC14$date_time))# Time difference between consecutive points
tinopal_PC14$time_sec <- tinopal_PC14$time_diff*60
area_tinopal_PC14 <- trapz(as.numeric(tinopal_PC14$time_sec), tinopal_PC14$value)
print(area_tinopal_PC14) # 22014.64 µg x sec/L

# area_tinopal_PC14 is the area under the curve in µg·seconds/L
flow_rate <- 0.2  # in seconds per liter (s/L)
first_datetime <- min(tinopal_PC14$date_time)  # First date-time value
last_datetime <- max(tinopal_PC14$date_time)   # Last date-time value

# Calculate the difference between the first and last date-time
time_diff <- last_datetime - first_datetime

# Convert the difference to seconds
total_duration_seconds <- as.numeric(time_diff, units = "secs")
total_duration_hours <- as.numeric(time_diff, units = "hours")


# Calculate the total mass over time (µg·seconds)
total_mass_over_time <- area_tinopal_PC14 * flow_rate
print(total_mass_over_time) # 4402.928 µg


mass_per_second <- total_mass_over_time / total_duration_seconds  # µg/s


# Calculate total mass recovered (µg)
total_mass_recovered <- mass_per_second * total_duration_seconds
print(total_mass_recovered) # 4402.928 µg or 0.00440293 g

750/100
0.00440293/7.5 # 0.0005870573 % of the original injection mass (realistic?)


sum(tinopal_PC14$value)
sum(tinopal_PC14_kit$value)

## for tinopal kit
#PC14

tinopal_PC14_kit <- PC14_kit[, c(2,4)]
tinopal_PC14_kit$time_diff <- c(0, diff(tinopal_PC14_kit$date_time))# Time difference between consecutive points
tinopal_PC14_kit$time_sec <- tinopal_PC14_kit$time_diff*60*60
area_tinopal_PC14_kit <- trapz(as.numeric(tinopal_PC14_kit$time_sec), tinopal_PC14_kit$value)
print(area_tinopal_PC14_kit) # 587308 µg x sec/L

# area_tinopal_PC14 is the area under the curve in µg·seconds/L
flow_rate <- 0.2  # in liters per second (L/s)
first_datetime_kit <- min(tinopal_PC14_kit$date_time)  # First date-time value
last_datetime_kit <- max(tinopal_PC14_kit$date_time)   # Last date-time value

# Calculate the difference between the first and last date-time
time_diff_kit <- last_datetime_kit - first_datetime_kit

# Convert the difference to seconds
total_duration_seconds_kit <- as.numeric(time_diff_kit, units = "secs")
total_duration_hours <- as.numeric(time_diff, units = "hours")


# Calculate the total mass over time (µg·seconds)
total_mass_over_time_kit <- area_tinopal_PC14_kit * flow_rate
print(total_mass_over_time_kit) # 117461.6 µg or 0.117462 g


mass_per_second_kit <- total_mass_over_time_kit / total_duration_seconds_kit  # µg/s


# Calculate total mass recovered (µg)
total_mass_recovered_kit <- mass_per_second_kit * total_duration_seconds_kit
print(total_mass_recovered_kit) #


######### try again
data1 <- tinopal_PC14  # Replace with actual dataset
data2 <- tinopal_PC14_kit

# Ensure datetime columns are in correct format
data1$datetime <- as.POSIXct(data1$date_time, format="%d-%m-%y %H:%M:%S")  # Adjust if necessary
data2$datetime <- as.POSIXct(data2$date_time, format="%d-%m-%y %H:%M:%S")

# Calculate time differences in seconds
data1$time_diff_seconds <- c(600, as.numeric(diff(data1$datetime), units = "secs"))
data2$time_diff_seconds <- c(600, as.numeric(diff(data2$datetime), units = "secs"))

duration1 <- as.numeric(diff(range(data1$datetime)), units = "secs")
duration2 <- as.numeric(diff(range(data2$datetime)), units = "secs")

area1 <- trapz(data1$time_diff_seconds, data1$conc)
area2 <- trapz(data2$time_diff_seconds, data2$Tinopal)

# Print areas
print(area1)
print(area2)

# Apply flow rate to calculate total mass over time
flow_rate <- 0.2  # in seconds per liter
total_mass1 <- area1 * flow_rate
total_mass2 <- area2 * flow_rate

# Print total mass recovered
print(total_mass1) # 1210.928 µg
print(total_mass2) # 117461.6 µg

