# Diver data

getwd()
library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(viridis)
library(patchwork)

# load data from excel
data_divers <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/diver_data_clean.xlsx")
# clean dataset
data_diver <- data_divers[,c(2,3,4,5,6,9,13,17,21,25,29,33)] # select relevant columns
data_diver <- data_diver %>% 
  rename(date = Date , date_time = Date_Time, hours = TEMPO, mbar = UNIT_mbar, 
         temp = TEMPERATURE, PC01 = SUBJECTIVITY_PC01, PC08 = SUBJECTIVITY_PC08,
         PC09 = SUBJECTIVITY_PC09, PC10 = SUBJECTIVITY_PC10, PC11 = SUBJECTIVITY_PC11,
         PC13 = SUBJECTIVITY_PC13, PC21 = SUBJECTIVITY_PC21) %>% # rename columns
  select(date, date_time, PC01, PC08, PC09, PC10, PC11, PC13, PC21) # select relevant columns
data_diver <- na.omit(data_diver)

data_diver1 <- data_diver %>% 
  select(date_time, PC01, PC08, PC09, PC10, PC11, PC13, PC21) %>%
  gather(key = "Piezometer", value = "subjectivity", -date_time) # collapse the piezometer columns into one to plot later
data_diver1 <- na.omit(data_diver1) # remove NAs otherwise plots will not show properly

# plot piezometry using ggplot
plot_divers <- ggplot(data_diver1, aes(x = date_time, y = subjectivity)) + 
  geom_line(aes(color = Piezometer), size = 1) +
  labs(title = "Diver Measurements", 
       subtitle = "", 
       x = "Date & Time", y = "Piezometry (m)") +
  scale_colour_viridis_d(option = "plasma", direction = -1) +
  theme(panel.background = element_rect(fill = "white"))
plot_divers
ggsave(plot_divers, filename = "plot_divers.jpeg")

# plot the singular piezometers

PC01 <- ggplot(data_diver, aes(x = date_time, y = PC01)) + 
  geom_line()
PC08 <- ggplot(data_diver, aes(x = date_time, y = PC08)) + 
  geom_line()
PC09 <- ggplot(data_diver, aes(x = date_time, y = PC09)) + 
  geom_line()
PC10 <- ggplot(data_diver, aes(x = date_time, y = PC10)) + 
  geom_line()
PC11 <- ggplot(data_diver, aes(x = date_time, y = PC11)) + 
  geom_line()
PC13 <- ggplot(data_diver, aes(x = date_time, y = PC13)) + 
  geom_line()
PC21 <- ggplot(data_diver, aes(x = date_time, y = PC21)) + 
  geom_line()

# combine them using patchwork package
all_plots <- (PC01 + PC08) / (PC09 + PC10) / (PC11 + PC13)/ PC21 
print(all_plots) 
ggsave(all_plots, filename = "all_plots.jpeg")


################################################################################
# lets create means for each piezometer and day of analysis

data <- data.frame(data_diver) # create a dataframe to work with

# clean up the dataset so only the necessary columns remain
data <- data %>%
  select(date, PC01, PC08, PC09, PC10, PC11, PC13, PC21) %>%
  rename(date_col = date)
  
data$date_col <- as.Date(data$date_col) # transform the date colum into Date format

str(data) # check the dataframe


calculate_means_by_date <- function(data, date_col, target_date) {
  # Ensure target_date is in Date format
  target_date <- as.Date(target_date)
  
  # Filter the data for the target date
  filtered_data <- data %>%
    filter(!!sym(date_col) == target_date)
  
  # Calculate means for each numeric column
  means <- filtered_data %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
  
  # Add the target date to the result
  means <- cbind(target_date, means)
  return(means)
}

calculate_means_for_all_dates <- function(data, date_col) {
  unique_dates <- unique(data[[date_col]])
  
  # Ensure we are passing the date directly
  results <- lapply(unique_dates, function(date) {
    # Call the means calculation function
    calculate_means_by_date(data, date_col, date)
  })
  
  # Combine all results into one data frame
  final_results <- do.call(rbind, results)
  return(final_results)
}

# Call the function with your data frame
all_means_result <- calculate_means_for_all_dates(data, "date_col")

# Print the resulting data frame
print(all_means_result)

# export the created dataset
write.csv(all_means_result, "C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/diver_means.csv")

dataset <- all_means_result %>%
  gather(key = "Piezometer", value = "subjectivity", -target_date) # collapse all piezometers into one column to allow plotting

# plot the mean values per day of all piezometers
plot_divers_mean <- ggplot(dataset, aes(x = target_date, y = subjectivity)) + 
  geom_line(aes(color = Piezometer), size = 1) +
  labs(title = "Diver Measurements", 
       subtitle = "", 
       x = "Date & Time", y = "Piezometry (m)") +
  scale_colour_viridis_d(option = "plasma", direction = -1) +
  theme(panel.background = element_rect(fill = "white"))
plot_divers_mean
ggsave(plot_divers_mean, filename = "plot_divers_mean.jpeg")

plot_divers + plot_divers_mean # check the two, all values and mean values side by side


# create individual plots of all mean piezometer values 
PC01_mean <- ggplot(all_means_result, aes(x = target_date, y = PC01)) +
  geom_line()
PC08_mean <- ggplot(all_means_result, aes(x = target_date, y = PC08)) +
  geom_line()
PC09_mean <- ggplot(all_means_result, aes(x = target_date, y = PC09)) +
  geom_line()
PC10_mean <- ggplot(all_means_result, aes(x = target_date, y = PC10)) +
  geom_line()
PC11_mean <- ggplot(all_means_result, aes(x = target_date, y = PC11)) +
  geom_line()
PC13_mean <- ggplot(all_means_result, aes(x = target_date, y = PC13)) +
  geom_line()
PC21_mean <- ggplot(all_means_result, aes(x = target_date, y = PC21)) +
  geom_line()

# show all of them together using patchwork
all_plots_mean <- (PC01_mean + PC08_mean) / (PC09_mean + PC10_mean) / (PC11_mean+ PC13_mean)/ PC21_mean
print(all_plots_mean)
ggsave(all_plots_mean, filename = "all_plots_mean.jpeg")


################################################################################

data_piez <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/piez.xlsx")

data_piez1 <- data_piez %>% 
  gather(key = "Piezometer", value = "subjectivity", -date)

plot_piez <- ggplot(data_piez1, aes(x = date, y = subjectivity)) + 
  geom_line(aes(color = Piezometer), size = 1) +
  labs(title = "Diver Measurements", 
       subtitle = "", 
       x = "Date & Time", y = "Piezometry (m)") +
  scale_colour_viridis_d(option = "plasma", direction = -1) +
  theme(panel.background = element_rect(fill = "white"))
plot_piez

PC07 <- ggplot(data_piez, aes(x = date, y = PC07)) +
  geom_point()


################################################################################
# normalize data (using the altitude of each Piezometer) for better graphic of general changes of piezometric level

data_quota <- data_divers[,c(2,3,4,5,6,10,14,18,22,26,30,34)]# select relevant columns
data_quota <- data_quota[-432,]
data_quota <- data_quota %>% 
  rename(date = Date , date_time = Date_Time, hours = TEMPO, mbar = UNIT_mbar, 
         temp = TEMPERATURE, PC01 = QUOTA_PC01, PC08 = QUOTA_PC08,
         PC09 = QUOTA_PC09, PC10 = QUOTA_PC10, PC11 = QUOTA_PC11,
         PC13 = QUOTA_PC13, PC21 = QUOTA_PC21) %>% # rename columns
  select(date, date_time, PC01, PC08, PC09, PC10, PC11, PC13, PC21) # select relevant columns
data_quota <- na.omit(data_quota)

data_quota1 <- data_quota %>% 
  select(date_time, PC01, PC08, PC09, PC10, PC11, PC13, PC21) %>%
  gather(key = "Piezometer", value = "quota", -date_time) # collapse the piezometer columns into one to plot later

# plot piezometry using ggplot
plot_quota <- ggplot(data_quota1, aes(x = date_time, y = quota)) + 
  geom_line(aes(color = Piezometer), size = 1) +
  labs(title = "Diver Measurements", 
       subtitle = "Quota of each Piezometer", 
       x = "Date & Time", y = "Piezometry (m)") +
  scale_colour_viridis_d(option = "plasma", direction = -1) +
  theme(panel.background = element_rect(fill = "white"))
plot_quota
ggsave(plot_quota, filename = "plot_quota.jpeg")

# plot the singular piezometers

PC01_q <- ggplot(data_quota, aes(x = date_time, y = PC01)) + 
  geom_line()
PC08_q <- ggplot(data_quota, aes(x = date_time, y = PC08)) + 
  geom_line()
PC09_q <- ggplot(data_quota, aes(x = date_time, y = PC09)) + 
  geom_line()
PC10_q <- ggplot(data_quota, aes(x = date_time, y = PC10)) + 
  geom_line()
PC11_q <- ggplot(data_quota, aes(x = date_time, y = PC11)) + 
  geom_line()
PC13_q <- ggplot(data_quota, aes(x = date_time, y = PC13)) + 
  geom_line()
PC21_q <- ggplot(data_quota, aes(x = date_time, y = PC21)) + 
  geom_line()

# combine them using patchwork package
all_plots_q <- (PC01_q + PC08_q) / (PC09_q + PC10_q) / (PC11_q + PC13_q)/ PC21_q 
print(all_plots_q) 
ggsave(all_plots_q, filename = "all_plots_q.jpeg")

################################################################################
# use original data to create a piezometric map to interpret groundwater flow (maybe mean or multiple over the two weeks?)

