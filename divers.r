# Diver data

getwd()

library(dplyr)   
library(stats)
library(factoextra)
library(readxl)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(tidyr)
library(dplyr)
library(viridis)
library(patchwork)
library(car)
library(fpp)
library(forecast)


# load data from excel
data_divers <- read_excel("data/diver_data_clean.xlsx")
piez_data <- read_excel("data/piez.xlsx", sheet = "dati_pomp")
data_piez_manual <- read_excel("data/piez.xlsx", sheet = "manual")
events <- read_excel("data/all_measurements.xlsx", sheet = "events")
pluviometry <- read_excel("data/piez.xlsx", sheet = "pluvio")


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

EDA_plots <- function(x){
  par(mfrow = c(2,2))
  hist(x)
  boxplot(x)
  qqnorm(x)
  qqline(x)
  plot(density(x[!is.na(x)]))
  abline(v = mean(x, na.rm = T), col = "red")
  abline(v = median(x, na.rm = T), col = "green")
  abline(v = sd(x, na.rm = T), col = "blue")
}
EDA_plots(data_quota$PC11)
dev.off()

par(mfrow = c(1, 1))    
ggplot(data_quota, aes(x = date_time, y = PC01)) + 
  geom_line() +
  geom_line(PC01_trend)

par(mfrow = c(1,2))
# plot and detrend the singular piezometric movements
plot(y = as.ts(data_quota$PC01), x = data_quota$date_time)
PC01_trend <- ma(data_quota$PC01, order = 50, centre = T)
plot(y = as.ts(data_quota$PC01), x = data_quota$date_time)
lines(PC01_trend, col = 2)
plot(as.ts(PC01_trend))
deplot_PC01 <- data_quota$PC01 - PC01_trend
plot(as.ts(deplot_PC01))

PC08_trend <- ma(data_quota$PC08, order = 50, centre = T)
plot(as.ts(data_quota$PC08))
lines(PC08_trend, col =2)
deplot_PC08 <- data_quota$PC08 - PC08_trend
plot(as.ts(deplot_PC08))

PC09_trend <- ma(data_quota$PC09, order = 50, centre = T)
plot(as.ts(data_quota$PC09))
lines(PC09_trend, col =2)
deplot_PC09 <- data_quota$PC09 - PC09_trend
plot(as.ts(deplot_PC09))

PC10_trend <- ma(data_quota$PC10, order = 50, centre = T)
plot(as.ts(data_quota$PC10))
lines(PC10_trend, col =2)
deplot_PC10 <- data_quota$PC10 - PC10_trend
plot(as.ts(deplot_PC10))


par(mfrow = c(3, 2))  
PC11_trend <- ma(data_quota$PC11, order = 50, centre = T)
plot(as.ts(data_quota$PC11))
lines(PC11_trend, col =2)
deplot_PC11 <- data_quota$PC11 - PC11_trend
plot(as.ts(deplot_PC11))

PC13_trend <- ma(data_quota$PC13, order = 50, centre = T)
plot(as.ts(data_quota$PC13))
lines(PC13_trend, col =2)
deplot_PC13 <- data_quota$PC13 - PC13_trend
plot(as.ts(deplot_PC13))

PC21_trend <- ma(data_quota$PC21, order = 50, centre = T)
plot(as.ts(data_quota$PC21))
lines(PC21_trend, col =2)
deplot_PC21 <- data_quota$PC21 - PC21_trend
plot(as.ts(deplot_PC21))


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


data_piez1 <- data_allpiez %>% 
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
  gather(key = "Borehole", value = "quota", -date_time)# collapse the piezometer columns into one to plot later

data_piez_manual <- data_piez_manual %>%
  rename(Borehole = Piezometer)

data_quota1$plot_type <- c("line")
data_piez_manual$plot_type <- c("dashed")

events_piez <- events[c(1, 10, 11, 16), -3]
events_piez$y_position = c(0.62, 0.62, 0.61, 0.62)
events_piez[1,1] <- as.POSIXct("2024-07-16 02:00:00")
date = as_datetime("2024-07-17 13:54:27")
events_piez <- events_piez %>% add_row(date_time = date, event = "Installing pressure transducer", y_position = 0.61)


pluv <- pluviometry[, c(1,3)] %>%
  rename (prec = bari_campus)
  
pluv$Precipitation <- c("Rainfall in mm")
pluv$plot_type <- c("bar")
pluv[8,2] <- 1.6

combined_data_piez <- bind_rows(data_quota1, data_piez_manual)
combined_data_piez$label <- combined_data_piez$Borehole


df_summary <- combined_data_piez %>%
  group_by(Borehole) %>%
  filter(row_number() == n())  # Get the last row for each group


# plot piezometry using ggplot
plot_quota <- ggplot(combined_data_piez, aes(x = date_time, y = quota)) + 
  geom_line(data = combined_data_piez[combined_data_piez$plot_type == "line", ], aes(color = Borehole), linewidth = 1) +
  geom_line(data = combined_data_piez[combined_data_piez$plot_type == "dashed", ], aes(color = Borehole), linetype = "dashed", linewidth = 1) +
  geom_point(data = combined_data_piez[combined_data_piez$plot_type == "dashed", ], aes(color = Borehole), size = 1) + 
  geom_label_repel(data = df_summary, aes(label = label, colour = label, size = 8), 
                   box.padding = 0.5,  # Adjust padding as needed
                   point.padding = 0.2, label.size = 1, direction = "y",
                   hjust = 0) +
  geom_vline(xintercept = events_piez$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             size = 0.5) +
  labs(x = "Date (July 2024)", y = "Hydraulic head (m a.s.l.)") +
  scale_colour_viridis_d(option = "turbo", direction = -1) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "none", axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  scale_x_datetime(limits = as.POSIXct(c("2024-07-16", "2024-07-31")), date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_piez, aes(x = date_time, y = y_position, label = event, size = 8), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20), sec.axis = sec_axis(~ .*4, breaks = scales::pretty_breaks(n = 10), name = "Precepitation (mm)"))
plot_quota
ggsave(plot_quota, filename = "plot_quota_labels.jpeg", width = 14, height = , dpi = 500)

################################################################################
# plot the individual borehole quotas

PC01_q <- ggplot(data_quota, aes(x = date_time, y = PC01)) + 
  geom_line() +
  scale_x_datetime(limits = as.POSIXct(c("2024-07-15", "2024-07-30")), date_breaks = "1 day", date_labels = '%d-%m-%Y')
  
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
scale_x_datetime(limits = as.POSIXct(c("2024-07-15", "2024-07-30")), date_breaks = "1 day", date_labels = '%d-%m-%Y') +
  

# combine them using patchwork package
all_plots_q <- (PC01_q + PC08_q) / (PC09_q + PC10_q) / (PC11_q + PC13_q)/ PC21_q 
print(all_plots_q) 
ggsave(all_plots_q, filename = "all_plots_q.jpeg")

################################################################################
# use original data to create a piezometric map to interpret groundwater flow (maybe mean or multiple over the two weeks?)

