# Sample data

getwd()
install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("viridis")
install.packages("patchwork")
library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(viridis)
library(patchwork)

################################################################################
# load data from excel

sample_data <- read_excel("data/all_measurements.xlsx", sheet = "All")

bailer_data <- read_excel("data/all_measurements.xlsx", sheet = "bailer_data")

dna_data <- read_excel("data/all_measurements.xlsx", sheet = "DNA")
data_PC07 <- read_excel("data/all_measurements.xlsx", sheet = "PC07")
data_PC12 <- read_excel("data/all_measurements.xlsx", sheet = "PC12")
data_PC16 <- read_excel("data/all_measurements.xlsx", sheet = "PC16")


piez_data <- read_excel("data/piez.xlsx", sheet = "dati_pomp")

PC03_kit <- read_excel("data/all_measurements.xlsx", sheet = "PC03-kit")
PC14_kit <- read_excel("data/all_measurements.xlsx", sheet = "PC14-kit")
PC15_kit <- read_excel("data/all_measurements.xlsx", sheet = "PC15-kit")


PC03_fluo <- read_excel("data/all_measurements.xlsx", sheet = "PC03-aquaread")
PC14_fluo <- read_excel("data/all_measurements.xlsx", sheet = "PC14-fluo")
PC15_fluo <- read_excel("data/all_measurements.xlsx", sheet = "PC15-aquaread")

data_divers <- read_excel("data/diver_data_clean.xlsx")

events <- read_excel("data/all_measurements.xlsx", sheet = "events")

################################################################################  
# show tracer BTCs for each tracer
KUM1_plot <- dna_data %>% 
  ggplot(aes(x = date_time, y = KUM1norm, shape = Piezometer)) + 
  geom_point() +
  geom_line() +
  labs(title = "KUM1", 
       subtitle = "xy", 
       x = "Date", y = "DNA concentration (g/l)") +
  theme(panel.background = element_rect(fill = "white")) + 
  geom_point(aes(color = Piezometer)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Piezometer")

KUM2_plot <- dna_data %>% 
  ggplot(aes(x = date_time, y = KUM2norm, shape = Piezometer)) + 
  geom_point() +
  geom_line () +
  labs(title = "KUM2", 
       subtitle = "xy", 
       x = "Date", y = "DNA concentration (g/l)") +
  theme(panel.background = element_rect(fill = "white")) + 
  geom_point(aes(color = Piezometer)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Piezometer")

KUM3_plot <- dna_data %>% 
  ggplot(aes(x = date_time, y = KUM3norm, shape = Piezometer)) + 
  geom_point() +
  geom_line () +
  labs(title = "KUM3", 
       subtitle = "xy", 
       x = "Date", y = "DNA concentration (g/l)") +
  theme(panel.background = element_rect(fill = "white")) + 
  geom_point(aes(color = Piezometer)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Piezometer")

KUM_combined <- KUM1_plot/KUM2_plot/KUM3_plot # show them all above each other
ggsave(KUM_combined, filename = "KUM_combined.jpeg") # save as image

################################################################################
# show tracer BTCs for each piezometer
# PC03
PC03_data <- dna_data[c(1:13),] %>% 
  select(date_time, KUM1norm, KUM2norm, KUM3norm) %>%
  gather(key = "Tracer", value = "DNA_conc", -date_time) # collapse the tracer columns into one to plot later

PC03_plot <- PC03_data %>% 
  ggplot(aes(x = date_time, y = DNA_conc, shape = Tracer)) + 
  geom_line () +
  geom_point() +
  labs(title = "PC03", 
       subtitle = "xy", 
       x = "Date", y = "DNA concentration (g/l)") +
  theme(panel.background = element_rect(fill = "white")) + 
  geom_point(aes(color = Tracer)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Tracer")

# PC14
PC14_data <- dna_data[c(14:28),] %>% 
  select(date_time, KUM1norm, KUM2norm, KUM3norm) %>%
  gather(key = "Tracer", value = "DNA_conc", -date_time) # collapse the tracer columns into one to plot later

PC14_plot <- PC14_data %>% 
  ggplot(aes(x = date_time, y = DNA_conc, shape = Tracer)) + 
  geom_line () +
  geom_point() +
  labs(title = "PC14", 
       subtitle = "xy", 
       x = "Date", y = "DNA concentration (g/l)") +
  theme(panel.background = element_rect(fill = "white")) + 
  geom_point(aes(color = Tracer)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Tracer")

# PC15
PC15_data <- dna_data[c(28:38),] %>% 
  select(date_time, KUM1norm, KUM2norm, KUM3norm) %>%
  gather(key = "Tracer", value = "DNA_conc", -date_time) # collapse the tracer columns into one to plot later

PC15_plot <- PC15_data %>% 
  ggplot(aes(x = date_time, y = DNA_conc, shape = Tracer)) + 
  geom_line () +
  geom_point() +
  labs(title = "PC15", 
       subtitle = "xy", 
       x = "Date", y = "DNA concentration (g/l)") +
  theme(panel.background = element_rect(fill = "white")) + 
  geom_point(aes(color = Tracer)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Tracer")

Piez_KUM_combined <- PC03_plot/PC14_plot/PC15_plot # show them all above each other
ggsave(Piez_KUM_combined, filename = "Piez_KUM_combined.jpeg") # save as image



################################################################################
# combine the datasets to plot them together
PC03_dna <- PC03_data
PC03_dna$Piezometer <- c("PCO3") # first add a source column
PC14_dna <- PC14_data
PC14_dna$Piezometer <- c("PC14")
PC15_dna <- PC15_data
PC15_dna$Piezometer <- c("PC15")
  
# Combine datasets
combined_dna <- bind_rows(PC03_dna, PC14_dna, PC15_dna)

# Create the plot
stacked_dna_plots <- ggplot(combined_dna, aes(x = date_time, y = DNA_conc, color = Tracer)) +
  geom_line() + 
  geom_point () +
  labs(title = "All three DNA tracers", x = "Date", y = "DNA") +
  theme_minimal() +
  facet_wrap(~ Piezometer, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Tracer")+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(stacked_dna_plots, filename = "stacked_dna_plots.jpeg")


################################################################################
# prep and stack datasets to create multiplots using facet_wrap and filtering by parameter


############ PC03
# insert all important events at PC03
events_PC03 <- events[c(1, 4, 7, 10, 11, 12, 16),]
events_PC03_DNA <- events_PC03[c(1, 2, 4, 5, 7), -3]
events_PC03_DNA$parameter <- c("DNA")
events_PC03_DNA$y_position = c(2.5e-07, 2.3e-07, 2.5e-07, 2.3e-07, 2.5e-07)
events_PC03_fluo <- events_PC03[c(3, 6), -3]
events_PC03_fluo$parameter <- c("Fluorescence")
events_PC03_fluo$y_position = c(250, 250)

# prep continuous fluo data
PC03_fluo <- PC03_fluo[,c(1, 15)] # select relevant columns
PC03_fluo$type <- c("uranine")
PC03_fluo$parameter <- c("Fluorescence")
PC03_fluo$plot_type <- c("line")
PC03_fluo <- PC03_fluo %>% 
  rename(date_time = `Date+Time`, value = "Uranine [µg/l]") %>% # rename columns
  na.omit(PC03_fluo)

ggplot(PC03_fluo, aes(x=date_time, y = value)) +
  geom_line() +
  labs(title = "Fluorescence in PC03", x = "Date", y = "Uranine (ppb)") +
  theme_minimal()

# prep sample fluo data
PC03_kit <- PC03_kit[,c(2,3)]
PC03_kit$type <- c("uranine_samples")
PC03_kit$parameter <- c("Fluorescence")
PC03_kit$plot_type <- c("point")
PC03_kit <- PC03_kit %>%
  rename(date_time = date_time, value = "Uranine", parameter = parameter, type = type)
 
# prep dna data 
PC03_dna_data <- PC03_data %>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC03_dna_data$parameter <- c("DNA")
PC03_dna_data$plot_type <- c("point")

# prep piez data
PC03_piez <- piez_data[c(1:14), c(3,4)]
PC03_piez$type <- c("water_level_PC03")
PC03_piez$parameter <- c("Piezometry")
PC03_piez$plot_type <- c("line")
PC03_piez <- PC03_piez %>%
  rename(date_time = date_time, value = level, type = type, parameter = parameter)

# prep closest diver data
PC09_diver <- data_divers[,c(3,18)] # select relevant columns
PC09_diver <- PC09_diver %>% 
  rename(date_time = Date_Time, value = QUOTA_PC09)
PC09_diver <- na.omit(data_diver)
PC09_diver$type <- c("water_level_PC09")
PC09_diver$parameter <- c("Piezometry")
PC09_diver$plot_type <- c("line")



# combine the datasets
combined_data_PC03 <- bind_rows(PC03_dna_data, PC03_kit, PC03_fluo, PC03_piez, PC09_diver)
tracer_data_PC03 <- bind_rows(PC03_dna_data, PC03_kit, PC03_fluo)


# create plot of all tracers measured at PC03, stacked over each other 
stacked_PC03_plots <- ggplot(combined_data_PC03, aes(x = date_time, y = value, color = type)) +
  geom_point(data = combined_data_PC03[combined_data_PC03$plot_type == "point", ], size = 1) + 
  geom_line(data = combined_data_PC03[combined_data_PC03$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = combined_data_PC03[combined_data_PC03$parameter == "DNA", ], size = 0.5) +
  #geom_smooth(data = combined_data_PC03[combined_data_PC03$plot_type == "point", ], 
              #size = 3, method="loess", se=F)
  geom_vline(xintercept = events_PC03$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(title = "Parameters measured at PC03", subtitle = "In addition: diver continuous water levels from PC09", 
       x = "Date", y = "Water level (m a.s.l.)                               Fluorescence (ppb)                            DNA (g/l)       ") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B","darkblue", "black"), name = "type") +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration", Piezometry = "Piezometric level") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d-%m-%Y')
    
# plot without piezometry
tracer_PC03_plots <- ggplot(tracer_data_PC03, aes(x = date_time, y = value, color = type)) +
  geom_point(data = tracer_data_PC03[tracer_data_PC03$plot_type == "point", ], size = 1) + 
  geom_line(data = tracer_data_PC03[tracer_data_PC03$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = tracer_data_PC03[tracer_data_PC03$parameter == "DNA", ], size = 0.5) +
  #geom_smooth(data = combined_data_PC03[combined_data_PC03$plot_type == "point", ], 
  #size = 3, method="loess", se=F)
  geom_vline(xintercept = events_PC03$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(title = "Tracers measured at PC03", subtitle = "Both from groundwater samples as well as continuous fluorescence measurements", 
       x = "Date", y = "Fluorescence (ppb)                                               DNA (g/l)") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B", "darkblue"), name = "type") +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d-%m-%Y') +
  geom_text(data = events_PC03_DNA, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC03_fluo, aes(x = date_time, y = y_position, label = event), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)

# save the plot as a pdf
ggsave(tracer_PC03_plots, filename = "tracer_PC03_labels.pdf", width = 14, height = 8, dpi = 500)


################################################################################

########### PC014 
# insert all important events for PC14
events_PC14 <- events[c(2, 5, 8, 10, 11, 12),]
events_PC14_DNA <- events_PC14[c(1, 2, 4, 5, 6), -3]
events_PC14_DNA$parameter <- c("DNA")
events_PC14_DNA$y_position = c(7e-08, 6.5e-08, 7e-08, 6.5e-08, 7e-08)
events_PC14_fluo <- events_PC14[c(3), -3]
events_PC14_fluo$parameter <- c("Fluorescence")
events_PC14_fluo$y_position = c(700)


# prep continuous fluo data
PC14_fluo <- PC14_fluo %>%
  rename(date_time = date_time, value = conc, type = tracer, parameter = type)
PC14_fluo$plot_type <- c("line")
PC14_fluo$parameter <- c("Fluorescence")

ggplot(PC14_fluo, aes(x = date_time, y = value, color = type)) +
  geom_line() +
  labs(title = "Fluorescence in PC14", x = "Date", y = "Tracer (ppb)") +
  theme_minimal()

# prep sample fluo data
PC14_kit <- PC14_kit[,c(2,4)]
PC14_kit$type <- c("tinopal_samples")
PC14_kit$Tracer <- c("Fluorescence")
PC14_kit$plot_type <- c("point")
PC14_kit <- PC14_kit %>%
  rename(date_time = date_time, value = "Tinopal", parameter = Tracer, type = type)

# prep dna data
PC14_dna_data <- PC14_data %>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC14_dna_data$parameter <- c("DNA")
PC14_dna_data$plot_type <- c("point")

# prep piez data
PC14_piez <- piez_data[c(15:28), c(3,4)]
PC14_piez$type <- c("water_level_PC03")
PC14_piez$parameter <- c("Piezometry")
PC14_piez$plot_type <- c("line")
PC14_piez <- PC14_piez %>%
  rename(value = level)

# prep closest diver data
PC11_diver <- data_divers[,c(3,26)] # select relevant columns
PC11_diver <- PC11_diver %>% 
  rename(date_time = Date_Time, value = QUOTA_PC11)
PC11_diver <- na.omit(PC11_diver)
PC11_diver$type <- c("water_level_PC11")
PC11_diver$parameter <- c("Piezometry")
PC11_diver$plot_type <- c("line")

# combine all the datasets
combined_data_PC14 <- bind_rows(PC14_dna_data, PC14_kit, PC14_fluo, PC14_piez, PC11_diver)
tracer_data_PC14 <- bind_rows(PC14_dna_data, PC14_kit, PC14_fluo)

# create plot of all parameters measured at PC14, stacked over each other 
stacked_PC14_plots <- ggplot(combined_data_PC14, aes(x = date_time, y = value, color = type)) +
  geom_point(data = combined_data_PC14[combined_data_PC14$plot_type == "point", ], size = 1) + 
  geom_line(data = combined_data_PC14[combined_data_PC14$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = combined_data_PC14[combined_data_PC14$parameter == "DNA", ], size = 0.5) +
  geom_vline(xintercept = events_PC14$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(title = "Parameters measured at PC14", subtitle = "In addition: diver continuous water levels from PC11", 
       x = "Date", y = "Water level (m a.s.l.)                               Fluorescence (ppb)                            DNA (g/l)       ") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#B452CD", "#5D478B", "darkblue", "black"), name = "type") +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration", Piezometry = "Piezometric level") ) ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d-%m-%Y')

stacked_PC14_plots + scale_colour_viridis_d(option = "viridis", direction = -1)

# save the plot as a pdf
ggsave(stacked_PC14_plots, filename = "stacked_PC14_plots.pdf", width = 14, height = 8, dpi = 500)

# plot without piezometry
tracer_PC14_plots <- ggplot(tracer_data_PC14, aes(x = date_time, y = value, color = type)) +
  geom_point(data = tracer_data_PC14[tracer_data_PC14$plot_type == "point", ], size = 1) + 
  geom_line(data = tracer_data_PC14[tracer_data_PC14$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = tracer_data_PC14[tracer_data_PC14$parameter == "DNA", ], size = 0.5) +
  geom_vline(xintercept = events_PC14$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(title = "Tracers measured at PC14", subtitle = "Both from groundwater samples as well as continuous fluorescence measurements", 
       x = "Date", y = "Fluorescence (ppb)                                               DNA (g/l)") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B", "darkblue"), name = "type") +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d-%m-%Y') +
  geom_text(data = events_PC14_DNA, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC14_fluo, aes(x = date_time, y = y_position, label = event), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)
ggsave(tracer_PC14_plots, filename = "tracer_PC14_labels.pdf", width = 14, height = 8, dpi = 500)




################################################################################
######### PC15
# load relevant events that happened at PC15
events_PC15 <- events[c(3, 6, 9, 10, 11, 13, 14, 15, 16),]
events_PC15_DNA <- events_PC15[c(1, 2, 4, 5, 9), -3]
events_PC15_DNA$parameter <- c("DNA")
events_PC15_DNA$y_position = c(5e-07, 4.5e-07, 5e-07, 4.5e-07, 5e-07)
events_PC15_fluo <- events_PC15[c(3, 6, 7, 8), -3]
events_PC15_fluo$parameter <- c("Fluorescence")
events_PC15_fluo$y_position = c(15, 15, 13.5, 15)


# prep continuous fluo data
PC15_fluo <- PC15_fluo[c(-493, -636), c(1, 15)] # select relevant columns and remove 2 outlier values
PC15_fluo$type <- c("uranine")
PC15_fluo$parameter <- c("Fluorescence")
PC15_fluo$plot_type <- c("line")
PC15_fluo <- PC15_fluo %>% 
  rename(value = "Uranine [µg/l]") %>% # rename columns
  na.omit(PC15_fluo)

ggplot(PC15_fluo, aes(x=date_time, y = value)) +
  geom_line() +
  labs(title = "Fluorescence in PC15", x = "Date", y = "Uranine (ppb)") +
  theme_minimal()

# prep sample fluo data
PC15_kit <- PC15_kit[,c(2,3)]
PC15_kit$type <- c("uranine_samples")
PC15_kit$parameter <- c("Fluorescence")
PC15_kit$plot_type <- c("point")
PC15_kit <- PC15_kit %>%
  rename(date_time = date_time, value = "Uranine", parameter = parameter, type = type)

# prep dna data
PC15_dna_data <- PC15_data %>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC15_dna_data$parameter <- c("DNA")
PC15_dna_data$plot_type <- ("point")

# prep piez data
PC15_piez <- piez_data[c(29:41), c(3,4)]
PC15_piez$type <- c("water_level_PC03")
PC15_piez$parameter <- c("Piezometry")
PC15_piez$plot_type <- c("line")
PC15_piez <- PC15_piez %>%
  rename(value = level)


# combine all datasets
combined_data_PC15 <- bind_rows(PC15_dna_data, PC15_kit, PC15_fluo, PC15_piez, PC11_diver)
tracer_data_PC15 <- bind_rows(PC15_dna_data, PC15_kit, PC15_fluo)

# create plot of all parameters measured at PC15, stacked over each other 
stacked_PC15_plots <- ggplot(combined_data_PC15, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(data = combined_data_PC15[combined_data_PC15$plot_type == "point", ], size = 1) + 
  geom_line(data = combined_data_PC15[combined_data_PC15$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = combined_data_PC15[combined_data_PC15$parameter == "DNA", ], size = 0.5) +
  geom_vline(xintercept = events_PC15$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
    labs(title = "Parameters measured at PC15", subtitle = "In addition: diver continuous water levels from PC11", 
       x = "Date", y = "Water level (m a.s.l.)                               Fluorescence (ppb)                            DNA (g/l)       ") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B","darkblue", "black"), name = "type") +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", # with facet_wrap one can chose how to cluster data and display in multiple panels, here parameter is used as a filter
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration", 
                                      Piezometry = "Piezometric level") ) )  + # use labeller to change the titles of each panel
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d-%m-%Y') # to adjust the x-axis for dates


stacked_PC15_plots + scale_colour_viridis_d(option = "viridis", direction = -1)

# save the plot as a pdf
ggsave(stacked_PC15_plots, filename = "stacked_PC15_plots.pdf", width = 14, height = 8, dpi = 500)


# plots of only tracers
tracer_PC15_plots <- ggplot(tracer_data_PC15, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(data = tracer_data_PC15[tracer_data_PC15$plot_type == "point", ], size = 1) + 
  geom_line(data = tracer_data_PC15[tracer_data_PC15$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = tracer_data_PC15[tracer_data_PC15$parameter == "DNA", ], size = 0.5) +
  geom_vline(xintercept = events_PC15$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
   labs(title = "Tracers measured at PC15", subtitle = "Both from groundwater samples as well as continuous fluorescence measurements", 
       x = "Date", y = "Fluorescence (ppb)                                               DNA (g/l)") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B", "darkblue"), name = "type") +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d-%m-%Y') +
  geom_text(data = events_PC15_DNA, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC15_fluo, aes(x = date_time, y = y_position, label = event), 
          vjust = 1.5, color = "red", 
          inherit.aes = FALSE)
ggsave(tracer_PC15_plots, filename = "tracer_PC15_labels.pdf", width = 14, height = 8, dpi = 500)





################################################################################
# repeat for bailer locations
# insert relevant bailer events
events_bailer <- events[c(1, 4, 7, 10, 11, 16), -3]
events_bailer$event <- c("Start pump", "Injection DNA", "Injection Fluorescence", "Stop pumps", "Start pumps", "Final stop pumps")

events_bailer_DNA <- events_bailer[c(1, 2, 4, 5, 6),]
events_bailer_DNA$Piezometer <- c("PC07")
events_bailer_DNA$y_position = c(6.5e-08, 6e-08, 6.5e-08, 6e-08, 6.5e-08)
events_bailer_fluo <- events_bailer[c(1, 3, 4, 5, 6),]
events_bailer_fluo$Piezometer <- c("PC07")
events_bailer_fluo$y_position = c(40, 35, 40, 35, 40)


bailer_fluo <- bailer_data[c(1:31),]
bailer_dna <- bailer_data[c(32:76),]

# create plot for dna in bailers
dna_bailer_plots <- ggplot(bailer_dna, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(size = 1) + 
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = events_bailer_DNA$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(title = "DNA concentration measured at PC07, PC12, PC16", subtitle = "From groundwater samples", 
       x = "Date", y = "DNA (g/l)") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "type") +
  facet_wrap(~ Piezometer, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(PC07 = "PC07", PC12 = "PC12", PC16 = "PC16") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(limits = as.POSIXct(c("2024-07-15", "2024-07-30")), date_breaks = "1 day", date_labels = '%d-%m-%Y') +
  geom_text(data = events_bailer_DNA, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
           inherit.aes = FALSE)
ggsave(dna_bailer_plots, filename = "dna_bailer_plots.jpeg", width = 14, height = 8, dpi = 500)



# fluo plots in bailers
fluo_bailer_plots <- ggplot(bailer_fluo, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(size = 1) + 
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = events_bailer_fluo$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(title ="Fluorescent concentration measured at PC07, PC12, PC16", subtitle = "From groundwater samples", 
       x = "Date", y = "Fluorescence (ppb)") +
  scale_color_manual(values = c("#5D478B", "#DDA0DD"), name = "type") +
  facet_wrap(~ Piezometer, ncol = 1, scales = "fixed", 
             labeller = as_labeller(c(PC07 = "PC07", PC12 = "PC12", PC16 = "PC16") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right") +
  scale_x_datetime(limits = as.POSIXct(c("2024-07-15", "2024-07-30")), date_breaks = "1 day", date_labels = '%d-%m-%Y') +
  geom_text(data = events_bailer_fluo, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE)
ggsave(fluo_bailer_plots, filename = "fluo_bailer_plots.jpeg", width = 14, height = 8, dpi = 500)



