# Sample data

getwd()
library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(viridis)
library(patchwork)

# load data from excel

sample_data <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "All")

dna_data <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "DNA")

PC03_kit <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "PC03-kit")
PC14_kit <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "PC14-kit")
PC15_kit <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "PC15-kit")


PC03_fluo <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "PC03-aquaread")
PC14_fluo <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "PC14-fluo")
PC15_fluo <- read_excel("C:/Users/lolan/Documents/Lola Studium/Master/thesis/data/all_measurements.xlsx", sheet = "PC15-aquaread")

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
ggsave(KUM_combined, filename = "KUM_combined.jpeg")

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
ggsave(Piez_KUM_combined, filename = "Piez_KUM_combined.jpeg")


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
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(stacked_dna_plots, filename = "stacked_dna_plots.jpeg")

# stack dna and fluo datasets
# PC03

PC03_fluo <- PC03_fluo[,c(1, 15)] # select relevant columns
PC03_fluo$type <- c("uranine")
PC03_fluo$Tracer <- c("fluorescence")
PC03_fluo <- PC03_fluo %>% 
  rename(date_time = `Date+Time`, conc = "Uranine [µg/l]", tracer = Tracer) %>% # rename columns
  na.omit(PC03_fluo)

ggplot(PC03_fluo, aes(x=date_time, y = conc)) +
  geom_line() +
  labs(title = "Fluorescence in PC03", x = "Date", y = "Uranine (ppb)") +
  theme_minimal()

PC03_kit <- PC03_kit[,c(2,3)]
PC03_kit$type <- c("uranine_samples")
PC03_kit$Tracer <- c("fluorescence")
PC03_kit <- PC03_kit %>%
  rename(date_time = date_time, conc = "Uranine", tracer = Tracer, type = type)
  
PC03_dna_data <- PC03_data %>%
  rename(date_time = date_time, conc = "DNA_conc", type = Tracer)
PC03_dna_data$tracer <- c("DNA")



combined_data_PC03 <- bind_rows(PC03_dna_data, PC03_kit, PC03_fluo)

# create plot of all tracers measured at PC03, stacked over each other 
stacked_PC03_plots <- ggplot(combined_data_PC03, aes(x = date_time, y = conc, color = type)) +
  geom_point() + 
  geom_line() +
  labs(title = "Tracers recovered at PC03", subtitle = "DNA in g/l, Fluorescence in ppb", x = "Date", y = "Tracer concentration") +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B"), name = "type") +
  facet_wrap(~ tracer, ncol = 1, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right")
 
stacked_PC03_plots + scale_colour_viridis_d(option = "viridis", direction = -1)

ggsave(stacked_PC03_plots, filename = "stacked_PC03_plots.pdf")







# PC014 
PC14_fluo <- PC14_fluo %>%
  rename(date_time = date_time, conc = conc, type = tracer, tracer = type)# select relevant columns
ggplot(PC14_fluo, aes(x=date_time, y = conc, color = tracer)) +
  geom_line() +
  labs(title = "Fluorescence in PC14", x = "Date", y = "Tracer (ppb)") +
  theme_minimal()

PC14_kit <- PC14_kit[,c(2,4)]
PC14_kit$type <- c("tinopal_samples")
PC14_kit$Tracer <- c("fluorescence")
PC14_kit <- PC14_kit %>%
  rename(date_time = date_time, conc = "Tinopal", tracer = Tracer, type = type)

PC14_dna_data <- PC14_data %>%
  rename(date_time = date_time, conc = "DNA_conc", type = Tracer)
PC14_dna_data$tracer <- c("DNA")



combined_data_PC14 <- bind_rows(PC14_dna_data, PC14_kit, PC14_fluo)

# create plot of all tracers measured at PC14, stacked over each other 
stacked_PC14_plots <- ggplot(combined_data_PC14, aes(x = date_time, y = conc, color = type)) +
  geom_point() + 
  geom_line() +
  labs(title = "Tracers recovered at PC14", subtitle = "DNA in g/l, Fluorescence in ppb", x = "Date", y = "Tracer concentration") +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#B452CD", "#5D478B"), name = "type") +
  facet_wrap(~ tracer, ncol = 1, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right")

stacked_PC03_plots + scale_colour_viridis_d(option = "viridis", direction = -1)

ggsave(stacked_PC14_plots, filename = "stacked_PC14_plots.pdf")


# PC15

PC15_fluo$type <- c("uranine")
PC15_fluo$tracer <- c("fluorescence")
PC15_fluo <- PC15_fluo %>%
  rename(date_time = date_time, conc = uranine, tracer = tracer, type = type)


ggplot(PC15_fluo, aes(x=date_time, y = conc)) +
  geom_line() +
  labs(title = "Fluorescence in PC03", x = "Date", y = "Uranine (ppb)") +
  theme_minimal()

PC15_kit <- PC15_kit[,c(2,3)]
PC15_kit$type <- c("uranine_samples")
PC15_kit$Tracer <- c("fluorescence")
PC15_kit <- PC15_kit %>%
  rename(date_time = date_time, conc = "Uranine", tracer = Tracer, type = type)

PC15_dna_data <- PC15_data %>%
  rename(date_time = date_time, conc = "DNA_conc", type = Tracer)
PC15_dna_data$tracer <- c("DNA")



combined_data_PC15 <- bind_rows(PC15_dna_data, PC15_kit, PC15_fluo)

# create plot of all tracers measured at PC15, stacked over each other 
stacked_PC15_plots <- ggplot(combined_data_PC15, aes(x = date_time, y = conc, color = type)) +
  geom_point() + 
  geom_line() +
  labs(title = "Tracers recovered at PC03", subtitle = "DNA in g/l, Fluorescence in ppb", x = "Date", y = "Tracer concentration") +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422","#DDA0DD", "#5D478B"), name = "type") +
  facet_wrap(~ tracer, ncol = 1, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right")

stacked_PC03_plots + scale_colour_viridis_d(option = "viridis", direction = -1)

ggsave(stacked_PC15_plots, filename = "stacked_PC15_plots.pdf")






# Reshape to long format, not sure why this doesn't work
PC03_long <- combined_data_PC03 %>%
  select(date_time, Tracer, conc)
  pivot_longer(cols = c(Tracer, conc), 
               names_to = "Variable", values_to = "Value")
  
g <- ggplot(combined_data_PC03, aes(x = date_time, y = , color = Variable)) +
    geom_line() +
    labs(title = "PC03 DNA + Uranine ", x = "Date", y = "Tracer") +
    theme_minimal()

# load in sample fluo data, extract relevant columns, rename and bind to combined dataset to display as well
