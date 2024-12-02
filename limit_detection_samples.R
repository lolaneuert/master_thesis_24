# Sample data

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

tph <-  read_excel("data/all_measurements.xlsx", sheet = "TPH")

events <- read_excel("data/all_measurements.xlsx", sheet = "events")
events[16,1] <- as_datetime("2024-07-28 04:30:00")
################################################################################
# filter dna data for limit of detection

dna_data <- dna_data%>% 
  select(date_time, KUM1norm, KUM2norm, KUM3norm, Piezometer) %>%
  gather(key = "Tracer", value = "DNA_conc", -date_time, -Piezometer)
dna_dl_av1 <- subset(dna_data, !(Tracer == "KUM1norm" & DNA_conc < 5.57165E-09))
dna_dl_av2 <- subset(dna_dl_av1, !(Tracer == "KUM2norm" & DNA_conc < 1.79316E-09))
dna_dl_av3 <- subset(dna_dl_av2, !(Tracer == "KUM3norm" & DNA_conc < 7.56722E-09))

dna_dl_ex1 <- subset(dna_data, !(Tracer == "KUM1norm" & DNA_conc < 3.42272E-08))
dna_dl_ex2 <- subset(dna_dl_ex1, !(Tracer == "KUM2norm" & DNA_conc < 7.5094E-09))
dna_dl_ex3 <- subset(dna_dl_ex2, !(Tracer == "KUM3norm" & DNA_conc < 2.83538E-08))

dna_dl_min1 <- subset(dna_data, !(Tracer == "KUM1norm" & DNA_conc < 4.56644E-12))
dna_dl_min2 <- subset(dna_dl_min1, !(Tracer == "KUM2norm" & DNA_conc < 3.42454E-19))
dna_dl_min3 <- subset(dna_dl_min2, !(Tracer == "KUM3norm" & DNA_conc < 3.31999E-10))


PC03_dl_av <- subset(dna_dl_av3, (Piezometer == "PC03"))
PC14_dl_av <- subset(dna_dl_av3, (Piezometer == "PC14"))
PC15_dl_av <- subset(dna_dl_av3, (Piezometer == "PC15"))

PC03_dl_ex <- subset(dna_dl_ex3, (Piezometer == "PC03"))
PC14_dl_ex <- subset(dna_dl_ex3, (Piezometer == "PC14"))
PC15_dl_ex <- subset(dna_dl_ex3, (Piezometer == "PC15"))

PC03_dl_min <- subset(dna_dl_min3, (Piezometer == "PC03"))
PC14_dl_min <- subset(dna_dl_min3, (Piezometer == "PC14"))
PC15_dl_min <- subset(dna_dl_min3, (Piezometer == "PC15"))

bailer_data_dna <- subset(bailer_data, !(parameter == "Fluorescence" ))
dna_bailer_ex1 <- subset(bailer_data_dna, !(type == "KUM1norm" & value < 3.42272E-08))
dna_bailer_ex2 <- subset(dna_bailer_ex1, !(type == "KUM2norm" & value < 7.5094E-09))
dna_bailer_ex3 <- subset(dna_bailer_ex2, !(type == "KUM3norm" & value < 2.83538E-08))




################################################################################
# prep and stack datasets to create multiplots using facet_wrap and filtering by parameter


############ PC03
# insert all important events at PC03
events_PC03 <- events[c(1, 4, 7, 10, 11, 12, 16),]
events_PC03[2,2] <- "Injection KUM1 PC09"
events_PC03[3,2] <- "Injection Uranine PC09"
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


# prep sample fluo data
PC03_kit <- PC03_kit[,c(2,3)]
PC03_kit$type <- c("uranine_samples")
PC03_kit$parameter <- c("Fluorescence")
PC03_kit$plot_type <- c("point")
PC03_kit <- PC03_kit %>%
  rename(value = "Uranine")
PC03_kit <- subset(PC03_kit, !(value == 0))

# prep dna data 
PC03_dna_data_av <- PC03_dl_av[,c(1,3,4)] %>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC03_dna_data_av$parameter <- c("DNA")
PC03_dna_data_av$plot_type <- c("point")

PC03_dna_data_ex <-  PC03_dl_ex[,c(1,3,4)]%>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC03_dna_data_ex$parameter <- c("DNA")
PC03_dna_data_ex$plot_type <- c("point")

# combine the datasets
tracer_data_PC03_av <- bind_rows(PC03_dna_data_av, PC03_kit, PC03_fluo)
tracer_data_PC03_ex <- bind_rows(PC03_dna_data_ex, PC03_kit, PC03_fluo)
tracer_data_PC03_av <- tracer_data_PC03_av %>%
  mutate(value_transformed = ifelse(parameter == "DNA", log10(value), value))
tracer_data_PC03_ex <- tracer_data_PC03_ex %>%
  mutate(value_transformed = ifelse(parameter == "DNA", log10(value), value))


# plot  PC03 av
tracer_PC03_plots_av <- ggplot(tracer_data_PC03_av, aes(x = date_time, y = value, color = type)) +
  geom_point(data = tracer_data_PC03_av[tracer_data_PC03_av$plot_type == "point", ], size = 1) + 
  geom_line(data = tracer_data_PC03_av[tracer_data_PC03_av$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = tracer_data_PC03_av[tracer_data_PC03_av$parameter == "DNA", ], size = 0.5) +
  geom_line(data = tracer_data_PC03_av[tracer_data_PC03_av$type == "uranine_samples", ], size = 0.5) +
  geom_vline(xintercept = events_PC03$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(x = "Date (July 2024)", y = "Uranine (ppb)                                               DNA (g/L)") +
  scale_color_manual(values = c("#EE7600", "#EEB422","#DDA0DD", "mediumorchid4"), name = "Type of tracer", labels = c("KUM2", "KUM3", "Uranine (probe)", "Uranine (lab)")) +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Uranine concentration")))  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",  legend.title=element_text(size=15), 
    legend.text=element_text(size=10)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_PC03_DNA, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC03_fluo, aes(x = date_time, y = y_position, label = event), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)

# save the plot as a pdf
ggsave(tracer_PC03_plots_av, filename = "tracer_PC03_av.png", width = 14, height = 8, dpi = 500)


# plot PC03_ex
tracer_PC03_plots_ex <- ggplot(tracer_data_PC03_ex, aes(x = date_time, y = value, color = type)) +
  geom_point(data = tracer_data_PC03_ex[tracer_data_PC03_ex$plot_type == "point", ], size = 2) + 
  geom_line(data = tracer_data_PC03_ex[tracer_data_PC03_ex$plot_type == "line", ], linewidth = 1) +
  geom_line(data = tracer_data_PC03_ex[tracer_data_PC03_ex$parameter == "DNA", ], size = 1) +
  geom_line(data = tracer_data_PC03_ex[tracer_data_PC03_ex$type == "uranine_samples", ], size = 1) +
  geom_vline(xintercept = events_PC03$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 1) +
  labs(x = "Date (July 2024)", y = "Uranine (ppb)                                               DNA (g/L)") +
  scale_color_manual(values = c("#EEB422","#DDA0DD", "mediumorchid4"), name = "Type of tracer", labels = c("KUM3", "Uranine (probe)", "Uranine (lab)")) +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Uranine concentration")))  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",  legend.title=element_text(size=15), 
    legend.text=element_text(size=10), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_PC03_DNA, aes(x = date_time, y = y_position, label = event, size = 8), 
            vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC03_fluo, aes(x = date_time, y = y_position, label = event, size = 8), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)

# save the plot as a pdf
ggsave(tracer_PC03_plots_ex, filename = "tracer_PC03_ex.png", width = 18, height = 8, dpi = 500)




################################################################################

########### PC014 
# insert all important events for PC14
events_PC14 <- events[c(2, 5, 8, 10, 11, 16),]
events_PC14[2,2] <- "Injection KUM2 PC10"
events_PC14[3,2] <- "Injection Tinopal PC10"
events_PC14_DNA <- events_PC14[c(1, 2, 4, 5, 6), -3]
events_PC14_DNA[5,1] <- as_datetime("2024-07-28 06:30:00")
events_PC14_DNA[5,2] <- "Final stop pumps"
events_PC14_DNA$parameter <- c("DNA")
events_PC14_DNA$y_position = c(7e-08, 6.5e-08, 7e-08, 6.5e-08, 7e-08)
events_PC14_fluo <- events_PC14[3, -3]
events_PC14_fluo$parameter <- c("Fluorescence")
events_PC14_fluo$y_position = c(700)


# prep continuous fluo data
PC14_fluo <- PC14_fluo %>%
  rename(date_time = date_time, value = conc, type = tracer, parameter = type)
PC14_fluo$plot_type <- c("line")
PC14_fluo$parameter <- c("Fluorescence")

# prep sample fluo data
PC14_kit <- PC14_kit[,c(2,4)]
PC14_kit$type <- c("tinopal_samples")
PC14_kit$Tracer <- c("Fluorescence")
PC14_kit$plot_type <- c("point")
PC14_kit <- PC14_kit %>%
  rename(value = "Tinopal", parameter = Tracer)
PC14_kit <- subset(PC14_kit, !(value == 0))

# prep dna data
PC14_dna_data_av <- PC14_dl_av[,c(1,3,4)] %>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC14_dna_data_av$parameter <- c("DNA")
PC14_dna_data_av$plot_type <- c("point")

PC14_dna_data_ex <-  PC14_dl_ex[,c(1,3,4)]%>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC14_dna_data_ex$parameter <- c("DNA")
PC14_dna_data_ex$plot_type <- c("point")

# combine the datasets
tracer_data_PC14_av <- bind_rows(PC14_dna_data_av, PC14_kit, PC14_fluo)
tracer_data_PC14_ex <- bind_rows(PC14_dna_data_ex, PC14_kit, PC14_fluo)
tracer_data_PC14_av <- tracer_data_PC14_av %>%
  mutate(value_transformed = ifelse(parameter == "DNA", log10(value), value))
tracer_data_PC14_ex <- tracer_data_PC14_ex %>%
  mutate(value_transformed = ifelse(parameter == "DNA", log10(value), value))


# plot PC14 av
tracer_PC14_plots_av <- ggplot(tracer_data_PC14_av, aes(x = date_time, y = value, color = type)) +
  geom_point(data = tracer_data_PC14_av[tracer_data_PC14_av$plot_type == "point", ], size = 1) + 
  geom_line(data = tracer_data_PC14_av[tracer_data_PC14_av$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = tracer_data_PC14_av[tracer_data_PC14_av$parameter == "DNA", ], size = 0.5) +
  geom_line(data = tracer_data_PC14_av[tracer_data_PC14_av$type == "tinopal_samples", ], size = 0.5) +
  geom_vline(xintercept = events_PC14$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(x = "Date (July 2024)", y = "Fluorescence (ppb)                                               DNA (g/L)") +
  scale_color_manual(values = c("#EE7600", "#EEB422", "darkblue", "steelblue", "#DDA0DD"), name = "Type of tracer", labels = c("KUM2", "KUM3", "Tinopal (probe)", "Tinopal (lab)", "Uranine (probe)")) +
  # add back into line above
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom", legend.title=element_text(size=15), 
    legend.text=element_text(size=10), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_PC14_DNA, aes(x = date_time, y = y_position, label = event, size = 8), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC14_fluo, aes(x = date_time, y = y_position, label = event, size = 8), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)
ggsave(tracer_PC14_plots_av, filename = "tracer_PC14_av.png", width = 14, height = 8, dpi = 500)

# plot PC14 ex
tracer_PC14_plots_ex <- ggplot(tracer_data_PC14_ex, aes(x = date_time, y = value, color = type)) +
  geom_point(data = tracer_data_PC14_ex[tracer_data_PC14_ex$plot_type == "point", ], size = 2) + 
  geom_line(data = tracer_data_PC14_ex[tracer_data_PC14_ex$plot_type == "line", ], linewidth = 1) +
  geom_line(data = tracer_data_PC14_ex[tracer_data_PC14_ex$parameter == "DNA", ], size = 1) +
  geom_line(data = tracer_data_PC14_ex[tracer_data_PC14_ex$type == "tinopal_samples", ], size = 1) +
  geom_vline(xintercept = events_PC14$date_time, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 1) +
  labs(x = "Date (July 2024)", y = "Fluorescence (ppb)                                               DNA (g/L)") +
  scale_color_manual(values = c("#EE7600", "#EEB422", "darkblue", "steelblue", "#DDA0DD"), name = "Type of tracer", labels = c("KUM2", "KUM3", "Tinopal (probe)", "Tinopal (lab)", "Uranine (probe)")) +
  # add back into line above
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Fluorescent concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right", legend.title=element_text(size=15), 
    legend.text=element_text(size=10), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_PC14_DNA, aes(x = date_time, y = y_position, label = event, size = 8), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC14_fluo, aes(x = date_time, y = y_position, label = event, size = 8), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)
ggsave(tracer_PC14_plots_ex, filename = "tracer_PC14_ex.png", width = 18, height = 8, dpi = 500)



################################################################################
######### PC15
# load relevant events that happened at PC15
events_PC15 <- events[c(3, 6, 9, 10, 11, 13, 14, 15, 16),]
events_PC15[2,2] <- "Injection KUM3 PC11"
events_PC15[3,2] <- "Injection Uranine PC11"
events_PC15[2,1] <- as.POSIXct("2024-07-16 24:00:00")
events_PC15[3,1] <- as.POSIXct("2024-07-16 24:00:00")
events_PC15_DNA <- events_PC15[c(1, 2, 4, 5, 9), -3]
events_PC15_DNA$parameter <- c("DNA")
events_PC15_DNA$y_position = c(5e-07, 4.5e-07, 5e-07, 4.5e-07, 5e-07)
events_PC15_fluo <- events_PC15[c(3, 6, 7, 8), -3]
events_PC15_fluo$parameter <- c("Fluorescence")
events_PC15_fluo$y_position = c(15, 15, 13.5, 15)
events_PC15_fluo[2,2] <- "Install continuous probe"
events_PC15_fluo[3,2] <- "Outage continuous probe"
events_PC15_fluo[3,3] <- "Fluorescence"
events_PC15_fluo[4,2] <- "Reactivation continuous probe"



# prep continuous fluo data
PC15_fluo <- PC15_fluo[c(-493, -636), c(1, 15)] # select relevant columns and remove 2 outlier values
PC15_fluo$type <- c("uranine")
PC15_fluo$parameter <- c("Fluorescence")
PC15_fluo$plot_type <- c("line")
PC15_fluo <- PC15_fluo %>% 
  rename(value = "Uranine [µg/l]") %>% # rename columns
  na.omit(PC15_fluo)


# prep sample fluo data
PC15_kit <- PC15_kit[,c(2,3)]
PC15_kit$type <- c("uranine_samples")
PC15_kit$parameter <- c("Fluorescence")
PC15_kit$plot_type <- c("point")
PC15_kit <- PC15_kit %>%
  rename(date_time = date_time, value = "Uranine", parameter = parameter, type = type)
PC15_kit <- subset(PC15_kit, !(value == 0))
# prep dna data
PC15_dna_data_av <- PC15_dl_av[,c(1,3,4)] %>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC15_dna_data_av$parameter <- c("DNA")
PC15_dna_data_av$plot_type <- ("point")


PC15_dna_data_ex <-  PC15_dl_ex[,c(1,3,4)]%>%
  rename(date_time = date_time, value = "DNA_conc", type = Tracer)
PC15_dna_data_ex$parameter <- c("DNA")
PC15_dna_data_ex$plot_type <- c("point")

# combine the datasets
tracer_data_PC15_av <- bind_rows(PC15_dna_data_av, PC15_kit, PC15_fluo)
tracer_data_PC15_ex <- bind_rows(PC15_dna_data_ex, PC15_kit, PC15_fluo)
tracer_data_PC15_av <- tracer_data_PC15_av %>%
  mutate(value_transformed = ifelse(parameter == "DNA", log10(value), value))
tracer_data_PC15_ex <- tracer_data_PC15_ex %>%
  mutate(value_transformed = ifelse(parameter == "DNA", log10(value), value))


# plots PC15 av
tracer_PC15_plots_av <- ggplot(tracer_data_PC15_av, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(data = tracer_data_PC15_av[tracer_data_PC15_av$plot_type == "point", ], size = 1) + 
  geom_line(data = tracer_data_PC15_av[tracer_data_PC15_av$plot_type == "line", ], linewidth = 0.5) +
  geom_line(data = tracer_data_PC15_av[tracer_data_PC15_av$parameter == "DNA", ], size = 0.5) +
  geom_line(data = tracer_data_PC15_av[tracer_data_PC15_av$type == "uranine_samples",], size = 0.5) +
  geom_vline(xintercept = events_PC15$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 0.5) +
  labs(x = "Date (July 2024)", y = "Uranine (ppb)                                               DNA (g/L)") +
  scale_color_manual(values = c("#EE7600", "#EEB422","#DDA0DD", "mediumorchid4"), name ="Type of tracer", labels = c("KUM2", "KUM3", "Uranine (probe)", "Uranine (lab)")) +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Uranine concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom", legend.title=element_text(size=15), 
    legend.text=element_text(size=10)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_PC15_DNA, aes(x = date_time, y = y_position, label = event), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC15_fluo, aes(x = date_time, y = y_position, label = event), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)
ggsave(tracer_PC15_plots_av, filename = "tracer_PC15_av.png", width = 14, height = 8, dpi = 500)


# plots PC15 ex
tracer_PC15_plots_ex <- ggplot(tracer_data_PC15_ex, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(data = tracer_data_PC15_ex[tracer_data_PC15_ex$plot_type == "point", ], size = 2) + 
  geom_line(data = tracer_data_PC15_ex[tracer_data_PC15_ex$plot_type == "line", ], linewidth = 1) +
  geom_line(data = tracer_data_PC15_ex[tracer_data_PC15_ex$parameter == "DNA", ], size = 1) +
  geom_line(data = tracer_data_PC15_ex[tracer_data_PC15_ex$type == "uranine_samples",], size = 1) +
  geom_vline(xintercept = events_PC15$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 1) +
  labs(x = "Date (July 2024)", y = "Uranine (ppb)                                               DNA (g/L)") +
  scale_color_manual(values = c("#EEB422","#DDA0DD", "mediumorchid4"), name ="Type of tracer", labels = c("KUM3", "Uranine (probe)", "Uranine (lab)")) +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(DNA = "DNA concentration", Fluorescence = "Uranine concentration") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right", legend.title=element_text(size=15), 
    legend.text=element_text(size=10), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_PC15_DNA, aes(x = date_time, y = y_position, label = event, size = 8), 
            vjust = 1.5 , color = "red", 
            inherit.aes = FALSE) +
  geom_text(data = events_PC15_fluo, aes(x = date_time, y = y_position, label = event, size = 8), 
            vjust = 1.5, color = "red", 
            inherit.aes = FALSE)
ggsave(tracer_PC15_plots_ex, filename = "tracer_PC15_ex.png", width = 18, height = 8, dpi = 500)



# repeat for bailer locations
# insert relevant bailer events
events_bailer <- events[c(1, 4, 7, 10, 11, 16), -3]
events_bailer$event <- c("Start pump", "Injection DNA", "Injection Fluorescence", "Stop pumps", "Start pumps", "Final stop pumps")

events_bailer_DNA <- events_bailer[c(1, 2, 4, 5, 6),]
events_bailer_DNA$Piezometer <- c("PC07")
events_bailer_DNA$y_position = c(6.5e-08, 6e-08, 6.5e-08, 6e-08, 6.5e-08)
events_bailer_DNA_log <- events_bailer_DNA
events_bailer_DNA_log$y_position = c(-3, -4, -3, -4, -3)
events_bailer_fluo <- events_bailer[c(1, 3, 4, 5, 6),]
events_bailer_fluo$Piezometer <- c("PC07")
events_bailer_fluo$y_position = c(40, 35, 40, 35, 40)


bailer_fluo <- bailer_data[c(1:31),]
bailer_fluo <- bailer_fluo[c(6:11),]


# create plot for dna in bailers
dna_bailer_plots_ex <- ggplot(dna_bailer_ex3, aes(x = date_time, y = value, color = type)) + # here variable type is used to differentiate what is shown
  geom_point(size = 2) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = events_bailer_DNA$date_time, # adds a vertical line to show selected events
             linetype = "dashed", 
             color = "red", 
             linewidth = 1) +
  labs(x = "Date (July 2024)", y = "DNA (g/L)") +
  scale_color_manual(values = c("#B03060", "#EE7600", "#EEB422"), name = "Type of tracer", labels = c("KUM1", "KUM2", "KUM3")) +
  facet_wrap(~ Piezometer, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(PC07 = "PC07", PC12 = "PC12", PC16 = "PC16") ) )  +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right", legend.title=element_text(size=15), 
    legend.text=element_text(size=10), axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  scale_x_datetime(limits = as.POSIXct(c("2024-07-15", "2024-07-30")), date_breaks = "1 day", date_labels = '%d') +
  geom_text(data = events_bailer_DNA, aes(x = date_time, y = y_position, label = event, size = 8), vjust = 1.5 , color = "red", 
            inherit.aes = FALSE)
ggsave(dna_bailer_plots_ex, filename = "dna_bailer_plots_ex.jpeg", width = 14, height = 8, dpi = 1000)
