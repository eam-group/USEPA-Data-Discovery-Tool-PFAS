
####NARS data review & water concentration calculation

#Written by: Matt Dunn (Tetra Tech) & Hannah Ferriby (Tetra Tech)
#Date created: 6/24/25
#Date updated: 10/24/25

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(ggpubr)
library(sf)


options(scipen=999) ### no scientific notation


####Load Data####
#concentration data 
data <- read_xlsx('Data/NARS/final-nla-2022-pfas-public-release-file-8-19-24_0.xlsx') %>%
  mutate(`EPA Sample ID` = as.factor(`EPA Sample ID`))

#fish info
species_data <- read_xlsx('Data/NARS/final-nla-2022-pfas-public-release-file-8-19-24_0_Species.xlsx') %>%
  mutate(`EPA Sample ID` = as.factor(`EPA Sample ID`)) %>%
  select(`EPA Sample ID`, Family, `Species - Scientific Name`, 
         `Species - Common Name`) %>%
  distinct(`EPA Sample ID`, .keep_all=TRUE)

####Join Datasets####

###combine species data with concentration data, this is now concentration and species for each site
#Convert NARS data to ng/kg
combined_data <- left_join(data, species_data, by="EPA Sample ID") %>%
  mutate(`Units 1` = 'ug/kg') %>% #ng/g to ug/kg are equivalent
  select(!`Units 2`)

##count data 
unique_count_species <- length(unique(species_data$`Species - Scientific Name`))

####Data Processing####
detect_summary <- combined_data %>%
  group_by(Analyte, `Units 1`) %>%
  reframe(Analyte = Analyte,
          `Units 1` = `Units 1`,
          DL_avg = mean(MDL, na.rm=T),
          DL_median = median(MDL, na.rm=T),
          DL_std = sd(MDL, na.rm=T),
          DL_min = min(MDL, na.rm=T),
          DL_max = max(MDL, na.rm=T)) %>%
  unique() %>%
  filter(!is.na(`Units 1`))


combined_data2 <- combined_data %>%
  left_join(detect_summary, by = c('Analyte', 'Units 1')) %>%
  mutate(detection_limit_value_flag = ifelse(MDL >= 2*DL_median, 'FAIL', 'PASS'),
         sample_lower_than_detection_limit_flag = ifelse(Amount < MDL, 'FAIL', 'PASS'),
         analytical_lab_flag = ifelse(is.na(`Lab Flag`), 'PASS', 'FAIL'))


nrow(filter(combined_data2, analytical_lab_flag == 'PASS')) #1157

####Analysis####
#BAF = Cbiota / Cwater
#BAF from Burkhard 2021
#Cbiota from NARS
#Solve for Cwater

BAFs <- data.frame(Analyte = c('PFBA', 'PFPeA', 'PFHxA', 'PFHpA', 'PFOA', 'PFNA',
                               'PFDA', 'PFUnA', 'PFDoA', 'PFTrDA', 'PFTeDA',
                               'PFBS', 'PFHxS', 'PFHpS', 'PFOS', 'PFOSA'),
                   BAF = c(0.47, -0.31, 0.21, -0.16, 0.93, 2.16, 3.10, 3.88,
                           4.77, 4.66, 4.38, 1.35, 1.30, 2.20, 3.18, 2.95),
                   BAF_std = c(0.96, 0.57, 1.33, 1.27, 1.15, 0.78, 0.50, 0.8,
                               1.72, 0.16, 0, 0.84, 0.90, 0, 0.68, 0.94))


### lower boundary = higher BAF, upper boundary = lower BAF
Cwater_analysis <- combined_data2 %>%
  left_join(BAFs, by = 'Analyte') %>%
  mutate(Cwater = (Amount/(10^BAF))*1000,
         Cwater_lower = (Amount/(10^(BAF+BAF_std)))*1000,
         Cwater_upper = (Amount/(10^(BAF-BAF_std)))*1000,
         Cwater_units = 'ng/L')

write_csv(Cwater_analysis, 'output/test_cwater.csv')

paste0("J flags are ", round(nrow(filter(combined_data2, str_detect(`Lab Flag`, 'J')))/
                               nrow(combined_data2),4) * 100, '% of all samples')

paste0("J flags are ", round(nrow(filter(Cwater_analysis, !is.na(Cwater) & str_detect(`Lab Flag`, 'J')))/
         nrow(Cwater_analysis),4) * 100, '% of Cwater calculations')

####Summary Table####
summary_cwater <- Cwater_analysis %>%
  filter(!is.na(Cwater)) %>%
  select(Analyte, Cwater) %>%
  unique() %>%
  group_by(Analyte) %>%
  reframe(Analyte = Analyte,
          Units = 'ng/L',
          n_samples = n(),
          Min = round(min(Cwater),4),
          Q_25 = round(quantile(Cwater, 0.25),4),
          Med = round(median(Cwater),4),
          Mean = round(mean(Cwater),4),
          Q_75 = round(quantile(Cwater, 0.75),4),
          Max = round(max(Cwater),4)) %>%
  unique()

write_csv(summary_cwater, 'output/NARS_figures/summary_cwater.csv')

####Water Plots####

####Boxplot####
#boxplot with limits
#pfoa acute - 3100 ug/L
#pfoa chronic - 100 ug/L
#pfos acute - 71 ug/L
#pfos chronic - 0.25 ug/L

#To make acute/chronic lines only appear over their specific analyte
#Define thresholds per analyte
thresholds <- data.frame(
  Analyte = c("PFOA", "PFOA", "PFOS", "PFOS", 'PFBA', 'PFHxA', 'PFNA', 'PFDA',
              'PFBS', 'PFHxS'),
  Type = c("Acute", "Chronic", "Acute", "Chronic", 'Benchmark', 'Benchmark'
           , 'Benchmark', 'Benchmark', 'Benchmark', 'Benchmark'),
  Threshold = c(3100000, 100000, 71000, 250, 5300000, 4800000, 650000,
                500000, 5000000, 210000)
  )

thresholds$x <- as.numeric(factor(thresholds$Analyte))
thresholds$xmin <- thresholds$x - 0.3  # boxplot default width is 0.6
thresholds$xmax <- thresholds$x + 0.3

Cwater_4_plots <- Cwater_analysis %>%
  filter(!is.na(Cwater)) %>%
  pivot_longer(cols = c(Cwater, Cwater_upper, Cwater_lower)) %>%
  left_join(thresholds, by = 'Analyte') %>%
  mutate(Analyte = factor(Analyte,
                          levels = c('PFBA', 'PFPeA', 'PFHpA', 'PFOA', 'PFNA',
                                     'PFDA', 'PFUnA', 'PFDoA', 'PFTrDA', 'PFTeDA',
                                     'PFHxS', 'PFHpS', 'PFOS', 'PFOSA')))

cwater_name_list <- c(
  'Cwater_lower' = 'Cwater Lower',
  'Cwater' = 'Cwater',
  'Cwater_upper' = 'Cwater Upper'
)

ggplot() + 
  #boxplot of detected samples
  geom_boxplot(data = Cwater_4_plots,
               aes(x = Analyte, y = value)) +
  geom_segment(data = Cwater_4_plots, 
               aes(x = as.numeric(Analyte) - 0.3,
                   xend = as.numeric(Analyte) + 0.3,
                   y = Threshold, yend = Threshold,
                   color = Type),
                 linetype = "solid", size = 0.8) + 
  scale_y_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = c(0.00001, 0.001, 0.1,  10,
                           1000, 100000, 10000000)) + 
  ylab('Estimated Value (ng/L)') + 
  facet_wrap(~name, nrow = 3, labeller = as_labeller(cwater_name_list)) + 
  theme_bw() +
  scale_color_manual(name = 'Threshold', values = c("Acute" = "#fac748",
                                                    "Chronic" = "#8390fa",
                                                    "Benchmark" = "#1d2f6f"),
                     na.translate = F) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background =element_rect(fill="#fbf6ef"),
        legend.position = 'top')

ggsave('output/NARS_figures/Cwater_threshold_boxplots.jpg', units = 'in',
       height = 7, width = 6, dpi = 300)

####Maps####
states <- st_read('data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp') %>%
  filter(!STATEFP %in% c('60', '66', '69', '78',
                         '15', '02', '72'))

#Number of Samples by State
data_4_maps <- combined_data2 %>%
  left_join(states, by = c('State' = 'STUSPS')) %>%
  group_by(State) %>%
  reframe(State = State,
          geometry = geometry,
          n = n()) %>%
  unique() %>%
  st_as_sf()

ggplot() +
  geom_sf(data = states, fill = 'gray', color = 'black') + 
  geom_sf(data = data_4_maps, aes(fill = n),
          color = 'black') + 
  theme_bw() +
  scale_fill_continuous(name = 'Total Number of\n Fish Samples',
                        palette = c("#1d2f6f","#8390fa", "#fac748")) +
  theme(legend.position = 'top')

ggsave('output/NARS_figures/fish_samples_map.jpg', units = 'in',
       height = 5, width = 6, dpi = 300)



#Avg concentration by state
data_4_maps2 <- combined_data2 %>%
  filter(Analyte %in% c('PFBA', 'PFPeA', 'PFHpA', 'PFOA', 'PFNA',
                        'PFDA', 'PFUnA', 'PFDoA', 'PFTrDA', 'PFTeDA',
                        'PFHxS', 'PFHpS', 'PFOS', 'PFOSA')) %>%
  left_join(states, by = c('State' = 'STUSPS')) %>%
  group_by(State, Analyte) %>%
  reframe(State = State,
          geometry = geometry,
          Analyte = Analyte,
          value = mean(Amount, na.rm = T)) %>%
  unique() %>%
  st_as_sf() 

ggplot() +
  geom_sf(data = states, fill = 'gray', color = 'black') + 
  geom_sf(data = data_4_maps2, aes(fill = value),
          color = 'black') + 
  theme_bw() +
  facet_wrap(~Analyte, nrow = 7) +
  scale_fill_continuous(name = 'Average Sample\nMeasurement (ug/kg)',
                        palette = c("#1d2f6f","#8390fa", "#fac748"),
                        na.value = 'gray',
                        trans = "log10") +
  theme(legend.position = 'top',
        strip.background = element_rect(fill="#fbf6ef"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/NARS_figures/avg_samples_map.jpg', units = 'in',
       height = 9, width = 6.5, dpi = 300)


#Average predicted Cwater by state
data_4_maps3 <- Cwater_analysis %>%
  filter(Analyte %in% c('PFBA', 'PFPeA', 'PFHpA', 'PFOA', 'PFNA',
                        'PFDA', 'PFUnA', 'PFDoA', 'PFTrDA', 'PFTeDA',
                        'PFHxS', 'PFHpS', 'PFOS', 'PFOSA')) %>%
  left_join(states, by = c('State' = 'STUSPS')) %>%
  group_by(State, Analyte) %>%
  reframe(State = State,
          geometry = geometry,
          Analyte = Analyte,
          value = mean(Cwater, na.rm = T)) %>%
  unique() %>%
  st_as_sf() 

ggplot() +
  geom_sf(data = states, fill = 'gray', color = 'black') + 
  geom_sf(data = data_4_maps3, aes(fill = value),
          color = 'black') + 
  theme_bw() +
  facet_wrap(~Analyte, nrow = 7) +
  scale_fill_continuous(name = 'Average Cwater\nEstimation (ng/L)',
                        palette = c("#1d2f6f","#8390fa", "#fac748"),
                        na.value = 'gray',
                        trans = "log10") +
  theme(legend.position = 'top',
        strip.background = element_rect(fill="#fbf6ef"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(angle = 45, hjust = 1))

ggsave('output/NARS_figures/avg_estimated_map.jpg', units = 'in',
       height = 9, width = 6.5, dpi = 300)


####**OLD**####
#####Boxplot#####
# # Map analyte names to x-axis positions
# thresholds$x <- as.numeric(factor(thresholds$Analyte))
# thresholds$xmin <- thresholds$x - 0.3  # boxplot default width is 0.6
# thresholds$xmax <- thresholds$x + 0.3
# 
# estimate <- ggplot() + 
#   geom_boxplot(data = Cwater_analysis, aes(x = Analyte, y = Cwater*1000))+
#   geom_segment(data = thresholds,
#                aes(x = xmin, xend = xmax,
#                    y = Threshold, yend = Threshold,
#                    color = Type),
#                linetype = "dashed", size = 0.8) +
#   scale_y_log10(waiver()) +
#   ylab('Water Concentration (ng/L)') +
#   theme_classic() +
#   scale_color_manual(name = 'Standard', values = c('#03a5fc', '#d10804'))+
#   ggtitle("Estimated Water Concetrations from BAF")
# 
# ###Upper Limit
# 
# upperestimate <- ggplot() + 
#   geom_boxplot(data = Cwater_analysis, aes(x = Analyte, y = Cwater_upper*1000))+
#   geom_segment(data = thresholds,
#                aes(x = xmin, xend = xmax,
#                    y = Threshold, yend = Threshold,
#                    color = Type),
#                linetype = "dashed", size = 0.8) +
#   scale_y_log10(waiver()) +
#   ylab('Water Concentration (ng/L)') +
#   theme_classic() +
#   scale_color_manual(name = 'Standard', values = c('#03a5fc', '#d10804'))+
#   ggtitle("Estimated Water Concetrations from BAF + StDev")
# 
# 
# ###Lower Limit
# lowerestimate <- ggplot() + 
#   geom_boxplot(data = Cwater_analysis, aes(x = Analyte, y = Cwater_lower*1000))+
#   geom_segment(data = thresholds,
#                aes(x = xmin, xend = xmax,
#                    y = Threshold, yend = Threshold,
#                    color = Type),
#                linetype = "dashed", size = 0.8) +
#   scale_y_log10(waiver()) +
#   ylab('Water Concentration (ng/L)') +
#   theme_classic() +
#   scale_color_manual(name = 'Standard', values = c('#03a5fc', '#d10804'))+
#   ggtitle("Estimated Water Concetrations from BAF - StDev")
# lowerestimate
# 
# ##### all 3 together 
# 
# long_data <- pivot_longer(data=Cwater_analysis, cols=Cwater:Cwater_upper, names_to="Estimation", values_to = "Concentration")
# long_data
# 
# label_names <- list(
#   'Cwater'="Estimation",
#   'Cwater_lower' = "Lower Bound",
#   'Cwater_upper'="Upper Bound"
# )
# 
# labeller_function <- function(variable, value){
#   return(label_names[value])
# }
# 
# 
# ggplot() + 
#   geom_boxplot(data = long_data, aes(x = Analyte, y = Concentration*1000))+
#   geom_segment(data = thresholds,
#                aes(x = xmin, xend = xmax,
#                    y = Threshold, yend = Threshold,
#                    color = Type),
#                linetype = "dashed", size = 0.8) +
#   facet_wrap(~Estimation, scales="fixed", labeller=labeller_function)+
#   scale_y_log10(waiver()) +
#   ylab('Water Concentration (ng/L)') +
#   theme_classic() +
#   scale_color_manual(name = 'Standard', values = c('#03a5fc', '#d10804'))+
#   ggtitle("Estimated Water Concetrations from NARS Fish Tissue")
# 
# ggsave('output/NARS_figures/estimated_Water_Conc.jpg', 
#        height = 5, width = 8, dpi = 500)
# 
# 
# #####Frequency#####
# freq_species_df <- as.data.frame(table(species_data$`Species - Scientific Name`))
# freq_name_df <- as.data.frame(table(species_data$`Species - Common Name`))
# 
# ggplot(freq_species_df, aes(x = Var1, y = Freq)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Frequency of Species Sampled", x = "Values", y = "Frequency") +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
#   #scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
#   xlab("Species")+
#   ylab("Frequency of Sampling")+
#   theme(axis.text.y = element_text(size =10))
# 
# ggplot(freq_name_df, aes(x = Var1, y = Freq)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Frequency of Species Sampled", x = "Values", y = "Frequency") +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
#   #scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
#   xlab("Species")+
#   ylab("Frequency of Sampling")+
#   theme(axis.text.y = element_text(size =10))
# 
# 
# 
# #####Concentration#####
# unique_count <- length(unique(data$Analyte))
# unique_values <- unique(data$Analyte)
# freq_table <- table(data$Analyte)
# 
# 
# cleaned_data <- data %>%
#   filter(!is.na(Amount))
# ##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)
# 
# 
# unique_count_clean <- length(unique(cleaned_data$Analyte))
# ##24 compounds with actual data 
# 
# ### remove NAs in analyte detection for combined data
# 
# cleaned_data_combined <- combined_data %>%
#   filter(!is.na(Amount))
# ##2001 measurements left (2001/16520 = 87.8% were non detects, 12.2% were detects)
# 
# unique_count_clean <- length(unique(cleaned_data$Analyte))
# ##24 compounds with actual data 
# 
# unique_values_clean <- unique(cleaned_data$Analyte)
# unique_values_clean
# ### created list of compounds
# ##PFCA: PFBA (3), PFPeA (1), PFHpA (5), PFOA (4), PFNA (95), PFDA (330), PFUnA (349), PFDoA (293), PFTrDA (205), PFTeDA (164)
# ##PFSA: PFHxS (18), PFHpS (14), PFOS (357), PFNS (7), PFDS (92), PFDoS (1)
# ##FTCA: 3:3 FTCA (1), 5:3 FTCA (1), 7:3 FTCA (7)
# ##FASA: PFOSA (10)
# ##FOSE: N-EtFOSE (18), N-MeFOSAA (15), N-EtFOSAA (9)
# ##Other: PFMBA (2)
# 
# freq_table_cleaned <- table(cleaned_data$Analyte)
# freq_table_cleaned
# 
# barplot(freq_table_cleaned, main = "Frequency of Unique Values", col = "skyblue", xlab = "Values", ylab = "Frequency")
# 
# freq_df <- as.data.frame(table(cleaned_data$Analyte))
# 
# ggplot(freq_df, aes(x = Var1, y = Freq)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Compounds Detected Above Detection Limits In Fish Tissue", x = "Values", y = "Frequency") +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
#   scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
#   xlab("Analyte")+
#   ylab("Frequency of Detection")+
#   theme(axis.text.y = element_text(size =10))
# 
# 
# unique_sites_clean <- unique(cleaned_data$`Site ID`)
# unique_sites_clean
# ###383 sample IDs with atleast one data point above detection limits
# 
# unique_states_clean <- unique(cleaned_data$State)
# unique_states_clean
# ###47 states (missing only Hawaii, Alaska, North Dakota)
# 
# freq_table_cleaned_states <- table(cleaned_data$State)
# freq_table_cleaned_states
# freq_state_df <- as.data.frame(table(cleaned_data$State))
# 
# ggplot(freq_state_df, aes(x = Var1, y = Freq)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Detections per State", x = "Values", y = "Frequency") +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
#   xlab("State")+
#   ylab("Frequency of Detection")+
#   theme(axis.text.y = element_text(size =10))
# 
# #####Stats by group#####
# 
# result <- cleaned_data %>%
#   group_by(Analyte) %>%
#   summarise(Mean = mean(Amount), Median = median(Amount), st.dev = sd(Amount))
# print(result)
# 
# #####Box Plot#####
# 
# cleaned_data$Analyte <- as.factor(cleaned_data$Analyte)
# cleaned_data$Amount <- as.numeric(cleaned_data$Amount)
# 
# functional_df <- tibble(Analyte = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"), 
#                         Type = c("PFECA", "FTCA", "FTCA", "FTCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFCA", "PFSA", "PFSA", "PFSA", "FOSE", "FOSAA", "FOSAA", "FASA", "PFSA", "PFSA", "PFSA"))
# 
# plots<- left_join(cleaned_data, functional_df, by="Analyte")
# 
# 
# 
# 
# 
# p <- ggplot(plots, aes(x=Analyte, y=Amount, color=plots$Type, fill = plots$Type))+ 
#   geom_boxplot(outlier.colour="black", outlier.shape=16,
#                outlier.size=2, notch=FALSE)+
#   scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size =10))+
#   ylab("Concentration (ppb)")+
#   theme(axis.text.y = element_text(size =10))+
#   ggtitle("Detectable Tissue Concentrations in Fish - 2022 (ppb)")+
#   scale_y_log10()+
#   scale_color_manual('Group', values=c('#A52A2A', '#720c99','darkgreen','#a76004','darkred','black','blue')) +
#   scale_fill_manual('Group', values = c('#A0522D', 'purple','green','orange','red','gray50','#5b8cf0'))
# p
# ggsave('output/NARS_figures/fish tissue concentrations.jpg', 
#        height = 5, width = 8, dpi = 500)
# 
# cleaned_data
# 
# #### plots for just PFOA and PFOS 
# 
# plots_PFOA_PFOS <- plots%>%
#   filter(Analyte %in% c('PFOA', 'PFOS'))
# 
# pp <- ggplot(plots_PFOA_PFOS, aes(x=Analyte, y=Amount, color=Type))+ 
#   geom_boxplot(outlier.colour="black", outlier.shape=16,
#                outlier.size=2, notch=FALSE)+
#   #scale_x_discrete(limits = c("PFMBA", "3:3 FTCA", "5:3 FTCA", "7:3 FTCA", "PFBA", "PFPeA", "PFHpA", "PFOA", "PFNA", "PFDA", "PFUnA", "PFDoA", "PFTrDA", "PFTeDA", "PFHxS", "PFHpS", "PFOS", "N-EtFOSE", "N-MeFOSAA", "N-EtFOSAA", "PFOSA", "PFNS", "PFDS", "PFDoS"))+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size =14))+
#   ylab("Concentration (ppb)")+
#   theme(axis.text.y = element_text(size =14))+
#   ggtitle("Detectable Tissue Concentrations in Fish - 2022")+
#   scale_y_log10()+
#   scale_color_manual('Group', values=c('red','blue'))
# 
# pp

