# ---- header ----
#
# author: Hannah Ferriby
# date created: 2025-12-02
# email:  hannah.ferriby@tetratech.com
# 
# script name: comparison_analysis.R
# 
#
# script description:
# compare calculated estimates of Cwater to measured water concentrations
#
#
# ---- notes: ----
#
#
# ---- to do: ----
#
#
# ---- load libraries ----
library(dplyr)
library(readr)
library(ggplot2)

#---- load data----
#fish tissue
nars <- read_csv('output/test_cwater.csv')
#surface water
wqp <- read_csv('output/EPATADA_Original_data_with_flags_tags_20251202.csv')

t <- wqp %>%
  filter(Abbrev.Name == 'PFDoA') %>%
  filter(sample_lower_than_detection_limit_flag == 'Uncensored')

#----combine datasets----
nars_sel <- nars %>%
  select(`Site ID`, State, Analyte, `Units 1`, Cwater,
         Cwater_lower, Cwater_upper, Cwater_units) %>%
  unique() %>%
  rename(SiteID = `Site ID`,
         Units = `Units 1`,
         Result = Cwater,
         Abbrev.Name = Analyte) %>%
  mutate(source = 'NARS') %>%
  filter(!is.na(Abbrev.Name))

wqp_sel <- wqp %>%
  filter(sample_lower_than_detection_limit_flag == "Uncensored") %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'UG/KG') %>%
  filter(!is.na(Abbrev.Name)) %>%
  select(TADA.MonitoringLocationIdentifier, Abbrev.Name, TADA.ResultMeasureValue,
         TADA.ResultMeasure.MeasureUnitCode, StateCode) %>%
  unique() %>%
  rename(SiteID = TADA.MonitoringLocationIdentifier,
         Result = TADA.ResultMeasureValue,
         Units = TADA.ResultMeasure.MeasureUnitCode,
         State = StateCode) %>%
  mutate(source = 'WQP',
         Result = Result *1000, #convert from ug/l to ng/l
         Units = 'NG/L')
  

combo <- nars_sel %>%
  full_join(wqp_sel) %>%
  mutate(Abbrev.Name = factor(Abbrev.Name, levels = c('PFBA', 'PFPeA', 'PFHxA',
                                                      'PFHpA', 'PFOA', 'PFNA',
                                                      'PFDA', 'PFUnA', 'PFDoA',
                                                      'PFTrDA', 'PFTeDA', 'PFBS',
                                                      'PFHxS', 'PFHpS', 'PFOS',
                                                      'PFOSA', 'GenX'))) 



#row check
nrow(nars_sel)
nrow(wqp_sel)
nrow(nars_sel)+ nrow(wqp_sel)
nrow(combo)

combo_filt <- combo %>%
  filter(!Abbrev.Name %in% c("GenX", "PFBS", "PFHxA"))

#----boxplots----
give.n <- function(x){
  return(c(y = (min(x) - 0.5), label = length(x)))
}

ggplot(data = combo_filt, aes(x = source, y = Result, fill = source)) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text", size = 2.5, position = position_dodge(width = 0.75)) +
  facet_wrap(~Abbrev.Name, scale = 'free_y', nrow = 2) +
  scale_y_log10() + 
  labs(x = 'Data Source', y = 'Result (ng/L)') + 
  theme_bw() +
  scale_fill_manual(values = c("#8390fa", "#fac748")) + 
  theme(legend.position = 'none',
        strip.background = element_rect(fill="#fbf6ef"))

ggsave('output/NARS_figures/wqp_vs_nars_boxplot.jpg', units = 'in',
       dpi = 300, width = 12, height = 6)

