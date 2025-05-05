#PFAS Data Pull
#Following instructions outlined by Matt Dunn in 
#notebook entry from 2025-04-23

#Written by: Hannah Ferriby, hannah.ferriby@tetratech.com
#Date created: 2025-5-2
#Date updated: 2025-5-5


####Set Up####
library(EPATADA)
library(tidyverse)

####Download####
data <- TADA_DataRetrieval(characteristicName = c('PFOA ion'
                                                  , 'Perfluorooctanoic acid'
                                                  # , 'PERFLUOROOCTANOIC ACID'
                                                  ,'Perfluorooctanesulfonate'
                                                  , 'Perfluorooctane sulfonic acid'
                                                  # , 'Potassium perfluorooctanesulfonate'
                                                  , 'Perfluorooctanesulfonate (PFOS)'
                                                  # , 'POTASSIUM PERFLUOROOCTANESULFONATE'
                                                  ),
                           sampleMedia = c('Water', 'Tissue'), 
                           applyautoclean = T)


####Filter####
unique(data$ActivityMediaSubdivisionName)

data_filt <- data %>%
  filter(ActivityMediaSubdivisionName == 'Surface Water')

####Export####
write_csv(data_filt, 'output/data_pull_SW_TISS_ONLY.csv')
