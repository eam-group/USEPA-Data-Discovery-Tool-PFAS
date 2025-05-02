#PFAS Data Pull
#Following instructions outlined by Matt Dunn in 
#notebook entry from 2025-04-23

#Written by: Hannah Ferriby, hannah.ferriby@tetratech.com
#Date created: 2025-5-2
#Date updated: 


####Set Up####
library(EPATADA)

####Download####
data <- TADA_DataRetrieval(characteristicName = c('PFOA ion'
                                                  # , 'Perfluoroctanoic acid'
                                                  # , 'PERFLUOROOCTANOIC ACID'
                                                  ,'Perfluorooctanesulfonate'
                                                  , 'Perfluorooctane sulfonic acid'
                                                  # , 'Potassium perfluorooctanesulfonate'
                                                  , 'Perfluorooctanesulfonate (PFOS)'
                                                  # , 'POTASSIUM PERFLUOROOCTANESULFONATE'
                                                  ))
