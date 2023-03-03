


rm(list=ls(all=TRUE))


# Library to perform colum medians and other usefull matrix algebra computations. 
library(matrixStats)

# Library for the latex exports in the nice format.
library(xtable)

# library(Matrix) for blog-diagonal matrixes creation and other matrix manipulations.
library(Matrix)

# This package is requred to run in RScript mode rathen than interactive mode.
library(methods)

# Loading package requred to read library(readxl)
library(readxl)

# Loading library(rjson) for json files. 
library(rjson)




# Saving the trends data as RData file.
load( file = paste("../../R_Data/google_trends_grob_data.RData") )
load( file = paste("../../R_Data/google_trends_pomynky_data.RData") )
load( file = paste("../../R_Data/google_trends_ritualnie_uslugi_data.RData") )
load( file = paste("../../R_Data/google_trends_truna_data.RData") )
load( file = paste("../../R_Data/google_trends_rytualni_posluhy_data.RData") )
ls()


pandemic_start <- as.Date("2020-03-15")

# Fix 2020.04.25
# Computing non-parametric p-scores

# Parsing pre-2020 and 2020
# Saving dates
date_2020_01_01 <- as.Date("2020-01-01", origin = "1970-01-01")
date_2020_12_31 <- as.Date("2020-12-31", origin = "1970-01-01")

date_2021_01_01 <- as.Date("2021-01-01", origin = "1970-01-01")
date_2022_01_01 <- as.Date("2022-01-01", origin = "1970-01-01")


# 2015-2019
google_trends_grob_data_2015_2019             <- google_trends_grob_data[ which( google_trends_grob_data$Date < date_2020_01_01 ), ]
google_trends_pomynky_data_2015_2019          <- google_trends_pomynky_data[ which( google_trends_pomynky_data$Date < date_2020_01_01 ), ]
google_trends_ritualnie_uslugi_data_2015_2019 <- google_trends_ritualnie_uslugi_data[ which( google_trends_ritualnie_uslugi_data$Date < date_2020_01_01 ), ]
google_trends_truna_data_2015_2019          <- google_trends_truna_data[ which( google_trends_truna_data$Date < date_2020_01_01 ), ]
google_trends_rytualni_posluhy_data_2015_2019 <- google_trends_rytualni_posluhy_data[ which( google_trends_rytualni_posluhy_data$Date < date_2020_01_01 ), ]

# Only 2020
google_trends_grob_data_2020             <- google_trends_grob_data[ which( ((google_trends_grob_data$Date >= date_2020_01_01)*(google_trends_grob_data$Date <= date_2020_12_31)) == 1 ), ]
google_trends_pomynky_data_2020          <- google_trends_pomynky_data[ which( ((google_trends_pomynky_data$Date >= date_2020_01_01)*(google_trends_pomynky_data$Date <= date_2020_12_31)) == 1 ), ]
google_trends_ritualnie_uslugi_data_2020 <- google_trends_ritualnie_uslugi_data[ which( ((google_trends_ritualnie_uslugi_data$Date >= date_2020_01_01)*(google_trends_ritualnie_uslugi_data$Date <= date_2020_12_31)) == 1 ), ]
google_trends_truna_data_2020          <- google_trends_truna_data[ which( ((google_trends_truna_data$Date >= date_2020_01_01)*(google_trends_truna_data$Date <= date_2020_12_31)) == 1 ), ]
google_trends_rytualni_posluhy_data_2020 <- google_trends_rytualni_posluhy_data[ which( ((google_trends_rytualni_posluhy_data$Date >= date_2020_01_01)*(google_trends_rytualni_posluhy_data$Date <= date_2020_12_31)) == 1 ), ]

# Only 2021
google_trends_grob_data_2021             <- google_trends_grob_data[ which( ((google_trends_grob_data$Date >= date_2021_01_01)*(google_trends_grob_data$Date <= date_2022_01_01)) == 1 ), ]
google_trends_pomynky_data_2021          <- google_trends_pomynky_data[ which( ((google_trends_pomynky_data$Date >= date_2021_01_01)*(google_trends_pomynky_data$Date <= date_2022_01_01)) == 1 ), ]
google_trends_ritualnie_uslugi_data_2021 <- google_trends_ritualnie_uslugi_data[ which( ((google_trends_ritualnie_uslugi_data$Date >= date_2021_01_01)*(google_trends_ritualnie_uslugi_data$Date <= date_2022_01_01)) == 1 ), ]
google_trends_truna_data_2021          <- google_trends_truna_data[ which( ((google_trends_truna_data$Date >= date_2021_01_01)*(google_trends_truna_data$Date <= date_2022_01_01)) == 1 ), ]
google_trends_rytualni_posluhy_data_2021 <- google_trends_rytualni_posluhy_data[ which( ((google_trends_rytualni_posluhy_data$Date >= date_2021_01_01)*(google_trends_rytualni_posluhy_data$Date <= date_2022_01_01)) == 1 ), ]



# Creating p-scores to be saved for 2020.
p_scores_frame_grob_2020             <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_pomynky_2020          <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_ritualnie_uslugi_2020 <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_truna_2020          <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_rytualni_posluhy_2020 <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
# Fixing month text 2020
p_scores_frame_grob_2020$month_text             <- as.character(p_scores_frame_grob_2020$month_text)
p_scores_frame_pomynky_2020$month_text          <- as.character(p_scores_frame_pomynky_2020$month_text)
p_scores_frame_ritualnie_uslugi_2020$month_text <- as.character(p_scores_frame_ritualnie_uslugi_2020$month_text)
p_scores_frame_truna_2020$month_text          <- as.character(p_scores_frame_truna_2020$month_text)
p_scores_frame_rytualni_posluhy_2020$month_text <- as.character(p_scores_frame_rytualni_posluhy_2020$month_text)




# Creating p-scores to be saved for 2021.
p_scores_frame_grob_2021             <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_pomynky_2021          <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_ritualnie_uslugi_2021 <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_truna_2021          <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
p_scores_frame_rytualni_posluhy_2021 <- data.frame( Month = month.name,
                                                    average_2015_2019_years = rep(0, length(month.name)),
                                                    Value = rep(0, length(month.name)),
                                                    p_score_value = rep(0, length(month.name)),
                                                    month_text = rep("X", length(month.name))  )
# Fixing month text 2021
p_scores_frame_grob_2021$month_text             <- as.character(p_scores_frame_grob_2021$month_text)
p_scores_frame_pomynky_2021$month_text          <- as.character(p_scores_frame_pomynky_2021$month_text)
p_scores_frame_ritualnie_uslugi_2021$month_text <- as.character(p_scores_frame_ritualnie_uslugi_2021$month_text)
p_scores_frame_truna_2021$month_text          <- as.character(p_scores_frame_truna_2021$month_text)
p_scores_frame_rytualni_posluhy_2021$month_text <- as.character(p_scores_frame_rytualni_posluhy_2021$month_text)




# Computing non-parametrix p-scores
for( m in month.name )
{
  # Debuggins step
  # m <- month.name[1] 
  
  # Getting current month for the past years
  month_current_frame_grob_2015_2019              <- google_trends_grob_data_2015_2019[ which(google_trends_grob_data_2015_2019$Month_text == m),  ]
  month_current_frame_pomynky_2015_2019           <- google_trends_pomynky_data_2015_2019[ which(google_trends_pomynky_data_2015_2019$Month_text == m),  ]
  month_current_frame_ritualnie_uslugi_2015_2019  <- google_trends_ritualnie_uslugi_data_2015_2019[ which(google_trends_ritualnie_uslugi_data_2015_2019$Month_text == m),  ]
  month_current_frame_truna_2015_2019           <- google_trends_truna_data_2015_2019[ which(google_trends_truna_data_2015_2019$Month_text == m),  ]
  month_current_frame_rytualni_posluhy_2015_2019  <- google_trends_rytualni_posluhy_data_2015_2019[ which(google_trends_rytualni_posluhy_data_2015_2019$Month_text == m),  ]

  # Getting current month for 2020
  month_current_frame_grob_2020              <- google_trends_grob_data_2020[ which(google_trends_grob_data_2020$Month_text == m),  ]
  month_current_frame_pomynky_2020           <- google_trends_pomynky_data_2020[ which(google_trends_pomynky_data_2020$Month_text == m),  ]
  month_current_frame_ritualnie_uslugi_2020  <- google_trends_ritualnie_uslugi_data_2020[ which(google_trends_ritualnie_uslugi_data_2020$Month_text == m),  ]
  month_current_frame_truna_2020           <- google_trends_truna_data_2020[ which(google_trends_truna_data_2020$Month_text == m),  ]
  month_current_frame_rytualni_posluhy_2020  <- google_trends_rytualni_posluhy_data_2020[ which(google_trends_rytualni_posluhy_data_2020$Month_text == m),  ]

  # Getting current month for 2021
  month_current_frame_grob_2021              <- google_trends_grob_data_2021[ which(google_trends_grob_data_2021$Month_text == m),  ]
  month_current_frame_pomynky_2021           <- google_trends_pomynky_data_2021[ which(google_trends_pomynky_data_2021$Month_text == m),  ]
  month_current_frame_ritualnie_uslugi_2021  <- google_trends_ritualnie_uslugi_data_2021[ which(google_trends_ritualnie_uslugi_data_2021$Month_text == m),  ]
  month_current_frame_truna_2021           <- google_trends_truna_data_2021[ which(google_trends_truna_data_2021$Month_text == m),  ]
  month_current_frame_rytualni_posluhy_2021  <- google_trends_rytualni_posluhy_data_2021[ which(google_trends_rytualni_posluhy_data_2021$Month_text == m),  ]


  # grob
  
  # 2020
  # Saving the current value
  p_scores_frame_grob_2020[ which(p_scores_frame_grob_2020$Month == m), "Value"  ] <- google_trends_grob_data_2020[ which(google_trends_grob_data_2020$Month_text == m) , "grob"]  
  value_current_grob <- google_trends_grob_data_2020[ which(google_trends_grob_data_2020$Month_text == m) , "grob"]
  
  # Saving the average value
  p_scores_frame_grob_2020[ which(p_scores_frame_grob_2020$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_grob_2015_2019$grob )
  mean_current_grob  <-  mean( month_current_frame_grob_2015_2019$grob )
  
  # Computing p-scores for non-parametric
  p_scores_frame_grob_2020[ which(p_scores_frame_grob_2020$Month == m), "p_score_value"  ] <-  100*(value_current_grob - mean_current_grob)/mean_current_grob

  # Saving month
  p_scores_frame_grob_2020[ which(p_scores_frame_grob_2020$Month == m), "month_text"  ] <-  m
  
  
  
  if ( m %in% month.name )
  {
    # 2021
    # Saving the current value
    p_scores_frame_grob_2021[ which(p_scores_frame_grob_2021$Month == m), "Value"  ] <- google_trends_grob_data_2021[ which(google_trends_grob_data_2021$Month_text == m) , "grob"]  
    value_current_grob <- google_trends_grob_data_2021[ which(google_trends_grob_data_2021$Month_text == m) , "grob"]
    
    # Saving the average value
    p_scores_frame_grob_2021[ which(p_scores_frame_grob_2021$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_grob_2015_2019$grob )
    mean_current_grob  <-  mean( month_current_frame_grob_2015_2019$grob )
    
    # Computing p-scores for non-parametric
    p_scores_frame_grob_2021[ which(p_scores_frame_grob_2021$Month == m), "p_score_value"  ] <-  100*(value_current_grob - mean_current_grob)/mean_current_grob

    # Saving month
    p_scores_frame_grob_2021[ which(p_scores_frame_grob_2021$Month == m), "month_text"  ] <-  m
    
  }

  
  
  
  # pomynky
  
  # 2020
  # Saving the current value
  p_scores_frame_pomynky_2020[ which(p_scores_frame_pomynky_2020$Month == m), "Value"  ] <- google_trends_pomynky_data_2020[ which(google_trends_pomynky_data_2020$Month_text == m) , "pomynky"]  
  value_current_pomynky <- google_trends_pomynky_data_2020[ which(google_trends_pomynky_data_2020$Month_text == m) , "pomynky"]
  
  # Saving the average value
  p_scores_frame_pomynky_2020[ which(p_scores_frame_pomynky_2020$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_pomynky_2015_2019$pomynky )
  mean_current_pomynky  <-  mean( month_current_frame_pomynky_2015_2019$pomynky )
  
  # Computing p-scores for non-parametric
  p_scores_frame_pomynky_2020[ which(p_scores_frame_pomynky_2020$Month == m), "p_score_value"  ] <-  100*(value_current_pomynky - mean_current_pomynky)/mean_current_pomynky
  
  # Saving month
  p_scores_frame_pomynky_2020[ which(p_scores_frame_pomynky_2020$Month == m), "month_text"  ] <-  m
  
  
  
  if ( m %in% month.name )
  {
    # 2021
    # Saving the current value
    p_scores_frame_pomynky_2021[ which(p_scores_frame_pomynky_2021$Month == m), "Value"  ] <- google_trends_pomynky_data_2021[ which(google_trends_pomynky_data_2021$Month_text == m) , "pomynky"]  
    value_current_pomynky <- google_trends_pomynky_data_2021[ which(google_trends_pomynky_data_2021$Month_text == m) , "pomynky"]
    
    # Saving the average value
    p_scores_frame_pomynky_2021[ which(p_scores_frame_pomynky_2021$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_pomynky_2015_2019$pomynky )
    mean_current_pomynky  <-  mean( month_current_frame_pomynky_2015_2019$pomynky )
    
    # Computing p-scores for non-parametric
    p_scores_frame_pomynky_2021[ which(p_scores_frame_pomynky_2021$Month == m), "p_score_value"  ] <-  100*(value_current_pomynky - mean_current_pomynky)/mean_current_pomynky
    
    # Saving month
    p_scores_frame_pomynky_2021[ which(p_scores_frame_pomynky_2021$Month == m), "month_text"  ] <-  m
    
  }
  
  
  
  
  
  # rytualni_posluhy
  
  # 2020
  # Saving the current value
  p_scores_frame_rytualni_posluhy_2020[ which(p_scores_frame_rytualni_posluhy_2020$Month == m), "Value"  ] <- google_trends_rytualni_posluhy_data_2020[ which(google_trends_rytualni_posluhy_data_2020$Month_text == m) , "rytualni_posluhy"]  
  value_current_rytualni_posluhy <- google_trends_rytualni_posluhy_data_2020[ which(google_trends_rytualni_posluhy_data_2020$Month_text == m) , "rytualni_posluhy"]
  
  # Saving the average value
  p_scores_frame_rytualni_posluhy_2020[ which(p_scores_frame_rytualni_posluhy_2020$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_rytualni_posluhy_2015_2019$rytualni_posluhy )
  mean_current_rytualni_posluhy  <-  mean( month_current_frame_rytualni_posluhy_2015_2019$rytualni_posluhy )
  
  # Computing p-scores for non-parametric
  p_scores_frame_rytualni_posluhy_2020[ which(p_scores_frame_rytualni_posluhy_2020$Month == m), "p_score_value"  ] <-  100*(value_current_rytualni_posluhy - mean_current_rytualni_posluhy)/mean_current_rytualni_posluhy
  
  # Saving month
  p_scores_frame_rytualni_posluhy_2020[ which(p_scores_frame_rytualni_posluhy_2020$Month == m), "month_text"  ] <-  m
  
  
  
  if ( m %in% month.name )
  {
    # 2021
    # Saving the current value
    p_scores_frame_rytualni_posluhy_2021[ which(p_scores_frame_rytualni_posluhy_2021$Month == m), "Value"  ] <- google_trends_rytualni_posluhy_data_2021[ which(google_trends_rytualni_posluhy_data_2021$Month_text == m) , "rytualni_posluhy"]  
    value_current_rytualni_posluhy <- google_trends_rytualni_posluhy_data_2021[ which(google_trends_rytualni_posluhy_data_2021$Month_text == m) , "rytualni_posluhy"]
    
    # Saving the average value
    p_scores_frame_rytualni_posluhy_2021[ which(p_scores_frame_rytualni_posluhy_2021$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_rytualni_posluhy_2015_2019$rytualni_posluhy )
    mean_current_rytualni_posluhy  <-  mean( month_current_frame_rytualni_posluhy_2015_2019$rytualni_posluhy )
    
    # Computing p-scores for non-parametric
    p_scores_frame_rytualni_posluhy_2021[ which(p_scores_frame_rytualni_posluhy_2021$Month == m), "p_score_value"  ] <-  100*(value_current_rytualni_posluhy - mean_current_rytualni_posluhy)/mean_current_rytualni_posluhy
    
    # Saving month
    p_scores_frame_rytualni_posluhy_2021[ which(p_scores_frame_rytualni_posluhy_2021$Month == m), "month_text"  ] <-  m
    
  }
  

 # truna
  
  # 2020
  # Saving the current value
  p_scores_frame_truna_2020[ which(p_scores_frame_truna_2020$Month == m), "Value"  ] <- google_trends_truna_data_2020[ which(google_trends_truna_data_2020$Month_text == m) , "truna"]  
  value_current_truna <- google_trends_truna_data_2020[ which(google_trends_truna_data_2020$Month_text == m) , "truna"]
  
  # Saving the average value
  p_scores_frame_truna_2020[ which(p_scores_frame_truna_2020$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_truna_2015_2019$truna )
  mean_current_truna  <-  mean( month_current_frame_truna_2015_2019$truna )
  
  # Computing p-scores for non-parametric
  p_scores_frame_truna_2020[ which(p_scores_frame_truna_2020$Month == m), "p_score_value"  ] <-  100*(value_current_truna - mean_current_truna)/mean_current_truna
  
  # Saving month
  p_scores_frame_truna_2020[ which(p_scores_frame_truna_2020$Month == m), "month_text"  ] <-  m
  
  
  
  if ( m %in% month.name )
  {
    # 2021
    # Saving the current value
    p_scores_frame_truna_2021[ which(p_scores_frame_truna_2021$Month == m), "Value"  ] <- google_trends_truna_data_2021[ which(google_trends_truna_data_2021$Month_text == m) , "truna"]  
    value_current_truna <- google_trends_truna_data_2021[ which(google_trends_truna_data_2021$Month_text == m) , "truna"]
    
    # Saving the average value
    p_scores_frame_truna_2021[ which(p_scores_frame_truna_2021$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_truna_2015_2019$truna )
    mean_current_truna  <-  mean( month_current_frame_truna_2015_2019$truna )
    
    # Computing p-scores for non-parametric
    p_scores_frame_truna_2021[ which(p_scores_frame_truna_2021$Month == m), "p_score_value"  ] <-  100*(value_current_truna - mean_current_truna)/mean_current_truna
    
    # Saving month
    p_scores_frame_truna_2021[ which(p_scores_frame_truna_2021$Month == m), "month_text"  ] <-  m
    
  }
  
  
  
  
  
  # ritualnie_uslugi
  
  # 2020
  # Saving the current value
  p_scores_frame_ritualnie_uslugi_2020[ which(p_scores_frame_ritualnie_uslugi_2020$Month == m), "Value"  ] <- google_trends_ritualnie_uslugi_data_2020[ which(google_trends_ritualnie_uslugi_data_2020$Month_text == m) , "ritualnie_uslugi"]  
  value_current_ritualnie_uslugi <- google_trends_ritualnie_uslugi_data_2020[ which(google_trends_ritualnie_uslugi_data_2020$Month_text == m) , "ritualnie_uslugi"]
  
  # Saving the average value
  p_scores_frame_ritualnie_uslugi_2020[ which(p_scores_frame_ritualnie_uslugi_2020$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_ritualnie_uslugi_2015_2019$ritualnie_uslugi )
  mean_current_ritualnie_uslugi  <-  mean( month_current_frame_ritualnie_uslugi_2015_2019$ritualnie_uslugi )
  
  # Computing p-scores for non-parametric
  p_scores_frame_ritualnie_uslugi_2020[ which(p_scores_frame_ritualnie_uslugi_2020$Month == m), "p_score_value"  ] <-  100*(value_current_ritualnie_uslugi - mean_current_ritualnie_uslugi)/mean_current_ritualnie_uslugi
  
  # Saving month
  p_scores_frame_ritualnie_uslugi_2020[ which(p_scores_frame_ritualnie_uslugi_2020$Month == m), "month_text"  ] <-  m
  
  
  
  if ( m %in% month.name )
  {
    # 2021
    # Saving the current value
    p_scores_frame_ritualnie_uslugi_2021[ which(p_scores_frame_ritualnie_uslugi_2021$Month == m), "Value"  ] <- google_trends_ritualnie_uslugi_data_2021[ which(google_trends_ritualnie_uslugi_data_2021$Month_text == m) , "ritualnie_uslugi"]  
    value_current_ritualnie_uslugi <- google_trends_ritualnie_uslugi_data_2021[ which(google_trends_ritualnie_uslugi_data_2021$Month_text == m) , "ritualnie_uslugi"]
    
    # Saving the average value
    p_scores_frame_ritualnie_uslugi_2021[ which(p_scores_frame_ritualnie_uslugi_2021$Month == m), "average_2015_2019_years"  ] <-  mean( month_current_frame_ritualnie_uslugi_2015_2019$ritualnie_uslugi )
    mean_current_ritualnie_uslugi  <-  mean( month_current_frame_ritualnie_uslugi_2015_2019$ritualnie_uslugi )
    
    # Computing p-scores for non-parametric
    p_scores_frame_ritualnie_uslugi_2021[ which(p_scores_frame_ritualnie_uslugi_2021$Month == m), "p_score_value"  ] <-  100*(value_current_ritualnie_uslugi - mean_current_ritualnie_uslugi)/mean_current_ritualnie_uslugi
    
    # Saving month
    p_scores_frame_ritualnie_uslugi_2021[ which(p_scores_frame_ritualnie_uslugi_2021$Month == m), "month_text"  ] <-  m
    
  }


# Enf for -> for( m in month.name )
}  



# Fixing month2 text 2020
p_scores_frame_grob_2020$month_text2             <- paste0("2020-", p_scores_frame_grob_2020$month_text)
p_scores_frame_pomynky_2020$month_text2          <- paste0("2020-", p_scores_frame_pomynky_2020$month_text)
p_scores_frame_ritualnie_uslugi_2020$month_text2 <- paste0("2020-", p_scores_frame_ritualnie_uslugi_2020$month_text)
p_scores_frame_truna_2020$month_text2          <- paste0("2020-", p_scores_frame_truna_2020$month_text)
p_scores_frame_rytualni_posluhy_2020$month_text2 <- paste0("2020-", p_scores_frame_rytualni_posluhy_2020$month_text)

# Fixing month3 text 2020
p_scores_frame_grob_2020$month_text3             <- paste0("2020-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_pomynky_2020$month_text3          <- paste0("2020-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_ritualnie_uslugi_2020$month_text3 <- paste0("2020-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_truna_2020$month_text3          <- paste0("2020-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_rytualni_posluhy_2020$month_text3 <- paste0("2020-", c("01","02","03","04","05","06","07","08","09","10","11","12"))

# Fixing color 2020
p_scores_frame_grob_2020$color             <- c( rep("#005BBB", 2), rep("#FFD500", 10) )
p_scores_frame_pomynky_2020$color          <- c( rep("#005BBB", 2), rep("#FFD500", 10) )
p_scores_frame_ritualnie_uslugi_2020$color <- c( rep("#005BBB", 2), rep("#FFD500", 10) )
p_scores_frame_truna_2020$color          <- c( rep("#005BBB", 2), rep("#FFD500", 10) )
p_scores_frame_rytualni_posluhy_2020$color <- c( rep("#005BBB", 2), rep("#FFD500", 10) )




# Fixing month2 text 2021
p_scores_frame_grob_2021$month_text2             <- paste0("2021-", p_scores_frame_grob_2021$month_text)
p_scores_frame_pomynky_2021$month_text2          <- paste0("2021-", p_scores_frame_pomynky_2021$month_text)
p_scores_frame_ritualnie_uslugi_2021$month_text2 <- paste0("2021-", p_scores_frame_ritualnie_uslugi_2021$month_text)
p_scores_frame_truna_2021$month_text2          <- paste0("2021-", p_scores_frame_truna_2021$month_text)
p_scores_frame_rytualni_posluhy_2021$month_text2 <- paste0("2021-", p_scores_frame_rytualni_posluhy_2021$month_text)

# Fixing month3 text 2021
p_scores_frame_grob_2021$month_text3             <- paste0("2021-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_pomynky_2021$month_text3          <- paste0("2021-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_ritualnie_uslugi_2021$month_text3 <- paste0("2021-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_truna_2021$month_text3          <- paste0("2021-", c("01","02","03","04","05","06","07","08","09","10","11","12"))
p_scores_frame_rytualni_posluhy_2021$month_text3 <- paste0("2021-", c("01","02","03","04","05","06","07","08","09","10","11","12"))


# Fixing color 2021
p_scores_frame_grob_2021$color             <- c( rep("#FFD500", 12) )
p_scores_frame_pomynky_2021$color          <- c( rep("#FFD500", 12) )
p_scores_frame_ritualnie_uslugi_2021$color <- c( rep("#FFD500", 12) )
p_scores_frame_truna_2021$color          <- c( rep("#FFD500", 12) )
p_scores_frame_rytualni_posluhy_2021$color <- c( rep("#FFD500", 12) )



# Combining 2020 and 2021
p_scores_frame_grob_2020_2021             <- rbind(p_scores_frame_grob_2020, p_scores_frame_grob_2021)
p_scores_frame_pomynky_2020_2021          <- rbind(p_scores_frame_pomynky_2020, p_scores_frame_pomynky_2021)
p_scores_frame_ritualnie_uslugi_2020_2021 <- rbind(p_scores_frame_ritualnie_uslugi_2020, p_scores_frame_ritualnie_uslugi_2021)
p_scores_frame_truna_2020_2021          <- rbind(p_scores_frame_truna_2020, p_scores_frame_truna_2021)
p_scores_frame_rytualni_posluhy_2020_2021 <- rbind(p_scores_frame_rytualni_posluhy_2020, p_scores_frame_rytualni_posluhy_2021)



# Saving the data as RData file.
# 2020
save( p_scores_frame_grob_2020,             file = paste("../../R_Data/p_scores_frame_grob_2020.RData") )
save( p_scores_frame_pomynky_2020,          file = paste("../../R_Data/p_scores_frame_pomynky_2020.RData") )
save( p_scores_frame_ritualnie_uslugi_2020, file = paste("../../R_Data/p_scores_frame_ritualnie_uslugi_2020.RData") )
save( p_scores_frame_truna_2020,          file = paste("../../R_Data/p_scores_frame_truna_2020.RData") )
save( p_scores_frame_rytualni_posluhy_2020, file = paste("../../R_Data/p_scores_frame_rytualni_posluhy_2020.RData") )
# 2021
save( p_scores_frame_grob_2021,             file = paste("../../R_Data/p_scores_frame_grob_2021.RData") )
save( p_scores_frame_pomynky_2021,          file = paste("../../R_Data/p_scores_frame_pomynky_2021.RData") )
save( p_scores_frame_ritualnie_uslugi_2021, file = paste("../../R_Data/p_scores_frame_ritualnie_uslugi_2021.RData") )
save( p_scores_frame_truna_2021,          file = paste("../../R_Data/p_scores_frame_truna_2021.RData") )
save( p_scores_frame_rytualni_posluhy_2021, file = paste("../../R_Data/p_scores_frame_rytualni_posluhy_2021.RData") )

# 2020 and 2021
save( p_scores_frame_grob_2020_2021,             file = paste("../../R_Data/p_scores_frame_grob_2020_2021.RData") )
save( p_scores_frame_pomynky_2020_2021,          file = paste("../../R_Data/p_scores_frame_pomynky_2020_2021.RData") )
save( p_scores_frame_ritualnie_uslugi_2020_2021, file = paste("../../R_Data/p_scores_frame_ritualnie_uslugi_2020_2021.RData") )
save( p_scores_frame_truna_2020_2021,          file = paste("../../R_Data/p_scores_frame_truna_2020_2021.RData") )
save( p_scores_frame_rytualni_posluhy_2020_2021, file = paste("../../R_Data/p_scores_frame_rytualni_posluhy_2020_2021.RData") )





# Generating pdf output.
pdf( paste( "../../Plots/FigureTBD02a.pdf", sep = ""), height = 18, width = 27)
# Defining the number of plots
par( par(mfrow=c(2,3)),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(3,2), by.col = TRUE),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,3,5,2,4,6), nrow = 2, ncol = 3, byrow = TRUE)
# Setting layaout
layout(layout_matrix)




# grob

# First plot
lower_index_grob <- 1
upper_index_grob <- length(p_scores_frame_grob_2020_2021$p_score_value)
range_total <- c(lower_index_grob:upper_index_grob)

p_score_min_grob <- min(p_scores_frame_grob_2020_2021$p_score_value[range_total])
p_score_max_grob <- max(p_scores_frame_grob_2020_2021$p_score_value[range_total])


barplot( p_scores_frame_grob_2020_2021$p_score_value[range_total], 
         col= p_scores_frame_grob_2020_2021$color[range_total], 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_grob-15, p_score_max_grob+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"grob\"",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = p_scores_frame_grob_2020_2021$month_text3[range_total], 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 2)

legend( x = "topright", 
        inset= c(0.06, 0.08), 
        legend = c("Pre Epidemic", "During Epidemic"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 


# Label A
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "A"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)



# Second plot
value_combine <- c(p_scores_frame_grob_2020_2021$average_2015_2019_years, 
                   p_scores_frame_grob_2020_2021$Value,
                   p_scores_frame_pomynky_2020_2021$average_2015_2019_years, 
                   p_scores_frame_pomynky_2020_2021$Value,
                   p_scores_frame_ritualnie_uslugi_2020_2021$average_2015_2019_years, 
                   p_scores_frame_ritualnie_uslugi_2020_2021$Value)

plot(x = as.integer(rownames(p_scores_frame_grob_2020_2021[range_total,])),
     y = p_scores_frame_grob_2020_2021$average_2015_2019_years[range_total],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Averaged (2015-2019) vs 2020-2021 Data\nGoogle Trend: \"grob\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Trend Value",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = as.integer(rownames(p_scores_frame_grob_2020_2021[range_total,])),
      y = p_scores_frame_grob_2020_2021$average_2015_2019_years[range_total],
      col = "#005BBB",
      #col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(rownames(p_scores_frame_grob_2020_2021[range_total,])),
      y = p_scores_frame_grob_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(rownames(p_scores_frame_grob_2020_2021[range_total,])),
      y = p_scores_frame_grob_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep(3, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topright", 
        inset= c(0.06, 0.04), 
        legend = c("Averaged Trend", "Google Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(rownames(p_scores_frame_grob_2020_2021[range_total,])))
final_date   <- max(as.integer(rownames(p_scores_frame_grob_2020_2021[range_total,])))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_grob_2020_2021$Month[range_total]),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( p_scores_frame_grob_2020_2021$month_text3[range_total] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine )
y_max_value <- max( value_combine )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label B
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "D"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)





# Third plot
lower_index_pomynky <- 1
upper_index_pomynky <- length(p_scores_frame_pomynky_2020_2021$p_score_value)
range_total <- c(lower_index_pomynky:upper_index_pomynky)

p_score_min_pomynky <- min(p_scores_frame_pomynky_2020_2021$p_score_value[range_total])
p_score_max_pomynky <- max(p_scores_frame_pomynky_2020_2021$p_score_value[range_total])


barplot( p_scores_frame_pomynky_2020_2021$p_score_value[range_total], 
         col= p_scores_frame_pomynky_2020_2021$color[range_total], 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_pomynky-15, p_score_max_pomynky+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"pominki\"",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = p_scores_frame_pomynky_2020_2021$month_text3[range_total], 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 2)

legend( x = "topleft", 
        inset= c(0.06, 0.08), 
        legend = c("Pre Epidemic", "During Epidemic"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 


# Label C
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "B"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)



# Fourth graph
value_combine <- c(p_scores_frame_grob_2020_2021$average_2015_2019_years, 
                   p_scores_frame_grob_2020_2021$Value,
                   p_scores_frame_pomynky_2020_2021$average_2015_2019_years, 
                   p_scores_frame_pomynky_2020_2021$Value,
                   p_scores_frame_ritualnie_uslugi_2020_2021$average_2015_2019_years, 
                   p_scores_frame_ritualnie_uslugi_2020_2021$Value)

plot(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
     y = p_scores_frame_pomynky_2020_2021$average_2015_2019_years[range_total],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Averaged (2015-2019) vs 2020-2021 Data\nGoogle Trend: \"pominki\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Trend Value",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
      y = p_scores_frame_pomynky_2020_2021$average_2015_2019_years[range_total],
      col = "#005BBB",
      #col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
      y = p_scores_frame_pomynky_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
      y = p_scores_frame_pomynky_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep(3, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.14, 0.04), 
        legend = c("Averaged Trend", "Google Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])))
final_date   <- max(as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_pomynky_2020_2021$Month[range_total]),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( p_scores_frame_pomynky_2020_2021$month_text3[range_total] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine )
y_max_value <- max( value_combine )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label D
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "E"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)




# Fifth graph
lower_index_ritualnie_uslugi <- 1
upper_index_ritualnie_uslugi <- length(p_scores_frame_ritualnie_uslugi_2020_2021$p_score_value)
range_total <- c(lower_index_ritualnie_uslugi:upper_index_ritualnie_uslugi)

p_score_min_ritualnie_uslugi <- min(p_scores_frame_ritualnie_uslugi_2020_2021$p_score_value[range_total])
p_score_max_ritualnie_uslugi <- max(p_scores_frame_ritualnie_uslugi_2020_2021$p_score_value[range_total])


barplot( p_scores_frame_ritualnie_uslugi_2020_2021$p_score_value[range_total], 
         col= p_scores_frame_ritualnie_uslugi_2020_2021$color[range_total], 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_ritualnie_uslugi-15, p_score_max_ritualnie_uslugi+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"ritualnie uslugi\"",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = p_scores_frame_ritualnie_uslugi_2020_2021$month_text3[range_total], 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 2)

legend( x = "topleft", 
        inset= c(0.06, 0.02), 
        legend = c("Pre Epidemic", "During Epidemic"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 


# Label E
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "C"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)



# Fifth graph

value_combine <- c(p_scores_frame_grob_2020_2021$average_2015_2019_years, 
                   p_scores_frame_grob_2020_2021$Value,
                   p_scores_frame_pomynky_2020_2021$average_2015_2019_years, 
                   p_scores_frame_pomynky_2020_2021$Value,
                   p_scores_frame_ritualnie_uslugi_2020_2021$average_2015_2019_years, 
                   p_scores_frame_ritualnie_uslugi_2020_2021$Value)

plot(x = as.integer(rownames(p_scores_frame_ritualnie_uslugi_2020_2021[range_total,])),
     y = p_scores_frame_ritualnie_uslugi_2020_2021$average_2015_2019_years[range_total],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Averaged (2015-2019) vs 2020-2021 Data\nGoogle Trend: \"ritualnie uslugi\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Trend Value",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = as.integer(rownames(p_scores_frame_ritualnie_uslugi_2020_2021[range_total,])),
      y = p_scores_frame_ritualnie_uslugi_2020_2021$average_2015_2019_years[range_total],
      col = "#005BBB",
      #col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(rownames(p_scores_frame_ritualnie_uslugi_2020_2021[range_total,])),
      y = p_scores_frame_ritualnie_uslugi_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(rownames(p_scores_frame_ritualnie_uslugi_2020_2021[range_total,])),
      y = p_scores_frame_ritualnie_uslugi_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep(3, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.26, 0.04), 
        legend = c("Averaged Trend", "Google Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(rownames(p_scores_frame_ritualnie_uslugi_2020_2021[range_total,])))
final_date   <- max(as.integer(rownames(p_scores_frame_ritualnie_uslugi_2020_2021[range_total,])))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_ritualnie_uslugi_2020_2021$Month[range_total]),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( p_scores_frame_ritualnie_uslugi_2020_2021$month_text3[range_total] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine )
y_max_value <- max( value_combine )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label F
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "F"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)


dev.off()




# Generating pdf output.
pdf( paste( "../../Plots/FigureTBD02b.pdf", sep = ""), height = 18, width = 27)
# Defining the number of plots
par( par(mfrow=c(2,3)),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(3,2), by.col = TRUE),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,3,5,2,4,6), nrow = 2, ncol = 3, byrow = TRUE)
# Setting layaout
layout(layout_matrix)

# Fifth graph
lower_index_truna <- 1
upper_index_truna <- length(p_scores_frame_truna_2020_2021$p_score_value)
range_total <- c(lower_index_truna:upper_index_truna)

p_score_min_truna <- min(p_scores_frame_truna_2020_2021$p_score_value[range_total])
p_score_max_truna <- max(p_scores_frame_truna_2020_2021$p_score_value[range_total])


barplot( p_scores_frame_truna_2020_2021$p_score_value[range_total], 
         col= p_scores_frame_truna_2020_2021$color[range_total], 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_truna-15, p_score_max_truna+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"truna\"",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = p_scores_frame_truna_2020_2021$month_text3[range_total], 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 2)

legend( x = "topleft", 
        inset= c(0.06, 0.08), 
        legend = c("Pre Epidemic", "During Epidemic"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 


# Label G
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "A"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)



# Fifth graph

value_combine <- c(p_scores_frame_grob_2020_2021$average_2015_2019_years, 
                   p_scores_frame_grob_2020_2021$Value,
                   p_scores_frame_pomynky_2020_2021$average_2015_2019_years, 
                   p_scores_frame_pomynky_2020_2021$Value,
                   p_scores_frame_truna_2020_2021$average_2015_2019_years, 
                   p_scores_frame_truna_2020_2021$Value)

plot(x = as.integer(rownames(p_scores_frame_truna_2020_2021[range_total,])),
     y = p_scores_frame_truna_2020_2021$average_2015_2019_years[range_total],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Averaged (2015-2019) vs 2020-2021 Data\nGoogle Trend: \"truna\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Trend Value",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = as.integer(rownames(p_scores_frame_truna_2020_2021[range_total,])),
      y = p_scores_frame_truna_2020_2021$average_2015_2019_years[range_total],
      col = "#005BBB",
      #col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(rownames(p_scores_frame_truna_2020_2021[range_total,])),
      y = p_scores_frame_truna_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(rownames(p_scores_frame_truna_2020_2021[range_total,])),
      y = p_scores_frame_truna_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep(3, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.14, 0.04), 
        legend = c("Averaged Trend", "Google Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(rownames(p_scores_frame_truna_2020_2021[range_total,])))
final_date   <- max(as.integer(rownames(p_scores_frame_truna_2020_2021[range_total,])))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_truna_2020_2021$Month[range_total]),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( p_scores_frame_truna_2020_2021$month_text3[range_total] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine )
y_max_value <- max( value_combine )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label H
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "D"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)


# Third plot
lower_index_pomynky <- 1
upper_index_pomynky <- length(p_scores_frame_pomynky_2020_2021$p_score_value)
range_total <- c(lower_index_pomynky:upper_index_pomynky)

p_score_min_pomynky <- min(p_scores_frame_pomynky_2020_2021$p_score_value[range_total])
p_score_max_pomynky <- max(p_scores_frame_pomynky_2020_2021$p_score_value[range_total])


barplot( p_scores_frame_pomynky_2020_2021$p_score_value[range_total], 
         col= p_scores_frame_pomynky_2020_2021$color[range_total], 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_pomynky-15, p_score_max_pomynky+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"pomynky\"",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = p_scores_frame_pomynky_2020_2021$month_text3[range_total], 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 2)

legend( x = "topleft", 
        inset= c(0.06, 0.08), 
        legend = c("Pre Epidemic", "During Epidemic"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 


# Label C
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "B"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)



# Fourth graph
value_combine <- c(p_scores_frame_grob_2020_2021$average_2015_2019_years, 
                   p_scores_frame_grob_2020_2021$Value,
                   p_scores_frame_pomynky_2020_2021$average_2015_2019_years, 
                   p_scores_frame_pomynky_2020_2021$Value,
                   p_scores_frame_ritualnie_uslugi_2020_2021$average_2015_2019_years, 
                   p_scores_frame_ritualnie_uslugi_2020_2021$Value)

plot(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
     y = p_scores_frame_pomynky_2020_2021$average_2015_2019_years[range_total],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Averaged (2015-2019) vs 2020-2021 Data\nGoogle Trend: \"pomynky\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Trend Value",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
      y = p_scores_frame_pomynky_2020_2021$average_2015_2019_years[range_total],
      col = "#005BBB",
      #col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
      y = p_scores_frame_pomynky_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])),
      y = p_scores_frame_pomynky_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep(3, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.14, 0.04), 
        legend = c("Averaged Trend", "Google Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])))
final_date   <- max(as.integer(rownames(p_scores_frame_pomynky_2020_2021[range_total,])))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_pomynky_2020_2021$Month[range_total]),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( p_scores_frame_pomynky_2020_2021$month_text3[range_total] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine )
y_max_value <- max( value_combine )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label D
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "E"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)


# Fifth graph
lower_index_rytualni_posluhy <- 1
upper_index_rytualni_posluhy <- length(p_scores_frame_rytualni_posluhy_2020_2021$p_score_value)
range_total <- c(lower_index_rytualni_posluhy:upper_index_rytualni_posluhy)

p_score_min_rytualni_posluhy <- min(p_scores_frame_rytualni_posluhy_2020_2021$p_score_value[range_total])
p_score_max_rytualni_posluhy <- max(p_scores_frame_rytualni_posluhy_2020_2021$p_score_value[range_total])


barplot( p_scores_frame_rytualni_posluhy_2020_2021$p_score_value[range_total], 
         col= p_scores_frame_rytualni_posluhy_2020_2021$color[range_total], 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_rytualni_posluhy-15, p_score_max_rytualni_posluhy+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"rytualni posluhy\"",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = p_scores_frame_rytualni_posluhy_2020_2021$month_text3[range_total], 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 2)

legend( x = "topleft", 
        inset= c(0.06, 0.08), 
        legend = c("Pre Epidemic", "During Epidemic"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 


# Label I
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "C"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)



# Fifth graph

value_combine <- c(p_scores_frame_grob_2020_2021$average_2015_2019_years, 
                   p_scores_frame_grob_2020_2021$Value,
                   p_scores_frame_pomynky_2020_2021$average_2015_2019_years, 
                   p_scores_frame_pomynky_2020_2021$Value,
                   p_scores_frame_rytualni_posluhy_2020_2021$average_2015_2019_years, 
                   p_scores_frame_rytualni_posluhy_2020_2021$Value)

plot(x = as.integer(rownames(p_scores_frame_rytualni_posluhy_2020_2021[range_total,])),
     y = p_scores_frame_rytualni_posluhy_2020_2021$average_2015_2019_years[range_total],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Averaged (2015-2019) vs 2020-2021 Data\nGoogle Trend: \"rytualni posluhy\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Trend Value",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = as.integer(rownames(p_scores_frame_rytualni_posluhy_2020_2021[range_total,])),
      y = p_scores_frame_rytualni_posluhy_2020_2021$average_2015_2019_years[range_total],
      col = "#005BBB",
      #col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(rownames(p_scores_frame_rytualni_posluhy_2020_2021[range_total,])),
      y = p_scores_frame_rytualni_posluhy_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(rownames(p_scores_frame_rytualni_posluhy_2020_2021[range_total,])),
      y = p_scores_frame_rytualni_posluhy_2020_2021$Value[range_total],
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep(3, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.16, 0.04), 
        legend = c("Averaged Trend", "Google Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 2 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(rownames(p_scores_frame_rytualni_posluhy_2020_2021[range_total,])))
final_date   <- max(as.integer(rownames(p_scores_frame_rytualni_posluhy_2020_2021[range_total,])))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_rytualni_posluhy_2020_2021$Month[range_total]),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( p_scores_frame_rytualni_posluhy_2020_2021$month_text3[range_total] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine )
y_max_value <- max( value_combine )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label J
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "F"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)


dev.off()



