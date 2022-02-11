# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.24. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)

# Library to perform column medians and other useful matrix algebra computations. 
# library(matrixStats)

# Library for the latex exports in the nice format.
# library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
# library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
# library(methods)

# Loading package required to read library(readxl)
# library(readxl)

# Loading library(rjson) for json files. 
# library(rjson)

# install.packages("pdftools")
# library(pdftools)

# install.packages("tm")
# library(tm)

# Libraries to read hml pages
# library(XML)
# library(RCurl)
# library(rlist)

# install.packages("forecast")
# library("forecast") - libary for time series forecasting.
# library("forecast")


# install.packages("prophet")
# library("prophet") - libary for time series forecasting.
library("prophet")





# Setting the correct working directory.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Belarus Mortality Analysis"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()



# Reading previous datasets

# Daily COVID-19 incidence data
load( file = paste("R_Data/belarus_incidence_data_frame_covid19.RData") )

# Cumulative data: incidence, recovered, and mortality
load( file = paste("R_Data/belarus_statistics_data_frame_covid19.RData") )

# Monthly COVID-19 mortality data
load( file = paste("R_Data/monthly_death_data_frame_covid19.RData") )

# Monthly overall mortality data since 2011
load( file = paste("R_Data/belarus_un_mortality_data_month_only_since_2011.RData") )

# Monthly overall mortality data since 2015
load( file = paste("R_Data/belarus_un_mortality_data_month_only_since_2015.RData") )

# Loading demograhics data
load( file = paste("R_Data/demographics_aggregated_2011_2020.RData") )



# Fix 2021.05.05
# Adding regressors
demographics_aggregated_2011_2020_transposed <- data.frame(t(demographics_aggregated_2011_2020)[-1,])
names(demographics_aggregated_2011_2020_transposed) <- t(demographics_aggregated_2011_2020)[1,]
demographics_aggregated_2011_2020_transposed$Year <- c(2011:2020)

demographics_aggregated_2011_2020_transposed$Age70Up  <- as.numeric(as.character(demographics_aggregated_2011_2020_transposed$`70Up`))
demographics_aggregated_2011_2020_transposed$Age65_69 <- as.numeric(as.character(demographics_aggregated_2011_2020_transposed$`65-69`))

demographics_aggregated_2011_2020_transposed$Age65Up  <- demographics_aggregated_2011_2020_transposed$Age65_69 + demographics_aggregated_2011_2020_transposed$Age70Up




# Fixing the data for the package for five years.
# Number of records BEFORE the epidemic start.
number_of_records_five <- dim(demographics_aggregated_2011_2020_transposed)[1] 


merged_five <- data.frame( ds      =  as.Date( paste0( c(2011:(2011+number_of_records_five-1)), "-01-01" )  ),
                           y       =  as.numeric(as.character(demographics_aggregated_2011_2020_transposed$AllAges)) )
                           # Age65Up =  demographics_aggregated_2011_2020_transposed$Age65Up )   

data_to_feed_truncated_five <- merged_five

# Listing names of the created objects.
names(data_to_feed_truncated_five)


# Creating a prophet object.
prophet_object_five <- prophet(data_to_feed_truncated_five)


# Full frame for predictions. Dates only extraction
no_rows <- dim( data_to_feed_truncated_five )[1]
data_to_feed_full_five_dates_only <- data.frame( ds = c( as.character( data_to_feed_truncated_five$ds ), as.character( data_to_feed_truncated_five$ds[number_of_records_five] + 366 ) ) )


# data_to_feed_full_five_dates_only <- data_to_feed_full_five

# Precting for the specified dates.
prophet_predictions_extra_year <- predict(prophet_object_five, data_to_feed_full_five_dates_only )
# Fixing dates
prophet_predictions_extra_year$ds <- as.Date(prophet_predictions_extra_year$ds)


# Summaries for mortalities
dim(prophet_predictions_extra_year)
head(prophet_predictions_extra_year)
tail(prophet_predictions_extra_year)

# Adding original data
prophet_predictions_extra_year_plus_original_data <- base::merge( x = data_to_feed_truncated_five, y = prophet_predictions_extra_year, by = "ds", all = TRUE )
dim(prophet_predictions_extra_year_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_extra_year_plus_original_data)
head(prophet_predictions_extra_year_plus_original_data)
tail(prophet_predictions_extra_year_plus_original_data)







# Saving the data as RData file.
save( prophet_predictions_extra_year, file = paste("R_Data/prophet_predictions_extra_year.RData") )

# Saving the data as RData file.
save( prophet_predictions_extra_year_plus_original_data, file = paste("R_Data/prophet_predictions_extra_year_plus_original_data.RData") )






