


rm(list=ls(all=TRUE))


# Library to perform colum medians and other usefull matrix algebra computations. 
# library(matrixStats)

# Library for the latex exports in the nice format.
# library(xtable)

# library(Matrix) for blog-diagonal matrixes creation and other matrix manipulations.
# library(Matrix)

# This package is requred to run in RScript mode rathen than interactive mode.
# library(methods)

# Loading package requred to read library(readxl)
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



# Loading the trends data from RData files.
load( file = paste("../R_Data/google_trends_grob_data.RData") )
load( file = paste("../R_Data/google_trends_pominki_data.RData") )
load( file = paste("../R_Data/google_trends_ritualnie_uslugi_data.RData") )
load( file = paste("../R_Data/google_trends_truna_data.RData") )
load( file = paste("../R_Data/google_trends_ritualni_poslugi_data.RData") )
ls()


pandemic_start <- as.Date("2020-03-15")
cut_date <- as.Date("2020-01-15")


# grob

# Fixing the data for the package 
# Number of records BEFORE the epidemic start
number_of_records_grob <- max(which(google_trends_grob_data$Date < pandemic_start))

data_to_feed_full_grob      <- data.frame( ds =  google_trends_grob_data$Date,
                                           y  =  google_trends_grob_data$grob )

data_to_feed_truncated_grob <- data.frame( ds =  google_trends_grob_data$Date[c(1:number_of_records_grob)],
                                           y  =  google_trends_grob_data$grob[c(1:number_of_records_grob)] )


# Listing names of the created objects.
names(data_to_feed_full_grob)
names(data_to_feed_truncated_grob)


# Creating a prophet object.
prophet_object_grob <- prophet(data_to_feed_truncated_grob)

# Full frame for predictions. Dates only extraction
data_to_feed_full_grob_only <- subset( data_to_feed_full_grob, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_grob <- predict(prophet_object_grob, data_to_feed_full_grob_only )
# Fixing dates
prophet_predictions_grob$ds <- as.Date(prophet_predictions_grob$ds)



# Summaries for mortalities
dim(prophet_predictions_grob)
head(prophet_predictions_grob)
tail(prophet_predictions_grob)

# Adding original data
prophet_predictions_grob_plus_original_data <- base::merge( x = data_to_feed_full_grob, y = prophet_predictions_grob, by = "ds" )
dim(prophet_predictions_grob_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_grob_plus_original_data)
head(prophet_predictions_grob_plus_original_data)
tail(prophet_predictions_grob_plus_original_data)


# lower scores
prophet_predictions_grob_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_grob_plus_original_data$y - prophet_predictions_grob_plus_original_data$yhat_lower ) / prophet_predictions_grob_plus_original_data$yhat_lower
# upper scores
prophet_predictions_grob_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_grob_plus_original_data$y - prophet_predictions_grob_plus_original_data$yhat_upper ) / prophet_predictions_grob_plus_original_data$yhat_upper
# scores
prophet_predictions_grob_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_grob_plus_original_data$y - prophet_predictions_grob_plus_original_data$yhat ) / prophet_predictions_grob_plus_original_data$yhat


# Computing the raw excess trends
# raw excess
prophet_predictions_grob_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_grob_plus_original_data$y - prophet_predictions_grob_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_grob_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_grob_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_grob_plus_original_data, file = paste("../R_Data/prophet_predictions_grob_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_grob_plus_original_data_subset <- 
  prophet_predictions_grob_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_grob_plus_original_data_subset, file = paste("../R_Data/prophet_predictions_grob_plus_original_data_subset.RData") )

# Min and Max
p_score_min_grob <- min( c(prophet_predictions_grob_plus_original_data_subset$p_scores) )
p_score_max_grob <- max( c(prophet_predictions_grob_plus_original_data_subset$p_scores) )














# pominki

# Fixing the data for the package 
# Number of records BEFORE the epidemic start
number_of_records_pominki <- max(which(google_trends_pominki_data$Date < pandemic_start))

data_to_feed_full_pominki      <- data.frame( ds =  google_trends_pominki_data$Date,
                                           y  =  google_trends_pominki_data$pominki )

data_to_feed_truncated_pominki <- data.frame( ds =  google_trends_pominki_data$Date[c(1:number_of_records_pominki)],
                                           y  =  google_trends_pominki_data$pominki[c(1:number_of_records_pominki)] )


# Listing names of the created objects.
names(data_to_feed_full_pominki)
names(data_to_feed_truncated_pominki)


# Creating a prophet object.
prophet_object_pominki <- prophet(data_to_feed_truncated_pominki)

# Full frame for predictions. Dates only extraction
data_to_feed_full_pominki_only <- subset( data_to_feed_full_pominki, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_pominki <- predict(prophet_object_pominki, data_to_feed_full_pominki_only )
# Fixing dates
prophet_predictions_pominki$ds <- as.Date(prophet_predictions_pominki$ds)



# Summaries for mortalities
dim(prophet_predictions_pominki)
head(prophet_predictions_pominki)
tail(prophet_predictions_pominki)

# Adding original data
prophet_predictions_pominki_plus_original_data <- base::merge( x = data_to_feed_full_pominki, y = prophet_predictions_pominki, by = "ds" )
dim(prophet_predictions_pominki_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_pominki_plus_original_data)
head(prophet_predictions_pominki_plus_original_data)
tail(prophet_predictions_pominki_plus_original_data)


# lower scores
prophet_predictions_pominki_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_pominki_plus_original_data$y - prophet_predictions_pominki_plus_original_data$yhat_lower ) / prophet_predictions_pominki_plus_original_data$yhat_lower
# upper scores
prophet_predictions_pominki_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_pominki_plus_original_data$y - prophet_predictions_pominki_plus_original_data$yhat_upper ) / prophet_predictions_pominki_plus_original_data$yhat_upper
# scores
prophet_predictions_pominki_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_pominki_plus_original_data$y - prophet_predictions_pominki_plus_original_data$yhat ) / prophet_predictions_pominki_plus_original_data$yhat


# Computing the raw excess trends
# raw excess
prophet_predictions_pominki_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_pominki_plus_original_data$y - prophet_predictions_pominki_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_pominki_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_pominki_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_pominki_plus_original_data, file = paste("../R_Data/prophet_predictions_pominki_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_pominki_plus_original_data_subset <- 
  prophet_predictions_pominki_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_pominki_plus_original_data_subset, file = paste("../R_Data/prophet_predictions_pominki_plus_original_data_subset.RData") )

# Min and Max
p_score_min_pominki <- min( c(prophet_predictions_pominki_plus_original_data_subset$p_scores) )
p_score_max_pominki <- max( c(prophet_predictions_pominki_plus_original_data_subset$p_scores) )









# ritualnie_uslugi

# Fixing the data for the package 
# Number of records BEFORE the epidemic start
number_of_records_ritualnie_uslugi <- max(which(google_trends_ritualnie_uslugi_data$Date < pandemic_start))

data_to_feed_full_ritualnie_uslugi      <- data.frame( ds =  google_trends_ritualnie_uslugi_data$Date,
                                              y  =  google_trends_ritualnie_uslugi_data$ritualnie_uslugi )

data_to_feed_truncated_ritualnie_uslugi <- data.frame( ds =  google_trends_ritualnie_uslugi_data$Date[c(1:number_of_records_ritualnie_uslugi)],
                                              y  =  google_trends_ritualnie_uslugi_data$ritualnie_uslugi[c(1:number_of_records_ritualnie_uslugi)] )


# Listing names of the created objects.
names(data_to_feed_full_ritualnie_uslugi)
names(data_to_feed_truncated_ritualnie_uslugi)


# Creating a prophet object.
prophet_object_ritualnie_uslugi <- prophet(data_to_feed_truncated_ritualnie_uslugi)

# Full frame for predictions. Dates only extraction
data_to_feed_full_ritualnie_uslugi_only <- subset( data_to_feed_full_ritualnie_uslugi, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_ritualnie_uslugi <- predict(prophet_object_ritualnie_uslugi, data_to_feed_full_ritualnie_uslugi_only )
# Fixing dates
prophet_predictions_ritualnie_uslugi$ds <- as.Date(prophet_predictions_ritualnie_uslugi$ds)



# Summaries for mortalities
dim(prophet_predictions_ritualnie_uslugi)
head(prophet_predictions_ritualnie_uslugi)
tail(prophet_predictions_ritualnie_uslugi)

# Adding original data
prophet_predictions_ritualnie_uslugi_plus_original_data <- base::merge( x = data_to_feed_full_ritualnie_uslugi, y = prophet_predictions_ritualnie_uslugi, by = "ds" )
dim(prophet_predictions_ritualnie_uslugi_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_ritualnie_uslugi_plus_original_data)
head(prophet_predictions_ritualnie_uslugi_plus_original_data)
tail(prophet_predictions_ritualnie_uslugi_plus_original_data)


# lower scores
prophet_predictions_ritualnie_uslugi_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_ritualnie_uslugi_plus_original_data$y - prophet_predictions_ritualnie_uslugi_plus_original_data$yhat_lower ) / prophet_predictions_ritualnie_uslugi_plus_original_data$yhat_lower
# upper scores
prophet_predictions_ritualnie_uslugi_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_ritualnie_uslugi_plus_original_data$y - prophet_predictions_ritualnie_uslugi_plus_original_data$yhat_upper ) / prophet_predictions_ritualnie_uslugi_plus_original_data$yhat_upper
# scores
prophet_predictions_ritualnie_uslugi_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_ritualnie_uslugi_plus_original_data$y - prophet_predictions_ritualnie_uslugi_plus_original_data$yhat ) / prophet_predictions_ritualnie_uslugi_plus_original_data$yhat


# Computing the raw excess trends
# raw excess
prophet_predictions_ritualnie_uslugi_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_ritualnie_uslugi_plus_original_data$y - prophet_predictions_ritualnie_uslugi_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_ritualnie_uslugi_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_ritualnie_uslugi_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_ritualnie_uslugi_plus_original_data, file = paste("../R_Data/prophet_predictions_ritualnie_uslugi_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_ritualnie_uslugi_plus_original_data_subset <- 
  prophet_predictions_ritualnie_uslugi_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_ritualnie_uslugi_plus_original_data_subset, file = paste("../R_Data/prophet_predictions_ritualnie_uslugi_plus_original_data_subset.RData") )

# Min and Max
p_score_min_ritualnie_uslugi <- min( c(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$p_scores) )
p_score_max_ritualnie_uslugi <- max( c(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$p_scores) )





# truna

# Fixing the data for the package 
# Number of records BEFORE the epidemic start
number_of_records_truna <- max(which(google_trends_truna_data$Date < pandemic_start))

data_to_feed_full_truna      <- data.frame( ds =  google_trends_truna_data$Date,
                                           y  =  google_trends_truna_data$truna )

data_to_feed_truncated_truna <- data.frame( ds =  google_trends_truna_data$Date[c(1:number_of_records_truna)],
                                           y  =  google_trends_truna_data$truna[c(1:number_of_records_truna)] )


# Listing names of the created objects.
names(data_to_feed_full_truna)
names(data_to_feed_truncated_truna)


# Creating a prophet object.
prophet_object_truna <- prophet(data_to_feed_truncated_truna)

# Full frame for predictions. Dates only extraction
data_to_feed_full_truna_only <- subset( data_to_feed_full_truna, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_truna <- predict(prophet_object_truna, data_to_feed_full_truna_only )
# Fixing dates
prophet_predictions_truna$ds <- as.Date(prophet_predictions_truna$ds)



# Summaries for mortalities
dim(prophet_predictions_truna)
head(prophet_predictions_truna)
tail(prophet_predictions_truna)

# Adding original data
prophet_predictions_truna_plus_original_data <- base::merge( x = data_to_feed_full_truna, y = prophet_predictions_truna, by = "ds" )
dim(prophet_predictions_truna_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_truna_plus_original_data)
head(prophet_predictions_truna_plus_original_data)
tail(prophet_predictions_truna_plus_original_data)


# lower scores
prophet_predictions_truna_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_truna_plus_original_data$y - prophet_predictions_truna_plus_original_data$yhat_lower ) / prophet_predictions_truna_plus_original_data$yhat_lower
# upper scores
prophet_predictions_truna_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_truna_plus_original_data$y - prophet_predictions_truna_plus_original_data$yhat_upper ) / prophet_predictions_truna_plus_original_data$yhat_upper
# scores
prophet_predictions_truna_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_truna_plus_original_data$y - prophet_predictions_truna_plus_original_data$yhat ) / prophet_predictions_truna_plus_original_data$yhat


# Computing the raw excess trends
# raw excess
prophet_predictions_truna_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_truna_plus_original_data$y - prophet_predictions_truna_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_truna_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_truna_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_truna_plus_original_data, file = paste("../R_Data/prophet_predictions_truna_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_truna_plus_original_data_subset <- 
  prophet_predictions_truna_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_truna_plus_original_data_subset, file = paste("../R_Data/prophet_predictions_truna_plus_original_data_subset.RData") )

# Min and Max
p_score_min_truna <- min( c(prophet_predictions_truna_plus_original_data_subset$p_scores) )
p_score_max_truna <- max( c(prophet_predictions_truna_plus_original_data_subset$p_scores) )









# ritualni_poslugi

# Fixing the data for the package 
# Number of records BEFORE the epidemic start
number_of_records_ritualni_poslugi <- max(which(google_trends_ritualni_poslugi_data$Date < pandemic_start))

data_to_feed_full_ritualni_poslugi      <- data.frame( ds =  google_trends_ritualni_poslugi_data$Date,
                                              y  =  google_trends_ritualni_poslugi_data$ritualni_poslugi )

data_to_feed_truncated_ritualni_poslugi <- data.frame( ds =  google_trends_ritualni_poslugi_data$Date[c(1:number_of_records_ritualni_poslugi)],
                                              y  =  google_trends_ritualni_poslugi_data$ritualni_poslugi[c(1:number_of_records_ritualni_poslugi)] )


# Listing names of the created objects.
names(data_to_feed_full_ritualni_poslugi)
names(data_to_feed_truncated_ritualni_poslugi)


# Creating a prophet object.
prophet_object_ritualni_poslugi <- prophet(data_to_feed_truncated_ritualni_poslugi)

# Full frame for predictions. Dates only extraction
data_to_feed_full_ritualni_poslugi_only <- subset( data_to_feed_full_ritualni_poslugi, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_ritualni_poslugi <- predict(prophet_object_ritualni_poslugi, data_to_feed_full_ritualni_poslugi_only )
# Fixing dates
prophet_predictions_ritualni_poslugi$ds <- as.Date(prophet_predictions_ritualni_poslugi$ds)



# Summaries for mortalities
dim(prophet_predictions_ritualni_poslugi)
head(prophet_predictions_ritualni_poslugi)
tail(prophet_predictions_ritualni_poslugi)

# Adding original data
prophet_predictions_ritualni_poslugi_plus_original_data <- base::merge( x = data_to_feed_full_ritualni_poslugi, y = prophet_predictions_ritualni_poslugi, by = "ds" )
dim(prophet_predictions_ritualni_poslugi_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_ritualni_poslugi_plus_original_data)
head(prophet_predictions_ritualni_poslugi_plus_original_data)
tail(prophet_predictions_ritualni_poslugi_plus_original_data)


# lower scores
prophet_predictions_ritualni_poslugi_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_ritualni_poslugi_plus_original_data$y - prophet_predictions_ritualni_poslugi_plus_original_data$yhat_lower ) / prophet_predictions_ritualni_poslugi_plus_original_data$yhat_lower
# upper scores
prophet_predictions_ritualni_poslugi_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_ritualni_poslugi_plus_original_data$y - prophet_predictions_ritualni_poslugi_plus_original_data$yhat_upper ) / prophet_predictions_ritualni_poslugi_plus_original_data$yhat_upper
# scores
prophet_predictions_ritualni_poslugi_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_ritualni_poslugi_plus_original_data$y - prophet_predictions_ritualni_poslugi_plus_original_data$yhat ) / prophet_predictions_ritualni_poslugi_plus_original_data$yhat


# Computing the raw excess trends
# raw excess
prophet_predictions_ritualni_poslugi_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_ritualni_poslugi_plus_original_data$y - prophet_predictions_ritualni_poslugi_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_ritualni_poslugi_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_ritualni_poslugi_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_ritualni_poslugi_plus_original_data, file = paste("../R_Data/prophet_predictions_ritualni_poslugi_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_ritualni_poslugi_plus_original_data_subset <- 
  prophet_predictions_ritualni_poslugi_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_ritualni_poslugi_plus_original_data_subset, file = paste("../R_Data/prophet_predictions_ritualni_poslugi_plus_original_data_subset.RData") )

# Min and Max
p_score_min_ritualni_poslugi <- min( c(prophet_predictions_ritualni_poslugi_plus_original_data_subset$p_scores) )
p_score_max_ritualni_poslugi <- max( c(prophet_predictions_ritualni_poslugi_plus_original_data_subset$p_scores) )









# Generating pdf output.
pdf( paste( "../Plots/FigureTBD03a.pdf", sep = ""), height = 15, width = 25)
# Defining the number of plots
par( par(mfrow=c(3,2)),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(3,2), by.col = TRUE),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,3,5,2,4,6), nrow = 2, ncol = 3, byrow = TRUE)
# Setting layaout
layout(layout_matrix)



# grob

# First plot

lower_index_grob <- max(which(prophet_predictions_grob_plus_original_data_subset$ds < cut_date)) + 1
upper_index_grob <- length(prophet_predictions_grob_plus_original_data_subset$p_scores_upper)
range_grob <- c(lower_index_grob:upper_index_grob)
pandemic_data_length = upper_index_grob - max(which(prophet_predictions_grob_plus_original_data_subset$ds < pandemic_start))
range_grob_last18 <- c((upper_index_grob - pandemic_data_length + 1):upper_index_grob)

barplot( prophet_predictions_grob_plus_original_data_subset$p_scores_upper[range_grob], 
         col= c( rep("#005BBB", 2), rep("#FFD500", (length(range_grob)-2)) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_grob-15, p_score_max_grob+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"grob\"",
         # names.arg = as.character(p_scores_frame_grob_jan_june$Month), 
         names.arg = prophet_predictions_grob_plus_original_data_subset$year_month_text[range_grob], 
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



# grob

# Fourth graph

value_combine <- c(prophet_predictions_grob_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_grob_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_grob_plus_original_data_subset$ds),
     y = prophet_predictions_grob_plus_original_data_subset$yhat_upper,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Fitted (2015-2019) and Predicted (2020-21) \nvs 2015-2021 Data\nGoogle Trend: \"grob\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 1.6,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_grob_plus_original_data_subset$ds),
      y = prophet_predictions_grob_plus_original_data_subset$yhat_upper,
      col = "#005BBB",
      #col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_grob_plus_original_data_subset$ds)[range_grob_last18],
      y = prophet_predictions_grob_plus_original_data_subset$yhat_upper[range_grob_last18],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_grob_plus_original_data_subset$ds)[range_grob_last18],
      y = prophet_predictions_grob_plus_original_data_subset$yhat_upper[range_grob_last18],
      #col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_grob_plus_original_data_subset$ds),
      y = prophet_predictions_grob_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_grob_plus_original_data_subset$ds),
      y = prophet_predictions_grob_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( pandemic_start, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.12, 0.04), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_grob_plus_original_data_subset$ds))
final_date   <- max(as.integer(prophet_predictions_grob_plus_original_data_subset$ds))
number_of_dates <- length( as.integer(prophet_predictions_grob_plus_original_data_subset$ds) )


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(prophet_predictions_grob_plus_original_data_subset$ds),  by = 5 )
# x_indexes_to_display <-  prophet_predictions_grob_plus_original_data_subset$ds
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- prophet_predictions_grob_plus_original_data_subset$ds[x_indexes_to_display]
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_grob_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_grob_plus_original_data_subset$year_month_text[x_indexes_to_display] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine  )
y_max_value <- max( value_combine  )
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

txt <- "D"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)






# pominki

# Second plot

lower_index_pominki <- max(which(prophet_predictions_pominki_plus_original_data_subset$ds < cut_date)) + 1
upper_index_pominki <- length(prophet_predictions_pominki_plus_original_data_subset$p_scores_upper)
range_pominki <- c(lower_index_pominki:upper_index_pominki)
pandemic_data_length = upper_index_pominki - max(which(prophet_predictions_grob_plus_original_data_subset$ds < pandemic_start))
range_pominki_last18 <- c((upper_index_pominki - pandemic_data_length + 1):upper_index_pominki)

barplot( prophet_predictions_pominki_plus_original_data_subset$p_scores_upper[range_pominki], 
         col= c( rep("#005BBB", 2), rep("#FFD500", (length(range_pominki)-2)) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_pominki-15, p_score_max_pominki+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"pominki\"",
         # names.arg = as.character(p_scores_frame_pominki_jan_june$Month), 
         names.arg = prophet_predictions_pominki_plus_original_data_subset$year_month_text[range_pominki], 
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


# Label B
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



# pominki

# Fifths graph

value_combine <- c(prophet_predictions_pominki_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_pominki_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
     y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Fitted (2015-2019) and Predicted (2020-21) \nvs 2015-2021 Data\nGoogle Trend: \"pominki\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.175 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 1.6,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
      y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper,
      col = "#005BBB",
      #col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds)[range_pominki_last18],
      y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper[range_pominki_last18],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds)[range_pominki_last18],
      y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper[range_pominki_last18],
      #col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
      y = prophet_predictions_pominki_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
      y = prophet_predictions_pominki_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( pandemic_start, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.12, 0.04), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_pominki_plus_original_data_subset$ds))
final_date   <- max(as.integer(prophet_predictions_pominki_plus_original_data_subset$ds))
number_of_dates <- length( as.integer(prophet_predictions_pominki_plus_original_data_subset$ds) )


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(prophet_predictions_pominki_plus_original_data_subset$ds),  by = 5 )
# x_indexes_to_display <-  prophet_predictions_pominki_plus_original_data_subset$ds
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- prophet_predictions_pominki_plus_original_data_subset$ds[x_indexes_to_display]
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_pominki_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_pominki_plus_original_data_subset$year_month_text[x_indexes_to_display] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine  )
y_max_value <- max( value_combine  )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label E
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







# ritualnie_uslugi

# Third plot

lower_index_ritualnie_uslugi <- max(which(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds < cut_date)) + 1
upper_index_ritualnie_uslugi <- length(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$p_scores_upper)
range_ritualnie_uslugi <- c(lower_index_ritualnie_uslugi:upper_index_ritualnie_uslugi)
pandemic_data_length = upper_index_ritualnie_uslugi - max(which(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds < pandemic_start))
range_ritualnie_uslugi_last18 <- c((upper_index_ritualnie_uslugi - pandemic_data_length + 1):upper_index_ritualnie_uslugi)

barplot( prophet_predictions_ritualnie_uslugi_plus_original_data_subset$p_scores_upper[range_ritualnie_uslugi], 
         col= c( rep("#005BBB", 2), rep("#FFD500", (length(range_ritualnie_uslugi)-2)) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_ritualnie_uslugi-15, p_score_max_ritualnie_uslugi+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"ritualnie uslugi\"",
         # names.arg = as.character(p_scores_frame_ritualnie_uslugi_jan_june$Month), 
         names.arg = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$year_month_text[range_ritualnie_uslugi], 
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

txt <- "C"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 6 / 5
text(x, y, txt, cex = 4)






# ritualnie_uslugi

# Fifths graph

value_combine <- c(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_ritualnie_uslugi_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds),
     y = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$yhat_upper,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Fitted (2015-2019) and Predicted (2020-21) \nvs 2015-2021 Data\nGoogle Trend: \"ritualnie uslugi\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 1.6,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds),
      y = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$yhat_upper,
      col = "#005BBB",
      #col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds)[range_ritualnie_uslugi_last18],
      y = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$yhat_upper[range_ritualnie_uslugi_last18],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds)[range_ritualnie_uslugi_last18],
      y = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$yhat_upper[range_ritualnie_uslugi_last18],
      #col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds),
      y = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds),
      y = prophet_predictions_ritualnie_uslugi_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( pandemic_start, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.12, 0.04), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds))
final_date   <- max(as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds))
number_of_dates <- length( as.integer(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds) )


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds),  by = 5 )
# x_indexes_to_display <-  prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- prophet_predictions_ritualnie_uslugi_plus_original_data_subset$ds[x_indexes_to_display]
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_ritualnie_uslugi_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_ritualnie_uslugi_plus_original_data_subset$year_month_text[x_indexes_to_display] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine  )
y_max_value <- max( value_combine  )
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
pdf( paste( "../Plots/FigureTBD03b.pdf", sep = ""), height = 15, width = 25)
# Defining the number of plots
par( par(mfrow=c(3,2)),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(3,2), by.col = TRUE),  mar=c(7.1, 5.1, 5.1, 2.1)  )
# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,3,5,2,4,6), nrow = 2, ncol = 3, byrow = TRUE)
# Setting layaout
layout(layout_matrix)



# truna

# Second plot

lower_index_truna <- max(which(prophet_predictions_truna_plus_original_data_subset$ds < cut_date)) + 1
upper_index_truna <- length(prophet_predictions_truna_plus_original_data_subset$p_scores_upper)
range_truna <- c(lower_index_truna:upper_index_truna)
pandemic_data_length = upper_index_truna - max(which(prophet_predictions_truna_plus_original_data_subset$ds < pandemic_start))
range_truna_last18 <- c((upper_index_truna - pandemic_data_length + 1):upper_index_truna)

barplot( prophet_predictions_truna_plus_original_data_subset$p_scores_upper[range_truna], 
         col= c( rep("#005BBB", 2), rep("#FFD500", (length(range_truna)-2)) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_truna-15, p_score_max_truna+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"truna\"",
         # names.arg = as.character(p_scores_frame_truna_jan_june$Month), 
         names.arg = prophet_predictions_truna_plus_original_data_subset$year_month_text[range_truna], 
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



# truna

# Fifths graph

value_combine <- c(prophet_predictions_truna_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_truna_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_truna_plus_original_data_subset$ds),
     y = prophet_predictions_truna_plus_original_data_subset$yhat_upper,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Fitted (2015-2019) and Predicted (2020-21) \nvs 2015-2021 Data\nGoogle Trend: \"truna\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.175 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 1.6,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_truna_plus_original_data_subset$ds),
      y = prophet_predictions_truna_plus_original_data_subset$yhat_upper,
      col = "#005BBB",
      #col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_truna_plus_original_data_subset$ds)[range_truna_last18],
      y = prophet_predictions_truna_plus_original_data_subset$yhat_upper[range_truna_last18],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_truna_plus_original_data_subset$ds)[range_truna_last18],
      y = prophet_predictions_truna_plus_original_data_subset$yhat_upper[range_truna_last18],
      #col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_truna_plus_original_data_subset$ds),
      y = prophet_predictions_truna_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_truna_plus_original_data_subset$ds),
      y = prophet_predictions_truna_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( pandemic_start, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.12, 0.04), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_truna_plus_original_data_subset$ds))
final_date   <- max(as.integer(prophet_predictions_truna_plus_original_data_subset$ds))
number_of_dates <- length( as.integer(prophet_predictions_truna_plus_original_data_subset$ds) )


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(prophet_predictions_truna_plus_original_data_subset$ds),  by = 5 )
# x_indexes_to_display <-  prophet_predictions_truna_plus_original_data_subset$ds
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- prophet_predictions_truna_plus_original_data_subset$ds[x_indexes_to_display]
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_truna_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_truna_plus_original_data_subset$year_month_text[x_indexes_to_display] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine  )
y_max_value <- max( value_combine  )
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




# pominki

# Second plot

lower_index_pominki <- max(which(prophet_predictions_pominki_plus_original_data_subset$ds < cut_date)) + 1
upper_index_pominki <- length(prophet_predictions_pominki_plus_original_data_subset$p_scores_upper)
range_pominki <- c(lower_index_pominki:upper_index_pominki)
pandemic_data_length = upper_index_pominki - max(which(prophet_predictions_pominki_plus_original_data_subset$ds < pandemic_start))
range_pominki_last18 <- c((upper_index_pominki - pandemic_data_length + 1):upper_index_pominki)

barplot( prophet_predictions_pominki_plus_original_data_subset$p_scores_upper[range_pominki], 
         col= c( rep("#005BBB", 2), rep("#FFD500", (length(range_pominki)-2)) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_pominki-15, p_score_max_pominki+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"pominki\"",
         # names.arg = as.character(p_scores_frame_pominki_jan_june$Month), 
         names.arg = prophet_predictions_pominki_plus_original_data_subset$year_month_text[range_pominki], 
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


# Label B
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



# pominki

# Fifths graph

value_combine <- c(prophet_predictions_pominki_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_pominki_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
     y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Fitted (2015-2019) and Predicted (2020-21) \nvs 2015-2021 Data\nGoogle Trend: \"pominki\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.175 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 1.6,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
      y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper,
      col = "#005BBB",
      #col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds)[range_pominki_last18],
      y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper[range_pominki_last18],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds)[range_pominki_last18],
      y = prophet_predictions_pominki_plus_original_data_subset$yhat_upper[range_pominki_last18],
      #col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
      y = prophet_predictions_pominki_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_pominki_plus_original_data_subset$ds),
      y = prophet_predictions_pominki_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( pandemic_start, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.12, 0.04), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_pominki_plus_original_data_subset$ds))
final_date   <- max(as.integer(prophet_predictions_pominki_plus_original_data_subset$ds))
number_of_dates <- length( as.integer(prophet_predictions_pominki_plus_original_data_subset$ds) )


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(prophet_predictions_pominki_plus_original_data_subset$ds),  by = 5 )
# x_indexes_to_display <-  prophet_predictions_pominki_plus_original_data_subset$ds
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- prophet_predictions_pominki_plus_original_data_subset$ds[x_indexes_to_display]
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_pominki_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_pominki_plus_original_data_subset$year_month_text[x_indexes_to_display] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine  )
y_max_value <- max( value_combine  )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label E
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




# ritualni_poslugi

# Third plot

lower_index_ritualni_poslugi <- max(which(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds < cut_date)) + 1
upper_index_ritualni_poslugi <- length(prophet_predictions_ritualni_poslugi_plus_original_data_subset$p_scores_upper)
range_ritualni_poslugi <- c(lower_index_ritualni_poslugi:upper_index_ritualni_poslugi)
pandemic_data_length = upper_index_ritualni_poslugi - max(which(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds < pandemic_start))
range_ritualni_poslugi_last18 <- c((upper_index_ritualni_poslugi - pandemic_data_length + 1):upper_index_ritualni_poslugi)

barplot( prophet_predictions_ritualni_poslugi_plus_original_data_subset$p_scores_upper[range_ritualni_poslugi], 
         col= c( rep("#005BBB", 2), rep("#FFD500", (length(range_ritualni_poslugi)-2)) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min_ritualni_poslugi-15, p_score_max_ritualni_poslugi+15), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores (in Percent) for 2020-2021\nGoogle Trend: \"ritualni poslugi\"",
         # names.arg = as.character(p_scores_frame_ritualni_poslugi_jan_june$Month), 
         names.arg = prophet_predictions_ritualni_poslugi_plus_original_data_subset$year_month_text[range_ritualni_poslugi], 
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






# ritualni_poslugi

# Fifths graph

value_combine <- c(prophet_predictions_ritualni_poslugi_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_ritualni_poslugi_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds),
     y = prophet_predictions_ritualni_poslugi_plus_original_data_subset$yhat_upper,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Fitted (2015-2019) and Predicted (2020-21) \nvs 2015-2021 Data\nGoogle Trend: \"ritualni poslugi\"",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( min(value_combine),
               max(value_combine) * 1.01 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 1.6,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds),
      y = prophet_predictions_ritualni_poslugi_plus_original_data_subset$yhat_upper,
      col = "#005BBB",
      #col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds)[range_ritualni_poslugi_last18],
      y = prophet_predictions_ritualni_poslugi_plus_original_data_subset$yhat_upper[range_ritualni_poslugi_last18],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds)[range_ritualni_poslugi_last18],
      y = prophet_predictions_ritualni_poslugi_plus_original_data_subset$yhat_upper[range_ritualni_poslugi_last18],
      #col = "#005BBB",
      col = "#FFD500",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds),
      y = prophet_predictions_ritualni_poslugi_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds),
      y = prophet_predictions_ritualni_poslugi_plus_original_data_subset$y,
      #col = "#005BBB",
      col = "#00bb61",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( pandemic_start, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.12, 0.04), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Trend", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds))
final_date   <- max(as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds))
number_of_dates <- length( as.integer(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds) )


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds),  by = 5 )
# x_indexes_to_display <-  prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- prophet_predictions_ritualni_poslugi_plus_original_data_subset$ds[x_indexes_to_display]
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_ritualni_poslugi_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_ritualni_poslugi_plus_original_data_subset$year_month_text[x_indexes_to_display] )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min( value_combine  )
y_max_value <- max( value_combine  )
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



