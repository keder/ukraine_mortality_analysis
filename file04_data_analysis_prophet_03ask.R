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

# Cummulative data: incidence, recovered, and mortality
load( file = paste("R_Data/belarus_statistics_data_frame_covid19.RData") )

# Monthly COVID-19 mortality data
load( file = paste("R_Data/monthly_death_data_frame_covid19.RData") )

# Monthly overall mortality data since 2011
load( file = paste("R_Data/belarus_un_mortality_data_month_only_since_2011.RData") )

# Monthly overall mortality data since 2015
load( file = paste("R_Data/belarus_un_mortality_data_month_only_since_2015.RData") )

# Loading demograhics data
load( file = paste("R_Data/demographics_aggregated_2011_2020.RData") )




# Fixing the data for the package for five years
# Number of records BEFORE the epidemic start
number_of_records_five <- dim(belarus_un_mortality_data_month_only_since_2015)[1] - 4

data_to_feed_full_five      <- data.frame( ds =  belarus_un_mortality_data_month_only_since_2015$date_fixed,
                                           y  =  belarus_un_mortality_data_month_only_since_2015$Value )

data_to_feed_truncated_five <- data.frame( ds =  belarus_un_mortality_data_month_only_since_2015$date_fixed[c(1:number_of_records_five)],
                                           y  =  belarus_un_mortality_data_month_only_since_2015$Value[c(1:number_of_records_five)] )


# Listing names of the created objects.
names(data_to_feed_full_five)
names(data_to_feed_truncated_five)


# Creating a prophet object.
prophet_object_five <- prophet(data_to_feed_truncated_five)

# Full frame for predictions. Dates only extraction
data_to_feed_full_five_dates_only <- subset( data_to_feed_full_five, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_five <- predict(prophet_object_five, data_to_feed_full_five_dates_only )
# Fixing dates
prophet_predictions_five$ds <- as.Date(prophet_predictions_five$ds)


# Summaries for mortalities
dim(prophet_predictions_five)
head(prophet_predictions_five)
tail(prophet_predictions_five)

# Adding original data
prophet_predictions_five_plus_original_data <- base::merge( x = data_to_feed_full_five, y = prophet_predictions_five, by = "ds" )
dim(prophet_predictions_five_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_five_plus_original_data)
head(prophet_predictions_five_plus_original_data)
tail(prophet_predictions_five_plus_original_data)


# lower scores
prophet_predictions_five_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat_lower ) / prophet_predictions_five_plus_original_data$yhat_lower
# upper scores
prophet_predictions_five_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat_upper ) / prophet_predictions_five_plus_original_data$yhat_upper
# scores
prophet_predictions_five_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat ) / prophet_predictions_five_plus_original_data$yhat


# Computing the raw excess mortalities
# raw excess
prophet_predictions_five_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_five_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_five_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_five_plus_original_data, file = paste("R_Data/prophet_predictions_five_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_five_plus_original_data_subset <- 
      prophet_predictions_five_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_five_plus_original_data_subset, file = paste("R_Data/prophet_predictions_five_plus_original_data_subset.RData") )








# Fixing the data for the package for eight years
# Number of records BEFORE the epidemic start
number_of_records_eight <- dim(belarus_un_mortality_data_month_only_since_2011)[1] - 4

data_to_feed_full_eight      <- data.frame( ds =  belarus_un_mortality_data_month_only_since_2011$date_fixed,
                                           y  =  belarus_un_mortality_data_month_only_since_2011$Value )

data_to_feed_truncated_eight <- data.frame( ds =  belarus_un_mortality_data_month_only_since_2011$date_fixed[c(1:number_of_records_eight)],
                                           y  =  belarus_un_mortality_data_month_only_since_2011$Value[c(1:number_of_records_eight)] )


# Listing names of the created objects.
names(data_to_feed_full_eight)
names(data_to_feed_truncated_eight)


# Creating a prophet object.
prophet_object_eight <- prophet(data_to_feed_truncated_eight)

# Full frame for predictions. Dates only extraction
data_to_feed_full_eight_dates_only <- subset( data_to_feed_full_eight, select = -c(y) ) 

# Precting for the specified dates.
prophet_predictions_eight <- predict(prophet_object_eight, data_to_feed_full_eight_dates_only )
# Fixing dates
prophet_predictions_eight$ds <- as.Date(prophet_predictions_eight$ds)


# Summaries for mortalities
dim(prophet_predictions_eight)
head(prophet_predictions_eight)
tail(prophet_predictions_eight)

# Adding original data
prophet_predictions_eight_plus_original_data <- base::merge( x = data_to_feed_full_eight, y = prophet_predictions_eight, by = "ds" )
dim(prophet_predictions_eight_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_eight_plus_original_data)
head(prophet_predictions_eight_plus_original_data)
tail(prophet_predictions_eight_plus_original_data)


# lower scores
prophet_predictions_eight_plus_original_data$p_scores_lower <- 
  100 * ( prophet_predictions_eight_plus_original_data$y - prophet_predictions_eight_plus_original_data$yhat_lower ) / prophet_predictions_eight_plus_original_data$yhat_lower
# upper scores
prophet_predictions_eight_plus_original_data$p_scores_upper <- 
  100 * ( prophet_predictions_eight_plus_original_data$y - prophet_predictions_eight_plus_original_data$yhat_upper ) / prophet_predictions_eight_plus_original_data$yhat_upper
# scores
prophet_predictions_eight_plus_original_data$p_scores <- 
  100 * ( prophet_predictions_eight_plus_original_data$y - prophet_predictions_eight_plus_original_data$yhat ) / prophet_predictions_eight_plus_original_data$yhat


# Computing the raw excess mortalities
# raw excess
prophet_predictions_eight_plus_original_data$raw_y_minus_yhat_upper <- 
  prophet_predictions_eight_plus_original_data$y - prophet_predictions_eight_plus_original_data$yhat_upper


# Creating year and month in text  
prophet_predictions_eight_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_eight_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save( prophet_predictions_eight_plus_original_data, file = paste("R_Data/prophet_predictions_eight_plus_original_data.RData") )

# Creating a sumbset with predicitons only
prophet_predictions_eight_plus_original_data_subset <- 
  prophet_predictions_eight_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper") ]

# Saving the data as RData file.
save( prophet_predictions_eight_plus_original_data_subset, file = paste("R_Data/prophet_predictions_eight_plus_original_data_subset.RData") )





# Min and Max
p_score_min <- min( c(prophet_predictions_five_plus_original_data_subset$p_scores, prophet_predictions_eight_plus_original_data_subset$p_scores) )
p_score_max <- max( c(prophet_predictions_five_plus_original_data_subset$p_scores, prophet_predictions_eight_plus_original_data_subset$p_scores) )





# Generating pdf output.
pdf( paste( getwd(), "/Plots/Figure03a.pdf", sep = ""), height = 15, width = 15)
# Definign the number of plots
par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# First plot

lower_index_five <- length(prophet_predictions_five_plus_original_data_subset$p_scores_upper) - 17
upper_index_five <- length(prophet_predictions_five_plus_original_data_subset$p_scores_upper)
range_five <- c(lower_index_five:upper_index_five)
range_five_last4 <- c(upper_index_five - c(3:0))

barplot( prophet_predictions_five_plus_original_data_subset$p_scores_upper[range_five], 
         col= c( rep("darkblue", length(prophet_predictions_five_plus_original_data_subset$p_scores[range_five])-4), rep("orange", 4) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min-5, p_score_max+5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores For 2015/01-2020/02 Model Fits\n & 2020/03-2020/06 Prediction",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = prophet_predictions_five_plus_original_data_subset$year_month_text[range_five], 
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
        fill = c("darkblue", "darkorange"),   
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

lower_index_eight <- length(prophet_predictions_eight_plus_original_data_subset$p_scores_upper) - 17
upper_index_eight <- length(prophet_predictions_eight_plus_original_data_subset$p_scores_upper)
range_eight <- c(lower_index_eight:upper_index_eight)
range_eight_last4 <- c(upper_index_eight - c(3:0))

barplot( prophet_predictions_eight_plus_original_data_subset$p_scores_upper[range_eight], 
         col= c( rep("darkblue", length(prophet_predictions_eight_plus_original_data_subset$p_scores[range_eight])-4), rep("orange", 4) ), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c(p_score_min-5, p_score_max+5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "P-Scores For 2011/01-2020/02 Model Fits\n & 2020/03-2020/06 Prediction",
         # names.arg = as.character(p_scores_frame_eight_jan_june$Month), 
         names.arg = prophet_predictions_eight_plus_original_data_subset$year_month_text[range_eight], 
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
        fill = c("darkblue", "darkorange"),   
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






# Third graph

value_combine <- c(prophet_predictions_five_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_five_plus_original_data_subset$y,
                   prophet_predictions_eight_plus_original_data_subset$yhat_upper,
                   prophet_predictions_eight_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
     y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five],
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Reported (2015/01-2020/02)\nFitted (2015/01-2020/02) & Predicted (2020/03-2020/06)",
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
     cex.main = 1.45,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five],
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five_last4],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five_last4],
      # col = "darkblue",
      col = "darkorange",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five_last4],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five_last4],
      #col = "darkblue",
      col = "darkorange",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$y[range_five],
      #col = "darkblue",
      col = "darkturquoise",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$y[range_five],
      #col = "darkblue",
      col = "darkturquoise",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")

lines(x = rep( max(as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five]) - 3.5 * 30, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)

legend( x = "topleft", 
        inset= c(0.08, 0.08), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Data", "Epidemic Start"), 
        col = "black", 
        fill = c("darkblue", "darkorange", "darkturquoise", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five])
final_date   <- max(as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five])
number_of_dates <- length( as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five] )


# Indexes to display
# x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_five_jan_june$Month),  by = 1 )
x_indexes_to_display <-  prophet_predictions_five_plus_original_data_subset$ds[range_five]
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_five_plus_original_data_subset$year_month_text[range_five] )
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






# Fourth graph

value_combine <- c(prophet_predictions_five_plus_original_data_subset$yhat_upper, 
                   prophet_predictions_five_plus_original_data_subset$y,
                   prophet_predictions_eight_plus_original_data_subset$yhat_upper,
                   prophet_predictions_eight_plus_original_data_subset$y)


plot(x = as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight],
     y = prophet_predictions_eight_plus_original_data_subset$yhat_upper[range_eight],
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Reported (2011/01-2020/02)\nFitted (2011/01-2020/02) & Predicted (2020/03-2020/06)",
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
     cex.main = 1.45,
     cex.sub = 2
)
lines(x = as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight],
      y = prophet_predictions_eight_plus_original_data_subset$yhat_upper[range_eight],
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight_last4],
      y = prophet_predictions_eight_plus_original_data_subset$yhat_upper[range_eight_last4],
      # col = "darkblue",
      col = "darkorange",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight_last4],
      y = prophet_predictions_eight_plus_original_data_subset$yhat_upper[range_eight_last4],
      #col = "darkblue",
      col = "darkorange",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight],
      y = prophet_predictions_eight_plus_original_data_subset$y[range_eight],
      #col = "darkblue",
      col = "darkturquoise",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight],
      y = prophet_predictions_eight_plus_original_data_subset$y[range_eight],
      #col = "darkblue",
      col = "darkturquoise",
      # col = color_01, 
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")

lines(x = rep( max(as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight]) - 3.5 * 30, 10), 
      y = c( rep( min(value_combine), 5),  rep( max(value_combine), 5) ), 
      col="red", 
      lwd = 1, 
      lty = 2)

legend( x = "topleft", 
        inset= c(0.08, 0.08), 
        legend = c("Fitted Trend", "Predicted Trend", "Actual Data", "Epidemic Start"), 
        col = "black", 
        fill = c("darkblue", "darkorange", "darkturquoise", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight])
final_date   <- max(as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight])
number_of_dates <- length( as.integer(prophet_predictions_eight_plus_original_data_subset$ds)[range_eight] )


# Indexes to display
# x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_five_jan_june$Month),  by = 1 )
x_indexes_to_display <-  prophet_predictions_eight_plus_original_data_subset$ds[range_eight]
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( prophet_predictions_eight_plus_original_data_subset$year_month_text[range_eight] )
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






dev.off()





