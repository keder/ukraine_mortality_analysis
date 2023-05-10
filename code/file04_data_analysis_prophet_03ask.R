
rm(list = ls(all = TRUE))

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

pandemic_start <- as.Date("2020-03-15")


# install.packages("prophet")
# library("prophet") - libary for time series forecasting.
library("prophet")



# Reading previous datasets

# Monthly overall mortality data since 2015
load(file = paste("../../R_Data/ukraine_un_mortality_data_month_only_since_2015.RData"))

# Loading demograhics data
load(file = paste("../../R_Data/demographics_aggregated_2011_2020.RData"))




# Fixing the data for the package for five years
# Number of records BEFORE the epidemic start
number_of_records_five <- max(which(ukraine_un_mortality_data_month_only_since_2015$date_fixed < pandemic_start))

data_to_feed_full_five <- data.frame(
      ds = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value
)

data_to_feed_truncated_five <- data.frame(
      ds = ukraine_un_mortality_data_month_only_since_2015$date_fixed[c(1:number_of_records_five)],
      y = ukraine_un_mortality_data_month_only_since_2015$Value[c(1:number_of_records_five)]
)


# Listing names of the created objects.
names(data_to_feed_full_five)
names(data_to_feed_truncated_five)


# Creating a prophet object.
prophet_object_five <- prophet(data_to_feed_truncated_five)

# Full frame for predictions. Dates only extraction
data_to_feed_full_five_dates_only <- subset(data_to_feed_full_five, select = -c(y))

# Precting for the specified dates.
prophet_predictions_five <- predict(prophet_object_five, data_to_feed_full_five_dates_only)
# Fixing dates
prophet_predictions_five$ds <- as.Date(prophet_predictions_five$ds)
# Forecast diagnostics 
horizon <- 365
last_date <- max(data_to_feed_truncated_five$ds)
# cutoffs = data_to_feed_truncated_five$ds[data_to_feed_truncated_five$ds < (last_date-horizon)]
# cutoffs = cutoffs[2:length(cutoffs)]
# print(cutoffs)
cv_results = cross_validation(prophet_object_five, period=30, initial = 365*4, horizon=horizon, units="days")
prophet_predictions_five_plus_original_data_diag = performance_metrics(cv_results)
save(prophet_predictions_five_plus_original_data_diag, file = paste("../../R_Data/prophet_predictions_five_plus_original_data_diag.RData"))



# Summaries for mortalities
dim(prophet_predictions_five)
head(prophet_predictions_five)
tail(prophet_predictions_five)

# Adding original data
prophet_predictions_five_plus_original_data <- base::merge(x = data_to_feed_full_five, y = prophet_predictions_five, by = "ds")
dim(prophet_predictions_five_plus_original_data)


# Summaries for mortalities
dim(prophet_predictions_five_plus_original_data)
head(prophet_predictions_five_plus_original_data)
tail(prophet_predictions_five_plus_original_data)


# lower scores
prophet_predictions_five_plus_original_data$p_scores_lower <-
      100 * (prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat_lower) / prophet_predictions_five_plus_original_data$yhat_lower
# upper scores
prophet_predictions_five_plus_original_data$p_scores_upper <-
      100 * (prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat_upper) / prophet_predictions_five_plus_original_data$yhat_upper
# scores
prophet_predictions_five_plus_original_data$p_scores <-
      100 * (prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat) / prophet_predictions_five_plus_original_data$yhat


# Computing the raw excess mortalities
# raw excess
prophet_predictions_five_plus_original_data$raw_y_minus_yhat_upper <-
      prophet_predictions_five_plus_original_data$y - prophet_predictions_five_plus_original_data$yhat_upper


# Creating year and month in text
prophet_predictions_five_plus_original_data$year_month_text <- substr(x = as.character(prophet_predictions_five_plus_original_data$ds), start = 1, stop = 7)


# Saving the data as RData file.
save(prophet_predictions_five_plus_original_data, file = paste("../../R_Data/prophet_predictions_five_plus_original_data.RData"))

# Creating a sumbset with predicitons only
prophet_predictions_five_plus_original_data_subset <-
      prophet_predictions_five_plus_original_data[, c("ds", "year_month_text", "y", "yhat", "yhat_lower", "yhat_upper", "p_scores", "p_scores_lower", "p_scores_upper", "raw_y_minus_yhat_upper")]

# Saving the data as RData file.
save(prophet_predictions_five_plus_original_data_subset, file = paste("../../R_Data/prophet_predictions_five_plus_original_data_subset.RData"))




# Min and Max
p_score_min <- min(c(prophet_predictions_five_plus_original_data_subset$p_scores))
p_score_max <- max(c(prophet_predictions_five_plus_original_data_subset$p_scores))





# Generating pdf output.
pdf(paste("../../Plots/Figure03a.pdf", sep = ""), height = 15, width = 25)
# Definign the number of plots
par(par(mfrow = c(2, 1)), mar = c(5.5, 5.1, 5.1, 2.1))


# First plot

lower_index_five <- 1
upper_index_five <- length(prophet_predictions_five_plus_original_data_subset$p_scores_upper)
range_five <- c(lower_index_five:upper_index_five)
pandemic_data_length = upper_index_five - max(which(prophet_predictions_five_plus_original_data_subset$ds < pandemic_start))
range_five_last4 <- c((upper_index_five - pandemic_data_length + 1):upper_index_five)

barplot(prophet_predictions_five_plus_original_data_subset$p_scores_upper[range_five],
      col = c(rep("#005BBB", length(range_five) - pandemic_data_length), rep("#FFD500", pandemic_data_length)),
      legend = TRUE,
      border = TRUE,
      # xlim = c(1, 5),
      ylim = c(p_score_min - 5, p_score_max + 5),
      args.legend = list(bty = "n", border = TRUE),
      ylab = "",
      xlab = "",
      main = "P-Scores For 2015/01-2020/02 Model Fits\n & 2020/03-2021/12 Prediction",
      # names.arg = as.character(p_scores_frame_five_jan_june$Month),
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
      # names.arg = as.character(p_scores_frame_five_jan_june$Month),
      names.arg = prophet_predictions_five_plus_original_data_subset$year_month_text[range_five],
      cex.names = 1.25,
      cex.lab = 2,
      cex.axis = 1.75,
      cex.main = 2,
      cex = 2,
      las = 2
)


legend(
      x = "topleft",
      inset = c(0.06, 0.08),
      legend = c("Pre Epidemic", "During Epidemic"),
      col = "black",   
      fill = c("#005BBB", "#FFD500"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
        # pch = c(19, 20),  
      # pch = c(19, 20),
      cex = 2
)


# Label A
par(xpd = NA)

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "A"
x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
y <- y[2] - strheight(txt, cex = 4) * 6 / 5
text(x, y, txt, cex = 4)









# Third graph

value_combine <- c(
      prophet_predictions_five_plus_original_data_subset$yhat_upper,
      prophet_predictions_five_plus_original_data_subset$y
)


plot(
      x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five],
      col = "#005BBB",
      # col = color_01,
     # col = color_01, 
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l",
      # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
      main = "Reported (2015/01-2021/12)\nFitted (2015/01-2020/02) & Predicted (2020/03-2021/12)",
      # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
      ylim = c(
            min(value_combine),
            max(value_combine) * 1.01
      ),
      # ylim = c(0, y_max_value_current * 1.2  ),
      # xlab = "Time",    
      xlab = "",
      ylab = "Counts",
      xaxt = "n",
      yaxt = "n",
      cex = 3,
      cex.axis = 1.55,
      cex.lab = 2,
      cex.main = 1.45,
      cex.sub = 2
)
lines(
      x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five],
      col = "#005BBB",
      # col = "#00bb61",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)
lines(
      x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five_last4],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five_last4],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l"
)
lines(
      x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five_last4],
      y = prophet_predictions_five_plus_original_data_subset$yhat_upper[range_five_last4],
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)
lines(
      x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$y[range_five],
      # col = "#005BBB",
      col = "#00bb61",
      # col = color_01,
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l"
)
lines(
      x = as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five],
      y = prophet_predictions_five_plus_original_data_subset$y[range_five],
      # col = "#005BBB",
      col = "#00bb61",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)

lines(
      x = rep(as.integer(pandemic_start), 10),
      y = c(rep(min(value_combine), 5), rep(max(value_combine), 5)),
      col = "red",
      lwd = 1,
      lty = 2
)

legend(
      x = "topleft",
      inset = c(0.25, 0.05),
      legend = c("Fitted Trend", "Predicted Trend", "Actual Data", "Epidemic Start"),
      col = "black",   
      fill = c("#005BBB", "#FFD500", "#00bb61", "red"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
        # pch = c(19, 20),  
      # pch = c(19, 20),
      cex = 1.85
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five])
final_date <- max(as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five])
number_of_dates <- length(as.integer(prophet_predictions_five_plus_original_data_subset$ds)[range_five])


# Indexes to display
# x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_five_jan_june$Month),  by = 1 )
x_indexes_to_display <- prophet_predictions_five_plus_original_data_subset$ds[range_five]
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist <- as.character(prophet_predictions_five_plus_original_data_subset$year_month_text[range_five])
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.03 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.2)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(value_combine)
y_max_value <- max(value_combine)
y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist <- as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label B
par(xpd = NA)

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "B"
x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
y <- y[2] - strheight(txt, cex = 4) * 6 / 5
text(x, y, txt, cex = 4)






dev.off()