# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.24. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)

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



# Loding data saved in file01

# Reading COVID-19 data from onliner.by

# Reading the data in R
load(file = paste("../R_Data/belarus_incidence_data_frame_covid19.RData"))
# Reading the data in R
load(file = paste("../R_Data/belarus_statistics_data_frame_covid19.RData"))

# Reading mortailty UN Data

# Reading the RAW data in R
load(file = paste("../R_Data/belarus_un_mortality_data.RData"))
# Reading the PROCESSED data in R
load(file = paste("../R_Data/monthly_death_data_frame_covid19.RData"))

ls()




# Fix 2020.04.25
# Computing non-parametric p-scores

# Extracting the last five years 2015 and later
# years after 2015
belarus_un_mortality_data_truncated_with_others <- belarus_un_mortality_data[which(belarus_un_mortality_data$Year >= 2015), ]
# Dropping total and unknown
belarus_un_mortality_data_truncated_month <- belarus_un_mortality_data_truncated_with_others[which(belarus_un_mortality_data_truncated_with_others$Month %in% month.name), ]
dim(belarus_un_mortality_data_truncated_month)

# Parsing pre-2020 and 2020
# 2015-2019
belarus_un_mortality_data_truncated_month_2015_2019 <- belarus_un_mortality_data_truncated_month[which(belarus_un_mortality_data_truncated_month$Year < 2020), ]
# Only 2020
belarus_un_mortality_data_truncated_month_2020 <- belarus_un_mortality_data_truncated_month[which(belarus_un_mortality_data_truncated_month$Year >= 2020), ]

data_month_count <- 14


# Creating p-scores to be saved.
p_scores_frame_five_length = dim(belarus_un_mortality_data_truncated_month_2020)[1]
p_scores_frame_five <- data.frame(
      Month = belarus_un_mortality_data_truncated_month_2020$Month,
      Year = belarus_un_mortality_data_truncated_month_2020$Year,
      average_five_years = rep(0, p_scores_frame_five_length),
      Value = rep(0, p_scores_frame_five_length),
      p_score_value = rep(0, p_scores_frame_five_length),
      month_text = rep("X", p_scores_frame_five_length)
)




# Computing non-parametrix p-scores
for (i in 1:nrow(belarus_un_mortality_data_truncated_month_2020)) {
      # Debuggins step
      # m <- month.name[1]
      item = belarus_un_mortality_data_truncated_month_2020[i,]

      # Getting current month for the past five years
      month_current_frame_2015_2019 <- belarus_un_mortality_data_truncated_month_2015_2019[which(belarus_un_mortality_data_truncated_month_2015_2019$Month == item$Month), ]

      # Saving the average value
      p_scores_frame_five[which(p_scores_frame_five$Month == item$Month & p_scores_frame_five$Year == item$Year), "Value"] <- item$Value
      value_current <- item$Value

      # Saving the average value
      p_scores_frame_five[which(p_scores_frame_five$Month == item$Month & p_scores_frame_five$Year == item$Year), "average_five_years"] <- mean(month_current_frame_2015_2019$Value)
      mean_current <- mean(month_current_frame_2015_2019$Value)

      # Computing p-scores for non-parametric
      p_scores_frame_five[which(p_scores_frame_five$Month == item$Month & p_scores_frame_five$Year == item$Year), "p_score_value"] <- 100 * (value_current - mean_current) / mean_current
}



# Fix 2021.04.26.
for (i in 1:nrow(p_scores_frame_five))
{
      temp_month_number <- match(p_scores_frame_five[i,]$Month, month.name)

      temp_month_number_text_only <- as.character(temp_month_number)
      temp_month_number_text <- temp_month_number_text_only
      temp_month_number_text[which(temp_month_number < 10)] <- paste0("0", temp_month_number_text_only[which(temp_month_number < 10)])
      # Fixing month
      p_scores_frame_five[i,]$month_text <- as.character(p_scores_frame_five[i,]$month_text)
      p_scores_frame_five[i,]$month_text <- paste0(p_scores_frame_five[i,]$Year, "-", temp_month_number_text)
}


# Fix 2021.05.06
# Saving the data as RData file.
save(p_scores_frame_five, file = paste("../R_Data/p_scores_frame_five.RData"))




# Fix 2020.04.25
# Computing non-parametric p-scores

# Extracting the last eight years 2011 and later
# years after 2011
belarus_un_mortality_data_truncated_with_others <- belarus_un_mortality_data[which(belarus_un_mortality_data$Year >= 2011), ]
# Dropping total and unknown
belarus_un_mortality_data_truncated_month <- belarus_un_mortality_data_truncated_with_others[which(belarus_un_mortality_data_truncated_with_others$Month %in% month.name), ]
dim(belarus_un_mortality_data_truncated_month)

# Parsing pre-2020 and 2020
# 2011-2019
belarus_un_mortality_data_truncated_month_2011_2019 <- belarus_un_mortality_data_truncated_month[which(belarus_un_mortality_data_truncated_month$Year < 2020), ]
# Only 2020
belarus_un_mortality_data_truncated_month_2020 <- belarus_un_mortality_data_truncated_month[which(belarus_un_mortality_data_truncated_month$Year >= 2020), ]



# Creating p-scores to be saved.
p_scores_frame_five_length = dim(belarus_un_mortality_data_truncated_month_2020)[1]
p_scores_frame_eight <- data.frame(
      Month = belarus_un_mortality_data_truncated_month_2020$Month,
      Year = belarus_un_mortality_data_truncated_month_2020$Year,
      average_eight_years = rep(0, p_scores_frame_five_length),
      Value = rep(0, p_scores_frame_five_length),
      p_score_value = rep(0, p_scores_frame_five_length),
      month_text = rep("X", p_scores_frame_five_length)
)



# Computing non-parametrix p-scores
for (i in 1:nrow(belarus_un_mortality_data_truncated_month_2020))
{
      # Debuggins step
      # m <- month.name[1]
      item = belarus_un_mortality_data_truncated_month_2020[i,]

      # Getting current month for the past eight years
      month_current_frame_2011_2019 <- belarus_un_mortality_data_truncated_month_2011_2019[which(belarus_un_mortality_data_truncated_month_2011_2019$Month == item$Month), ]

      # Saving the average value
      p_scores_frame_eight[which(p_scores_frame_five$Month == item$Month & p_scores_frame_five$Year == item$Year), "Value"] <- item$Value
      value_current <- item$Value

      # Saving the average value
      p_scores_frame_eight[which(p_scores_frame_five$Month == item$Month & p_scores_frame_five$Year == item$Year), "average_eight_years"] <- mean(month_current_frame_2011_2019$Value)
      mean_current <- mean(month_current_frame_2011_2019$Value)

      # Computing p-scores for non-parametric
      p_scores_frame_eight[which(p_scores_frame_five$Month == item$Month & p_scores_frame_five$Year == item$Year), "p_score_value"] <- 100 * (value_current - mean_current) / mean_current
}



# Fix 2021.04.26.
for (i in 1:nrow(p_scores_frame_eight))
{
      temp_month_number <- match(p_scores_frame_eight[i,]$Month, month.name)

      temp_month_number_text_only <- as.character(temp_month_number)
      temp_month_number_text <- temp_month_number_text_only
      temp_month_number_text[which(temp_month_number < 10)] <- paste0("0", temp_month_number_text_only[which(temp_month_number < 10)])
      # Fixing month
      p_scores_frame_eight[i,]$month_text <- as.character(p_scores_frame_eight[i,]$month_text)
      p_scores_frame_eight[i,]$month_text <- paste0(p_scores_frame_eight[i,]$Year, "-", temp_month_number_text)
}

# Fix 2021.05.06
# Saving the data as RData file.
save(p_scores_frame_eight, file = paste("../R_Data/p_scores_frame_eight.RData"))




# Creating a subset to plot

# Min and Max
p_score_min <- min(c(p_scores_frame_five$p_score_value, p_scores_frame_eight$p_score_value))
p_score_max <- max(c(p_scores_frame_five$p_score_value, p_scores_frame_eight$p_score_value))




# Generating pdf output.
pdf("../Plots/Figure02c.pdf", height = 15, width = 15)
# Definign the number of plots
par(par(mfrow = c(2, 2)), mar = c(5.1, 5.1, 5.1, 2.1))


# First plot

barplot(p_scores_frame_five$p_score_value,
      col = c("darkblue", "darkblue", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange"),
      legend = TRUE,
      border = TRUE,
      # xlim = c(1, 5),
      ylim = c(p_score_min - 5, p_score_max + 5),
      args.legend = list(bty = "n", border = TRUE),
      ylab = "",
      xlab = "",
      main = "P-Scores (in Percent) for 2020\nWith 2015-2019 Years Average",
      # names.arg = as.character(p_scores_frame_five$Month),
      names.arg = as.character(p_scores_frame_five$month_text),
      cex.names = 1.5,
      cex.lab = 2,
      cex.axis = 1.75,
      cex.main = 2,
      cex = 2,
      las = 1
)


legend(
      x = "topleft",
      inset = c(0.06, 0.08),
      legend = c("Pre Epidemic", "During Epidemic"),
      col = "black",
      fill = c("darkblue", "darkorange"),
      pt.cex = c(4, 2),
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






# Second plot

barplot(p_scores_frame_eight$p_score_value,
      col = c("darkblue", "darkblue", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange"),
      legend = TRUE,
      border = TRUE,
      # xlim = c(1, 5),
      ylim = c(p_score_min - 5, p_score_max + 5),
      args.legend = list(bty = "n", border = TRUE),
      ylab = "",
      xlab = "",
      main = "P-Scores (in Percent) for 2020\nWith 2011-2019 Years Average",
      # names.arg = as.character(p_scores_frame_eight$Month),
      names.arg = as.character(p_scores_frame_eight$month_text),
      cex.names = 1.5,
      cex.lab = 2,
      cex.axis = 1.75,
      cex.main = 2,
      cex = 2,
      las = 1
)


legend(
      x = "topleft",
      inset = c(0.06, 0.08),
      legend = c("Pre Epidemic", "During Epidemic"),
      col = "black",
      fill = c("darkblue", "darkorange"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      cex = 2
)


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






# Third graph

value_combine <- c(
      p_scores_frame_five$Value,
      p_scores_frame_five$average_five_years,
      p_scores_frame_eight$Value,
      p_scores_frame_eight$average_eight_years
)


plot(
      x = as.integer(rownames(p_scores_frame_five)),
      y = p_scores_frame_five$average_five_years,
      col = "darkblue",
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l",
      # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
      main = "Averaged (2015-2019) and 2020 Mortality",
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
      cex.main = 2,
      cex.sub = 2
)
lines(
      x = as.integer(rownames(p_scores_frame_five)),
      y = p_scores_frame_five$average_five_years,
      col = "darkblue",
      # col = "darkturquoise",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)
lines(
      x = as.integer(rownames(p_scores_frame_five)),
      y = p_scores_frame_five$Value,
      # col = "darkblue",
      col = "darkturquoise",
      # col = color_01,
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l"
)
lines(
      x = as.integer(rownames(p_scores_frame_five)),
      y = p_scores_frame_five$Value,
      # col = "darkblue",
      col = "darkturquoise",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)
lines(
      x = as.integer(rownames(p_scores_frame_five)),
      y = p_scores_frame_five$Value,
      # col = "darkblue",
      col = "darkturquoise",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)

lines(
      x = rep(2.5, 10),
      y = c(rep(min(value_combine), 5), rep(max(value_combine), 5)),
      col = "red",
      lwd = 1,
      lty = 2
)

legend(
      x = "topleft",
      inset = c(0.35, 0.08),
      legend = c("Averaged Data", "2020 Data", "Epidemic Start"),
      col = "black",
      fill = c("darkblue", "darkturquoise", "red"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      cex = 1.85
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(as.integer(rownames(p_scores_frame_five))))
final_date <- as.integer(max(as.integer(rownames(p_scores_frame_five))))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <- seq(from = 1, to = length(p_scores_frame_five$Month), by = 1)
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five$Month )
x_lablist <- as.character(p_scores_frame_five$month_text)
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.025 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(c(p_scores_frame_five$average_five_years, p_scores_frame_five$Value))
y_max_value <- max(c(p_scores_frame_five$average_five_years, p_scores_frame_five$Value))
y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist <- as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label C
par(xpd = NA)

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "C"
x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
y <- y[2] - strheight(txt, cex = 4) * 6 / 5
text(x, y, txt, cex = 4)






# Fourth graph

value_combine <- c(
      p_scores_frame_five$Value,
      p_scores_frame_five$average_five_years,
      p_scores_frame_eight$Value,
      p_scores_frame_eight$average_eight_years
)


plot(
      x = as.integer(rownames(p_scores_frame_eight)),
      y = p_scores_frame_eight$average_eight_years,
      col = "darkblue",
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l",
      # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
      main = "Averaged (2011-2019) and 2020 Mortality",
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
      cex.main = 2,
      cex.sub = 2
)
lines(
      x = as.integer(rownames(p_scores_frame_eight)),
      y = p_scores_frame_eight$average_eight_years,
      col = "darkblue",
      # col = "darkturquoise",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)
lines(
      x = as.integer(rownames(p_scores_frame_eight)),
      y = p_scores_frame_eight$Value,
      # col = "darkblue",
      col = "darkturquoise",
      # col = color_01,
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l"
)
lines(
      x = as.integer(rownames(p_scores_frame_eight)),
      y = p_scores_frame_eight$Value,
      # col = "darkblue",
      col = "darkturquoise",
      # col = color_01,
      lwd = 15,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
)

lines(
      x = rep(2.5, 10),
      y = c(rep(min(value_combine), 5), rep(max(value_combine), 5)),
      col = "red",
      lwd = 1,
      lty = 2
)

legend(
      x = "topleft",
      inset = c(0.35, 0.08),
      legend = c("Averaged Data", "2020 Data", "Epidemic Start"),
      col = "black",
      fill = c("darkblue", "darkturquoise", "red"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      cex = 1.85
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(as.integer(rownames(p_scores_frame_eight))))
final_date <- as.integer(max(as.integer(rownames(p_scores_frame_eight))))
number_of_dates <- final_date - initial_date + 1


# Indexes to display
x_indexes_to_display <- seq(from = 1, to = length(p_scores_frame_eight$Month), by = 1)
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_eight$Month )
x_lablist <- as.character(p_scores_frame_eight$month_text)

axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y = par()$usr[3] - 0.025 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(c(p_scores_frame_eight$average_eight_years, p_scores_frame_eight$Value))
y_max_value <- max(c(p_scores_frame_eight$average_eight_years, p_scores_frame_eight$Value))
y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
y_lablist <- as.character(round(y_tlab, digits = 0))
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


# Label D
par(xpd = NA)

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "D"
x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
y <- y[2] - strheight(txt, cex = 4) * 6 / 5
text(x, y, txt, cex = 4)






dev.off()