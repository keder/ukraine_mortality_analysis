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


pandemic_start <- as.Date("2020-02-15")


# Loding data saved in file01

# Reading COVID-19 data from onliner.by


# Reading mortailty UN Data

# Reading the RAW data in R
load(file = paste("../R_Data/ukraine_un_mortality_data.RData"))

ls()




# Fix 2020.04.25
# Computing non-parametric p-scores

# Extracting the last five years 2015 and later
# years after 2015
belarus_un_mortality_data_truncated_with_others <- ukraine_un_mortality_data[which(ukraine_un_mortality_data$Year >= 2015), ]
# Dropping total and unknown
belarus_un_mortality_data_truncated_month <- belarus_un_mortality_data_truncated_with_others[which(belarus_un_mortality_data_truncated_with_others$Month %in% month.name), ]
dim(belarus_un_mortality_data_truncated_month)

# Parsing pre-2020 and 2020
# 2015-2019
belarus_un_mortality_data_truncated_month_2015_2019 <- belarus_un_mortality_data_truncated_month[which(belarus_un_mortality_data_truncated_month$Year < 2020), ]
# Only 2020
belarus_un_mortality_data_truncated_month_2020 <- belarus_un_mortality_data_truncated_month[which(belarus_un_mortality_data_truncated_month$Year >= 2020), ]


# Creating p-scores to be saved.
p_scores_frame_five_length = dim(belarus_un_mortality_data_truncated_month)[1]
p_scores_frame_five <- data.frame(
      Month = belarus_un_mortality_data_truncated_month$Month,
      Year = belarus_un_mortality_data_truncated_month$Year,
      average_five_years = rep(0, p_scores_frame_five_length),
      Value = rep(0, p_scores_frame_five_length),
      p_score_value = rep(0, p_scores_frame_five_length),
      month_text = rep("X", p_scores_frame_five_length),
      date = belarus_un_mortality_data_truncated_month$date_fixed
)




# Computing non-parametrix p-scores
for (i in 1:nrow(belarus_un_mortality_data_truncated_month)) {
      # Debuggins step
      # m <- month.name[1]
      item = belarus_un_mortality_data_truncated_month[i,]

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




# Creating a subset to plot

# Min and Max
p_score_min <- min(p_scores_frame_five$p_score_value)
p_score_max <- max(p_scores_frame_five$p_score_value)




# Generating pdf output.
pdf("../Plots/Figure02c.pdf", height = 15, width = 15)
# Definign the number of plots
par(par(mfrow = c(2, 1)), mar = c(7.1, 5.1, 5.1, 2.1))


# First plot

pandemic_data_length = nrow(p_scores_frame_five) - which(p_scores_frame_five$date == pandemic_start)

barplot(p_scores_frame_five$p_score_value,
      col = c(rep("#005BBB", nrow(p_scores_frame_five) - pandemic_data_length), rep("#FFD500", pandemic_data_length)),
      legend = TRUE,
      border = TRUE,
      # xlim = c(1, 5),
      ylim = c(p_score_min - 5, p_score_max + 5),
      args.legend = list(bty = "n", border = TRUE),
      ylab = "",
      xlab = "",
      main = "P-Scores (in Percent) for 2015-2021\nWith 2015-2019 Years Average",
      # names.arg = as.character(p_scores_frame_five$Month),
      names.arg = as.character(p_scores_frame_five$month_text),
      cex.names = 1.2,
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
      p_scores_frame_five$Value,
      p_scores_frame_five$average_five_years
)

plot(
      x = as.integer(p_scores_frame_five$date),
      y = p_scores_frame_five$average_five_years,
      col = "#005BBB",
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l",
      # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
      main = "Averaged (2015-2019) and 2015-2021 Mortality",
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
      cex.main = 1.55,
      cex.sub = 2
)
lines(
      x = as.integer(p_scores_frame_five$date),
      y = p_scores_frame_five$average_five_years,
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
      x = as.integer(p_scores_frame_five$date),
      y = p_scores_frame_five$Value,
      # col = "#005BBB",
      col = "#FFD500",
      # col = color_01,
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "l"
)
lines(
      x = as.integer(p_scores_frame_five$date),
      y = p_scores_frame_five$Value,
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
      x = rep(pandemic_start, 10),
      y = c(rep(min(value_combine), 5), rep(max(value_combine), 5)),
      col = "red",
      lwd = 1,
      lty = 2
)

legend(
      x = "topleft",
      inset = c(0.15, 0.08),
      legend = c("Averaged Data", "2015-2021 Data", "Epidemic Start"),
      col = "black",
      fill = c("#005BBB", "#FFD500", "red"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      cex = 1.85
)
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/

# Creating labels by month and converting.
# Values that define the graph boundaries for x-xis
initial_date    <- as.integer(min(as.integer(p_scores_frame_five$date)))
final_date      <- as.integer(max(as.integer(p_scores_frame_five$date)))
number_of_dates <- length(as.integer(p_scores_frame_five$date))
lenght_in_time  <- final_date - initial_date


# Indexes to display on the graph (i.e. coordinates on x axis)
# x_indexes_to_display <- seq(from = p_scores_frame_five$date[1], to = max(p_scores_frame_five$date), by = 3*floor(lenght_in_time/number_of_dates))
x_indexes_to_display <- seq(from = p_scores_frame_five$date[1], to = max(p_scores_frame_five$date), by = 2*floor(lenght_in_time/number_of_dates))
# Actual lab elements as text to be printed.
x_lablist <- substr(x = x_indexes_to_display, start = 1, stop = 7)
# Adding dashes to coordinates
axis(1, at = x_indexes_to_display, labels = FALSE)
# Adding actual labels
text(x = x_indexes_to_display, y = par()$usr[3] - 0.025 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.2)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(c(p_scores_frame_five$average_five_years, p_scores_frame_five$Value))
y_max_value <- max(c(p_scores_frame_five$average_five_years, p_scores_frame_five$Value))
# Indexes to display on the graph (i.e. coordinates on y axis)
y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
# Actual lab elements as text to be printed.
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