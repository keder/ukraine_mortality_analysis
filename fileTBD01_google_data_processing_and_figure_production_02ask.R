


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

cut_date = as.Date("2022-01-01")


# google trends data relative path
google_trends_grob_data_relative_path              <- "../Data/Ukraine_data/grob_gt.csv"
google_trends_pominki_data_relative_path           <- "../Data/Ukraine_data/pominki_gt.csv"
google_trends_ritualnie_uslugi_data_relative_path  <- "../Data/Ukraine_data/ritualnie_uslugi_gt.csv"
google_trends_truna_data_relative_path              <- "../Data/Ukraine_data/truna_gt.csv"
google_trends_ritualni_poslugi_data_relative_path  <- "../Data/Ukraine_data/ritualni_poslugi_gt.csv"



# Reading data
google_trends_grob_data             <- read.table( file = google_trends_grob_data_relative_path,  sep =",", header = TRUE )
google_trends_pominki_data          <- read.table( file = google_trends_pominki_data_relative_path,  sep =",", header = TRUE )
google_trends_ritualnie_uslugi_data <- read.table( file = google_trends_ritualnie_uslugi_data_relative_path,  sep =",", header = TRUE )
google_trends_truna_data         <- read.table( file = google_trends_truna_data_relative_path, sep =",", header = TRUE )
google_trends_ritualni_poslugi_data <- read.table( file = google_trends_ritualni_poslugi_data_relative_path, sep =",", header = TRUE )
names(google_trends_grob_data)[2]             <- "grob"
names(google_trends_pominki_data)[2]          <- "pominki"
names(google_trends_ritualnie_uslugi_data)[2] <- "ritualnie_uslugi"
names(google_trends_truna_data)[2]          <- "truna"
names(google_trends_ritualni_poslugi_data)[2] <- "ritualni_poslugi"
# Fixing dates
google_trends_grob_data$Date             <- as.Date( paste0(google_trends_grob_data$Month, "-15") ) 
google_trends_pominki_data$Date          <- as.Date( paste0(google_trends_pominki_data$Month, "-15") ) 
google_trends_ritualnie_uslugi_data$Date <- as.Date( paste0(google_trends_ritualnie_uslugi_data$Month, "-15") ) 
google_trends_truna_data$Date          <- as.Date( paste0(google_trends_truna_data$Month, "-15") ) 
google_trends_ritualni_poslugi_data$Date <- as.Date( paste0(google_trends_ritualni_poslugi_data$Month, "-15") )  
# Creating month
google_trends_grob_data$Month_text             <- format(google_trends_grob_data$Date,"%B")
google_trends_pominki_data$Month_text          <- format(google_trends_pominki_data$Date,"%B")
google_trends_ritualnie_uslugi_data$Month_text <- format(google_trends_ritualnie_uslugi_data$Date,"%B")
google_trends_truna_data$Month_text          <- format(google_trends_truna_data$Date,"%B")
google_trends_ritualni_poslugi_data$Month_text <- format(google_trends_ritualni_poslugi_data$Date,"%B")
# Cut 2022 data
google_trends_grob_data = google_trends_grob_data[which(google_trends_grob_data$Date < cut_date),]
google_trends_pominki_data = google_trends_pominki_data[which(google_trends_pominki_data$Date < cut_date),]
google_trends_ritualnie_uslugi_data = google_trends_ritualnie_uslugi_data[which(google_trends_ritualnie_uslugi_data$Date < cut_date),]
google_trends_truna_data = google_trends_truna_data[which(google_trends_truna_data$Date < cut_date),]
google_trends_ritualni_poslugi_data = google_trends_ritualni_poslugi_data[which(google_trends_ritualni_poslugi_data$Date < cut_date),]


# Summaries
head(google_trends_grob_data)
head(google_trends_pominki_data)
head(google_trends_ritualnie_uslugi_data)
# Dimensions
dim(google_trends_grob_data)
dim(google_trends_pominki_data)
dim(google_trends_ritualnie_uslugi_data)




# Saving the trends data as RData file.
save( google_trends_grob_data, file = paste("../R_Data/google_trends_grob_data.RData") )
save( google_trends_pominki_data, file = paste("../R_Data/google_trends_pominki_data.RData") )
save( google_trends_ritualnie_uslugi_data, file = paste("../R_Data/google_trends_ritualnie_uslugi_data.RData") )
save( google_trends_truna_data, file = paste("../R_Data/google_trends_truna_data.RData") )
save( google_trends_ritualni_poslugi_data, file = paste("../R_Data/google_trends_ritualni_poslugi_data.RData") )
ls()






# Generating pdf output.
pdf( paste( "../Plots/FigureTBD01a.pdf", sep = ""), height = 10, width = 9)
# Definign the number of plots
par( par(mfrow=c(3,1)),  mar=c(5.1, 5.1, 3, 2.1)  )


# First graph

combined_value_min <- min( google_trends_grob_data$grob )
combined_value_max <- max( google_trends_grob_data$grob )

combined_date_min  <- min( google_trends_grob_data$Date )
combined_date_max  <- max( google_trends_grob_data$Date )


# First plot (Belarus)
plot(x = google_trends_grob_data$Date,
     y = google_trends_grob_data$grob,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Google Trend: \"grob\"",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min*0.9, combined_value_max * 1.10 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Percent",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = google_trends_grob_data$Date,
      y = google_trends_grob_data$grob,
      #col = "#005BBB",
      col = "#005BBB",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( as.Date("2020-03-01", origin ="1970-01-01"), 2), 
      y = c( combined_value_min,  combined_value_max ),
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.17, 0.05), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.25 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(google_trends_grob_data$Date))
final_date   <- as.integer(max(google_trends_grob_data$Date))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(google_trends_grob_data$Date),  by = 2 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- google_trends_grob_data$Date[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character( google_trends_grob_data$Date[x_indexes_to_display] ), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)



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
y <- y[2] - strheight(txt, cex=4) * 4 / 5
text(x, y, txt, cex = 4)





# Second graph

combined_value_min <- min( google_trends_pominki_data$pominki )
combined_value_max <- max( google_trends_pominki_data$pominki )

combined_date_min  <- min( google_trends_grob_data$Date )
combined_date_max  <- max( google_trends_grob_data$Date )


# First plot (Belarus)
plot(x = google_trends_grob_data$Date,
     y = google_trends_pominki_data$pominki,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Google Trend: \"pominki\"",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min*0.9, combined_value_max * 1.10 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Percent",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = google_trends_grob_data$Date,
      y = google_trends_pominki_data$pominki,
      #col = "#005BBB",
      col = "#005BBB",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( as.Date("2020-03-01", origin ="1970-01-01"), 2), 
      y = c( combined_value_min,  combined_value_max ),
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.17, 0.05), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.25 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(google_trends_grob_data$Date))
final_date   <- as.integer(max(google_trends_grob_data$Date))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(google_trends_grob_data$Date),  by = 2 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- google_trends_grob_data$Date[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character( google_trends_grob_data$Date[x_indexes_to_display] ), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)



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
y <- y[2] - strheight(txt, cex=4) * 4 / 5
text(x, y, txt, cex = 4)




# Third graph

combined_value_min <- min( google_trends_ritualnie_uslugi_data$ritualnie_uslugi )
combined_value_max <- max( google_trends_ritualnie_uslugi_data$ritualnie_uslugi )

combined_date_min  <- min( google_trends_grob_data$Date )
combined_date_max  <- max( google_trends_grob_data$Date )


# First plot (Belarus)
plot(x = google_trends_grob_data$Date,
     y = google_trends_ritualnie_uslugi_data$ritualnie_uslugi,
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Google Trend: \"ritualnie uslugi\"",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min*0.9, combined_value_max * 1.10 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Percent",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = google_trends_grob_data$Date,
      y = google_trends_ritualnie_uslugi_data$ritualnie_uslugi,
      #col = "#005BBB",
      col = "#005BBB",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( as.Date("2020-03-01", origin ="1970-01-01"), 2), 
      y = c( combined_value_min,  combined_value_max ),
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.17, 0.05), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.25 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(google_trends_grob_data$Date))
final_date   <- as.integer(max(google_trends_grob_data$Date))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(google_trends_grob_data$Date),  by = 2 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- google_trends_grob_data$Date[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character( google_trends_grob_data$Date[x_indexes_to_display] ), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)



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
y <- y[2] - strheight(txt, cex=4) * 4 / 5
text(x, y, txt, cex = 4)


dev.off()


# Generating pdf output.
pdf( paste( "../Plots/FigureTBD01b.pdf", sep = ""), height = 9, width = 9)
# Definign the number of plots
par( par(mfrow=c(2,1)),  mar=c(5.1, 5.1, 3, 2.1)  )

# 4 graph

combined_value_min <- min( google_trends_truna_data[,2] )
combined_value_max <- max( google_trends_truna_data[,2] )

combined_date_min  <- min( google_trends_grob_data$Date )
combined_date_max  <- max( google_trends_grob_data$Date )


# First plot (Belarus)
plot(x = google_trends_grob_data$Date,
     y = google_trends_truna_data[,2],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Google Trend: \"truna\"",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min*0.9, combined_value_max * 1.10 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Percent",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = google_trends_grob_data$Date,
      y = google_trends_truna_data[,2],
      #col = "#005BBB",
      col = "#005BBB",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( as.Date("2020-03-01", origin ="1970-01-01"), 2), 
      y = c( combined_value_min,  combined_value_max ),
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.17, 0.05), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.25 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(google_trends_grob_data$Date))
final_date   <- as.integer(max(google_trends_grob_data$Date))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(google_trends_grob_data$Date),  by = 2 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- google_trends_grob_data$Date[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character( google_trends_grob_data$Date[x_indexes_to_display] ), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)



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
y <- y[2] - strheight(txt, cex=4) * 4 / 5
text(x, y, txt, cex = 4)




# 5 graph

combined_value_min <- min( google_trends_ritualni_poslugi_data[,2] )
combined_value_max <- max( google_trends_ritualni_poslugi_data[,2] )

combined_date_min  <- min( google_trends_grob_data$Date )
combined_date_max  <- max( google_trends_grob_data$Date )


# First plot (Belarus)
plot(x = google_trends_grob_data$Date,
     y = google_trends_ritualni_poslugi_data[,2],
     col = "#005BBB",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Google Trend: \"ritualni poslugi\"",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min*0.9, combined_value_max * 1.10 ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Percent",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = google_trends_grob_data$Date,
      y = google_trends_ritualni_poslugi_data[,2],
      #col = "#005BBB",
      col = "#005BBB",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
lines(x = rep( as.Date("2020-03-01", origin ="1970-01-01"), 2), 
      y = c( combined_value_min,  combined_value_max ),
      col="red", 
      lwd = 1, 
      lty = 2)
legend( x = "topleft", 
        inset= c(0.17, 0.05), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("#005BBB", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.25 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(google_trends_grob_data$Date))
final_date   <- as.integer(max(google_trends_grob_data$Date))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(google_trends_grob_data$Date),  by = 2 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- google_trends_grob_data$Date[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character( google_trends_grob_data$Date[x_indexes_to_display] ), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1)



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
y <- y[2] - strheight(txt, cex=4) * 4 / 5
text(x, y, txt, cex = 4)



dev.off()


