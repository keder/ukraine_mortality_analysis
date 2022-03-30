# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.24. ask
rm(list=ls(all=TRUE))
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



# Path for the data
incidence_data_all_path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"


# Mortality data from United Nations

# data relative path
belarus_un_mortality_relative_path  <- "../Data/Ukraine_data/UNdata_Export_20220304_172236734.csv"
ukraine_mortality_path = "../Data/Ukraine_data/raw_mortality.csv"
proportion_max = 0.025







# Working with COVID-19 data.

# Incidence
incidence_lines_all_raw = readLines(incidence_data_all_path)
incidence_lines_all_raw[100] = gsub(pattern = "Cote d\\'Ivoire", replacement = "Cote dIvoire", x = incidence_lines_all_raw[100])
incidence_data_all_raw_part00 = as.vector(data.frame(read.table(text = incidence_lines_all_raw[1], sep = ",")))
incidence_data_all_raw = data.frame(read.table(text = incidence_lines_all_raw[-1], sep = ","))
names(incidence_data_all_raw) = incidence_data_all_raw_part00
incidence_data_all_raw_line = incidence_data_all_raw[which(incidence_data_all_raw$`Country/Region`=="Ukraine"),]
ukraine_incidence_days = c(0, diff(as.integer(tail(t(incidence_data_all_raw_line), length(incidence_data_all_raw_line) - 4))))
dates = as.Date(tail(as.vector(t(incidence_data_all_raw_part00)), length(incidence_data_all_raw_part00) - 4), format = "%m/%d/%y")
ukraine_incidence_days_date = data.frame(ukraine_incidence_days, dates)
names(ukraine_incidence_days_date) = c("Incidence", "Date")


ukraine_incidence_days_date$Date_text <- substr( x = as.character(ukraine_incidence_days_date$Date), start = 1, stop = 7)
unique_month <- unique(ukraine_incidence_days_date$Date_text)

ukraine_incidence <- data.frame( Incidence = rep(0, length(unique_month)), Date_text = unique_month )

for (unique_month_current in unique_month )
{
  # Debug
  # unique_month_current <- unique_month[1]
  # unique_month_current <- unique_month[2]
  
  
  current_indexes <- which(ukraine_incidence_days_date$Date_text == unique_month_current)

  current_sum <- sum(ukraine_incidence_days_date[current_indexes,1])

  ukraine_incidence$Incidence[which( ukraine_incidence$Date_text ==  unique_month_current)] <- current_sum   
    
}

ukraine_incidence$Date_text = paste0(ukraine_incidence$Date_text, "-15")
ukraine_incidence$Date = as.Date(ukraine_incidence$Date_text)

# Saving the data as RData file.
save( ukraine_incidence, file = paste("../R_Data/ukraine_incidence.RData") )


# Mortality
mortality_lines_all_raw = readLines(ukraine_mortality_path)
mortality_data = data.frame(read.table(text = mortality_lines_all_raw, sep = ";"))
names(mortality_data) = c("Year", "Month", "Mortality")
month_num = match(mortality_data[1,]$Month, tolower(month.name))
mortality_data$Date = rep(as.Date(paste(mortality_data[1,]$Year, month_num, "15", sep="-")), times=nrow(mortality_data))
for (i in 1:nrow(mortality_data))
{
      month_num = match(mortality_data[i,]$Month, tolower(month.name))
      mortality_data[i,]$Date = as.Date(paste(mortality_data[i,]$Year, month_num, "15", sep="-"))
}

# Saving the data as RData file.
save( mortality_data, file = paste("../R_Data/mortality_data.RData") )


# Working with Unted Nations mortality data. 

# Reading data
ukraine_un_mortality_data <- read.table( file = belarus_un_mortality_relative_path, sep =";", header = TRUE )



# Fixing dates
# Converting month
ukraine_un_mortality_data$Month <- as.character(ukraine_un_mortality_data$Month)

# Date placeholder
ukraine_un_mortality_data$date_paceholder <- rep(15, dim(ukraine_un_mortality_data)[1]  )


# Month integer
ukraine_un_mortality_data$Month_integer  <-  match( ukraine_un_mortality_data$Month, month.name ) 
# date fixed
ukraine_un_mortality_data$date_text  <-  paste( ukraine_un_mortality_data$Year, ukraine_un_mortality_data$Month, ukraine_un_mortality_data$date_paceholder, sep="-" )
# date fixed
ukraine_un_mortality_data$date_fixed <-  as.Date( strptime( ukraine_un_mortality_data$date_text, format="%Y-%b-%d" ) )

# Soring the frames accoridng to dates
ukraine_un_mortality_data <- ukraine_un_mortality_data[ order(ukraine_un_mortality_data$date_fixed),  ]

# Saving the data as RData file.
save( ukraine_un_mortality_data, file = paste("../R_Data/ukraine_un_mortality_data.RData") )

# Fix 2021.04.28
# Extracting month only data
date_as_int_2015 <- as.integer( as.Date("2015-01-01", origin = "1970-01-01") )
which_month_only_belarus_2015 <- intersect( which(!is.na(ukraine_un_mortality_data$date_fixed)), which((ukraine_un_mortality_data$date_fixed>=date_as_int_2015)) )
# Subsets Month only
ukraine_un_mortality_data_month_only_since_2015 <- ukraine_un_mortality_data[which_month_only_belarus_2015, ]

only_new_data = mortality_data[which(!(mortality_data$Date %in% ukraine_un_mortality_data_month_only_since_2015$date_fixed)),]
only_new_data_df = data.frame(date_fixed = only_new_data$Date, Value = only_new_data$Mortality)
only_new_data_df[setdiff(names(ukraine_un_mortality_data_month_only_since_2015), names(only_new_data_df))] = NA
ukraine_un_mortality_data_month_only_since_2015 = rbind(ukraine_un_mortality_data_month_only_since_2015, only_new_data_df)

# Soring the frames accoridng to dates
ukraine_un_mortality_data_month_only_since_2015 <- ukraine_un_mortality_data_month_only_since_2015[ order(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  ]

# Saving the data as RData file.
save( ukraine_un_mortality_data_month_only_since_2015, file = paste("../R_Data/ukraine_un_mortality_data_month_only_since_2015.RData") )





# Generating pdf output.
pdf("../Plots/Figure01a.pdf", height = 8, width = 20)
# Definign the number of plots
#par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )


# First plot (Belarus)
plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Total Monthly Mortality",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      #col = "darkblue",
      col = "darkturquoise",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Monthly Records", "Interpolator"), 
        col = "black", 
        fill = c("darkturquoise", "darkblue"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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

dev.off()







# Generating pdf output.
pdf("../Plots/Figure01b.pdf", height = 8, width = 20)
# Definign the number of plots
#par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )


# First plot (Belarus)
plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Total Monthly Mortality",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Interpolated Records"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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

dev.off()







# Generating pdf output.
pdf("../Plots/Figure01c.pdf", height = 8, width = 20)
# Definign the number of plots
#par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )


# First plot (Belarus)
plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Total Monthly Mortality",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      #col = "darkblue",
      col = "darkturquoise",
      # col = color_01, 
      lwd = 5,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Monthly Records", "Interpolator"), 
        col = "black", 
        fill = c("darkturquoise", "darkblue"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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

dev.off()











# Generating pdf output.
pdf("../Plots/Figure01d.pdf", height = 15, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.5, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Total)",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 2,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 12,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Interpolated Records"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)



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



# Third graph
proportion_covid19 <- intersected_data$death_covid19/(intersected_data$Value+intersected_data$death_covid19)

summary_to_plot <-  rbind( proportion_covid19,
                           (proportion_max - proportion_covid19)  )
colnames(summary_to_plot) <- intersected_data$unique_month
rownames(summary_to_plot) <- NULL

barplot( summary_to_plot, col=c("darkorange", "darkblue"), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         ylim = c(0, proportion_max),
         main = "Proportion of COVID-19 Death in Total",
         names.arg = colnames(summary_to_plot), 
         cex.names = 1.1, 
         cex.lab = 1, 
         cex.axis = 1.25,
         cex.main = 2, 
         cex = 1,
         las = 2)

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


dev.off()












# Generating pdf output.
pdf("../Plots/Figure01e.pdf", height = 12, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Total)",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 2,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 12,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Interpolated Records"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)



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




dev.off()













# Generating pdf output.
pdf("../Plots/Figure01f.pdf", height = 12, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Total)",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 2,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 12,
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
        inset= c(0.04, 0.04), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("darkblue", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)



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





dev.off()













# Generating pdf output.
pdf("../Plots/Figure01g.pdf", height = 12, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( ukraine_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( ukraine_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( ukraine_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( ukraine_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
     y = ukraine_un_mortality_data_month_only_since_2015$Value,
     col = "darkblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Total)",
     xlim = c( combined_date_min,  combined_date_max  ),
     ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 2,
     cex.sub = 2
)
lines(x = ukraine_un_mortality_data_month_only_since_2015$date_fixed,
      y = ukraine_un_mortality_data_month_only_since_2015$Value,
      col = "darkblue",
      #col = "darkturquoise",
      # col = color_01, 
      lwd = 12,
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
        inset= c(0.04, 0.04), 
        legend = c("Interpolated Records", "Epidemic Start"), 
        col = "black", 
        fill = c("darkblue", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(ukraine_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(ukraine_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(ukraine_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- combined_value_min
y_max_value <- combined_value_max
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)



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



dev.off()












