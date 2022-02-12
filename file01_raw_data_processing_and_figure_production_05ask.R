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

# Covid data from onliner.by

# json data relative path
json1_data_relative_path <- "../Data/Other/onliner.scrap1.json"
json2_data_relative_path <- "../Data/Other/onliner.scrap2.json"


# Mortality data from United Nations

# data relative path
belarus_un_mortality_relative_path  <- "../Data/Death/Belarus_UNdata_Export_20210415_203326048/UNdata_Export_20210415_203326048_tweaked.csv"








# Working with COVID-19 data. 

# Reading dataset in json
belarus_incidence_data  <- fromJSON( file = json1_data_relative_path )
names(belarus_incidence_data)
# Reading dataset in json
belarus_statistics_data <- fromJSON( file = json2_data_relative_path )
names(belarus_statistics_data)



# Working with json1 data

# Number of days
num_of_rep_days <- length(belarus_incidence_data$data)

# Creating an array to save results.
belarus_incidence_data_frame_covid19 <- data.frame( matrix(0, nrow = num_of_rep_days, ncol = 2 ) )
names(belarus_incidence_data_frame_covid19) <- c("date", "onliner_cases_daily")
  


for( d in c(1:num_of_rep_days) )
{
  # Debuggins step
  # d <- 1 
  
  # Getting current date
  date_current <- as.Date( paste( belarus_incidence_data$data[d][[1]]$label, ".2020", sep =""),  "%d.%m.%Y " )

  # Procesign late dates correctly
  if ( date_current %in% c( as.Date(("2020-01-01")) :as.Date(("2020-03-01")) ) )
  {
    belarus_incidence_data_frame_covid19$date[d] <- date_current + 366
  }
  
  if ( date_current %in% c( as.Date(("2020-02-28")) :as.Date(("2020-12-31")) ) )
  {
    belarus_incidence_data_frame_covid19$date[d] <- date_current 
    
  }

  # Getting current data
  belarus_incidence_data_frame_covid19$onliner_cases_daily[d] <- as.numeric( belarus_incidence_data$data[d][[1]]$value )

}  
 
# Fixing the date format 
belarus_incidence_data_frame_covid19$date <- as.Date( belarus_incidence_data_frame_covid19$date, origin = "1970-01-01" )

# Saving the data as RData file.
save( belarus_incidence_data_frame_covid19, file = paste("../R_Data/belarus_incidence_data_frame_covid19.RData") )






# Working with json2 data

# Number of days
num_of_rep_days <- length(belarus_statistics_data$data)

# Creating an array to save results.
belarus_statistics_data_frame_covid19 <- data.frame( matrix(0, nrow = num_of_rep_days, ncol = 4 ) )
names(belarus_statistics_data_frame_covid19) <- c("date", "onliner_cases_cummulative", "onliner_recovered_cummulative", "onliner_death_cummulative")



for( d in c(1:num_of_rep_days) )
{
  # Debuggins step
  # d <- 1
  # d <- 20   
  
  # Getting current date
  date_current <- as.Date( paste( belarus_statistics_data$data[d][[1]]$label, ".2020", sep =""),  "%d.%m.%Y " )
  
  # Procesign late dates correctly
  if ( date_current %in% c( as.Date(("2020-01-01")) :as.Date(("2020-03-01")) ) )
  {
    belarus_statistics_data_frame_covid19$date[d] <- date_current + 366
  }
  
  if ( date_current %in% c( as.Date(("2020-02-28")) :as.Date(("2020-12-31")) ) )
  {
    belarus_statistics_data_frame_covid19$date[d] <- date_current 
    
  }
  
  # Getting current data
  
  # Reoving spaces if any
  cases_cummulative_character      <- gsub(pattern = " ", replacement = "", x = strsplit(x = belarus_statistics_data$data[d][[1]]$value, split = "\" ")[1])
  recovered_cummulative_character  <- gsub(pattern = " ", replacement = "", x = strsplit(x = belarus_statistics_data$data[d][[1]]$value, split = "\" ")[2])
  death_cummulative_character      <- gsub(pattern = " ", replacement = "", x = strsplit(x = belarus_statistics_data$data[d][[1]]$value, split = "\" ")[3])
  
  
  belarus_statistics_data_frame_covid19$onliner_cases_cummulative[d]     <- as.numeric( cases_cummulative_character )
  belarus_statistics_data_frame_covid19$onliner_recovered_cummulative[d] <- as.numeric( recovered_cummulative_character )
  belarus_statistics_data_frame_covid19$onliner_death_cummulative[d]     <- as.numeric( death_cummulative_character )
}  


# Fixing the date format 
belarus_statistics_data_frame_covid19$date <- as.Date( belarus_statistics_data_frame_covid19$date, origin = "1970-01-01" )

# Saving the data as RData file.
save( belarus_statistics_data_frame_covid19, file = paste("../R_Data/belarus_statistics_data_frame_covid19.RData") )




# Fix 2021.04.23.
# Accumulating mortality over 1 month.
belarus_statistics_data_frame_covid19$date_character <- as.character( belarus_statistics_data_frame_covid19$date )
belarus_statistics_data_frame_covid19$date_character_truncated <- substr(x = belarus_statistics_data_frame_covid19$date_character, start = 1, stop = 7)
unique_month <- unique(belarus_statistics_data_frame_covid19$date_character_truncated)


# Creating frame to save the results.
monthly_death_data_frame_covid19 <-  data.frame( unique_month  = unique_month, date_fixed = paste0(unique_month, "-15"), death_covid19 = rep(0, length(unique_month) )  )


for( i in c(3:length(unique_month)) )
{
  # Debugging step
  # i <- 3
  # i <- 6   

  # Extracting
  
  # Previous
  belarus_statistics_data_frame_covid19_previous <- belarus_statistics_data_frame_covid19[ belarus_statistics_data_frame_covid19$date_character_truncated == unique_month[i-1],  ]
  # Current
  belarus_statistics_data_frame_covid19_current  <- belarus_statistics_data_frame_covid19[ belarus_statistics_data_frame_covid19$date_character_truncated == unique_month[i],  ]
  
  
  # Extracting Max Values

  # Previous
  covid19_previous_max <- max(na.omit(belarus_statistics_data_frame_covid19_previous$onliner_death_cummulative))
  # Current
  covid19_current_max  <- max(na.omit(belarus_statistics_data_frame_covid19_current$onliner_death_cummulative))
   
  # Computing and saving monthly increment
  monthly_death_data_frame_covid19$death_covid19[ which(monthly_death_data_frame_covid19$unique_month == unique_month[i]) ] <- covid19_current_max - covid19_previous_max 
  # For the case of i = 2 saving the max 
  if (i == 3) 
  {
    # Case i == 3: i.e. filling the previous ones.
    monthly_death_data_frame_covid19$death_covid19[ 1 ] <- 0
    monthly_death_data_frame_covid19$death_covid19[ 2 ] <- covid19_previous_max 
    
  }      
  
}  


# Saving the data as RData file.
save( monthly_death_data_frame_covid19, file = paste("../R_Data/monthly_death_data_frame_covid19.RData") )






# Working with Unted Nations mortality data. 

# Reading data
belarus_un_mortality_data <- read.table( file = belarus_un_mortality_relative_path, sep =",", header = TRUE )

# Summaries
dim(belarus_un_mortality_data)
# Names
names(belarus_un_mortality_data)
# head
head(belarus_un_mortality_data)
# tail
tail(belarus_un_mortality_data)



# Fixing dates
# Converting month
belarus_un_mortality_data$Month <- as.character(belarus_un_mortality_data$Month)

# Date placeholder
belarus_un_mortality_data$date_paceholder <- rep(15, dim(belarus_un_mortality_data)[1]  )


# Month integer
belarus_un_mortality_data$Month_integer  <-  match( belarus_un_mortality_data$Month, month.name ) 
# date fixed
belarus_un_mortality_data$date_text  <-  paste( belarus_un_mortality_data$Year, belarus_un_mortality_data$Month, belarus_un_mortality_data$date_paceholder, sep="-" )
# date fixed
belarus_un_mortality_data$date_fixed <-  as.Date( strptime( belarus_un_mortality_data$date_text, format="%Y-%b-%d" ) )
# Summaries
summary(belarus_un_mortality_data$Value)

# Soring the frames accoridng to dates
belarus_un_mortality_data <- belarus_un_mortality_data[ order(belarus_un_mortality_data$date_fixed),  ]

# Saving the data as RData file.
save( belarus_un_mortality_data, file = paste("../R_Data/belarus_un_mortality_data.RData") )



# Fix 2021.04.28
# Extracting month only data
date_as_int_2015 <- as.integer( as.Date("2015-01-01", origin = "1970-01-01") )
which_month_only_belarus_2015 <- intersect( which(!is.na(belarus_un_mortality_data$date_fixed)), which((belarus_un_mortality_data$date_fixed>=date_as_int_2015)) )
# Subsets Month only
belarus_un_mortality_data_month_only_since_2015 <- belarus_un_mortality_data[which_month_only_belarus_2015, ]

# Soring the frames accoridng to dates
belarus_un_mortality_data_month_only_since_2015 <- belarus_un_mortality_data_month_only_since_2015[ order(belarus_un_mortality_data_month_only_since_2015$date_fixed),  ]

# Saving the data as RData file.
save( belarus_un_mortality_data_month_only_since_2015, file = paste("../R_Data/belarus_un_mortality_data_month_only_since_2015.RData") )



# Fix 2021.04.28
# Extracting month only data
date_as_int_2011 <- as.integer( as.Date("2011-01-01", origin = "1970-01-01") )
which_month_only_belarus_2011 <- intersect( which(!is.na(belarus_un_mortality_data$date_fixed)), which((belarus_un_mortality_data$date_fixed>=date_as_int_2011)) )
# Subsets Month only
belarus_un_mortality_data_month_only_since_2011 <- belarus_un_mortality_data[which_month_only_belarus_2011, ]

# Soring the frames accoridng to dates
belarus_un_mortality_data_month_only_since_2011 <- belarus_un_mortality_data_month_only_since_2011[ order(belarus_un_mortality_data_month_only_since_2011$date_fixed),  ]

# Saving the data as RData file.
save( belarus_un_mortality_data_month_only_since_2011, file = paste("../R_Data/belarus_un_mortality_data_month_only_since_2011.RData") )





# Fix 2021.04.24
# Merging two datasets into one.

belarus_un_mortality_data_month_only_since_2015$date_text <- as.Date( belarus_un_mortality_data_month_only_since_2015$date_fixed )
monthly_death_data_frame_covid19$date_text <- as.Date( monthly_death_data_frame_covid19$date_fixed )

intersected_data <- merge( x = belarus_un_mortality_data_month_only_since_2015, 
       y = monthly_death_data_frame_covid19 )




# Generating pdf output.
pdf("../Plots/Figure01a.pdf", height = 8, width = 20)
# Definign the number of plots
#par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )


# First plot (Belarus)
plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )


# First plot (Belarus)
plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )


# First plot (Belarus)
plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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
pdf("../Plots/Figure01d.pdf", height = 12, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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





# Second graph

# First plot (Belarus)
plot(x = intersected_data$date_fixed,
     y = intersected_data$death_covid19,
     col = "darkorange",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Confirmed COVID-19)",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     #ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = intersected_data$date_fixed,
      y = intersected_data$death_covid19,
      col = "darkorange",
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
        fill = c("darkorange"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(intersected_data$date_fixed))
final_date   <- as.integer(max(intersected_data$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(intersected_data$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- intersected_data$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(x_tlab), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.035*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(intersected_data$death_covid19)
y_max_value <- max(intersected_data$death_covid19)
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)


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
proportion_covid19 <- intersected_data$death_covid19/(intersected_data$Value+intersected_data$death_covid19)

summary_to_plot <-  rbind( proportion_covid19,
                           (0.02 - proportion_covid19)  )
colnames(summary_to_plot) <- intersected_data$unique_month
rownames(summary_to_plot) <- NULL

barplot( summary_to_plot, col=c("darkorange", "darkblue"), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of COVID-19 Death in Total",
         names.arg = colnames(summary_to_plot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 1)

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


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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





# Second graph

# First plot (Belarus)
plot(x = intersected_data$date_fixed,
     y = intersected_data$death_covid19,
     col = "darkturquoise",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Confirmed COVID-19)",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     #ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = intersected_data$date_fixed,
      y = intersected_data$death_covid19,
      col = "darkturquoise",
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
        fill = c("darkturquoise"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(intersected_data$date_fixed))
final_date   <- as.integer(max(intersected_data$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(intersected_data$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- intersected_data$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(x_tlab), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.035*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(intersected_data$death_covid19)
y_max_value <- max(intersected_data$death_covid19)
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)


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
proportion_covid19 <- intersected_data$death_covid19/(intersected_data$Value+intersected_data$death_covid19)

summary_to_plot <-  rbind( proportion_covid19,
                           (0.02 - proportion_covid19)  )
colnames(summary_to_plot) <- intersected_data$unique_month
rownames(summary_to_plot) <- NULL

barplot( summary_to_plot, col=c("darkturquoise", "darkblue"), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of COVID-19 Death in Total",
         names.arg = colnames(summary_to_plot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 1)

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
pdf("../Plots/Figure01f.pdf", height = 12, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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





# Second graph

# First plot (Belarus)
plot(x = intersected_data$date_fixed,
     y = intersected_data$death_covid19,
     col = "darkorange",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Confirmed COVID-19)",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     #ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = intersected_data$date_fixed,
      y = intersected_data$death_covid19,
      col = "darkorange",
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
        fill = c("darkorange"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(intersected_data$date_fixed))
final_date   <- as.integer(max(intersected_data$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(intersected_data$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- intersected_data$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(x_tlab), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.035*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(intersected_data$death_covid19)
y_max_value <- max(intersected_data$death_covid19)
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)


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
proportion_covid19 <- intersected_data$death_covid19/(intersected_data$Value+intersected_data$death_covid19)

summary_to_plot <-  rbind( proportion_covid19,
                           (0.02 - proportion_covid19)  )
colnames(summary_to_plot) <- intersected_data$unique_month
rownames(summary_to_plot) <- NULL

barplot( summary_to_plot, col=c("darkorange", "darkblue"), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of COVID-19 Death in Total",
         names.arg = colnames(summary_to_plot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 1)

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
pdf("../Plots/Figure01g.pdf", height = 12, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


combined_value_min <- min( belarus_un_mortality_data_month_only_since_2015$Value )
combined_value_max <- max( belarus_un_mortality_data_month_only_since_2015$Value )

combined_date_min  <- min( belarus_un_mortality_data_month_only_since_2015$date_fixed )
combined_date_max  <- max( belarus_un_mortality_data_month_only_since_2015$date_fixed )

layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))
#plot(1,main=1)
#plot(2,main=2)


# First graph

plot(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
     y = belarus_un_mortality_data_month_only_since_2015$Value,
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
lines(x = belarus_un_mortality_data_month_only_since_2015$date_fixed,
      y = belarus_un_mortality_data_month_only_since_2015$Value,
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
initial_date <- as.integer(min(belarus_un_mortality_data_month_only_since_2015$date_fixed))
final_date   <- as.integer(max(belarus_un_mortality_data_month_only_since_2015$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  = 1, to  = length(belarus_un_mortality_data_month_only_since_2015$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(belarus_un_mortality_data_month_only_since_2015$date_fixed[x_indexes_to_display]), start = 1, stop = 7 )
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





# Second graph

# First plot (Belarus)
plot(x = intersected_data$date_fixed,
     y = intersected_data$death_covid19,
     col = "darkturquoise",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Monthly Mortality (Confirmed COVID-19)",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     #ylim = c( combined_value_min, combined_value_max ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)
lines(x = intersected_data$date_fixed,
      y = intersected_data$death_covid19,
      col = "darkturquoise",
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
        fill = c("darkturquoise"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.5 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- as.integer(min(intersected_data$date_fixed))
final_date   <- as.integer(max(intersected_data$date_fixed))
number_of_dates <- final_date - initial_date


# Indexes to display
x_indexes_to_display <-  seq( from  =  1, to  = length(intersected_data$date_fixed),  by = 1 )
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- intersected_data$date_fixed[x_indexes_to_display]
# ctual lab labels
x_lablist  <-  substr( x = as.character(x_tlab), start = 1, stop = 7 )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.035*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- min(intersected_data$death_covid19)
y_max_value <- max(intersected_data$death_covid19)
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 0) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)


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
proportion_covid19 <- intersected_data$death_covid19/(intersected_data$Value+intersected_data$death_covid19)

summary_to_plot <-  rbind( proportion_covid19,
                           (0.02 - proportion_covid19)  )
colnames(summary_to_plot) <- intersected_data$unique_month
rownames(summary_to_plot) <- NULL

barplot( summary_to_plot, col=c("darkturquoise", "darkblue"), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Proportion of COVID-19 Death in Total",
         names.arg = colnames(summary_to_plot), 
         cex.names = 1.7, 
         cex.lab = 2, 
         cex.axis = 1.75,
         cex.main = 2, 
         cex = 2,
         las = 1)

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












