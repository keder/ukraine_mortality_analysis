# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.05.05. ask
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

# library(MASS) for MVN simulations.
library(MASS)

# This package is requred to run in RScript mode rathen than interactive mode.
library(methods)

# Libraries to work with fasta files.
# For readDNAStringSet()
library(seqinr)

# For write.fasta()
library(Biostrings)

# Loading package requred to read library(readxl)
library(readxl)


# Loading library(rjson) for json files. 
library(rjson)



# Setting the correct working directory.
# Debugging step to run on local machine instead instead of the code right above used for HiPer Gator.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Belarus Death Rates"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()


# xlsx data relative path
xlsx_data_relative_path  <- "Data/Other/Belarus_Coronavirus_Feb27_Dec31_2020_updated.xlsx"
json1_data_relative_path <- "Data/Other/onliner.scrap1.json"
json2_data_relative_path <- "Data/Other/onliner.scrap2.json"


# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()


# Reading dataset in exl
belarus_test_data <- data.frame( read_excel(xlsx_data_relative_path) )

# Reading dataset in json
belarus_incidence_data <- fromJSON( file = json1_data_relative_path )
names(belarus_incidence_data)

# Reading dataset in json
belarus_statistics_data <- fromJSON( file = json2_data_relative_path )
names(belarus_statistics_data)




# Printing the first few lines to check
# head(belarus_test_data)
belarus_test_data[1:20,]


# Fixing the column names
# variable_names <- names(belarus_test_data)
names(belarus_test_data) <- c("date", "registered", "recovered", "lethality", "no_conducted_tests")



# Fixing columns contents
# Date
belarus_test_data$date_fixed  <- as.Date( belarus_test_data$date, "%b %d, %Y" )   

# Fixing others
belarus_test_data$registered_fixed         <- as.integer(belarus_test_data$registered)
belarus_test_data$recovered_fixed          <- as.integer(belarus_test_data$recovered)
belarus_test_data$lethality_fixed          <- as.integer(belarus_test_data$lethality)
belarus_test_data$no_conducted_tests_fixed <- as.integer(belarus_test_data$no_conducted_tests)


# Daily incidence
belarus_test_data$lethality_fixed          <- as.integer(belarus_test_data$lethality)
belarus_test_data$no_conducted_tests_fixed <- as.integer(belarus_test_data$no_conducted_tests)


# Creating shifted versions
registered_fixed_shifted         <- c(0, belarus_test_data$registered_fixed[ -length(belarus_test_data$registered_fixed) ])
lethality_fixed_shifted          <- c(0, belarus_test_data$lethality_fixed[  -length(belarus_test_data$lethality_fixed) ])
no_conducted_tests_fixed_shifted <- c(0, belarus_test_data$no_conducted_tests_fixed[ -length(belarus_test_data$no_conducted_tests_fixed) ])


# Creating daily versions
belarus_test_data$registered_fixed_daily          <- belarus_test_data$registered_fixed - registered_fixed_shifted  
belarus_test_data$lethality_fixed_daily           <- belarus_test_data$lethality_fixed - lethality_fixed_shifted 
belarus_test_data$no_conducted_tests_fixed_daily  <- belarus_test_data$no_conducted_tests_fixed - no_conducted_tests_fixed_shifted 



# Getting subset with no missing data starting from April 21, 2020
belarus_test_data_subset <- belarus_test_data[ ( which(belarus_test_data$date_fixed == "2020-04-21") : dim(belarus_test_data)[1] ),  ]
# Printing summaries
head(belarus_test_data_subset)



# Working with json1 data

# Number of days
num_of_rep_days <- length(belarus_incidence_data$data)

# Creating an array to save results.
belarus_incidence_data_frame <- data.frame( matrix(0, nrow = num_of_rep_days, ncol = 2 ) )
names(belarus_incidence_data_frame) <- c("date", "onliner_cases_daily")
  


for( d in c(1:num_of_rep_days) )
{
  # Debuggins step
  # d <- 1 
  
  # Getting current date
  date_current <- as.Date( paste( belarus_incidence_data$data[d][[1]]$label, ".2020", sep =""),  "%d.%m.%Y " )

  # Procesign late dates correctly
  if ( date_current %in% c( as.Date(("2020-01-01")) :as.Date(("2020-03-01")) ) )
  {
    belarus_incidence_data_frame$date[d] <- date_current + 366
  }
  
  if ( date_current %in% c( as.Date(("2020-02-28")) :as.Date(("2020-12-31")) ) )
  {
    belarus_incidence_data_frame$date[d] <- date_current 
    
  }

  # Getting current data
  belarus_incidence_data_frame$onliner_cases_daily[d] <- as.numeric( belarus_incidence_data$data[d][[1]]$value )

}  
 
# Fixing the date format 
belarus_incidence_data_frame$date <- as.Date( belarus_incidence_data_frame$date, origin = "1970-01-01" )






# Working with json2 data

# Number of days
num_of_rep_days <- length(belarus_statistics_data$data)

# Creating an array to save results.
belarus_statistics_data_frame <- data.frame( matrix(0, nrow = num_of_rep_days, ncol = 4 ) )
names(belarus_statistics_data_frame) <- c("date", "onliner_cases_cummulative", "onliner_recovered_cummulative", "onliner_death_cummulative")



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
    belarus_statistics_data_frame$date[d] <- date_current + 366
  }
  
  if ( date_current %in% c( as.Date(("2020-02-28")) :as.Date(("2020-12-31")) ) )
  {
    belarus_statistics_data_frame$date[d] <- date_current 
    
  }
  
  # Getting current data
  
  # Reoving spaces if any
  cases_cummulative_character      <- gsub(pattern = " ", replacement = "", x = strsplit(x = belarus_statistics_data$data[d][[1]]$value, split = "\" ")[1])
  recovered_cummulative_character  <- gsub(pattern = " ", replacement = "", x = strsplit(x = belarus_statistics_data$data[d][[1]]$value, split = "\" ")[2])
  death_cummulative_character      <- gsub(pattern = " ", replacement = "", x = strsplit(x = belarus_statistics_data$data[d][[1]]$value, split = "\" ")[3])
  
  
  belarus_statistics_data_frame$onliner_cases_cummulative[d]     <- as.numeric( cases_cummulative_character )
  belarus_statistics_data_frame$onliner_recovered_cummulative[d] <- as.numeric( recovered_cummulative_character )
  belarus_statistics_data_frame$onliner_death_cummulative[d]     <- as.numeric( death_cummulative_character )
}  


# Fixing the date format 
belarus_statistics_data_frame$date <- as.Date( belarus_statistics_data_frame$date, origin = "1970-01-01" )



belarus_data_merged1 <- data.frame(merge(x = belarus_test_data_subset, y = belarus_incidence_data_frame, by.x = "date_fixed", by.y = "date",
                            all.x = TRUE, all.y = TRUE))

belarus_data_merged2 <- merge(x = belarus_data_merged1, y = belarus_statistics_data_frame, by.x = "date_fixed", by.y = "date",
                             all.x = TRUE, all.y = TRUE)



# Checking dimensions
dim(belarus_incidence_data_frame)
dim(belarus_test_data_subset)
dim(belarus_statistics_data_frame)

dim(belarus_data_merged1)
dim(belarus_data_merged2)


head(belarus_data_merged2)

belarus_data_merged2$onliner_cases_daily



# Extra checks b/w Alina and Onliner
# plot(belarus_data_merged2$registered_fixed, belarus_data_merged2$onliner_cases_cummulative)
# plot(belarus_data_merged2$recovered,  belarus_data_merged2$onliner_recovered_cummulative)
# plot(belarus_data_merged2$lethality,  belarus_data_merged2$onliner_death_cummulative)

sum( ! belarus_data_merged2$registered_fixed_daily == belarus_data_merged2$onliner_cases_daily )
sum( ! belarus_data_merged2$registered_fixed == belarus_data_merged2$onliner_cases_cummulative )
sum( ! belarus_data_merged2$recovered == belarus_data_merged2$onliner_recovered_cummulative )  
sum( ! belarus_data_merged2$lethality ==  belarus_data_merged2$onliner_death_cummulative)



belarus_data_merged2$date_fixed <= 




names(belarus_data_merged2)
head(belarus_data_merged2)



# Generating pdf output.
pdf( paste( getwd(), "/Plots/Figure04a.pdf", sep = ""), height = 15, width = 15)
# Definign the number of plots
par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )



# First plot
plot(x = belarus_data_merged2$date_fixed,
     y = belarus_data_merged2$registered_fixed_daily,
     col = "black",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Total and Positive\nCumulative Tests",
     ylim = c( min(c(belarus_data_merged2$registered_fixed_daily)),
               max(c(belarus_data_merged2$registered_fixed_daily)) ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts (in Thousands)",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.75,
     cex.main = 1.55,
     cex.sub = 1.55
)
lines(x = belarus_data_merged2$date_fixed,
      y = covid_belarus_adjusted_combined$positive,
      col = "darkorange",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Total Tests", "Positive Tests"), 
        col = "black", 
        fill = c("black", "darkorange"),   
        pt.cex = 4,  
        cex = 1.25 )  
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
x_tlab <- as.integer(covid_belarus_adjusted_combined$date)
x_lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
axis(1, at = x_tlab, labels = FALSE)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
mtext("", side = 1, line = 5, cex = 1)


# Y-axis
y_min_value <- min(c(covid_belarus_adjusted_combined$total, covid_belarus_adjusted_combined$positive))
y_max_value <- max(c(covid_belarus_adjusted_combined$total, covid_belarus_adjusted_combined$positive))
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( roundup_to(y_tlab, to = 1000)/1000 )
# lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
# lablist <- as.vector(c(1:3))
axis(2, at = y_tlab, labels = y_lablist, cex = 2)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
# text(y = y_tlab, x=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=lablist, srt=45, adj=1, xpd=TRUE, cex = 0.75)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down


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
plot(x = covid_belarus_adjusted_combined$date,
     y = covid_belarus_adjusted_combined$positive,
     col = "darkorange",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Cumulative Predicted Cases\nand Positive Tests",
     ylim = c( 0.8*min(c(covid_belarus_adjusted_combined$positive, covid_belarus_adjusted_combined$estimated_cases_lb)),
               max(c(covid_belarus_adjusted_combined$positive, covid_belarus_adjusted_combined$estimated_cases_ub)) ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Counts (in Thousands)",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)
lines(x = covid_belarus_adjusted_combined$date,
      y = covid_belarus_adjusted_combined$estimated_cases,
      col = "darkblue",
      # col = color_01, 
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = covid_belarus_adjusted_combined$date,
      y = covid_belarus_adjusted_combined$estimated_cases_lb,
      col = "darkblue",
      # col = color_01, 
      lwd = 5,
      lty =5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = covid_belarus_adjusted_combined$date,
      y = covid_belarus_adjusted_combined$estimated_cases_ub,
      col = "darkblue",
      # col = color_01, 
      lwd = 5,
      lty =5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Predicted Cases", "Positive Tests"), 
        col = "black", 
        fill = c("darkblue", "darkorange"),   
        pt.cex = 4,  
        cex = 1.15 )  
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
x_tlab <- as.integer(covid_belarus_adjusted_combined$date)
x_lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
axis(1, at = x_tlab, labels = FALSE)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
mtext("", side = 1, line = 4, cex = 1)


# Y-axis
y_min_value <- min(c(covid_belarus_adjusted_combined$positive, covid_belarus_adjusted_combined$estimated_cases_lb))
y_max_value <- max(c(covid_belarus_adjusted_combined$positive, covid_belarus_adjusted_combined$estimated_cases_ub))
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( roundup_to(y_tlab, to = 1000)/1000 )
# lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
# lablist <- as.vector(c(1:3))
axis(2, at = y_tlab, labels = y_lablist, cex = 2)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
# text(y = y_tlab, x=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=lablist, srt=45, adj=1, xpd=TRUE, cex = 0.75)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down


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



# Third plot
prop_upderestimation    <- covid_belarus_adjusted_combined$positive/covid_belarus_adjusted_combined$estimated_cases
prop_upderestimation_lb <- covid_belarus_adjusted_combined$positive/covid_belarus_adjusted_combined$estimated_cases_lb
prop_upderestimation_ub <- covid_belarus_adjusted_combined$positive/covid_belarus_adjusted_combined$estimated_cases_ub

posrate_proportion      <- covid_belarus_adjusted_combined$posrate

plot(x = covid_belarus_adjusted_combined$date,
     y = prop_upderestimation,
     col = "darkgreen",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Cumulative Proportions of\nDetected Cases and Positive Tests",
     ylim = c( min(c(prop_upderestimation, prop_upderestimation_lb, prop_upderestimation_ub, posrate_proportion)),
               max(c(prop_upderestimation, prop_upderestimation_lb, prop_upderestimation_ub, posrate_proportion)) ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     xlab = "",
     ylab = "Proportion",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)
lines(x = covid_belarus_adjusted_combined$date,
      y = prop_upderestimation_lb,
      col = "darkgreen",
      # col = color_01, 
      lwd = 5,
      lty =5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
lines(x = covid_belarus_adjusted_combined$date,
      y = prop_upderestimation_ub,
      col = "darkgreen",
      # col = color_01, 
      lwd = 5,
      lty =5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l")
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Detected Proportion"), 
        col = "black", 
        fill = c("darkgreen"),   
        pt.cex = 4,  
        cex = 1.25 )  
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
x_tlab <- as.integer(covid_belarus_adjusted_combined$date)
x_lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
axis(1, at = x_tlab, labels = FALSE)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
mtext("", side = 1, line = 4, cex = 1)


# Y-axis
y_min_value <- min(c(prop_upderestimation, prop_upderestimation_lb, prop_upderestimation_ub, posrate_proportion))
y_max_value <- max(c(prop_upderestimation, prop_upderestimation_lb, prop_upderestimation_ub, posrate_proportion))
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = (y_max_value-y_min_value)/5 )
y_lablist <- as.character( round(y_tlab,  digits = 3) )
# lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
# lablist <- as.vector(c(1:3))
axis(2, at = y_tlab, labels = y_lablist, cex = 2)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
# text(y = y_tlab, x=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=lablist, srt=45, adj=1, xpd=TRUE, cex = 0.75)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down


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





# Fourth plot
names(belarus_data_merged_fixed)
dim(belarus_data_merged_fixed)
head(belarus_data_merged_fixed)
# Defining with lag one
belarus_data_merged_fixed$no_conducted_tests_fixed_lag1   <- c( 0, belarus_data_merged_fixed$no_conducted_tests_fixed[-dim(belarus_data_merged_fixed)[1]] )
belarus_data_merged_fixed$onliner_cases_daily_cumulative <- cumsum(belarus_data_merged_fixed$onliner_cases_daily)
# Computing Defining with lag one
belarus_data_merged_fixed$no_conducted_tests_fixed_daily <- belarus_data_merged_fixed$no_conducted_tests_fixed - belarus_data_merged_fixed$no_conducted_tests_fixed_lag1
# Printing the outputs
# head(belarus_data_merged_fixed, n = 100)
# date_up_to_which_as_character is computed earlier.
which_date_up_to_which_as_character2_beginning <- which( as.character(belarus_data_merged_fixed$date) == "2020-04-15" )
which_date_up_to_which_as_character2_end       <- which( as.character(belarus_data_merged_fixed$date) == "2020-07-30" )
range_to_plot <- c( which_date_up_to_which_as_character2_beginning:which_date_up_to_which_as_character2_end )
daily_testing_proportion <- belarus_data_merged_fixed$onliner_cases_daily/belarus_data_merged_fixed$no_conducted_tests_fixed_daily
# Overriding divisions by zero
range_to_plot_fixed <- setdiff( x = range_to_plot, y = which(is.infinite(daily_testing_proportion)) )

plot(x = belarus_data_merged_fixed$date[range_to_plot_fixed],
     y = daily_testing_proportion[range_to_plot_fixed],
     col = "cornflowerblue",
     # col = color_01, 
     lwd = 5,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "l",
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Daily Proportions of\nPositive Tests",
     xlim = c( as.Date( belarus_data_merged_fixed$date[which_date_up_to_which_as_character2_beginning], origin = "1970-01-01"), 
               as.Date( belarus_data_merged_fixed$date[which_date_up_to_which_as_character2_end], origin = "1970-01-01") ),
     ylim = c( min( as.vector(na.omit(c(daily_testing_proportion[range_to_plot_fixed], posrate_proportion))) ),
               max( as.vector(na.omit(c(daily_testing_proportion[range_to_plot_fixed], posrate_proportion))) ) ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "Proportion",
     xaxt='n',
     yaxt='n',
     cex = 2,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)


lines(x = belarus_data_merged_fixed$date[range_to_plot_fixed],
      y = rep(0.05, length(belarus_data_merged_fixed$date[range_to_plot_fixed])),
      col = "red", 
      lwd = 1, 
      lty = 2)



legend( x = "topright", 
        inset= c(0.04, 0.04), 
        legend = c("Daily Tests", "Threshold (0.05)"), 
        col = "black", 
        fill = c("cornflowerblue", "red"),   
        pt.cex = 4,  
        cex = 1.25 )  
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

# X-axis
min_date_integer <- min( as.integer( belarus_data_merged_fixed$date[range_to_plot_fixed] ) )
max_date_integer <- max( as.integer( belarus_data_merged_fixed$date[range_to_plot_fixed] ) )



x_tlab <- seq( from = min_date_integer, to = max_date_integer, by = 15 )
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01")  ) 
axis(1, at = x_tlab, labels = FALSE)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 3)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
mtext("", side = 1, line = 4, cex = 1)


# Y-axis
# y_min_value <- min( as.vector(na.omit(c(daily_testing_proportion[range_to_plot_fixed], posrate_proportion))) )
y_min_value <- 0
y_max_value <- max( as.vector(na.omit(c(daily_testing_proportion[range_to_plot_fixed], posrate_proportion))) )
y_tlab  <- seq( from = y_min_value, to = y_max_value, by = 0.01 )
y_lablist <- as.character( round(y_tlab,  digits = 3) )
# lablist <- as.character( as.Date(covid_belarus_adjusted_combined$date)  ) 
# lablist <- as.vector(c(1:3))
#axis(2, at = y_tlab, labels = y_lablist, las = 2, cex = 3)
axis(2, at = y_tlab, labels = y_lablist, cex = 2)
# text( seq(1, 3, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# text(x = tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=lab, srt=45, adj=1, xpd=TRUE)
# text(y = y_tlab, x=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=lablist, srt=45, adj=1, xpd=TRUE, cex = 0.75)
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down


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

















