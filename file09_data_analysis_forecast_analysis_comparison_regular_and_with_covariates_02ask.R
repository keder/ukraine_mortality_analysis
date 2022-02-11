# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.24. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)


# Setting the correct working directory.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Belarus Death Rates"

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


# Loading data
# load( file = paste("R_Data/arima_predictions_five_plus_original_data.RData") )

# Saving the data as RData file.
load( file = paste("R_Data/arima_predictions_five_plus_original_data_subset.RData") )

# Saving the data as RData file.
# load( file = paste("R_Data/arima_predictions_eight_plus_original_data.RData") )

# Saving the data as RData file.
load( file = paste("R_Data/arima_predictions_eight_plus_original_data_subset.RData") )



# Loading data
# load( file = paste("R_Data/arima_predictions_five_plus_original_data_Age65Up.RData") )

# Saving the data as RData file.
load( file = paste("R_Data/arima_predictions_five_plus_original_data_Age65Up_subset.RData") )

# Saving the data as RData file.
# load( file = paste("R_Data/arima_predictions_eight_plus_original_data_Age65Up.RData") )

# Saving the data as RData file.
load( file = paste("R_Data/arima_predictions_eight_plus_original_data_Age65Up_subset.RData") )







# Fix 2021.04.29
# Adding regressors
demographics_aggregated_2011_2020_transposed <- data.frame(t(demographics_aggregated_2011_2020)[-1,])
names(demographics_aggregated_2011_2020_transposed) <- t(demographics_aggregated_2011_2020)[1,]
demographics_aggregated_2011_2020_transposed$Year <- c(2011:2020)

demographics_aggregated_2011_2020_transposed$Age70Up  <- as.numeric(as.character(demographics_aggregated_2011_2020_transposed$`70Up`))
demographics_aggregated_2011_2020_transposed$Age65_69 <- as.numeric(as.character(demographics_aggregated_2011_2020_transposed$`65-69`))

demographics_aggregated_2011_2020_transposed$Age65Up  <- demographics_aggregated_2011_2020_transposed$Age65_69 + demographics_aggregated_2011_2020_transposed$Age70Up




demographics_aggregated_2011_2020_transposed$AllAges <- as.numeric( as.character(demographics_aggregated_2011_2020_transposed$AllAges) )
demographics_aggregated_2011_2020_transposed$Age65Up <- as.numeric( as.character(demographics_aggregated_2011_2020_transposed$Age65Up) )


# Min and Max
p_score_min <- min( c(arima_predictions_five_plus_original_data_Age65Up_subset$p_scores, arima_predictions_eight_plus_original_data_Age65Up_subset$p_scores) )
p_score_max <- max( c(arima_predictions_five_plus_original_data_Age65Up_subset$p_scores, arima_predictions_eight_plus_original_data_Age65Up_subset$p_scores) )





# Generating pdf output.
pdf( paste( getwd(), "/Plots/FigureS04a.pdf", sep = ""), height = 15, width = 15)
# Definign the number of plots
par( par(mfrow=c(2,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )


# First plot

age_frame_summary <- rbind( demographics_aggregated_2011_2020_transposed$Age65Up,
                            demographics_aggregated_2011_2020_transposed$AllAges - demographics_aggregated_2011_2020_transposed$Age65Up)/1000
min_values <- min( colSums( age_frame_summary ) )
max_values <- max( colSums( age_frame_summary ) )


barplot( age_frame_summary, 
         col= c("darkorange", "darkblue"), 
         legend = TRUE, 
         border =  TRUE, 
         #xlim = c(1, 5), 
         ylim = c( 0, 10275 ), 
         args.legend = list(bty="n", border=TRUE), 
         ylab = "", 
         xlab = "", 
         main = "Age Structure During 2011 - 2020",
         # names.arg = as.character(p_scores_frame_five_jan_june$Month), 
         names.arg = as.character(demographics_aggregated_2011_2020_transposed$Year), 
         cex.names = 1.25, 
         cex.lab = 2, 
         cex.axis = 1.4,
         cex.main = 2, 
         cex = 2,
         las = 2)


legend( x = "topleft", 
        inset= c(0.075, 0.125), 
        legend = c("Younger Than 65", "Older Than 65"), 
        col = "black", 
        fill = c("darkblue", "darkorange"),   
        pt.cex = c(4, 2),
        bg ="white",
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





# Second graph

value_combine <- c(arima_predictions_eight_plus_original_data_subset$y_hat,
                   arima_predictions_eight_plus_original_data_Age65Up_subset$y_hat)


plot(x = as.integer(arima_predictions_eight_plus_original_data_Age65Up_subset$y_hat),
     y = as.integer(arima_predictions_eight_plus_original_data_subset$y_hat),
     col = c( rep( "darkblue", dim(arima_predictions_eight_plus_original_data_subset)[1] - 4 ),
              rep( "darkorange",  4 ) ),
     # col = color_01, 
     lwd = 2,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "p",
     pch = 19,     
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Predicted Without vs With Covariates",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( 0.99 * min(value_combine),
               max(value_combine) * 1.01  ),
     xlim = c( 0.99 * min(value_combine),
               max(value_combine) * 1.01  ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)

lines(x = c( min(value_combine):max(value_combine) ), 
      y = c( min(value_combine):max(value_combine) ), 
      col="red", 
      lwd = 1, 
      lty = 2)

legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Pre Epidemic", "During Epidemic","45 Degree Line"), 
        col = "black", 
        fill = c("darkblue", "darkorange", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(value_combine)
final_date   <- max(value_combine)
number_of_dates <- 10


# Indexes to display
x_indexes_to_display <-  round( seq( from  =  round(initial_date), to  = round(final_date),  by = floor((final_date - initial_date)/11) ))
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( x_indexes_to_display )
axis(1, at = x_tlab, labels = FALSE)
axis(1, at = x_tlab, labels = x_lablist, cex.axis = 1.25, las =2)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_tlab  <- x_indexes_to_display
y_lablist <- x_indexes_to_display
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25, las =2)


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

value_combine <- c(arima_predictions_eight_plus_original_data_subset$y_hat,
                   arima_predictions_eight_plus_original_data_subset$y)


plot(x = as.integer(arima_predictions_eight_plus_original_data_subset$y),
     y = as.integer(arima_predictions_eight_plus_original_data_subset$y_hat),
     col = c( rep( "darkblue", dim(arima_predictions_eight_plus_original_data_subset)[1] - 4 ),
              rep( "darkorange",  4 ) ),
     # col = color_01, 
     lwd = 2,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "p",
     pch = 19,     
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Observed vs Predicted and Fitted\n(Without Covariates)",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( 0.99 * min(value_combine),
               max(value_combine) * 1.01  ),
     xlim = c( 0.99 * min(value_combine),
               max(value_combine) * 1.01  ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)

lines(x = c( min(value_combine):max(value_combine) ), 
      y = c( min(value_combine):max(value_combine) ), 
      col="red", 
      lwd = 1, 
      lty = 2)

legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Pre Epidemic", "During Epidemic","45 Degree Line"), 
        col = "black", 
        fill = c("darkblue", "darkorange", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(value_combine)
final_date   <- max(value_combine)
number_of_dates <- 10


# Indexes to display
x_indexes_to_display <-  round( seq( from  =  round(initial_date), to  = round(final_date),  by = floor((final_date - initial_date)/11) ))
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( x_indexes_to_display )
axis(1, at = x_tlab, labels = FALSE)
axis(1, at = x_tlab, labels = x_lablist, cex.axis = 1.25, las =2)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_tlab  <- x_indexes_to_display
y_lablist <- x_indexes_to_display
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25, las =2)


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

value_combine <- c(arima_predictions_eight_plus_original_data_Age65Up_subset$y_hat,
                   arima_predictions_eight_plus_original_data_Age65Up_subset$y)


plot(x = as.integer(arima_predictions_eight_plus_original_data_Age65Up_subset$y),
     y = as.integer(arima_predictions_eight_plus_original_data_Age65Up_subset$y_hat),
     col = c( rep( "darkblue", dim(arima_predictions_eight_plus_original_data_Age65Up_subset)[1] - 4 ),
              rep( "darkorange",  4 ) ),
     # col = color_01, 
     lwd = 2,
     # pch = 16,
     # pch = shape_01,
     # pch = 17,
     type = "p",
     pch = 19,     
     # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
     main = "Observed vs Predicted and Fitted\n(With Covariates)",
     # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
     ylim = c( 0.99 * min(value_combine),
               max(value_combine) * 1.01  ),
     xlim = c( 0.99 * min(value_combine),
               max(value_combine) * 1.01  ),
     # ylim = c(0, y_max_value_current * 1.2  ),
     # xlab = "Time",
     xlab = "",     
     ylab = "",
     xaxt='n',
     yaxt='n',
     cex = 3,
     cex.axis = 1.55,
     cex.lab = 2,
     cex.main = 2,
     cex.sub = 2
)

lines(x = c( min(value_combine):max(value_combine) ), 
      y = c( min(value_combine):max(value_combine) ), 
      col="red", 
      lwd = 1, 
      lty = 2)

legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Pre Epidemic", "During Epidemic","45 Degree Line"), 
        col = "black", 
        fill = c("darkblue", "darkorange", "red"),   
        pt.cex = c(4, 2),
        # pch = c(19, 20),  
        cex = 1.85 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_date <- min(value_combine)
final_date   <- max(value_combine)
number_of_dates <- 10


# Indexes to display
x_indexes_to_display <-  round( seq( from  =  round(initial_date), to  = round(final_date),  by = floor((final_date - initial_date)/11) ))
# x_indexes_to_display[1] <- 1
# Actual lab elements
x_tlab <- x_indexes_to_display
# ctual lab labels
# x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
x_lablist  <- as.character( x_indexes_to_display )
axis(1, at = x_tlab, labels = FALSE)
axis(1, at = x_tlab, labels = x_lablist, cex.axis = 1.25, las =2)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_tlab  <- x_indexes_to_display
y_lablist <- x_indexes_to_display
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25, las =2)


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





