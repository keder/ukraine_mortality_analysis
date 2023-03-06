


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

# Loading library(lmtest) for Granger test. 
# https://www.statology.org/granger-causality-test-in-r/
library(lmtest)






# Loading the trends data as RData file.
load( file = paste("../../R_Data/google_trends_grob_data.RData") )
load( file = paste("../../R_Data/google_trends_pomynky_data.RData") )
load( file = paste("../../R_Data/google_trends_ritualnie_uslugi_data.RData") )
load( file = paste("../../R_Data/google_trends_truna_data.RData") )
load( file = paste("../../R_Data/google_trends_rytualni_posluhy_data.RData") )
dim(google_trends_grob_data)
dim(google_trends_pomynky_data)
dim(google_trends_ritualnie_uslugi_data)
ls()

# Loading the mortality data as RData file.
load( file = paste("../../R_Data/ukraine_un_mortality_data_month_only_since_2015.RData") )
dim(ukraine_un_mortality_data_month_only_since_2015)
ls()




# Frame to save results
frame_results_combined <- data.frame( Name = c("grob", "pomynky", "ritualnie_uslugi", "truna", "rytualni_posluhy"),
                                      rho_pearson = rep(0,5),
                                      rho_pearson_smooth = rep(0,5),
                                      rho_spearman = rep(0,5),
                                      rho_spearman_smooth = rep(0,5),
                                      grangertest = rep(0,5),
                                      grangertest_smooth  = rep(0,5) )
names(frame_results_combined)



# grob time series
merged_grob <- base::merge( x = google_trends_grob_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_grob$grob_scaled  <- 100 * merged_grob$grob / median(merged_grob$grob)
merged_grob$value_scaled <- 100 * merged_grob$Value / median(merged_grob$Value)
sum(!merged_grob$grob_scaled  == merged_grob$grob)


# Add integer dates
merged_grob$Date_integer <- as.integer(merged_grob$Date)
merged_grob$date_text <- format( merged_grob$Date, "%Y-%m" )


# loess fit_incidence_scaled
loess_fit_grob_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_grob, span = 0.25 )
summary(loess_fit_grob_incidence_scaled)
names(loess_fit_grob_incidence_scaled)

# Predicted values
merged_grob$incidence_scaled <- predict(loess_fit_grob_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_grob_predictor_scaled <- loess( grob_scaled ~ Date_integer, data = merged_grob, span = 0.25 )
summary(loess_fit_grob_predictor_scaled)
names(loess_fit_grob_predictor_scaled)

# Predicted values
merged_grob$predictor_scaled <- predict(loess_fit_grob_predictor_scaled)

# Correlations
# original data re-scaled
cor(merged_grob$value_scaled,  merged_grob$grob_scaled, method = "pearson")
cor(merged_grob$value_scaled,  merged_grob$grob_scaled, method = "spearman")
# smoothers fo te-scalled data
cor(merged_grob$incidence_scaled,  merged_grob$predictor_scaled, method = "pearson")
cor(merged_grob$incidence_scaled,  merged_grob$predictor_scaled, method = "spearman")

# Granger's test
# order 1
grangertest(y = merged_grob$grob_scaled, x = merged_grob$value_scaled, order = 1)
grangertest(y = merged_grob$predictor_scaled, x = merged_grob$incidence_scaled, order = 1)


# Saving the results
index_to_save <- which( frame_results_combined$Name =="grob")
# Correlations
# original data re-scaled
frame_results_combined$rho_pearson[index_to_save]   <-  cor(merged_grob$value_scaled,  merged_grob$grob_scaled, method = "pearson")
frame_results_combined$rho_spearman[index_to_save]  <-  cor(merged_grob$value_scaled,  merged_grob$grob_scaled, method = "spearman")
# smoothers fo te-scalled data
frame_results_combined$rho_pearson_smooth[index_to_save]   <-  cor(merged_grob$incidence_scaled,  merged_grob$predictor_scaled, method = "pearson")
frame_results_combined$rho_spearman_smooth[index_to_save]  <-  cor(merged_grob$incidence_scaled,  merged_grob$predictor_scaled, method = "spearman")

# Granger's test
# order 1
frame_results_combined$grangertest[index_to_save]        <- grangertest(y = merged_grob$grob_scaled, x = merged_grob$value_scaled, order = 1)$"Pr(>F)"[2]
frame_results_combined$grangertest_smooth[index_to_save] <- grangertest(y = merged_grob$predictor_scaled, x = merged_grob$incidence_scaled, order = 1)$"Pr(>F)"[2]





# pomynky time series
merged_pomynky <- base::merge( x = google_trends_pomynky_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_pomynky$pomynky_scaled  <- 100 * merged_pomynky$pomynky / median(merged_pomynky$pomynky)
merged_pomynky$value_scaled <- 100 * merged_pomynky$Value / median(merged_pomynky$Value)
sum(!merged_pomynky$pomynky_scaled  == merged_pomynky$pomynky)


# Add integer dates
merged_pomynky$Date_integer <- as.integer(merged_pomynky$Date)
merged_pomynky$date_text <- format( merged_grob$Date, "%Y-%m" )


# loess fit_incidence_scaled
loess_fit_pomynky_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_pomynky, span = 0.25 )
summary(loess_fit_pomynky_incidence_scaled)
names(loess_fit_pomynky_incidence_scaled)

# Predicted values
merged_pomynky$incidence_scaled <- predict(loess_fit_pomynky_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_pomynky_predictor_scaled <- loess( pomynky_scaled ~ Date_integer, data = merged_pomynky, span = 0.25 )
summary(loess_fit_pomynky_predictor_scaled)
names(loess_fit_pomynky_predictor_scaled)

# Predicted values
merged_pomynky$predictor_scaled <- predict(loess_fit_pomynky_predictor_scaled)

# Correlations
# original data re-scaled
cor(merged_pomynky$value_scaled,  merged_pomynky$pomynky_scaled, method = "pearson")
cor(merged_pomynky$value_scaled,  merged_pomynky$pomynky_scaled, method = "spearman")
# smoothers fo te-scalled data
cor(merged_pomynky$incidence_scaled,  merged_pomynky$predictor_scaled, method = "pearson")
cor(merged_pomynky$incidence_scaled,  merged_pomynky$predictor_scaled, method = "spearman")

# Granger's test
# order 1
grangertest(y = merged_pomynky$pomynky_scaled, x = merged_pomynky$value_scaled, order = 1)
grangertest(y = merged_pomynky$predictor_scaled, x = merged_pomynky$incidence_scaled, order = 1)


# Saving the results
index_to_save <- which( frame_results_combined$Name =="pomynky")
# Correlations
# original data re-scaled
frame_results_combined$rho_pearson[index_to_save]   <-  cor(merged_pomynky$value_scaled,  merged_pomynky$pomynky_scaled, method = "pearson")
frame_results_combined$rho_spearman[index_to_save]  <-  cor(merged_pomynky$value_scaled,  merged_pomynky$pomynky_scaled, method = "spearman")
# smoothers fo te-scalled data
frame_results_combined$rho_pearson_smooth[index_to_save]   <-  cor(merged_pomynky$incidence_scaled,  merged_pomynky$predictor_scaled, method = "pearson")
frame_results_combined$rho_spearman_smooth[index_to_save]  <-  cor(merged_pomynky$incidence_scaled,  merged_pomynky$predictor_scaled, method = "spearman")

# Granger's test
# order 1
frame_results_combined$grangertest[index_to_save]        <- grangertest(y = merged_pomynky$pomynky_scaled, x = merged_pomynky$value_scaled, order = 1)$"Pr(>F)"[2]
frame_results_combined$grangertest_smooth[index_to_save] <- grangertest(y = merged_pomynky$predictor_scaled, x = merged_pomynky$incidence_scaled, order = 1)$"Pr(>F)"[2]




# ritualnie_uslugi time series
merged_ritualnie_uslugi <- base::merge( x = google_trends_ritualnie_uslugi_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_ritualnie_uslugi$ritualnie_uslugi_scaled  <- 100 * merged_ritualnie_uslugi$ritualnie_uslugi / median(merged_ritualnie_uslugi$ritualnie_uslugi)
merged_ritualnie_uslugi$value_scaled <- 100 * merged_ritualnie_uslugi$Value / median(merged_ritualnie_uslugi$Value)
sum(!merged_ritualnie_uslugi$ritualnie_uslugi_scaled  == merged_ritualnie_uslugi$ritualnie_uslugi)


# Add integer dates
merged_ritualnie_uslugi$Date_integer <- as.integer(merged_ritualnie_uslugi$Date)
merged_ritualnie_uslugi$date_text <- format( merged_grob$Date, "%Y-%m" )


# loess fit_incidence_scaled
loess_fit_ritualnie_uslugi_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_ritualnie_uslugi, span = 0.25 )
summary(loess_fit_ritualnie_uslugi_incidence_scaled)
names(loess_fit_ritualnie_uslugi_incidence_scaled)

# Predicted values
merged_ritualnie_uslugi$incidence_scaled <- predict(loess_fit_ritualnie_uslugi_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_ritualnie_uslugi_predictor_scaled <- loess( ritualnie_uslugi_scaled ~ Date_integer, data = merged_ritualnie_uslugi, span = 0.25 )
summary(loess_fit_ritualnie_uslugi_predictor_scaled)
names(loess_fit_ritualnie_uslugi_predictor_scaled)

# Predicted values
merged_ritualnie_uslugi$predictor_scaled <- predict(loess_fit_ritualnie_uslugi_predictor_scaled)

# Correlations
# original data re-scaled
cor(merged_ritualnie_uslugi$value_scaled,  merged_ritualnie_uslugi$ritualnie_uslugi_scaled, method = "pearson")
cor(merged_ritualnie_uslugi$value_scaled,  merged_ritualnie_uslugi$ritualnie_uslugi_scaled, method = "spearman")
# smoothers fo te-scalled data
cor(merged_ritualnie_uslugi$incidence_scaled,  merged_ritualnie_uslugi$predictor_scaled, method = "pearson")
cor(merged_ritualnie_uslugi$incidence_scaled,  merged_ritualnie_uslugi$predictor_scaled, method = "spearman")

# Granger's test
# order 1
grangertest(y = merged_ritualnie_uslugi$ritualnie_uslugi_scaled, x = merged_ritualnie_uslugi$value_scaled, order = 1)
grangertest(y = merged_ritualnie_uslugi$predictor_scaled, x = merged_ritualnie_uslugi$incidence_scaled, order = 1)


# Saving the results
index_to_save <- which( frame_results_combined$Name =="ritualnie_uslugi")
# Correlations
# original data re-scaled
frame_results_combined$rho_pearson[index_to_save]   <-  cor(merged_ritualnie_uslugi$value_scaled,  merged_ritualnie_uslugi$ritualnie_uslugi_scaled, method = "pearson")
frame_results_combined$rho_spearman[index_to_save]  <-  cor(merged_ritualnie_uslugi$value_scaled,  merged_ritualnie_uslugi$ritualnie_uslugi_scaled, method = "spearman")
# smoothers fo te-scalled data
frame_results_combined$rho_pearson_smooth[index_to_save]   <-  cor(merged_ritualnie_uslugi$incidence_scaled,  merged_ritualnie_uslugi$predictor_scaled, method = "pearson")
frame_results_combined$rho_spearman_smooth[index_to_save]  <-  cor(merged_ritualnie_uslugi$incidence_scaled,  merged_ritualnie_uslugi$predictor_scaled, method = "spearman")

# Granger's test
# order 1
frame_results_combined$grangertest[index_to_save]        <- grangertest(y = merged_ritualnie_uslugi$ritualnie_uslugi_scaled, x = merged_ritualnie_uslugi$value_scaled, order = 1)$"Pr(>F)"[2]
frame_results_combined$grangertest_smooth[index_to_save] <- grangertest(y = merged_ritualnie_uslugi$predictor_scaled, x = merged_ritualnie_uslugi$incidence_scaled, order = 1)$"Pr(>F)"[2]



# truna time series
merged_truna <- base::merge( x = google_trends_truna_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_truna$truna_scaled  <- 100 * merged_truna$truna / median(merged_truna$truna)
merged_truna$value_scaled <- 100 * merged_truna$Value / median(merged_truna$Value)
sum(!merged_truna$truna_scaled  == merged_truna$truna)

# Add integer dates
merged_truna$Date_integer <- as.integer(merged_truna$Date)
merged_truna$date_text <- format( merged_grob$Date, "%Y-%m" )


# loess fit_incidence_scaled
loess_fit_truna_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_truna, span = 0.25 )
summary(loess_fit_truna_incidence_scaled)
names(loess_fit_truna_incidence_scaled)

# Predicted values
merged_truna$incidence_scaled <- predict(loess_fit_truna_incidence_scaled)

# Add integer dates
merged_truna$Date_integer <- as.integer(merged_truna$Date)

# loess fit_predictor_scaled
loess_fit_truna_predictor_scaled <- loess( truna_scaled ~ Date_integer, data = merged_truna, span = 0.25 )
summary(loess_fit_truna_predictor_scaled)
names(loess_fit_truna_predictor_scaled)

# Predicted values
merged_truna$predictor_scaled <- predict(loess_fit_truna_predictor_scaled)

# Correlations
# original data re-scaled
cor(merged_truna$value_scaled,  merged_truna$truna_scaled, method = "pearson")
cor(merged_truna$value_scaled,  merged_truna$truna_scaled, method = "spearman")
# smoothers fo te-scalled data
cor(merged_truna$incidence_scaled,  merged_truna$predictor_scaled, method = "pearson")
cor(merged_truna$incidence_scaled,  merged_truna$predictor_scaled, method = "spearman")

# Granger's test
# order 1
grangertest(y = merged_truna$truna_scaled, x = merged_truna$value_scaled, order = 1)
grangertest(y = merged_truna$predictor_scaled, x = merged_truna$incidence_scaled, order = 1)


# Saving the results
index_to_save <- which( frame_results_combined$Name =="truna")
# Correlations
# original data re-scaled
frame_results_combined$rho_pearson[index_to_save]   <-  cor(merged_truna$value_scaled,  merged_truna$truna_scaled, method = "pearson")
frame_results_combined$rho_spearman[index_to_save]  <-  cor(merged_truna$value_scaled,  merged_truna$truna_scaled, method = "spearman")
# smoothers fo te-scalled data
frame_results_combined$rho_pearson_smooth[index_to_save]   <-  cor(merged_truna$incidence_scaled,  merged_truna$predictor_scaled, method = "pearson")
frame_results_combined$rho_spearman_smooth[index_to_save]  <-  cor(merged_truna$incidence_scaled,  merged_truna$predictor_scaled, method = "spearman")

# Granger's test
# order 1
frame_results_combined$grangertest[index_to_save]        <- grangertest(y = merged_truna$truna_scaled, x = merged_truna$value_scaled, order = 1)$"Pr(>F)"[2]
frame_results_combined$grangertest_smooth[index_to_save] <- grangertest(y = merged_truna$predictor_scaled, x = merged_truna$incidence_scaled, order = 1)$"Pr(>F)"[2]




# rytualni_posluhy time series
merged_rytualni_posluhy <- base::merge( x = google_trends_rytualni_posluhy_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_rytualni_posluhy$rytualni_posluhy_scaled  <- 100 * merged_rytualni_posluhy$rytualni_posluhy / median(merged_rytualni_posluhy$rytualni_posluhy)
merged_rytualni_posluhy$value_scaled <- 100 * merged_rytualni_posluhy$Value / median(merged_rytualni_posluhy$Value)
sum(!merged_rytualni_posluhy$rytualni_posluhy_scaled  == merged_rytualni_posluhy$rytualni_posluhy)


# Add integer dates
merged_rytualni_posluhy$Date_integer <- as.integer(merged_rytualni_posluhy$Date)
merged_rytualni_posluhy$date_text <- format( merged_grob$Date, "%Y-%m" )


# loess fit_incidence_scaled
loess_fit_rytualni_posluhy_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_rytualni_posluhy, span = 0.25 )
summary(loess_fit_rytualni_posluhy_incidence_scaled)
names(loess_fit_rytualni_posluhy_incidence_scaled)

# Predicted values
merged_rytualni_posluhy$incidence_scaled <- predict(loess_fit_rytualni_posluhy_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_rytualni_posluhy_predictor_scaled <- loess( rytualni_posluhy_scaled ~ Date_integer, data = merged_rytualni_posluhy, span = 0.25 )
summary(loess_fit_rytualni_posluhy_predictor_scaled)
names(loess_fit_rytualni_posluhy_predictor_scaled)

# Predicted values
merged_rytualni_posluhy$predictor_scaled <- predict(loess_fit_rytualni_posluhy_predictor_scaled)

# Correlations
# original data re-scaled
cor(merged_rytualni_posluhy$value_scaled,  merged_rytualni_posluhy$rytualni_posluhy_scaled, method = "pearson")
cor(merged_rytualni_posluhy$value_scaled,  merged_rytualni_posluhy$rytualni_posluhy_scaled, method = "spearman")
# smoothers fo te-scalled data
cor(merged_rytualni_posluhy$incidence_scaled,  merged_rytualni_posluhy$predictor_scaled, method = "pearson")
cor(merged_rytualni_posluhy$incidence_scaled,  merged_rytualni_posluhy$predictor_scaled, method = "spearman")

# Granger's test
# order 1
grangertest(y = merged_rytualni_posluhy$rytualni_posluhy_scaled, x = merged_rytualni_posluhy$value_scaled, order = 1)
grangertest(y = merged_rytualni_posluhy$predictor_scaled, x = merged_rytualni_posluhy$incidence_scaled, order = 1)


# Saving the results
index_to_save <- which( frame_results_combined$Name =="rytualni_posluhy")
# Correlations
# original data re-scaled
frame_results_combined$rho_pearson[index_to_save]   <-  cor(merged_rytualni_posluhy$value_scaled,  merged_rytualni_posluhy$rytualni_posluhy_scaled, method = "pearson")
frame_results_combined$rho_spearman[index_to_save]  <-  cor(merged_rytualni_posluhy$value_scaled,  merged_rytualni_posluhy$rytualni_posluhy_scaled, method = "spearman")
# smoothers fo te-scalled data
frame_results_combined$rho_pearson_smooth[index_to_save]   <-  cor(merged_rytualni_posluhy$incidence_scaled,  merged_rytualni_posluhy$predictor_scaled, method = "pearson")
frame_results_combined$rho_spearman_smooth[index_to_save]  <-  cor(merged_rytualni_posluhy$incidence_scaled,  merged_rytualni_posluhy$predictor_scaled, method = "spearman")

# Granger's test
# order 1
frame_results_combined$grangertest[index_to_save]        <- grangertest(y = merged_rytualni_posluhy$rytualni_posluhy_scaled, x = merged_rytualni_posluhy$value_scaled, order = 1)$"Pr(>F)"[2]
frame_results_combined$grangertest_smooth[index_to_save] <- grangertest(y = merged_rytualni_posluhy$predictor_scaled, x = merged_rytualni_posluhy$incidence_scaled, order = 1)$"Pr(>F)"[2]





# Saving the data as RData file.
save( frame_results_combined, file = paste("../../R_Data/frame_results_combined.RData") )


# Creating xtable object
frame_results_combined_xtable <- xtable(x = frame_results_combined, digits = 2 )  
# Exporting as tex file
# Creating a path 
frame_results_combined_xtable_path_out <- paste("../../R_Output/frame_results_combined_xtable.tex", sep ="")
# Printing
print.xtable( x = frame_results_combined_xtable, type="latex", file = frame_results_combined_xtable_path_out, include.rownames = FALSE )





# Generating pdf output.
pdf( paste( "../../Plots/FigureTBD04a.pdf", sep = ""), height = 5, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,3)),  mar=c(7.1, 5.1, 3, 2.1)  )


# First plot
# grob
plot(x = merged_grob$Date,
     y = merged_grob$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"grob\"",
     ylim = c( min(merged_grob$value_scaled, merged_grob$grob_scaled), 
               max(merged_grob$value_scaled, merged_grob$grob_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_grob$Date,
      y = merged_grob$grob_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_grob$Date,
      y = merged_grob$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_grob$Date,
      y = merged_grob$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_grob <- as.integer( min(merged_grob$Date) )
final_value_grob   <- as.integer( max(merged_grob$Date) )
number_of_value_grob <- final_value_grob - initial_value_grob

x_tlab <- seq( from  = initial_value_grob, to  = final_value_grob,  by = trunc(number_of_value_grob/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_grob <- round( min(merged_grob$value_scaled, merged_grob$grob_scaled) )
y_max_value_grob <- round( max(merged_grob$value_scaled, merged_grob$grob_scaled) )
y_tlab  <- round( seq( from = y_min_value_grob, to = y_max_value_grob, by = (y_max_value_grob-y_min_value_grob)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
y <- y[2] - strheight(txt, cex=4) * 3.5 / 5
text(x, y, txt, cex = 4)






# Second plot
# pomynky
plot(x = merged_pomynky$Date,
     y = merged_pomynky$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"pominki\"",
     ylim = c( min(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled), 
               max(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$pomynky_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_pomynky <- as.integer( min(merged_pomynky$Date) )
final_value_pomynky   <- as.integer( max(merged_pomynky$Date) )
number_of_value_pomynky <- final_value_pomynky - initial_value_pomynky

x_tlab <- seq( from  = initial_value_pomynky, to  = final_value_pomynky,  by = trunc(number_of_value_pomynky/15) )   
x_lablist <- x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_pomynky <- round( min(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) )
y_max_value_pomynky <- round( max(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) )
y_tlab  <- round( seq( from = y_min_value_pomynky, to = y_max_value_pomynky, by = (y_max_value_pomynky-y_min_value_pomynky)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
y <- y[2] - strheight(txt, cex=4) * 3.5 / 5
text(x, y, txt, cex = 4)







# Third plot
# ritualnie_uslugi
plot(x = merged_ritualnie_uslugi$Date,
     y = merged_ritualnie_uslugi$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"ritualnie uslugi\"",
     ylim = c( min(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled)* 0.70, 
               max(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$ritualnie_uslugi_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_ritualnie_uslugi <- as.integer( min(merged_ritualnie_uslugi$Date) )
final_value_ritualnie_uslugi   <- as.integer( max(merged_ritualnie_uslugi$Date) )
number_of_value_ritualnie_uslugi <- final_value_ritualnie_uslugi - initial_value_ritualnie_uslugi

x_tlab <- seq( from  = initial_value_ritualnie_uslugi, to  = final_value_ritualnie_uslugi,  by = trunc(number_of_value_ritualnie_uslugi/15) )   
x_lablist <- x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_ritualnie_uslugi <- round( min(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled) )
y_max_value_ritualnie_uslugi <- round( max(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled) )
y_tlab  <- round( seq( from = y_min_value_ritualnie_uslugi, to = y_max_value_ritualnie_uslugi, by = (y_max_value_ritualnie_uslugi-y_min_value_ritualnie_uslugi)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
y <- y[2] - strheight(txt, cex=4) * 3.5 / 5
text(x, y, txt, cex = 4)

dev.off()




# Generating pdf output.
pdf( paste( "../../Plots/FigureTBD04b.pdf", sep = ""), height = 5, width = 15)
# Definign the number of plots
par( par(mfrow=c(1,3)),  mar=c(7.1, 5.1, 3, 2.1)  )



# Second plot
# truna
plot(x = merged_truna$Date,
     y = merged_truna$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"truna\"",
     ylim = c( min(merged_truna$value_scaled, merged_truna$truna_scaled), 
               max(merged_truna$value_scaled, merged_truna$truna_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_truna$Date,
      y = merged_truna$truna_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_truna$Date,
      y = merged_truna$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_truna$Date,
      y = merged_truna$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.1, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_truna <- as.integer( min(merged_truna$Date) )
final_value_truna   <- as.integer( max(merged_truna$Date) )
number_of_value_truna <- final_value_truna - initial_value_truna

x_tlab <- seq( from  = initial_value_truna, to  = final_value_truna,  by = trunc(number_of_value_truna/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_truna <- round( min(merged_truna$value_scaled, merged_truna$truna_scaled) )
y_max_value_truna <- round( max(merged_truna$value_scaled, merged_truna$truna_scaled) )
y_tlab  <- round( seq( from = y_min_value_truna, to = y_max_value_truna, by = (y_max_value_truna-y_min_value_truna)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )format( merged_grob$Date, "%Y-%m" )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


# Label B
par(xpd = NA )

di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from="in", to="user")
y <- grconvertY(c(0, di[2]), from="in", to="user")

fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]

txt <- "A"
x <- x[1] + strwidth(txt, cex=4) * 6 / 5
y <- y[2] - strheight(txt, cex=4) * 3.5 / 5
text(x, y, txt, cex = 4)




# Second plot
# pomynky
plot(x = merged_pomynky$Date,
     y = merged_pomynky$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"pomynky\"",
     ylim = c( min(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled), 
               max(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$pomynky_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_pomynky <- as.integer( min(merged_pomynky$Date) )
final_value_pomynky   <- as.integer( max(merged_pomynky$Date) )
number_of_value_pomynky <- final_value_pomynky - initial_value_pomynky

x_tlab <- seq( from  = initial_value_pomynky, to  = final_value_pomynky,  by = trunc(number_of_value_pomynky/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_pomynky <- round( min(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) )
y_max_value_pomynky <- round( max(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) )
y_tlab  <- round( seq( from = y_min_value_pomynky, to = y_max_value_pomynky, by = (y_max_value_pomynky-y_min_value_pomynky)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
y <- y[2] - strheight(txt, cex=4) * 3.5 / 5
text(x, y, txt, cex = 4)




# Third plot
# rytualni_posluhy
plot(x = merged_rytualni_posluhy$Date,
     y = merged_rytualni_posluhy$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"rytualni posluhy\"",
     ylim = c( min(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled)* 0.70, 
               max(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_rytualni_posluhy$Date,
      y = merged_rytualni_posluhy$rytualni_posluhy_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_rytualni_posluhy$Date,
      y = merged_rytualni_posluhy$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_rytualni_posluhy$Date,
      y = merged_rytualni_posluhy$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_rytualni_posluhy <- as.integer( min(merged_rytualni_posluhy$Date) )
final_value_rytualni_posluhy   <- as.integer( max(merged_rytualni_posluhy$Date) )
number_of_value_rytualni_posluhy <- final_value_rytualni_posluhy - initial_value_rytualni_posluhy

x_tlab <- seq( from  = initial_value_rytualni_posluhy, to  = final_value_rytualni_posluhy,  by = trunc(number_of_value_rytualni_posluhy/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_rytualni_posluhy <- round( min(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled) )
y_max_value_rytualni_posluhy <- round( max(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled) )
y_tlab  <- round( seq( from = y_min_value_rytualni_posluhy, to = y_max_value_rytualni_posluhy, by = (y_max_value_rytualni_posluhy-y_min_value_rytualni_posluhy)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
y <- y[2] - strheight(txt, cex=4) * 3.5 / 5
text(x, y, txt, cex = 4)





dev.off()














# Generating pdf output.
pdf( paste( "../../Plots/FigureTBD04c.pdf", sep = ""), height = 5, width = 25)
# Definign the number of plots
par( par(mfrow=c(1,5)),  mar=c(5.1, 5.1, 3, 2.1)  )


# First plot
# grob
plot(x = merged_grob$Date,
     y = merged_grob$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "l",
     main = "Mortality vs Google Trend \"grob\"",
     ylim = c( min(merged_grob$value_scaled, merged_grob$grob_scaled), 
               max(merged_grob$value_scaled, merged_grob$grob_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_grob$Date,
      y = merged_grob$grob_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = merged_grob$Date,
      y = merged_grob$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_grob$Date,
      y = merged_grob$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_grob <- as.integer( min(merged_grob$Date) )
final_value_grob   <- as.integer( max(merged_grob$Date) )
number_of_value_grob <- final_value_grob - initial_value_grob

x_tlab <- seq( from  = initial_value_grob, to  = final_value_grob,  by = trunc(number_of_value_grob/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_grob <- round( min(merged_grob$value_scaled, merged_grob$grob_scaled) )
y_max_value_grob <- round( max(merged_grob$value_scaled, merged_grob$grob_scaled) )
y_tlab  <- round( seq( from = y_min_value_grob, to = y_max_value_grob, by = (y_max_value_grob-y_min_value_grob)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
# pomynky
plot(x = merged_pomynky$Date,
     y = merged_pomynky$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "l",
     main = "Mortality vs Google Trend \"pominki\"",
     ylim = c( min(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled), 
               max(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$pomynky_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_pomynky$Date,
      y = merged_pomynky$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_pomynky <- as.integer( min(merged_pomynky$Date) )
final_value_pomynky   <- as.integer( max(merged_pomynky$Date) )
number_of_value_pomynky <- final_value_pomynky - initial_value_pomynky

x_tlab <- seq( from  = initial_value_pomynky, to  = final_value_pomynky,  by = trunc(number_of_value_pomynky/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_pomynky <- round( min(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) )
y_max_value_pomynky <- round( max(merged_pomynky$value_scaled, merged_pomynky$pomynky_scaled) )
y_tlab  <- round( seq( from = y_min_value_pomynky, to = y_max_value_pomynky, by = (y_max_value_pomynky-y_min_value_pomynky)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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
# ritualnie_uslugi
plot(x = merged_ritualnie_uslugi$Date,
     y = merged_ritualnie_uslugi$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "l",
     main = "Mortality vs Google Trend \"ritualnie uslugi\"",
     ylim = c( min(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled)* 0.70, 
               max(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$ritualnie_uslugi_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_ritualnie_uslugi <- as.integer( min(merged_ritualnie_uslugi$Date) )
final_value_ritualnie_uslugi   <- as.integer( max(merged_ritualnie_uslugi$Date) )
number_of_value_ritualnie_uslugi <- final_value_ritualnie_uslugi - initial_value_ritualnie_uslugi

x_tlab <- seq( from  = initial_value_ritualnie_uslugi, to  = final_value_ritualnie_uslugi,  by = trunc(number_of_value_ritualnie_uslugi/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_ritualnie_uslugi <- round( min(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled) )
y_max_value_ritualnie_uslugi <- round( max(merged_ritualnie_uslugi$value_scaled, merged_ritualnie_uslugi$ritualnie_uslugi_scaled) )
y_tlab  <- round( seq( from = y_min_value_ritualnie_uslugi, to = y_max_value_ritualnie_uslugi, by = (y_max_value_ritualnie_uslugi-y_min_value_ritualnie_uslugi)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


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




# Second plot
# truna
plot(x = merged_truna$Date,
     y = merged_truna$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "l",
     main = "Mortality vs Google Trend \"truna\"",
     ylim = c( min(merged_truna$value_scaled, merged_truna$truna_scaled), 
               max(merged_truna$value_scaled, merged_truna$truna_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_truna$Date,
      y = merged_truna$truna_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = merged_truna$Date,
      y = merged_truna$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_truna$Date,
      y = merged_truna$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.1, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_truna <- as.integer( min(merged_truna$Date) )
final_value_truna   <- as.integer( max(merged_truna$Date) )
number_of_value_truna <- final_value_truna - initial_value_truna

x_tlab <- seq( from  = initial_value_truna, to  = final_value_truna,  by = trunc(number_of_value_truna/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_truna <- round( min(merged_truna$value_scaled, merged_truna$truna_scaled) )
y_max_value_truna <- round( max(merged_truna$value_scaled, merged_truna$truna_scaled) )
y_tlab  <- round( seq( from = y_min_value_truna, to = y_max_value_truna, by = (y_max_value_truna-y_min_value_truna)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


# Label B
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







# Third plot
# rytualni_posluhy
plot(x = merged_rytualni_posluhy$Date,
     y = merged_rytualni_posluhy$value_scaled,
     col = "#00bb61",
     lwd = 5,
     pch = 19,
     type = "l",
     main = "Mortality vs Google Trend \"rytualni posluhy\"",
     ylim = c( min(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled)* 0.70, 
               max(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled) ),
     xlab = "",
     ylab = "Value (Standardized)",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = merged_rytualni_posluhy$Date,
      y = merged_rytualni_posluhy$rytualni_posluhy_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = merged_rytualni_posluhy$Date,
      y = merged_rytualni_posluhy$incidence_scaled,
      col = "#005BBB",
      lwd = 5,
      type = "l"
)
lines(x = merged_rytualni_posluhy$Date,
      y = merged_rytualni_posluhy$predictor_scaled,
      col = "#FFD500",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("#00bb61", "darkgoldenrod4", "#005BBB", "#FFD500"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_rytualni_posluhy <- as.integer( min(merged_rytualni_posluhy$Date) )
final_value_rytualni_posluhy   <- as.integer( max(merged_rytualni_posluhy$Date) )
number_of_value_rytualni_posluhy <- final_value_rytualni_posluhy - initial_value_rytualni_posluhy

x_tlab <- seq( from  = initial_value_rytualni_posluhy, to  = final_value_rytualni_posluhy,  by = trunc(number_of_value_rytualni_posluhy/15) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_rytualni_posluhy <- round( min(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled) )
y_max_value_rytualni_posluhy <- round( max(merged_rytualni_posluhy$value_scaled, merged_rytualni_posluhy$rytualni_posluhy_scaled) )
y_tlab  <- round( seq( from = y_min_value_rytualni_posluhy, to = y_max_value_rytualni_posluhy, by = (y_max_value_rytualni_posluhy-y_min_value_rytualni_posluhy)/5 ) )
y_lablist <- as.character( round(y_tlab,  digits = 4) )
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)


# Label C
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




dev.off()




