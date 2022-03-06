# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.10.01. ask
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

# Loading library(lmtest) for Granger test. 
# https://www.statology.org/granger-causality-test-in-r/
library(lmtest)






# Loading the trends data as RData file.
load( file = paste("../R_Data/google_trends_grob_data.RData") )
load( file = paste("../R_Data/google_trends_pominki_data.RData") )
load( file = paste("../R_Data/google_trends_ritualnie_uslugi_data.RData") )
dim(google_trends_grob_data)
dim(google_trends_pominki_data)
dim(google_trends_ritualnie_uslugi_data)
ls()

# Loading the mortality data as RData file.
load( file = paste("../R_Data/belarus_un_mortality_data_month_only_since_2015.RData") )
dim(belarus_un_mortality_data_month_only_since_2015)
ls()




# Frame to save results
frame_results_combined <- data.frame( Name = c("grob", "pominki", "ritualnie_uslugi"),
                                      rho_pearson = rep(0,3),
                                      rho_pearson_smooth = rep(0,3),
                                      rho_spearman = rep(0,3),
                                      rho_spearman_smooth = rep(0,3),
                                      grangertest = rep(0,3),
                                      grangertest_smooth  = rep(0,3) )
names(frame_results_combined)



# grob time series
merged_grob <- base::merge( x = google_trends_grob_data, y = belarus_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_grob$grob_scaled  <- 100 * merged_grob$grob / median(merged_grob$grob)
merged_grob$value_scaled <- 100 * merged_grob$Value / median(merged_grob$Value)
sum(!merged_grob$grob_scaled  == merged_grob$grob)


# Add integer dates
merged_grob$Date_integer <- as.integer(merged_grob$Date)


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





# pominki time series
merged_pominki <- base::merge( x = google_trends_pominki_data, y = belarus_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_pominki$pominki_scaled  <- 100 * merged_pominki$pominki / median(merged_pominki$pominki)
merged_pominki$value_scaled <- 100 * merged_pominki$Value / median(merged_pominki$Value)
sum(!merged_pominki$pominki_scaled  == merged_pominki$pominki)


# Add integer dates
merged_pominki$Date_integer <- as.integer(merged_pominki$Date)


# loess fit_incidence_scaled
loess_fit_pominki_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_pominki, span = 0.25 )
summary(loess_fit_pominki_incidence_scaled)
names(loess_fit_pominki_incidence_scaled)

# Predicted values
merged_pominki$incidence_scaled <- predict(loess_fit_pominki_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_pominki_predictor_scaled <- loess( pominki_scaled ~ Date_integer, data = merged_pominki, span = 0.25 )
summary(loess_fit_pominki_predictor_scaled)
names(loess_fit_pominki_predictor_scaled)

# Predicted values
merged_pominki$predictor_scaled <- predict(loess_fit_pominki_predictor_scaled)

# Correlations
# original data re-scaled
cor(merged_pominki$value_scaled,  merged_pominki$pominki_scaled, method = "pearson")
cor(merged_pominki$value_scaled,  merged_pominki$pominki_scaled, method = "spearman")
# smoothers fo te-scalled data
cor(merged_pominki$incidence_scaled,  merged_pominki$predictor_scaled, method = "pearson")
cor(merged_pominki$incidence_scaled,  merged_pominki$predictor_scaled, method = "spearman")

# Granger's test
# order 1
grangertest(y = merged_pominki$pominki_scaled, x = merged_pominki$value_scaled, order = 1)
grangertest(y = merged_pominki$predictor_scaled, x = merged_pominki$incidence_scaled, order = 1)


# Saving the results
index_to_save <- which( frame_results_combined$Name =="pominki")
# Correlations
# original data re-scaled
frame_results_combined$rho_pearson[index_to_save]   <-  cor(merged_pominki$value_scaled,  merged_pominki$pominki_scaled, method = "pearson")
frame_results_combined$rho_spearman[index_to_save]  <-  cor(merged_pominki$value_scaled,  merged_pominki$pominki_scaled, method = "spearman")
# smoothers fo te-scalled data
frame_results_combined$rho_pearson_smooth[index_to_save]   <-  cor(merged_pominki$incidence_scaled,  merged_pominki$predictor_scaled, method = "pearson")
frame_results_combined$rho_spearman_smooth[index_to_save]  <-  cor(merged_pominki$incidence_scaled,  merged_pominki$predictor_scaled, method = "spearman")

# Granger's test
# order 1
frame_results_combined$grangertest[index_to_save]        <- grangertest(y = merged_pominki$pominki_scaled, x = merged_pominki$value_scaled, order = 1)$"Pr(>F)"[2]
frame_results_combined$grangertest_smooth[index_to_save] <- grangertest(y = merged_pominki$predictor_scaled, x = merged_pominki$incidence_scaled, order = 1)$"Pr(>F)"[2]




# ritualnie_uslugi time series
merged_ritualnie_uslugi <- base::merge( x = google_trends_ritualnie_uslugi_data, y = belarus_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_ritualnie_uslugi$ritualnie_uslugi_scaled  <- 100 * merged_ritualnie_uslugi$ritualnie_uslugi / median(merged_ritualnie_uslugi$ritualnie_uslugi)
merged_ritualnie_uslugi$value_scaled <- 100 * merged_ritualnie_uslugi$Value / median(merged_ritualnie_uslugi$Value)
sum(!merged_ritualnie_uslugi$ritualnie_uslugi_scaled  == merged_ritualnie_uslugi$ritualnie_uslugi)


# Add integer dates
merged_ritualnie_uslugi$Date_integer <- as.integer(merged_ritualnie_uslugi$Date)


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



# Saving the data as RData file.
save( frame_results_combined, file = paste("../R_Data/frame_results_combined.RData") )


# Creating xtable object
frame_results_combined_xtable <- xtable(x = frame_results_combined, digits = 2 )  
# Exporting as tex file
# Creating a path 
frame_results_combined_xtable_path_out <- paste("../R_Output/frame_results_combined_xtable.tex", sep ="")
# Printing
print.xtable( x = frame_results_combined_xtable, type="latex", file = frame_results_combined_xtable_path_out, include.rownames = FALSE )





# Generating pdf output.
pdf( paste( "../Plots/FigureTBD04a.pdf", sep = ""), height = 4.75, width = 14.25)
# Definign the number of plots
par( par(mfrow=c(1,3)),  mar=c(5.1, 5.1, 3, 2.1)  )


# First plot
# grob
plot(x = merged_grob$Date,
     y = merged_grob$value_scaled,
     col = "darkblue",
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
      col = "darkturquoise",
      lwd = 5,
      type = "l"
)
lines(x = merged_grob$Date,
      y = merged_grob$predictor_scaled,
      col = "darkorange2",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("darkblue", "darkgoldenrod4", "darkturquoise", "darkorange2"),   
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
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01") ) 
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
# pominki
plot(x = merged_pominki$Date,
     y = merged_pominki$value_scaled,
     col = "darkblue",
     lwd = 5,
     pch = 19,
     type = "p",
     main = "Mortality vs Google Trend \"pominki\"",
     ylim = c( min(merged_pominki$value_scaled, merged_pominki$pominki_scaled), 
               max(merged_pominki$value_scaled, merged_pominki$pominki_scaled) ),
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
lines(x = merged_pominki$Date,
      y = merged_pominki$pominki_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "p",
      cex = 1.15
)
lines(x = merged_pominki$Date,
      y = merged_pominki$incidence_scaled,
      col = "darkturquoise",
      lwd = 5,
      type = "l"
)
lines(x = merged_pominki$Date,
      y = merged_pominki$predictor_scaled,
      col = "darkorange2",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.22, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("darkblue", "darkgoldenrod4", "darkturquoise", "darkorange2"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_pominki <- as.integer( min(merged_pominki$Date) )
final_value_pominki   <- as.integer( max(merged_pominki$Date) )
number_of_value_pominki <- final_value_pominki - initial_value_pominki

x_tlab <- seq( from  = initial_value_pominki, to  = final_value_pominki,  by = trunc(number_of_value_pominki/15) )   
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01") ) 
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_pominki <- round( min(merged_pominki$value_scaled, merged_pominki$pominki_scaled) )
y_max_value_pominki <- round( max(merged_pominki$value_scaled, merged_pominki$pominki_scaled) )
y_tlab  <- round( seq( from = y_min_value_pominki, to = y_max_value_pominki, by = (y_max_value_pominki-y_min_value_pominki)/5 ) )
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
     col = "darkblue",
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
      col = "darkturquoise",
      lwd = 5,
      type = "l"
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$predictor_scaled,
      col = "darkorange2",
      lwd = 5,
      type = "l"
)

legend( x = "bottomright", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("darkblue", "darkgoldenrod4", "darkturquoise", "darkorange2"),   
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
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01") ) 
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
pdf( paste( "../Plots/FigureTBD04b.pdf", sep = ""), height = 4.75, width = 14.25)
# Definign the number of plots
par( par(mfrow=c(1,3)),  mar=c(5.1, 5.1, 3, 2.1)  )


# First plot
# grob
plot(x = merged_grob$Date,
     y = merged_grob$value_scaled,
     col = "darkblue",
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
      col = "darkturquoise",
      lwd = 5,
      type = "l"
)
lines(x = merged_grob$Date,
      y = merged_grob$predictor_scaled,
      col = "darkorange2",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("darkblue", "darkgoldenrod4", "darkturquoise", "darkorange2"),   
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
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01") ) 
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
# pominki
plot(x = merged_pominki$Date,
     y = merged_pominki$value_scaled,
     col = "darkblue",
     lwd = 5,
     pch = 19,
     type = "l",
     main = "Mortality vs Google Trend \"pominki\"",
     ylim = c( min(merged_pominki$value_scaled, merged_pominki$pominki_scaled), 
               max(merged_pominki$value_scaled, merged_pominki$pominki_scaled) ),
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
lines(x = merged_pominki$Date,
      y = merged_pominki$pominki_scaled,
      col = "darkgoldenrod4",
      lwd = 5,
      pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = merged_pominki$Date,
      y = merged_pominki$incidence_scaled,
      col = "darkturquoise",
      lwd = 5,
      type = "l"
)
lines(x = merged_pominki$Date,
      y = merged_pominki$predictor_scaled,
      col = "darkorange2",
      lwd = 5,
      type = "l"
)

legend( x = "topleft", 
        inset= c(0.22, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("darkblue", "darkgoldenrod4", "darkturquoise", "darkorange2"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_pominki <- as.integer( min(merged_pominki$Date) )
final_value_pominki   <- as.integer( max(merged_pominki$Date) )
number_of_value_pominki <- final_value_pominki - initial_value_pominki

x_tlab <- seq( from  = initial_value_pominki, to  = final_value_pominki,  by = trunc(number_of_value_pominki/15) )   
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01") ) 
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_pominki <- round( min(merged_pominki$value_scaled, merged_pominki$pominki_scaled) )
y_max_value_pominki <- round( max(merged_pominki$value_scaled, merged_pominki$pominki_scaled) )
y_tlab  <- round( seq( from = y_min_value_pominki, to = y_max_value_pominki, by = (y_max_value_pominki-y_min_value_pominki)/5 ) )
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
     col = "darkblue",
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
      col = "darkturquoise",
      lwd = 5,
      type = "l"
)
lines(x = merged_ritualnie_uslugi$Date,
      y = merged_ritualnie_uslugi$predictor_scaled,
      col = "darkorange2",
      lwd = 5,
      type = "l"
)

legend( x = "bottomright", 
        inset= c(0.05, 0.05), 
        legend = c("Mortality Data", "Search Data", "Mortality Smooter", "Search Smooter"), 
        col = "black", 
        fill = c("darkblue", "darkgoldenrod4", "darkturquoise", "darkorange2"),   
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
x_lablist <- as.character( as.Date(x_tlab, origin = "1970-01-01") ) 
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




dev.off()




