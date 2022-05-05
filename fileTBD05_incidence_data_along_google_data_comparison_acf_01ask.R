# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.09.23. ask
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
load( file = paste("../R_Data/google_trends_truna_data.RData") )
load( file = paste("../R_Data/google_trends_ritualnii_poslugi_data.RData") )
dim(google_trends_grob_data)
dim(google_trends_pominki_data)
dim(google_trends_ritualnie_uslugi_data)
ls()

# Loading the mortality data as RData file.
load( file = paste("../R_Data/ukraine_un_mortality_data_month_only_since_2015.RData") )
dim(ukraine_un_mortality_data_month_only_since_2015)
ls()




# grob time series
merged_grob <- base::merge( x = google_trends_grob_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
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


# Creating ccf objects
ccf_grob_scaled          <- ccf(x = merged_grob$value_scaled,     y = merged_grob$grob_scaled  )
ccf_grob_scaled_smoothed <- ccf(x = merged_grob$incidence_scaled, y = merged_grob$predictor_scaled  )




# pominki time series
merged_pominki <- base::merge( x = google_trends_pominki_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
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


# Creating ccf objects
ccf_pominki_scaled          <- ccf(x = merged_pominki$value_scaled,     y = merged_pominki$pominki_scaled  )
ccf_pominki_scaled_smoothed <- ccf(x = merged_pominki$incidence_scaled, y = merged_pominki$predictor_scaled  )







# ritualnie_uslugi time series
merged_ritualnie_uslugi <- base::merge( x = google_trends_ritualnie_uslugi_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
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


# Creating ccf objects
ccf_ritualnie_uslugi_scaled          <- ccf(x = merged_ritualnie_uslugi$value_scaled,     y = merged_ritualnie_uslugi$ritualnie_uslugi_scaled  )
ccf_ritualnie_uslugi_scaled_smoothed <- ccf(x = merged_ritualnie_uslugi$incidence_scaled, y = merged_ritualnie_uslugi$predictor_scaled  )





# truna time series
merged_truna <- base::merge( x = google_trends_truna_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_truna$truna_scaled  <- 100 * merged_truna$truna / median(merged_truna$truna)
merged_truna$value_scaled <- 100 * merged_truna$Value / median(merged_truna$Value)
sum(!merged_truna$truna_scaled  == merged_truna$truna)


# Add integer dates
merged_truna$Date_integer <- as.integer(merged_truna$Date)


# loess fit_incidence_scaled
loess_fit_truna_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_truna, span = 0.25 )
summary(loess_fit_truna_incidence_scaled)
names(loess_fit_truna_incidence_scaled)

# Predicted values
merged_truna$incidence_scaled <- predict(loess_fit_truna_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_truna_predictor_scaled <- loess( truna_scaled ~ Date_integer, data = merged_truna, span = 0.25 )
summary(loess_fit_truna_predictor_scaled)
names(loess_fit_truna_predictor_scaled)

# Predicted values
merged_truna$predictor_scaled <- predict(loess_fit_truna_predictor_scaled)


# Creating ccf objects
ccf_truna_scaled          <- ccf(x = merged_truna$value_scaled,     y = merged_truna$truna_scaled  )
ccf_truna_scaled_smoothed <- ccf(x = merged_truna$incidence_scaled, y = merged_truna$predictor_scaled  )







# ritualnii_poslugi time series
merged_ritualnii_poslugi <- base::merge( x = google_trends_ritualnii_poslugi_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_ritualnii_poslugi$ritualnii_poslugi_scaled  <- 100 * merged_ritualnii_poslugi$ritualnii_poslugi / median(merged_ritualnii_poslugi$ritualnii_poslugi)
merged_ritualnii_poslugi$value_scaled <- 100 * merged_ritualnii_poslugi$Value / median(merged_ritualnii_poslugi$Value)
sum(!merged_ritualnii_poslugi$ritualnii_poslugi_scaled  == merged_ritualnii_poslugi$ritualnii_poslugi)


# Add integer dates
merged_ritualnii_poslugi$Date_integer <- as.integer(merged_ritualnii_poslugi$Date)


# loess fit_incidence_scaled
loess_fit_ritualnii_poslugi_incidence_scaled <- loess( value_scaled ~ Date_integer, data = merged_ritualnii_poslugi, span = 0.25 )
summary(loess_fit_ritualnii_poslugi_incidence_scaled)
names(loess_fit_ritualnii_poslugi_incidence_scaled)

# Predicted values
merged_ritualnii_poslugi$incidence_scaled <- predict(loess_fit_ritualnii_poslugi_incidence_scaled)



# loess fit_predictor_scaled
loess_fit_ritualnii_poslugi_predictor_scaled <- loess( ritualnii_poslugi_scaled ~ Date_integer, data = merged_ritualnii_poslugi, span = 0.25 )
summary(loess_fit_ritualnii_poslugi_predictor_scaled)
names(loess_fit_ritualnii_poslugi_predictor_scaled)

# Predicted values
merged_ritualnii_poslugi$predictor_scaled <- predict(loess_fit_ritualnii_poslugi_predictor_scaled)


# Creating ccf objects
ccf_ritualnii_poslugi_scaled          <- ccf(x = merged_ritualnii_poslugi$value_scaled,     y = merged_ritualnii_poslugi$ritualnii_poslugi_scaled  )
ccf_ritualnii_poslugi_scaled_smoothed <- ccf(x = merged_ritualnii_poslugi$incidence_scaled, y = merged_ritualnii_poslugi$predictor_scaled  )







# Generating pdf output.
pdf( paste( "../Plots/FigureTBD05a.pdf", sep = ""), height = 8, width = 20)
# Definign the number of plots
par( par(mfrow=c(2,5)),  mar=c(5.1, 5.1, 3, 2.1)  )


# First plot
# grob

plot(ccf_grob_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"grob\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 4,
     cex.sub = 2
)


# Second plot
# pominki
plot(ccf_pominki_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"pominki\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Third plot
# ritualnie_uslugi
plot(ccf_ritualnie_uslugi_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"ritualnie uslugi\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)

# Second plot
# truna
plot(ccf_truna_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"truna\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Third plot
# ritualnii_poslugi
plot(ccf_ritualnii_poslugi_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"ritualnii poslugi\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Fourth plot
# grob

plot(ccf_grob_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"grob\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Fifth plot
# pominki
plot(ccf_pominki_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"pominki\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Sixth plot
# ritualnie_uslugi
plot(ccf_ritualnie_uslugi_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"ritualnie uslugi\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)

# Fifth plot
# truna
plot(ccf_truna_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"truna\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Sixth plot
# ritualnii_poslugi
plot(ccf_ritualnii_poslugi_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"ritualnii poslugi\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


dev.off()








