


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




# pomynky time series
merged_pomynky <- base::merge( x = google_trends_pomynky_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_pomynky$pomynky_scaled  <- 100 * merged_pomynky$pomynky / median(merged_pomynky$pomynky)
merged_pomynky$value_scaled <- 100 * merged_pomynky$Value / median(merged_pomynky$Value)
sum(!merged_pomynky$pomynky_scaled  == merged_pomynky$pomynky)


# Add integer dates
merged_pomynky$Date_integer <- as.integer(merged_pomynky$Date)


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


# Creating ccf objects
ccf_pomynky_scaled          <- ccf(x = merged_pomynky$value_scaled,     y = merged_pomynky$pomynky_scaled  )
ccf_pomynky_scaled_smoothed <- ccf(x = merged_pomynky$incidence_scaled, y = merged_pomynky$predictor_scaled  )







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







# rytualni_posluhy time series
merged_rytualni_posluhy <- base::merge( x = google_trends_rytualni_posluhy_data, y = ukraine_un_mortality_data_month_only_since_2015, by.x = "Date", by.y =  "date_fixed"   )
# Geberating standardized values
merged_rytualni_posluhy$rytualni_posluhy_scaled  <- 100 * merged_rytualni_posluhy$rytualni_posluhy / median(merged_rytualni_posluhy$rytualni_posluhy)
merged_rytualni_posluhy$value_scaled <- 100 * merged_rytualni_posluhy$Value / median(merged_rytualni_posluhy$Value)
sum(!merged_rytualni_posluhy$rytualni_posluhy_scaled  == merged_rytualni_posluhy$rytualni_posluhy)


# Add integer dates
merged_rytualni_posluhy$Date_integer <- as.integer(merged_rytualni_posluhy$Date)


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


# Creating ccf objects
ccf_rytualni_posluhy_scaled          <- ccf(x = merged_rytualni_posluhy$value_scaled,     y = merged_rytualni_posluhy$rytualni_posluhy_scaled  )
ccf_rytualni_posluhy_scaled_smoothed <- ccf(x = merged_rytualni_posluhy$incidence_scaled, y = merged_rytualni_posluhy$predictor_scaled  )







# Generating pdf output.
pdf( paste( "../../Plots/FigureTBD05a.pdf", sep = ""), height = 8, width = 20)
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
# pomynky
plot(ccf_pomynky_scaled,
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

# Fifth plot
# pomynky
plot(ccf_pomynky_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"pomynky\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Third plot
# rytualni_posluhy
plot(ccf_rytualni_posluhy_scaled,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled CCF for \"rytualni posluhy\"",
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
# pomynky
plot(ccf_pomynky_scaled_smoothed,
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

# Fifth plot
# pomynky
plot(ccf_pomynky_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"pomynky\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


# Sixth plot
# rytualni_posluhy
plot(ccf_rytualni_posluhy_scaled_smoothed,
     col = "#005BBB",
     lwd = 5,
     main = "Scaled and Smoothed CCF for \"rytualni posluhy\"",
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)


dev.off()








