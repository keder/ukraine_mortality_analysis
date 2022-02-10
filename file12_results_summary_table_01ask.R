# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.05.06. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)

# Library for the latex exports in the nice format.
library(xtable)


# Setting the correct working directory.
# Debugging step to run on local machine instead instead of the code right above used for HiPer Gator.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Belarus Death Rates"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()





# Non-parametric Results load

# Loading the data as RData file.
load( file = paste("R_Data/p_scores_frame_five.RData") )
# Loading the data as RData file.
load( file = paste("R_Data/p_scores_frame_eight.RData") )




# Prophet Results load

# Loading the data as RData file.
load( file = paste("R_Data/prophet_predictions_five_plus_original_data_subset.RData") )
# Loading the data as RData file.
load( file = paste("R_Data/prophet_predictions_five_plus_original_data_Age65Up_subset.RData") )

# Loading the data as RData file.
load( file = paste("R_Data/prophet_predictions_eight_plus_original_data_subset.RData") )
# Loading the data as RData file.
load( file = paste("R_Data/prophet_predictions_eight_plus_original_data_Age65Up_subset.RData") )




# ARIMA Results load

# Loading the data as RData file.
load( file = paste("R_Data/arima_predictions_five_plus_original_data_subset.RData") )
# Loading the data as RData file.
load( file = paste("R_Data/arima_predictions_five_plus_original_data_Age65Up_subset.RData") )

# Loading the data as RData file.
load( file = paste("R_Data/arima_predictions_eight_plus_original_data_subset.RData") )
# Loading the data as RData file.
load( file = paste("R_Data/arima_predictions_eight_plus_original_data_Age65Up_subset.RData") )





# Defining the desired month
month_list_2020 <- c("2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06")
# Defining the desired data frame to store the summaries.




# Five years

p_scores_different_methods_five <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                               Year2020_01 = rep(0, 5),
                                               Year2020_02 = rep(0, 5),
                                               Year2020_03 = rep(0, 5),
                                               Year2020_04 = rep(0, 5),
                                               Year2020_05 = rep(0, 5),
                                               Year2020_06 = rep(0, 5) )
p_scores_different_methods_five                
                                          

counts_different_methods_five <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                             Year2020_01 = rep(0, 5),
                                             Year2020_02 = rep(0, 5),
                                             Year2020_03 = rep(0, 5),
                                             Year2020_04 = rep(0, 5),
                                             Year2020_05 = rep(0, 5),
                                             Year2020_06 = rep(0, 5) )
counts_different_methods_five                



# Processing p_scores

# Non-parametric
nonparam_correct_indexes_five <- which(p_scores_frame_five$month_text %in% month_list_2020)
p_scores_different_methods_five[1, c(2:7) ] <- round( p_scores_frame_five$p_score_value[nonparam_correct_indexes_five], 2)
counts_different_methods_five[1, c(2:7) ]   <- round( p_scores_frame_five$Value[nonparam_correct_indexes_five] - p_scores_frame_five$average_five_years[nonparam_correct_indexes_five], 2)


# Prophet
prophet_correct_indexes_five <- which(prophet_predictions_five_plus_original_data_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five[2, c(2:7) ] <- round( prophet_predictions_five_plus_original_data_subset$p_scores_upper[prophet_correct_indexes_five], 2)
counts_different_methods_five[2, c(2:7) ]   <- round( prophet_predictions_five_plus_original_data_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_five], 2)


# Prophet & 65 +
prophet_correct_indexes_Age65Up_five <- which(prophet_predictions_five_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five[3, c(2:7) ] <- round( prophet_predictions_five_plus_original_data_Age65Up_subset$p_scores_upper[prophet_correct_indexes_Age65Up_five], 2)
counts_different_methods_five[3, c(2:7) ]   <- round( prophet_predictions_five_plus_original_data_Age65Up_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_Age65Up_five], 2)


# ARIMA
arima_correct_indexes_five <- which(arima_predictions_five_plus_original_data_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five[4, c(2:7) ] <- round( arima_predictions_five_plus_original_data_subset$p_scores[arima_correct_indexes_five], 2)
counts_different_methods_five[4, c(2:7) ]   <- round( arima_predictions_five_plus_original_data_subset$raw_y_minus_y_hat[arima_correct_indexes_five], 2)


# ARIMA +
arima_correct_indexes_Age65Up_five <- which(arima_predictions_five_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five[5, c(2:7) ] <- round( arima_predictions_five_plus_original_data_Age65Up_subset$p_scores[arima_correct_indexes_Age65Up_five], 2)
counts_different_methods_five[5, c(2:7) ]   <- round( arima_predictions_five_plus_original_data_Age65Up_subset$raw_y_minus_y_hat[arima_correct_indexes_Age65Up_five], 2)



# Fix 2021.05.06
# Saving the data as RData file.
save( p_scores_different_methods_five, file = paste("R_Data/p_scores_different_methods_five.RData") )
# Saving the data as RData file.
save( counts_different_methods_five,   file = paste("R_Data/counts_different_methods_five.RData") )

# Creating xtable object
p_scores_different_methods_five_xtable <- xtable(x = p_scores_different_methods_five, digits = 2 )  
# Creating xtable object
counts_different_methods_five_xtable   <- xtable(x = counts_different_methods_five, digits = 0 )  


# Exporting as tex file
# Creating a path 
p_scores_different_methods_five_xtable_path_out <- paste("R_Output/p_scores_different_methods_five_xtable.tex", sep ="")
# Printing
print.xtable( x = p_scores_different_methods_five_xtable, type="latex", file = p_scores_different_methods_five_xtable_path_out, include.rownames = FALSE )

# Creating a path 
counts_different_methods_five_xtable_path_out <- paste("R_Output/counts_different_methods_five_xtable.tex", sep ="")
# Printing
print.xtable( x = counts_different_methods_five_xtable, type="latex", file = counts_different_methods_five_xtable_path_out, include.rownames = FALSE )









# Eight years

p_scores_different_methods_eight <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                               Year2020_01 = rep(0, 5),
                                               Year2020_02 = rep(0, 5),
                                               Year2020_03 = rep(0, 5),
                                               Year2020_04 = rep(0, 5),
                                               Year2020_05 = rep(0, 5),
                                               Year2020_06 = rep(0, 5) )
p_scores_different_methods_eight                


counts_different_methods_eight <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                             Year2020_01 = rep(0, 5),
                                             Year2020_02 = rep(0, 5),
                                             Year2020_03 = rep(0, 5),
                                             Year2020_04 = rep(0, 5),
                                             Year2020_05 = rep(0, 5),
                                             Year2020_06 = rep(0, 5) )
counts_different_methods_eight                



# Processing p_scores

# Non-parametric
nonparam_correct_indexes_eight <- which(p_scores_frame_eight$month_text %in% month_list_2020)
p_scores_different_methods_eight[1, c(2:7) ] <- round( p_scores_frame_eight$p_score_value[nonparam_correct_indexes_eight], 2)
counts_different_methods_eight[1, c(2:7) ]   <- round( p_scores_frame_eight$Value[nonparam_correct_indexes_eight] - p_scores_frame_eight$average_eight_years[nonparam_correct_indexes_eight], 2)


# Prophet
prophet_correct_indexes_eight <- which(prophet_predictions_eight_plus_original_data_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_eight[2, c(2:7) ] <- round( prophet_predictions_eight_plus_original_data_subset$p_scores_upper[prophet_correct_indexes_eight], 2)
counts_different_methods_eight[2, c(2:7) ]   <- round( prophet_predictions_eight_plus_original_data_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_eight], 2)


# Prophet & 65 +
prophet_correct_indexes_Age65Up_eight <- which(prophet_predictions_eight_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_eight[3, c(2:7) ] <- round( prophet_predictions_eight_plus_original_data_Age65Up_subset$p_scores_upper[prophet_correct_indexes_Age65Up_eight], 2)
counts_different_methods_eight[3, c(2:7) ]   <- round( prophet_predictions_eight_plus_original_data_Age65Up_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_Age65Up_eight], 2)


# ARIMA
arima_correct_indexes_eight <- which(arima_predictions_eight_plus_original_data_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_eight[4, c(2:7) ] <- round( arima_predictions_eight_plus_original_data_subset$p_scores[arima_correct_indexes_eight], 2)
counts_different_methods_eight[4, c(2:7) ]   <- round( arima_predictions_eight_plus_original_data_subset$raw_y_minus_y_hat[arima_correct_indexes_eight], 2)


# ARIMA +
arima_correct_indexes_Age65Up_eight <- which(arima_predictions_eight_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_eight[5, c(2:7) ] <- round( arima_predictions_eight_plus_original_data_Age65Up_subset$p_scores[arima_correct_indexes_Age65Up_eight], 2)
counts_different_methods_eight[5, c(2:7) ]   <- round( arima_predictions_eight_plus_original_data_Age65Up_subset$raw_y_minus_y_hat[arima_correct_indexes_Age65Up_eight], 2)



# Fix 2021.05.06
# Saving the data as RData file.
save( p_scores_different_methods_eight, file = paste("R_Data/p_scores_different_methods_eight.RData") )
# Saving the data as RData file.
save( counts_different_methods_eight,   file = paste("R_Data/counts_different_methods_eight.RData") )

# Creating xtable object
p_scores_different_methods_eight_xtable <- xtable(x = p_scores_different_methods_eight, digits = 2 )  
# Creating xtable object
counts_different_methods_eight_xtable   <- xtable(x = counts_different_methods_eight, digits = 0 )  


# Exporting as tex file
# Creating a path 
p_scores_different_methods_eight_xtable_path_out <- paste("R_Output/p_scores_different_methods_eight_xtable.tex", sep ="")
# Printing
print.xtable( x = p_scores_different_methods_eight_xtable, type="latex", file = p_scores_different_methods_eight_xtable_path_out, include.rownames = FALSE )

# Creating a path 
counts_different_methods_eight_xtable_path_out <- paste("R_Output/counts_different_methods_eight_xtable.tex", sep ="")
# Printing
print.xtable( x = counts_different_methods_eight_xtable, type="latex", file = counts_different_methods_eight_xtable_path_out, include.rownames = FALSE )













