


rm(list=ls(all=TRUE))


# Library for the latex exports in the nice format.
library(xtable)





# Non-parametric Results load

# Loading the data as RData file.
load( file = paste("../R_Data/p_scores_frame_five.RData") )




# Prophet Results load

# Loading the data as RData file.
load( file = paste("../R_Data/prophet_predictions_five_plus_original_data_subset.RData") )
# Loading the data as RData file.
load( file = paste("../R_Data/prophet_predictions_five_plus_original_data_Age65Up_subset.RData") )




# ARIMA Results load

# Loading the data as RData file.
load( file = paste("../R_Data/arima_predictions_five_plus_original_data_subset.RData") )
# Loading the data as RData file.
load( file = paste("../R_Data/arima_predictions_five_plus_original_data_Age65Up_subset.RData") )





# Defining the desired month
month_list_2020 <- c("2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08", "2020-09", "2020-10", "2020-11", "2020-12")
month_list_2021 <- c("2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10", "2021-11", "2021-12")
# Defining the desired data frame to store the summaries.




# Five years

p_scores_different_methods_five_2020 <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                              Jan = rep(0, 5),
                                               Feb = rep(0, 5),
                                               Mar = rep(0, 5),
                                               Apr = rep(0, 5),
                                               May = rep(0, 5),
                                               Jun = rep(0, 5),
                                               Jul = rep(0, 5),
                                               Aug = rep(0, 5),
                                               Sep = rep(0, 5),
                                               Oct = rep(0, 5),
                                               Nov = rep(0, 5),
                                               Dec = rep(0, 5),
                                               Total = rep(0, 5) )
p_scores_different_methods_five_2020   
p_scores_different_methods_five_2021 <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                                Jan = rep(0, 5),
                                               Feb = rep(0, 5),
                                               Mar = rep(0, 5),
                                               Apr = rep(0, 5),
                                               May = rep(0, 5),
                                               Jun = rep(0, 5),
                                               Jul = rep(0, 5),
                                               Aug = rep(0, 5),
                                               Sep = rep(0, 5),
                                               Oct = rep(0, 5),
                                               Nov = rep(0, 5),
                                               Dec = rep(0, 5),
                                               Total = rep(0, 5) )             
                                          

counts_different_methods_five_2020 <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                              Jan = rep(0, 5),
                                               Feb = rep(0, 5),
                                               Mar = rep(0, 5),
                                               Apr = rep(0, 5),
                                               May = rep(0, 5),
                                               Jun = rep(0, 5),
                                               Jul = rep(0, 5),
                                               Aug = rep(0, 5),
                                               Sep = rep(0, 5),
                                               Oct = rep(0, 5),
                                               Nov = rep(0, 5),
                                               Dec = rep(0, 5),
                                               Total = rep(0, 5) )
counts_different_methods_five_2020                
counts_different_methods_five_2021 <- data.frame( method = c("Non-parametric", "Prophet", "Prophet&65+", "ARIMA", "ARIMA&65+" ),
                                                Jan = rep(0, 5),
                                               Feb = rep(0, 5),
                                               Mar = rep(0, 5),
                                               Apr = rep(0, 5),
                                               May = rep(0, 5),
                                               Jun = rep(0, 5),
                                               Jul = rep(0, 5),
                                               Aug = rep(0, 5),
                                               Sep = rep(0, 5),
                                               Oct = rep(0, 5),
                                               Nov = rep(0, 5),
                                               Dec = rep(0, 5),
                                               Total = rep(0, 5) )



# Processing p_scores

# Non-parametric
nonparam_correct_indexes_five <- which(p_scores_frame_five$month_text %in% month_list_2020)
p_scores_different_methods_five_2020[1, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( p_scores_frame_five$p_score_value[nonparam_correct_indexes_five], 2)
p_scores_different_methods_five_2020[1,]$Total = sum(p_scores_different_methods_five_2020[1, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2020[1, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( p_scores_frame_five$Value[nonparam_correct_indexes_five] - p_scores_frame_five$average_five_years[nonparam_correct_indexes_five], 2)
counts_different_methods_five_2020[1,]$Total = sum(counts_different_methods_five_2020[1, c(2:(length(nonparam_correct_indexes_five)+1)) ])

nonparam_correct_indexes_five <- which(p_scores_frame_five$month_text %in% month_list_2021)
p_scores_different_methods_five_2021[1, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( p_scores_frame_five$p_score_value[nonparam_correct_indexes_five], 2)
p_scores_different_methods_five_2021[1,]$Total = sum(p_scores_different_methods_five_2021[1, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2021[1, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( p_scores_frame_five$Value[nonparam_correct_indexes_five] - p_scores_frame_five$average_five_years[nonparam_correct_indexes_five], 2)
counts_different_methods_five_2021[1,]$Total = sum(counts_different_methods_five_2021[1, c(2:(length(nonparam_correct_indexes_five)+1)) ])


# Prophet
prophet_correct_indexes_five <- which(prophet_predictions_five_plus_original_data_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five_2020[2, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( prophet_predictions_five_plus_original_data_subset$p_scores_upper[prophet_correct_indexes_five], 2)
p_scores_different_methods_five_2020[2,]$Total = sum(p_scores_different_methods_five_2020[2, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2020[2, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( prophet_predictions_five_plus_original_data_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_five], 2)
counts_different_methods_five_2020[2,]$Total = sum(counts_different_methods_five_2020[2, c(2:(length(nonparam_correct_indexes_five)+1)) ])

prophet_correct_indexes_five <- which(prophet_predictions_five_plus_original_data_subset$year_month_text %in% month_list_2021)
p_scores_different_methods_five_2021[2, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( prophet_predictions_five_plus_original_data_subset$p_scores_upper[prophet_correct_indexes_five], 2)
p_scores_different_methods_five_2021[2,]$Total = sum(p_scores_different_methods_five_2021[2, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2021[2, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( prophet_predictions_five_plus_original_data_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_five], 2)
counts_different_methods_five_2021[2,]$Total = sum(counts_different_methods_five_2021[2, c(2:(length(nonparam_correct_indexes_five)+1)) ])


# Prophet & 65 +
prophet_correct_indexes_Age65Up_five <- which(prophet_predictions_five_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five_2020[3, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( prophet_predictions_five_plus_original_data_Age65Up_subset$p_scores_upper[prophet_correct_indexes_Age65Up_five], 2)
p_scores_different_methods_five_2020[3,]$Total = sum(p_scores_different_methods_five_2020[3, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2020[3, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( prophet_predictions_five_plus_original_data_Age65Up_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_Age65Up_five], 2)
counts_different_methods_five_2020[3,]$Total = sum(counts_different_methods_five_2020[3, c(2:(length(nonparam_correct_indexes_five)+1)) ])

prophet_correct_indexes_Age65Up_five <- which(prophet_predictions_five_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2021)
p_scores_different_methods_five_2021[3, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( prophet_predictions_five_plus_original_data_Age65Up_subset$p_scores_upper[prophet_correct_indexes_Age65Up_five], 2)
p_scores_different_methods_five_2021[3,]$Total = sum(p_scores_different_methods_five_2021[3, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2021[3, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( prophet_predictions_five_plus_original_data_Age65Up_subset$raw_y_minus_yhat_upper[prophet_correct_indexes_Age65Up_five], 2)
counts_different_methods_five_2021[3,]$Total = sum(counts_different_methods_five_2021[3, c(2:(length(nonparam_correct_indexes_five)+1)) ])


# ARIMA
arima_correct_indexes_five <- which(arima_predictions_five_plus_original_data_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five_2020[4, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( arima_predictions_five_plus_original_data_subset$p_scores[arima_correct_indexes_five], 2)
p_scores_different_methods_five_2020[4,]$Total = sum(p_scores_different_methods_five_2020[4, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2020[4, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( arima_predictions_five_plus_original_data_subset$raw_y_minus_y_hat[arima_correct_indexes_five], 2)
counts_different_methods_five_2020[4,]$Total = sum(counts_different_methods_five_2020[4, c(2:(length(nonparam_correct_indexes_five)+1)) ])

arima_correct_indexes_five <- which(arima_predictions_five_plus_original_data_subset$year_month_text %in% month_list_2021)
p_scores_different_methods_five_2021[4, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( arima_predictions_five_plus_original_data_subset$p_scores[arima_correct_indexes_five], 2)
p_scores_different_methods_five_2021[4,]$Total = sum(p_scores_different_methods_five_2021[4, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2021[4, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( arima_predictions_five_plus_original_data_subset$raw_y_minus_y_hat[arima_correct_indexes_five], 2)
counts_different_methods_five_2021[4,]$Total = sum(counts_different_methods_five_2021[4, c(2:(length(nonparam_correct_indexes_five)+1)) ])


# ARIMA +
arima_correct_indexes_Age65Up_five <- which(arima_predictions_five_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2020)
p_scores_different_methods_five_2020[5, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( arima_predictions_five_plus_original_data_Age65Up_subset$p_scores[arima_correct_indexes_Age65Up_five], 2)
p_scores_different_methods_five_2020[5,]$Total = sum(p_scores_different_methods_five_2020[5, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2020[5, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( arima_predictions_five_plus_original_data_Age65Up_subset$raw_y_minus_y_hat[arima_correct_indexes_Age65Up_five], 2)
counts_different_methods_five_2020[5,]$Total = sum(counts_different_methods_five_2020[5, c(2:(length(nonparam_correct_indexes_five)+1)) ])

arima_correct_indexes_Age65Up_five <- which(arima_predictions_five_plus_original_data_Age65Up_subset$year_month_text %in% month_list_2021)
p_scores_different_methods_five_2021[5, c(2:(length(nonparam_correct_indexes_five)+1)) ] <- round( arima_predictions_five_plus_original_data_Age65Up_subset$p_scores[arima_correct_indexes_Age65Up_five], 2)
p_scores_different_methods_five_2021[5,]$Total = sum(p_scores_different_methods_five_2021[5, c(2:(length(nonparam_correct_indexes_five)+1)) ])
counts_different_methods_five_2021[5, c(2:(length(nonparam_correct_indexes_five)+1)) ]   <- round( arima_predictions_five_plus_original_data_Age65Up_subset$raw_y_minus_y_hat[arima_correct_indexes_Age65Up_five], 2)
counts_different_methods_five_2021[5,]$Total = sum(counts_different_methods_five_2021[5, c(2:(length(nonparam_correct_indexes_five)+1)) ])




# Fix 2021.05.06
# Saving the data as RData file.
save( p_scores_different_methods_five_2020, file = paste("../R_Data/p_scores_different_methods_five_2020.RData") )
# Saving the data as RData file.
save( counts_different_methods_five_2020,   file = paste("../R_Data/counts_different_methods_five_2020.RData") )

# Creating xtable object
p_scores_different_methods_five_xtable <- xtable(x = p_scores_different_methods_five_2020, digits = 2 )  
# Creating xtable object
counts_different_methods_five_xtable   <- xtable(x = counts_different_methods_five_2020, digits = 0 )  


# Exporting as tex file
# Creating a path 
p_scores_different_methods_five_xtable_path_out <- paste("../R_Output/p_scores_different_methods_five_xtable_2020.tex", sep ="")
# Printing
print.xtable( x = p_scores_different_methods_five_xtable, type="latex", file = p_scores_different_methods_five_xtable_path_out, include.rownames = FALSE )
write.csv(p_scores_different_methods_five_xtable, "../R_Output/p_scores_different_methods_five_xtable_2020.csv")

# Creating a path 
counts_different_methods_five_xtable_path_out <- paste("../R_Output/counts_different_methods_five_xtable_2020.tex", sep ="")
# Printing
print.xtable( x = counts_different_methods_five_xtable, type="latex", file = counts_different_methods_five_xtable_path_out, include.rownames = FALSE )




save( p_scores_different_methods_five_2021, file = paste("../R_Data/p_scores_different_methods_five_2021.RData") )
# Saving the data as RData file.
save( counts_different_methods_five_2021,   file = paste("../R_Data/counts_different_methods_five_2021.RData") )

# Creating xtable object
p_scores_different_methods_five_xtable <- xtable(x = p_scores_different_methods_five_2021, digits = 2 )  
# Creating xtable object
counts_different_methods_five_xtable   <- xtable(x = counts_different_methods_five_2021, digits = 0 )  


# Exporting as tex file
# Creating a path 
p_scores_different_methods_five_xtable_path_out <- paste("../R_Output/p_scores_different_methods_five_xtable_2021.tex", sep ="")
# Printing
print.xtable( x = p_scores_different_methods_five_xtable, type="latex", file = p_scores_different_methods_five_xtable_path_out, include.rownames = FALSE )
write.csv(p_scores_different_methods_five_xtable, "../R_Output/p_scores_different_methods_five_xtable_2021.csv")

# Creating a path 
counts_different_methods_five_xtable_path_out <- paste("../R_Output/counts_different_methods_five_xtable_2021.tex", sep ="")
# Printing
print.xtable( x = counts_different_methods_five_xtable, type="latex", file = counts_different_methods_five_xtable_path_out, include.rownames = FALSE )







