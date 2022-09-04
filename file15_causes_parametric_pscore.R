rm(list = ls(all = TRUE))


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

# library("prophet") - libary for time series forecasting.
library("prophet")

load(file = "../R_Data/mortality_causes_data.RData")

pandemic_start <- as.Date("2020-02-15")

dates <- mortality_causes_data$date
mortality_causes_data <- mortality_causes_data[, 1:(ncol(mortality_causes_data) - 4)]

number_of_records <- which(dates == pandemic_start)

prophet_pscores <- mortality_causes_data
prophet_pscores_lower <- mortality_causes_data
prophet_pscores_upper <- mortality_causes_data

cause_names <- names(mortality_causes_data)[1:length(names(mortality_causes_data))]

# default parameters
param_length = ncol(mortality_causes_data) - 1
plot_params <- data.frame(
    name = names(mortality_causes_data)[2:ncol(mortality_causes_data)],
    legend1_x = rep(0.06, param_length),
    legend1_y = rep(0.08, param_length),
    position1 = "topleft",
    cex1 = 2,
    padding1 = 0.0,
    legend2_x = rep(0.05, param_length),
    legend2_y = rep(0.05, param_length),
    position2 = "topleft",
    cex2 = 1.4,
    padding2 = 0.0
)
# customize some
plot_params[which(plot_params$name == "aids_caused_by_hiv"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "birth_defects"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "infectious_and_parasitic_diseases"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "perinatal_conditions"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "tuberculosis"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "violence"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "diseases_of_nervous_system"),]$padding2 = 0.2
plot_params[which(plot_params$name == "alcohol_poisoning"),]$padding2 = 0.3
plot_params[which(plot_params$name == "alcoholic_cardiomyopathy"),]$padding2 = 0.1
plot_params[which(plot_params$name == "cerebrovascular_disease"),]$padding2 = 0.1
plot_params[which(plot_params$name == "diseases_of_the_genitourinary_system"),]$padding2 = 0.1
plot_params[which(plot_params$name == "diseases_of_the_musculoskeletal_system"),]$padding2 = 0.3
plot_params[which(plot_params$name == "drowning"),]$padding2 = 0.3
plot_params[which(plot_params$name == "fire_and_smoke"),]$padding2 = 0.1
plot_params[which(plot_params$name == "malignant"),]$padding2 = 0.1
plot_params[which(plot_params$name == "outer_causes"),]$padding2 = 0.3
plot_params[which(plot_params$name == "poisoning_except_alcoholic_"),]$padding2 = 0.2
plot_params[which(plot_params$name == "suicide"),]$padding2 = 0.3
plot_params[which(plot_params$name == "traffic_accidents"),]$padding2 = 0.3
plot_params[which(plot_params$name == "tumors"),]$padding2 = 0.1
plot_params[which(plot_params$name == "aids_caused_by_hiv"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "alcohol_induced"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "alcoholic_cardiomyopathy"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "blood_and_marrow_diseases"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "cirrhosis"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "diseases_of_the_musculoskeletal_system"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "infectious_and_parasitic_diseases"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "malignant"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "mental_and_behaviour_disease"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "outer_causes"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "tuberculosis"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "tumors"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "diseases_of_nervous_system"),]$cex1 = 1.5
plot_params[which(plot_params$name == "poisoning_except_alcoholic_"),]$cex1 = 1.5
plot_params[which(plot_params$name == "skin_diseases"),]$cex1 = 1.7
plot_params[which(plot_params$name == "traffic_accidents"),]$cex1 = 1.7
plot_params[which(plot_params$name == "aids_caused_by_hiv"),]$cex1 = 1.5
plot_params[which(plot_params$name == "cirrhosis"),]$cex1 = 1.5
plot_params[which(plot_params$name == "mental_and_behaviour_disease"),]$cex1 = 1.5
plot_params[which(plot_params$name == "tuberculosis"),]$cex1 = 1.5
plot_params[which(plot_params$name == "diseases_of_nervous_system"),]$padding1 = 0.7
plot_params[which(plot_params$name == "poisoning_except_alcoholic_"),]$padding1 = 0.7
plot_params[which(plot_params$name == "skin_diseases"),]$padding1 = 0.7
plot_params[which(plot_params$name == "traffic_accidents"),]$padding1 = 0.7


for (i in 2:ncol(mortality_causes_data))
{
    values <- mortality_causes_data[, i]
    params = plot_params[i - 1,]
    data_to_feed_full <- data.frame(
        ds = dates,
        y = values
    )

    data_to_feed_truncated <- data.frame(
        ds = dates[c(1:number_of_records)],
        y = values[c(1:number_of_records)]
    )

    # Creating a prophet object.
    prophet_object <- prophet(data_to_feed_truncated)

    # Full frame for predictions. Dates only extraction
    data_to_feed_full_dates_only <- subset(data_to_feed_full, select = -c(y))

    # Precting for the specified dates.
    prophet_predictions <- predict(prophet_object, data_to_feed_full_dates_only)
    # Fixing dates
    prophet_predictions$ds <- as.Date(prophet_predictions$ds)

    prophet_pscores[, i] <- 100 * (values - prophet_predictions$yhat) / prophet_predictions$yhat
    prophet_pscores_lower[, i] <- 100 * (values - prophet_predictions$yhat_lower) / prophet_predictions$yhat_lower
    prophet_pscores_upper[, i] <- 100 * (values - prophet_predictions$yhat_upper) / prophet_predictions$yhat_upper

    p_score_min <- min(prophet_pscores[, i])
    p_score_max <- max(prophet_pscores[, i])

    pdf(paste0("../Plots/causes/prophet_pscores_", cause_names[i], ".pdf"), height = 15, width = 18)
    # Definign the number of plots
    par(par(mfrow = c(2, 1)), mar = c(5.5, 5.1, 5.1, 2.1))

    lower_index_five <- 1
    upper_index_five <- length(prophet_pscores[, i])
    range_five <- c(lower_index_five:upper_index_five)
    pandemic_data_length <- upper_index_five - which(dates == pandemic_start)
    range_five_last4 <- c(upper_index_five - c(pandemic_data_length:0))

    barplot(prophet_pscores[, i],
        col = c(rep("#005BBB", length(prophet_pscores[, i]) - pandemic_data_length), rep("#FFD500", pandemic_data_length)),
        legend = TRUE,
        border = TRUE,
        # xlim = c(1, 5),
        ylim = c(p_score_min - 5, p_score_max + 5) * (1 + params$padding1),
        args.legend = list(bty = "n", border = TRUE),
        ylab = "",
        xlab = "",
        main = "P-Scores For 2015/01-2020/02 Model Fits\n & 2020/03-2021/12 Prediction",
        # names.arg = as.character(p_scores_frame_five_jan_june$Month),
        # names.arg = as.character(p_scores_frame_five_jan_june$Month),
        # names.arg = as.character(p_scores_frame_five_jan_june$Month),
        names.arg = strftime(dates, format = "%Y-%m"),
        cex.names = 1.25,
        cex.lab = 2,
        cex.axis = 1.75,
        cex.main = 2,
        cex = 2,
        las = 2
    )


    legend(
        x = params$position1,
        inset = c(params$legend1_x, params$legend1_y),
        legend = c("Pre Epidemic", "During Epidemic"),
        col = "black",
        fill = c("#005BBB", "#FFD500"),
        pt.cex = c(4, 2),
        # pch = c(19, 20),
        # pch = c(19, 20),
        # pch = c(19, 20),
        cex = params$cex1
    )


    # Label A
    par(xpd = NA)

    di <- dev.size("in")
    x <- grconvertX(c(0, di[1]), from = "in", to = "user")
    y <- grconvertY(c(0, di[2]), from = "in", to = "user")

    fig <- par("fig")
    x <- x[1] + (x[2] - x[1]) * fig[1:2]
    y <- y[1] + (y[2] - y[1]) * fig[3:4]

    txt <- "A"
    x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
    y <- y[2] - strheight(txt, cex = 4) * 6 / 5
    text(x, y, txt, cex = 4)



    value_combine <- c(
        prophet_predictions$yhat_upper,
        mortality_causes_data[range_five, i]
    )


    plot(
        x = dates,
        y = prophet_predictions$yhat,
        col = "#005BBB",
        # col = color_01,
        # col = color_01,
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l",
        # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
        main = "Reported (2015/01-2021/12)\nFitted (2015/01-2020/02) & Predicted (2020/03-2021/12)",
        # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
        ylim = c(
            min(value_combine) * 0.90,
            max(value_combine) * (1.05 + params$padding2)
        ),
        # ylim = c(0, y_max_value_current * 1.2  ),
        # xlab = "Time",
        xlab = "",
        ylab = "Counts",
        xaxt = "n",
        yaxt = "n",
        cex = 3,
        cex.axis = 1.55,
        cex.lab = 2,
        cex.main = 1.45,
        cex.sub = 2
    )
    lines(
        x = as.integer(dates),
        y = prophet_predictions$yhat,
        col = "#005BBB",
        # col = "#00bb61",
        # col = color_01,
        lwd = 15,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
    )
    lines(
        x = as.integer(dates)[range_five_last4],
        y = prophet_predictions$yhat[range_five_last4],
        # col = "#005BBB",
        col = "#FFD500",
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l"
    )
    lines(
        x = as.integer(dates)[range_five_last4],
        y = prophet_predictions$yhat[range_five_last4],
        # col = "#005BBB",
        col = "#FFD500",
        # col = color_01,
        lwd = 15,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
    )
    lines(
        x = as.integer(dates)[range_five],
        y = mortality_causes_data[range_five, i],
        # col = "#005BBB",
        col = "#00bb61",
        # col = color_01,
        lwd = 5,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "l"
    )
    lines(
        x = as.integer(dates)[range_five],
        y = mortality_causes_data[range_five, i],
        # col = "#005BBB",
        col = "#00bb61",
        # col = color_01,
        lwd = 15,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
    )

    lines(
        x = rep(pandemic_start, 10),
        y = c(rep(min(value_combine), 5), rep(max(value_combine), 5)),
        col = "red",
        lwd = 1,
        lty = 2
    )

    legend(
        x = params$position2,
        inset = c(params$legend2_x, params$legend2_y),
        legend = c("Fitted Trend", "Predicted Trend", "Actual Data", "Epidemic Start"),
        col = "black",
        fill = c("#005BBB", "#FFD500", "#00bb61", "red"),
        pt.cex = c(4, 2),
        # pch = c(19, 20),
        # pch = c(19, 20),
        # pch = c(19, 20),
        cex = params$cex2
    )
    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
    # Creating labels by month and converting.


    # X-axis
    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
    # Creating labels by month and converting.
    initial_date <- min(as.integer(dates)[range_five])
    final_date <- max(as.integer(dates)[range_five])
    number_of_dates <- length(as.integer(dates)[range_five])


    # Indexes to display
    # x_indexes_to_display <-  seq( from  =  1, to  = length(p_scores_frame_five_jan_june$Month),  by = 1 )
    x_indexes_to_display <- dates[range_five]
    # x_indexes_to_display[1] <- 1
    # Actual lab elements
    x_tlab <- x_indexes_to_display
    # ctual lab labels
    # x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
    x_lablist <- strftime(dates, format = "%Y-%m")
    axis(1, at = x_tlab, labels = FALSE)
    text(x = x_tlab, y = par()$usr[3] - 0.03 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 0.9)


    # Y-axis
    # Adding axis label
    # labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
    y_min_value <- min(value_combine)
    y_max_value <- max(value_combine)
    y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
    y_lablist <- as.character(round(y_tlab, digits = 0))
    axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)


    # Label B
    par(xpd = NA)

    di <- dev.size("in")
    x <- grconvertX(c(0, di[1]), from = "in", to = "user")
    y <- grconvertY(c(0, di[2]), from = "in", to = "user")

    fig <- par("fig")
    x <- x[1] + (x[2] - x[1]) * fig[1:2]
    y <- y[1] + (y[2] - y[1]) * fig[3:4]

    txt <- "B"
    x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
    y <- y[2] - strheight(txt, cex = 4) * 6 / 5
    text(x, y, txt, cex = 4)

    dev.off()
}

# Saving the data as RData file.
save(prophet_pscores, file = paste("../R_Data/prophet_pscores.RData"))
save(prophet_pscores_lower, file = paste("../R_Data/prophet_pscores_lower.RData"))
save(prophet_pscores_upper, file = paste("../R_Data/prophet_pscores_upper.RData"))