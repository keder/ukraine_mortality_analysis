
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

load(file = "../R_Data/mortality_causes_data.RData")

pandemic_start <- as.Date("2020-02-15")

cause_indexes <- 2:(ncol(mortality_causes_data) - 4)
causes <- names(mortality_causes_data)[cause_indexes]

mortality_causes_data$Year <- strftime(mortality_causes_data$date, format = "%Y")
mortality_causes_data$Month <- strftime(mortality_causes_data$date, format = "%B")

mortality_causes_data_2015_2019 <- mortality_causes_data[which(mortality_causes_data$Year < 2020), ]

causes_p_scores_len <- nrow(mortality_causes_data)
causes_p_scores <- mortality_causes_data[, cause_indexes]
cause_averages <- mortality_causes_data[, cause_indexes]

# Computing non-parametrix p-scores
for (i in 1:nrow(mortality_causes_data)) {
    item <- mortality_causes_data[i, ]

    # Getting current month for the past five years
    month_current_frame_2015_2019 <- mortality_causes_data_2015_2019[which(mortality_causes_data_2015_2019$Month == item$Month), ]

    for (cause_index in cause_indexes)
    {
        p_score_i <- cause_index - 1

        value_current <- item[, cause_index]

        # Saving the average value
        mean_current <- mean(month_current_frame_2015_2019[, cause_index])
        cause_averages[i, p_score_i] = mean_current

        # Computing p-scores for non-parametric
        causes_p_scores[which(mortality_causes_data$Month == item$Month & mortality_causes_data$Year == item$Year), p_score_i] <- 100 * (value_current - mean_current) / mean_current
    }
}

plot_pscore_graph <- function(name, y_values, x_values, params) {
    pdf(paste0("../Plots/causes/pscore_", gsub("\\s+", "_", name), ".pdf"), height = 16, width = 20)
    par( par(mfrow=c(2,1)),  mar=c(5.5, 5.1, 5.1, 2.1)  )

    pandemic_data_length <- length(y_values) - which(x_values == pandemic_start)

    barplot(y_values,
        col = c(rep("#005BBB", length(y_values) - pandemic_data_length), rep("#FFD500", pandemic_data_length)),
        legend = TRUE,
        border = TRUE,
        # xlim = c(1, 5),
        ylim = c(min(y_values) - 15, max(y_values) + 15) * (1 + params$padding1),
        args.legend = list(bty = "n", border = TRUE),
        ylab = "",
        xlab = "",
        main = paste0("P-Scores (in Percent) for 2015-2021\nWith 2015-2019 Years Average (", name, ")"),
        # names.arg = as.character(p_scores_frame_five$Month),
        names.arg = as.character(x_values),
        cex.names = 1.2,
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
}



plot_averages_graph <- function(name, values, averages, x_values, params) {
    value_combine = c(values, averages)
    plot(
        x = as.integer(x_values),
        y = averages,
        col = "#005BBB",
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l",
        # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
        main = "Averaged (2015-2019) and 2015-2021 Mortality",
        # xlim = c( intersected_data$death_covid19,  combined_date_max  ),
        ylim = c(
            min(value_combine) * 0.9,
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
        cex.main = 1.55,
        cex.sub = 2
    )
    lines(
        x = as.integer(x_values),
        y = averages,
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
        x = as.integer(x_values),
        y = values,
        # col = "#005BBB",
        col = "#FFD500",
        # col = color_01,
        lwd = 5,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "l"
    )
    lines(
        x = as.integer(x_values),
        y = values,
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
        x = rep(pandemic_start, 10),
        y = c(rep(min(value_combine), 5), rep(max(value_combine), 5)),
        col = "red",
        lwd = 1,
        lty = 2
    )

    legend(
        x = params$position2,
        inset = c(params$legend2_x, params$legend2_y),
        legend = c("Averaged Data", "2015-2021 Data", "Epidemic Start"),
        col = "black",
        fill = c("#005BBB", "#FFD500", "red"),
        pt.cex = c(4, 2),
        # pch = c(19, 20),
        cex = params$cex2
    )
    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
    # Creating labels by month and converting.


    # X-axis
    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/

    # Creating labels by month and converting.
    # Values that define the graph boundaries for x-xis
    initial_date <- as.integer(min(as.integer(x_values)))
    final_date <- as.integer(max(as.integer(x_values)))
    number_of_dates <- length(as.integer(x_values))
    lenght_in_time <- final_date - initial_date


    # Indexes to display on the graph (i.e. coordinates on x axis)
    x_indexes_to_display <- seq(from = x_values[1], to = max(x_values), by = 2 * floor(lenght_in_time / number_of_dates))
    # Actual lab elements as text to be printed.
    x_lablist <- substr(x = x_indexes_to_display, start = 1, stop = 7)
    # Adding dashes to coordinates
    axis(1, at = x_indexes_to_display, labels = FALSE)
    # Adding actual labels
    text(x = x_indexes_to_display, y = par()$usr[3] - 0.025 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.2)


    # Y-axis
    # Adding axis label
    # labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
    y_min_value <- min(value_combine)
    y_max_value <- max(value_combine)
    # Indexes to display on the graph (i.e. coordinates on y axis)
    y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
    # Actual lab elements as text to be printed.
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
plot_params[which(plot_params$name == "aids_caused_by_hiv"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "alcohol_poisoning"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "birth_defects"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "blood_and_marrow_diseases"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "drowning"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "diseases_of_nervous_system"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "infectious_and_parasitic_diseases"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "mental_and_behaviour_disease"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "outer_causes"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "perinatal_conditions"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "suicide"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "traffic_accidents"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "tuberculosis"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "violence"),]$position1 = "bottomleft"
plot_params[which(plot_params$name == "diseases_of_the_musculoskeletal_system"),]$cex1 = 1.6
plot_params[which(plot_params$name == "drowning"),]$cex1 = 1.6
plot_params[which(plot_params$name == "aids_caused_by_hiv"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "birth_defects"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "infectious_and_parasitic_diseases"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "malignant"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "tuberculosis"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "tumors"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "violence"),]$position2 = "bottomleft"
plot_params[which(plot_params$name == "alcohol_poisoning"),]$padding2 = 0.3
plot_params[which(plot_params$name == "alcoholic_cardiomyopathy"),]$padding2 = 0.1
plot_params[which(plot_params$name == "diseases_of_the_genitourinary_system"),]$padding2 = 0.3
plot_params[which(plot_params$name == "diseases_of_the_musculoskeletal_system"),]$padding2 = 0.3
plot_params[which(plot_params$name == "drowning"),]$padding2 = 0.3
plot_params[which(plot_params$name == "outer_causes"),]$padding2 = 0.3
plot_params[which(plot_params$name == "perinatal_conditions"),]$padding2 = 0.3
plot_params[which(plot_params$name == "poisoning_except_alcoholic_"),]$padding2 = 0.3
plot_params[which(plot_params$name == "suicide"),]$padding2 = 0.3
plot_params[which(plot_params$name == "traffic_accidents"),]$padding2 = 0.3

for (i in cause_indexes)
{
    params = plot_params[i - 1,]
    plot_pscore_graph(gsub("\\.+", " ", names(mortality_causes_data)[i]), causes_p_scores[,i-1], mortality_causes_data$date, params)
    plot_averages_graph(gsub("\\.+", " ", names(mortality_causes_data)[i]), mortality_causes_data[,i], cause_averages[,i-1], mortality_causes_data$date, params)
}
