
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

causes_relative_path <- "../data/causes.csv"

mortality_causes_data <- read.table(file = causes_relative_path, sep = ";", header = TRUE)
mortality_causes_data$date = as.Date(paste(mortality_causes_data$date_str, "15", sep="-"))
new_names = gsub("\\.+", "_", names(mortality_causes_data))
names(mortality_causes_data) = new_names
save(mortality_causes_data, file = paste("../../R_Data/mortality_causes_data.RData"))

plot_mortality_graph <- function(name, y_values, x_values) {
    pdf(paste0("../../Plots/causes/", gsub("\\s+", "_", name), ".pdf"), height = 8, width = 20)
    plot(
        x = x_values,
        y = y_values,
        col = "#005BBB",
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l",
        # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
        main = paste0("Monthly Mortality (Total) ", name),
        xlim = c(min(x_values), max(x_values)),
        ylim = c(min(y_values), max(y_values)),
        # ylim = c(0, y_max_value_current * 1.2  ),
        # xlab = "Time",
        xlab = "",
        ylab = "Counts",
        xaxt = "n",
        yaxt = "n",
        cex = 2,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 2,
        cex.sub = 2
    )
    lines(
        x = x_values,
        y = y_values,
        col = "#005BBB",
        # col = "#00bb61",
        # col = color_01,
        lwd = 12,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
    )
    lines(
        x = rep(as.Date("2020-03-15", origin = "1970-01-01"), 2),
        y = c(min(y_values), max(y_values)),
        col = "red",
        lwd = 1,
        lty = 2
    )
    legend(
        x = "topleft",
        inset = c(0.04, 0.04),
        legend = c("Interpolated Records", "Epidemic Start"),
        col = "black",
        fill = c("#005BBB", "red"),
        pt.cex = c(4, 2),
        # pch = c(19, 20),
        cex = 1.5
    )

    # Indexes to display
    x_indexes_to_display <- seq(from = 1, to = length(x_values), by = 1)
    # x_indexes_to_display[1] <- 1
    # Actual lab elements
    x_tlab <- x_values[x_indexes_to_display]
    # ctual lab labels
    x_lablist <- substr(x = as.character(x_values[x_indexes_to_display]), start = 1, stop = 7)
    axis(1, at = x_tlab, labels = FALSE)
    text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 3)


    # Y-axis
    # Adding axis label
    # labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
    y_min_value <- min(y_values)
    y_max_value <- max(y_values)
    y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
    y_lablist <- as.character(round(y_tlab, digits = 0))
    axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

    dev.off()
}

for (i in 2:(ncol(mortality_causes_data)-1))
{
    plot_mortality_graph(gsub("_+", " ", names(mortality_causes_data)[i]), mortality_causes_data[,i], mortality_causes_data$date)
}
