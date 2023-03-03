
rm(list = ls(all = TRUE))

# Library to perform column medians and other useful matrix algebra computations.
library(matrixStats)

# Library for the latex exports in the nice format.
library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
library(methods)

# Loading package requred to read library(readxl)
library(readxl)

# Loading library(rjson) for json files.
library(rjson)

# install.packages("pdftools")
library(pdftools)

# install.packages("tm")
library(tm)


# Libraries to read hml pages
library(XML)
# library(RCurl)
library(rlist)

# Alternative way to read html tables
library(htmltab)


# Fix 2021.04.27
# Reeading individual pdf files
ukraine_demographics_path <- "../data/demographics.csv"

demographics_aggregated_2011_2020_detailed = read.csv(file = ukraine_demographics_path)

# Saving the data as RData file.
save(demographics_aggregated_2011_2020_detailed, file = paste("../../R_Data/demographics_aggregated_2011_2020_detailed.RData"))

demographics_aggregated_2011_2020 = data.frame(t(colSums(demographics_aggregated_2011_2020_detailed[which(demographics_aggregated_2011_2020_detailed$Age_Group < 70),-1])))
ages_70 = c(70:79, "80 and over")
demographics_aggregated_2011_2020 = rbind(demographics_aggregated_2011_2020, colSums(demographics_aggregated_2011_2020_detailed[which(demographics_aggregated_2011_2020_detailed$Age_Group %in% ages_70),-1]))
demographics_aggregated_2011_2020 = rbind(demographics_aggregated_2011_2020, demographics_aggregated_2011_2020_detailed[which(demographics_aggregated_2011_2020_detailed$Age_Group == "AllAges"),-1])
demographics_aggregated_2011_2020$Age_Group = c("65-69", "70Up", "AllAges")

# Saving the data as RData file.
save(demographics_aggregated_2011_2020, file = paste("../../R_Data/demographics_aggregated_2011_2020.RData"))