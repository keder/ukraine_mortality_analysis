# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.24. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)

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



# Setting the correct working directory.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Belarus Mortality Analysis"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()


# Fix 2021.04.27
# Reeading individual pdf files
year2012_demographics_path <- "Data/2012/35ad81e767560dbf217372ea19d83ac1/Demographic_Yearbook_2012_page57.xlsx"

# Reading data
year2012_demographics <- read_excel( path = year2012_demographics_path )
year2012_demographics_matrix_raw <- as.matrix( year2012_demographics )
colnames(year2012_demographics_matrix_raw) <- NULL
rownames(year2012_demographics_matrix_raw) <- NULL
year2012_demographics_matrix_raw
year2012_demographics_matrix <- as.matrix( year2012_demographics_matrix_raw )[ -c(1:7, 24:29), c(1, 2, 5)]
# Summaries
year2012_demographics_matrix[1,  1]  <- "AllAges"
year2012_demographics_matrix[16, 1] <- "70Up"


year2012_demographics_frame <- data.frame(year2012_demographics_matrix)
names(year2012_demographics_frame) <-  c("Age_Group", "Year2011", "Year2012")

year2012_demographics_frame$Age_Group <- as.character(year2012_demographics_frame$Age_Group)
year2012_demographics_frame$Year2011  <- gsub(" ", "", x = as.character(year2012_demographics_frame$Year2011), fixed = TRUE)
year2012_demographics_frame$Year2012  <- gsub(" ", "", x = as.character(year2012_demographics_frame$Year2012), fixed = TRUE)

# Saving the data as RData file.
save( year2012_demographics_frame, file = paste("R_Data/year2012_demographics_frame.RData") )





# Fix 2021.04.27
# Reeading individual pdf files
year2013_demographics_path <- "Data/2013/448292381f50f3439c0b6364238a29a2/Demographic_Yearbook_2013_page47.xlsx"

# Reading data
year2013_demographics <- read_excel( path = year2013_demographics_path )
year2013_demographics_matrix_raw <- as.matrix( year2013_demographics )
colnames(year2013_demographics_matrix_raw) <- NULL
rownames(year2013_demographics_matrix_raw) <- NULL
year2013_demographics_matrix_raw
year2013_demographics_matrix <- as.matrix( year2013_demographics_matrix_raw )[ -c(1:4, 21:26), c(1, 2, 5) ]
# Fixing column 1
year2013_demographics_matrix[,1] <- year2012_demographics_matrix[,1]
# Summaries

year2013_demographics_frame <- data.frame(year2013_demographics_matrix)
names(year2013_demographics_frame) <-  c("Age_Group", "Year2012", "Year2013")

year2013_demographics_frame$Age_Group <- as.character(year2013_demographics_frame$Age_Group)
year2013_demographics_frame$Year2012 <- gsub(" ", "", x = as.character(year2013_demographics_frame$Year2012), fixed = TRUE)
year2013_demographics_frame$Year2013 <- gsub(" ", "", x = as.character(year2013_demographics_frame$Year2013), fixed = TRUE)

# Saving the data as RData file.
save( year2013_demographics_frame, file = paste("R_Data/year2013_demographics_frame.RData") )




# Fix 2021.04.27
# Reeading individual pdf files
year2014_demographics_path <- "Data/2014/449f0f39ec980dc07144d90277b342a6/Demographic_Yearbook_2014_page47.xlsx"

# Reading data
year2014_demographics <- read_excel( path = year2014_demographics_path )
year2014_demographics_matrix_raw <- as.matrix( year2014_demographics )
colnames(year2014_demographics_matrix_raw) <- NULL
rownames(year2014_demographics_matrix_raw) <- NULL
year2014_demographics_matrix_raw
year2014_demographics_matrix <- as.matrix( year2014_demographics_matrix_raw )[ -c(1:4, 21:26), c(1, 2, 5) ]
# Fixing column 1
year2014_demographics_matrix[,1] <- year2012_demographics_matrix[,1]


year2014_demographics_frame <- data.frame(year2014_demographics_matrix)
names(year2014_demographics_frame) <-  c("Age_Group", "Year2013", "Year2014")

year2014_demographics_frame$Age_Group <- as.character(year2014_demographics_frame$Age_Group)
year2014_demographics_frame$Year2013 <- gsub(" ", "", x = as.character(year2014_demographics_frame$Year2013), fixed = TRUE)
year2014_demographics_frame$Year2014 <- gsub(" ", "", x = as.character(year2014_demographics_frame$Year2014), fixed = TRUE)

# Saving the data as RData file.
save( year2014_demographics_frame, file = paste("R_Data/year2014_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2015_demographics_path <- "Data/2015/7ffceadabefa4a931dba89911fc49146/w_dem_ejegodnik_2015/Demographic_Yearbook_2015_page54.xlsx"

# Reading data
year2015_demographics <- read_excel( path = year2015_demographics_path )
year2015_demographics_matrix_raw <- as.matrix( year2015_demographics )
colnames(year2015_demographics_matrix_raw) <- NULL
rownames(year2015_demographics_matrix_raw) <- NULL
year2015_demographics_matrix_raw
year2015_demographics_matrix <- as.matrix( year2015_demographics_matrix_raw )[ -c(1:5, 7:10, 26:45), c(1, 2, 5) ]
# Fixing column 1
year2015_demographics_matrix[1,  1]  <- "AllAges"
year2015_demographics_matrix[16, 1] <- "70Up"


year2015_demographics_frame <- data.frame(year2015_demographics_matrix)
names(year2015_demographics_frame) <-  c("Age_Group", "Year2014", "Year2015")

year2015_demographics_frame$Age_Group <- as.character(year2015_demographics_frame$Age_Group)
year2015_demographics_frame$Year2014 <- gsub(" ", "", x = as.character(year2015_demographics_frame$Year2014), fixed = TRUE)
year2015_demographics_frame$Year2015 <- gsub(" ", "", x = as.character(year2015_demographics_frame$Year2015), fixed = TRUE)

# Saving the data as RData file.
save( year2015_demographics_frame, file = paste("R_Data/year2015_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2016_demographics_path <- "Data/2016/8beb43d9440e5850ec016a5d2ef16f54/Demographic_Yearbook_2016_page53.xlsx"

# Reading data
year2016_demographics <- read_excel( path = year2016_demographics_path )
year2016_demographics_matrix_raw <- as.matrix( year2016_demographics )
colnames(year2016_demographics_matrix_raw) <- NULL
rownames(year2016_demographics_matrix_raw) <- NULL
year2016_demographics_matrix_raw
year2016_demographics_matrix <- as.matrix( year2016_demographics_matrix_raw )[ -c(1:5, 7:10, 26:45), c(1, 2, 5) ]
# Fixing column 1
year2016_demographics_matrix[1,  1]  <- "AllAges"
year2016_demographics_matrix[16, 1] <- "70Up"


year2016_demographics_frame <- data.frame(year2016_demographics_matrix)
names(year2016_demographics_frame) <-  c("Age_Group", "Year2015", "Year2016")

year2016_demographics_frame$Age_Group <- as.character(year2016_demographics_frame$Age_Group)
year2016_demographics_frame$Year2015 <- gsub(" ", "", x = as.character(year2016_demographics_frame$Year2015), fixed = TRUE)
year2016_demographics_frame$Year2016 <- gsub(" ", "", x = as.character(year2016_demographics_frame$Year2016), fixed = TRUE)

# Saving the data as RData file.
save( year2016_demographics_frame, file = paste("R_Data/year2016_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2017_demographics_path <- "Data/2017/230ec287e4883e0e6fd07ed20f71940e/Demographic_Yearbook_2017_page52.xlsx"

# Reading data
year2017_demographics <- read_excel( path = year2017_demographics_path )
year2017_demographics_matrix_raw <- as.matrix( year2017_demographics )
colnames(year2017_demographics_matrix_raw) <- NULL
rownames(year2017_demographics_matrix_raw) <- NULL
year2017_demographics_matrix_raw
year2017_demographics_matrix <- as.matrix( year2017_demographics_matrix_raw )[ -c(1:13, 15:18, 34:54), c(1, 2, 5) ]
# Fixing column 1
year2017_demographics_matrix[1,  1]  <- "AllAges"
year2017_demographics_matrix[16, 1] <- "70Up"


year2017_demographics_frame <- data.frame(year2017_demographics_matrix)
names(year2017_demographics_frame) <-  c("Age_Group", "Year2016", "Year2017")

year2017_demographics_frame$Age_Group <- as.character(year2017_demographics_frame$Age_Group)
year2017_demographics_frame$Year2016 <- gsub(" ", "", x = as.character(year2017_demographics_frame$Year2016), fixed = TRUE)
year2017_demographics_frame$Year2017 <- gsub(" ", "", x = as.character(year2017_demographics_frame$Year2017), fixed = TRUE)

# Saving the data as RData file.
save( year2017_demographics_frame, file = paste("R_Data/year2017_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2018_demographics_path <- "Data/2018/5e09f40d5f5306386bd515f90aa1b86c/Demographic_Yearbook_2018_page54.xlsx"

# Reading data
year2018_demographics <- read_excel( path = year2018_demographics_path )
year2018_demographics_matrix_raw <- as.matrix( year2018_demographics )
colnames(year2018_demographics_matrix_raw) <- NULL
rownames(year2018_demographics_matrix_raw) <- NULL
year2018_demographics_matrix_raw
year2018_demographics_matrix <- as.matrix( year2018_demographics_matrix_raw )[ -c(1:13, 15:18, 34:46), c(1, 2, 5) ]
# Fixing column 1
year2018_demographics_matrix[1,  1]  <- "AllAges"
year2018_demographics_matrix[16, 1] <- "70Up"


year2018_demographics_frame <- data.frame(year2018_demographics_matrix)
names(year2018_demographics_frame) <-  c("Age_Group", "Year2017", "Year2018")

year2018_demographics_frame$Age_Group <- as.character(year2018_demographics_frame$Age_Group)
year2018_demographics_frame$Year2017 <- gsub(" ", "", x = as.character(year2018_demographics_frame$Year2017), fixed = TRUE)
year2018_demographics_frame$Year2018 <- gsub(" ", "", x = as.character(year2018_demographics_frame$Year2018), fixed = TRUE)

# Saving the data as RData file.
save( year2018_demographics_frame, file = paste("R_Data/year2018_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2018_demographics_path <- "Data/2018/5e09f40d5f5306386bd515f90aa1b86c/Demographic_Yearbook_2018_page54.xlsx"

# Reading data
year2018_demographics <- read_excel( path = year2018_demographics_path )
year2018_demographics_matrix_raw <- as.matrix( year2018_demographics )
colnames(year2018_demographics_matrix_raw) <- NULL
rownames(year2018_demographics_matrix_raw) <- NULL
year2018_demographics_matrix_raw
year2018_demographics_matrix <- as.matrix( year2018_demographics_matrix_raw )[ -c(1:13, 15:18, 34:46), c(1, 2, 5) ]
# Fixing column 1
year2018_demographics_matrix[1,  1]  <- "AllAges"
year2018_demographics_matrix[16, 1] <- "70Up"


year2018_demographics_frame <- data.frame(year2018_demographics_matrix)
names(year2018_demographics_frame) <-  c("Age_Group", "Year2017", "Year2018")

year2018_demographics_frame$Age_Group <- as.character(year2018_demographics_frame$Age_Group)
year2018_demographics_frame$Year2017 <- gsub(" ", "", x = as.character(year2018_demographics_frame$Year2017), fixed = TRUE)
year2018_demographics_frame$Year2018 <- gsub(" ", "", x = as.character(year2018_demographics_frame$Year2018), fixed = TRUE)

# Saving the data as RData file.
save( year2018_demographics_frame, file = paste("R_Data/year2018_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2019_demographics_path <- "Data/2019/Demographic_Yearbook_2019_page52.xlsx"

# Reading data
year2019_demographics <- read_excel( path = year2019_demographics_path )
year2019_demographics_matrix_raw <- as.matrix( year2019_demographics )
colnames(year2019_demographics_matrix_raw) <- NULL
rownames(year2019_demographics_matrix_raw) <- NULL
year2019_demographics_matrix_raw
year2019_demographics_matrix <- as.matrix( year2019_demographics_matrix_raw )[ -c(1:13, 15:18, 34:46), c(1, 2, 5) ]
# Fixing column 1
year2019_demographics_matrix[1,  1]  <- "AllAges"
year2019_demographics_matrix[16, 1] <- "70Up"


year2019_demographics_frame <- data.frame(year2019_demographics_matrix)
names(year2019_demographics_frame) <-  c("Age_Group", "Year2018", "Year2019")

year2019_demographics_frame$Age_Group <- gsub(" ", "", x = as.character(year2019_demographics_frame$Age_Group), fixed = TRUE)
year2019_demographics_frame$Year2018  <- gsub(" ", "", x = as.character(year2019_demographics_frame$Year2018), fixed = TRUE)
year2019_demographics_frame$Year2019  <- gsub(" ", "", x = as.character(year2019_demographics_frame$Year2019), fixed = TRUE)

# Saving the data as RData file.
save( year2019_demographics_frame, file = paste("R_Data/year2019_demographics_frame.RData") )




# Fix 2021.04.27.
# Reeading individual pdf files
year2020_demographics_path <- "Data/2020/Demographic_Census_oct2019_page13.xlsx"

# Reading data
year2020_demographics <- read_excel( path = year2020_demographics_path )
year2020_demographics_matrix_raw <- as.matrix( year2020_demographics )
colnames(year2020_demographics_matrix_raw) <- NULL
rownames(year2020_demographics_matrix_raw) <- NULL
year2020_demographics_matrix_raw
year2020_demographics_matrix_detailed <- as.matrix( year2020_demographics_matrix_raw )[ -c(1:2, 4:5, 2), c(2, 3) ]
year2020_demographics_matrix <- year2020_demographics_matrix_detailed[1:16,]
year2020_demographics_matrix[16,] <-   sum( as.numeric( gsub(" ", "", x = as.character(year2020_demographics_matrix_detailed[c(16:18),2] ), fixed = TRUE)) )
# Fixing column 1
year2020_demographics_matrix[1,  1]  <- "AllAges"
year2020_demographics_matrix[16, 1] <- "70Up"


year2020_demographics_frame <- data.frame(year2020_demographics_matrix)
names(year2020_demographics_frame) <-  c("Age_Group", "Year2020")

year2020_demographics_frame$Age_Group <- as.character(year2020_demographics_frame$Age_Group)
year2020_demographics_frame$Age_Group <- gsub(" ", "", x = as.character(year2020_demographics_frame$Age_Group), fixed = TRUE)
year2020_demographics_frame$Year2020  <- gsub(" ", "", x = as.character(year2020_demographics_frame$Year2020), fixed = TRUE)

# Saving the data as RData file.
save( year2020_demographics_frame, file = paste("R_Data/year2020_demographics_frame.RData") )




# Combinigng the results
raw_combined <- cbind( year2012_demographics_frame,
                       year2013_demographics_frame,
                       year2014_demographics_frame,
                       year2015_demographics_frame,
                       year2016_demographics_frame,
                       year2017_demographics_frame,
                       year2018_demographics_frame,
                       year2019_demographics_frame,
                       year2020_demographics_frame )

# Raw data frame 
frame_raw <- data.frame(raw_combined )
# Extra checks
sum( !frame_raw$Year2012 == frame_raw$Year2012.1 )
sum( !frame_raw$Year2013 == frame_raw$Year2013.1 )
sum( !frame_raw$Year2014 == frame_raw$Year2014.1 )
sum( !frame_raw$Year2015 == frame_raw$Year2015.1 )
sum( !frame_raw$Year2016 == frame_raw$Year2016.1 )
sum( !frame_raw$Year2017 == frame_raw$Year2017.1 )
sum( !frame_raw$Year2018 == frame_raw$Year2018.1 )

# Extra checks
sum( !frame_raw$Age_Group == frame_raw$Age_Group.1 )
sum( !frame_raw$Age_Group == frame_raw$Age_Group.2 )
sum( !frame_raw$Age_Group == frame_raw$Age_Group.3 )
sum( !frame_raw$Age_Group == frame_raw$Age_Group.4 )
sum( !frame_raw$Age_Group == frame_raw$Age_Group.5 )
sum( !frame_raw$Age_Group == frame_raw$Age_Group.6 )
# Those two have encoding differences
# Checked manually
sum( !frame_raw$Age_Group == frame_raw$Age_Group.7 )
sum( !frame_raw$Age_Group == frame_raw$Age_Group.8 )
frame_raw$Age_Group
frame_raw$Age_Group.7
frame_raw$Age_Group.8

# Variables/columns to drop
list_to_drop <- c( "Year2012.1", "Year2013.1","Year2014.1","Year2015.1","Year2016.1","Year2017.1","Year2018.1",
                   "Age_Group.1","Age_Group.2","Age_Group.3","Age_Group.4","Age_Group.5","Age_Group.6","Age_Group.7","Age_Group.8")


# Final frame for regression
demographics_aggregated_2011_2020 <- frame_raw[, -which( names(frame_raw) %in% list_to_drop ) ]
print(demographics_aggregated_2011_2020)

# Saving the data as RData file.
save( demographics_aggregated_2011_2020, file = paste("R_Data/demographics_aggregated_2011_2020.RData") )















