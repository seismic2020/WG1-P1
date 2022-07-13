# Wrapper Code for Summer 2022 IJSE Paper Submission
# Last Modified: 07/13/22
# If you have any questions email Ben or Sarah


library(dplyr)

# -------------------------- MODIFY THESE -------------------------- 
# These scripts will need to be sourced wherever they are on your machine.  
source("~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/grade_penalty_wg1_p1.R")
source("~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/grade_penalty_functions.R")
source("~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/run_stem_summer_2022_pnas_v2.R")

# These paths will need to be updated to include where your sr and sc file are located
srFileName <- "~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/data/sr.csv"
scFileName <- "~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/data/sc.csv"

# Update to the folder where it will write
outputFileName <- "~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/output_MSU.csv"

# Update to your institution 
institution <- 'MSU'

# Update to match your upper and lower dates that correspond to 
# academic timing of: Summer 09 - Spring 2019
crse_termcd_lower_limit <- 9
crse_termcd_upper_limit <- 19

# -------------------------- Running Code and calling wrapper function -------------------------- 

# No update here
currentDate <- Sys.Date()

# Reading in your data
sr <- read.csv(srFileName, header = TRUE)
sc <- read.csv(scFileName, header = TRUE)

# If you have DFW in your data set, then comment out this line
sc <- mutate(sc, is_dfw = 0)

output <- run_stem_summer_2022_pnas_v2(sr,sc, crse_termcd_lower_limit, crse_termcd_upper_limit)

# -------------------- Output -------------------- 

# Start a sink file with a CSV extension
sink(outputFileName)

cat(institution)
cat('\n')
cat(paste('Data Ran on: ', currentDate))
cat('\n')

cat('Overall Demographics')
cat('\n')
write.csv(output[[1]])
cat('\n')
cat('____________________________')

cat('\n')
cat('\n')

cat('Grade Anomalies and SAI')
cat('\n')
write.csv(output[[2]])
cat('\n')
cat('____________________________')

cat('\n')
cat('\n')

cat('T-test')
cat('\n')
capture.output(print(output[[3]]))
cat('\n')
cat('____________________________')

cat('\n')
cat('\n')
cat('Regression, grade penalty ~ sai')
cat('\n')
write.csv(output[[4]])
cat('\n')
cat('____________________________')

cat('\n')
cat('\n')
# Close the sink
sink()


