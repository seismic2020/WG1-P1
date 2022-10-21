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

# Update to the folder where output files will be written

outputFilePath <- "~/Documents/GitHub/SEISMIC/analysis_PNAS_V2/output_MSU"
outputFileName <- paste(outputFilePath,".csv",sep="")
outputFileName_txt <- paste(outputFilePath,".txt",sep="")



# Update to your institution 
institution <- 'MSU'

# Update to match your upper and lower dates that correspond to 
# academic timing of: Summer 09 - Spring 2019
crse_termcd_lower_limit <- 9
crse_termcd_upper_limit <- 19

# -------------------------- Running Code and calling wrapper function -------------------------- 

# No update here
currentDate <- Sys.Date()

# We want the top 10 courses
top10 <- TRUE

# Reading in your data
sr <- read.csv(srFileName, header = TRUE)
sc <- read.csv(scFileName, header = TRUE)

# If you have DFW in your data set, then comment out this line
sc <- mutate(sc, is_dfw = 0)

output <- run_stem_summer_2022_pnas_v2(sr,sc, crse_termcd_lower_limit, crse_termcd_upper_limit,top10, outputFilePath)

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
cat('Regression, grade penalty ~ sai')
cat('\n')
write.csv(output[[4]])
cat('\n')
cat('____________________________')

cat('\n')
cat('\n')

cat('Top 10 courses')
cat('\n')
write.csv(output[[5]])
cat('\n')
cat('____________________________')

# Close the sink
sink()


# Writing out the t-test
chars <- capture.output(print(output[[3]]))
writeLines(chars,outputFileName_txt )


