#
#PURPOSE: Consider one course at a time, one term. This returns various demographic
#         breakdowns and returns plots that show differences in outcomes for single
#         demographic variables: ethnicity, gender, lowinc, and first gen.
#INPUT: see https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021
#       sc - table of COURSE LEVEL VARIABLES 
#       sr - table of STUDENT LEVEL VARIABLES 
#       COURSE - the crs_name for the course to be studied
#       TERM   - the crs_term for the course
#       aggregate_terms - set to TRUE if you want to disregard term and aggregate over all.
#OUTPUT: 1) This prints four plots the plot device (usually the plot view in RStudio)
#        2) This returns a list, a variable containing 3 tables
#        element 1: demographic statistics for the course by ethnicity, gender, LI, FG.
#        element 2: grade penalty statistics by the Molinaro classification
#        element 3: grade penalty statistics by the Fiorini classification
#DEPENDENCIES: Must have tidyverse installed, must source the 'grade_penalty_functions.R'
#              > source(str_c(<PATH_TO_YOUR_CODE>,'/grade_penalty_functions.R'))
#
#EXAMPLE:
#> kk <- grade_penalty_wg1_p1(sr,sc,COURSE='PHYSICS 140',TERM='FA 2012')
#> print(kk[[1]])
#> print(kk[[2]])
#> print(kk[[3]])
##############################
grade_penalty_wg1_p1 <- function(sr,sc,COURSE='PHYSICS 140',TERM='FA 2012',aggregate_terms=FALSE)
{
  library(tidyverse)
  sr <- add_molinaro_coding(sr) #Add Molinaro-type columns to the data set
  sr <- add_fiorini_coding(sr)  #Add Fiorini-type columns to the data set
  
  if (aggregate_terms == FALSE){sc <- sc %>% filter(crs_name == COURSE & crs_term == TERM)}  
  if (aggregate_terms == TRUE) {sc <- sc %>% filter(crs_name == COURSE)}  
  
  sc <- sc %>% left_join(sr,by='st_id')
  
  #compute N's by single demographic categories
  ds <- demographic_summary(sc)
  
  #compute simple grade penalty (grade-gpao) statistics for 
  #the different coding schemes.
  ms <- summarize_molinaro_statistics(sc)
  fs <- summarize_fiorini_statistics(sc)
  
  #make one of each plot by the single categories
  aa <- make_eth_grade_gpao_plot(sc)
  aa <- make_gender_grade_gpao_plot(sc)
  aa <- make_firstgen_grade_gpao_plot(sc)
  aa <- make_lowinc_grade_gpao_plot(sc)
  
  #return the summary statistics
  return(list(ds,ms,fs))
}



