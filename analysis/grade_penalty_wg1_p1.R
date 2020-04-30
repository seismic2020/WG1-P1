#
#PURPOSE: Consider one course at a time, one term. This returns various demographic
#         breakdowns and returns plots that show differences in outcomes for single
#         demographic variables: ethnicity, gender, lowinc, and first gen.
#INPUT: see https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021
#       sc - table of COURSE LEVEL VARIABLES 
#       sr - table of STUDENT LEVEL VARIABLES 
#       COURSE - the crs_name for the course to be studied
#       TERM   - the crs_term for the course
#       model  - a formula object that contains the regression model used by the regression models.
#       nohist - plot a grade-gpao histogram with each grade penalty plot. Default is TRUE, i.e. don't print one.
#       tau    - a tunable parameter for the quantile regression, set to 0.5 (the median) by default.
#       aggregate_terms - set to TRUE if you want to disregard term and aggregate over all.
#OUTPUT: 1) This prints four plots the plot device (usually the plot view in RStudio)
#        2) This returns a list, a variable containing 3 tables
#        element 1: demographic statistics for the course by ethnicity, gender, LI, FG.
#        element 2: grade penalty statistics by the Molinaro classification
#        element 3: grade penalty statistics by the Fiorini classification
#        element 4: quantile regression coefficients and standard errors
#        element 5: linear regression coefficients, standard errors, t-tests, p-vals
#DEPENDENCIES: Must have tidyverse installed, must source the 'grade_penalty_functions.R'
#              > source(str_c(<PATH_TO_YOUR_CODE>,'/grade_penalty_functions.R'))
#
#EXAMPLE:
# 1) Get summary statistics back for a course, as well as regressions for a basic model
#> kk <- grade_penalty_wg1_p1(sr,sc,COURSE='PHYSICS 140',TERM='FA 2012')
#> print(kk[[1]])
#> print(kk[[2]])
#> print(kk[[3]])
#> print(kk[[4]])
#> print(kk[[5]])
#> 
# 2) Get summary statistics back for course, and run a regression using Molinaro categories:
#> kk <- grade_penalty_wg1_p1(data[[1]],data[[2]] %>% drop_na(gpao,numgrade),
#                             COURSE='PHYSICS 140',TERM='FA 2012',model=as.formula(numgrade ~ gpao+opp))
#> print(kk[[4]])
#> print(kk[[5]])
#>
# Change log: 
# 18-Apr-2020: First commit
# 29-Apr-2020: - Recoded gender --> female
#              - added linear regression (GLM) and quantile regression (RQ) outputs
#              - added explcit NA handling.
##############################
grade_penalty_wg1_p1 <- function(sr,sc,COURSE='PHYSICS 140',TERM='FA 2012',
                                 tau=0.5,model = as.formula(numgrade ~ gpao),
                                 nohist=TRUE,aggregate_terms=FALSE)
{
  require(tidyverse) #data handling
  require(quantreg)  #quantile regression
  #require(lme4)     #in case we do hierarchical MLE regression (e.g. within terms)
  
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
  aa <- make_eth_grade_gpao_plot(sc,nohist=nohist)
  aa <- make_female_grade_gpao_plot(sc,nohist=nohist)
  aa <- make_firstgen_grade_gpao_plot(sc,nohist=nohist)
  aa <- make_lowinc_grade_gpao_plot(sc,nohist=nohist)
  
  #add the fiorini and molinaro coding for regression
  sc <- add_fiorini_coding(sc)
  sc <- add_molinaro_coding(sc)
  
  #run regressions...in the future we will put these functions behind the scenes.
  jj  <- rq(model,tau=tau,sc,na.action=na.exclude)  #quantile regression
  jj2 <- glm(model,data=sc,na.action=na.exclude)    #straight up linear regression
  mtx_rq  <- summary(jj)[3][[1]] #pull the coeffiencts for the quantile reg
  mtx_glm <- coef(summary(jj2))  #...and for the glm
  
  #return the summary statistics
  return(list(ds,ms,fs,mtx_rq,mtx_glm))
  #return(jj3)
}



