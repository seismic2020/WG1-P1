######
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
#       noplots - TRUE by default, will not output simple regression plots.
#OUTPUT: 1) This prints four plots the plot device (usually the plot view in RStudio)
#        2) This returns a list, a variable containing 3 tables
#        element 1: demographic statistics for the course by ethnicity, gender, LI, FG.
#        element 2: grade penalty statistics by the mutually exclusive classification
#        element 3: grade penalty statistics by the non mututallyl......, exclusive classification
#        element 4: quantile regression coefficients and standard errors
#        element 5: linear regression coefficients, standard errors, t-tests, p-vals
#        element 6: measure of 4-dimensional course diversity 
#DEPENDENCIES: Must source the 'grade_penalty_functions.R'
#              Must have tidyverse installed
#              Must have quantreg installed
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
#> print(kk[[6]])
# 2) Get summary statistics back for course, and run a regression using Molinaro categories:
#> kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
#                             COURSE='PHYSICS 140',TERM='FA 2012',model=as.formula(numgrade ~ gpao+opp))
#> print(kk[[4]])
#> print(kk[[5]])
#>
# Change log: 
# 18-Apr-2020: First commit
# 29-Apr-2020: - Recoded gender --> female
#              - added linear regression (GLM) and quantile regression (RQ) outputs
#              - added explcit NA handling.
# 28-May-2020: Added 4 dimensionas to analysis, renamed statistical outputs to
#              'mutually exclusive' and 'non-mutually' exclusive.
# 27-July-2020: Added an explicit cut to remove international and transfer students from the grade 
#               penalty and regression. Note the that they are still included in the first table.
# 06-Nov-2020: Added in 'noplots' which silences the ouput regression plots.
# 3-Mar-2021:  Added DFW rates by group and also courwe diversity from Simpson's index.
##############################
grade_penalty_wg1_p1 <- function(sr,sc,COURSE='PHYSICS 140',TERM='FA 2012',
                                 tau=0.5,model = as.formula(numgrade ~ gpao),
                                 nohist=TRUE,aggregate_terms=FALSE,noplots=TRUE)
{
  require(tidyverse) #data handling
  require(quantreg)  #quantile regression
  #require(lme4)     #in case we do hierarchical MLE regression (e.g. within terms)
	
### My data is not split into the sc/sr division, so this should be applied to sc instead
  sc <- add_ME_coding(sc) #Add Mmututally exclusive columns to the data set
  sc <- add_nonME_coding(sc)  #Add non-mututally exclusive columns to the data set  
  
  # sr <- add_ME_coding(sr) #Add Mmututally exclusive columns to the data set
  # sr <- add_nonME_coding(sr)  #Add non-mututally exclusive columns to the data set
  
  if (aggregate_terms == FALSE){sc <- sc %>% filter(crs_name == COURSE & crs_term == TERM)}  
  if (aggregate_terms == TRUE) {sc <- sc %>% filter(crs_name == COURSE)}  
  
### My data is not split into the sc/sr division, so this should be applied to sc instead
  # sc <- sc %>% left_join(sr,by='st_id')
  
  #compute N's by single demographic categories
  ds <- demographic_summary(sc)
  
  #Now omit international students and transfers
  sc <- sc %>% filter(international == 0 & transfer == 0)
  
  #compute simple grade penalty (grade-gpao) statistics for 
  #the different coding schemes.
  ms <- summarize_ME_statistics(sc)
  fs <- summarize_nonME_statistics(sc)
  
  #make one of each plot by the single categories
  if (noplots == FALSE)
  {
    aa <- make_eth_grade_gpao_plot(sc,nohist=nohist)
    aa <- make_female_grade_gpao_plot(sc,nohist=nohist)
    aa <- make_firstgen_grade_gpao_plot(sc,nohist=nohist)
    aa <- make_lowinc_grade_gpao_plot(sc,nohist=nohist)
  }

  #add the fiorini and molinaro coding for regression
  sc <- add_nonME_coding(sc)
  sc <- add_ME_coding(sc)
  
  #finally, add an actual grade penalty column to be tidy
  sc <- sc %>% mutate(grade_penalty=numgrade-gpao)
  
  #run regressions...in the future we will put these functions behind the scenes.
  jj  <- 1#rq(model,tau=tau,sc,na.action=na.exclude)  #quantile regression
  jj2 <- glm(model,data=sc)    #straight up linear regression
  mtx_rq  <- summary(jj)[3][[1]] #pull the coeffiencts for the quantile reg
  mtx_glm <- coef(summary(jj2))  #...and for the glm
  
  #this isn't quite ready yet and it's far backburner
  #boot_glm <- boot_regression(model,sc)
  
  #run the t-test of sai=[0,3] vs sai = 4.
  #this runs Welch's two-sample t-test
  #https://en.wikipedia.org/wiki/Welch%27s_t-test
  
  sc <- sc %>% mutate(binary_sai=ifelse(sai==4,'HI','LO'))
  ttest <- t.test(grade_penalty ~ binary_sai,data=sc)
  
  #course diversity.
  SIMP_DIV <- compute_diversity(sc)
  
  #return the summary statistics
  
  return(list(ds,ms,ttest,mtx_glm))
  #return(jj3)
}



