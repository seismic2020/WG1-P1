#This runs as a wrapper, calling grade_penalty_wg1_p1.R
#
#It takes the same two tables as inputs.
#This selects a specific course that each institution needs to specify. 
#. 
#
#INPUTS:
#sr, sc: the student course and student record tables
#crse_termcd_limit: The lower limit for TERM_CD of the cohort to include. 
#                   It includes this term and all those after.
##################################
create_intro_bio_layer <- function(sr,sc,crse_termcd_limit=1760,BIOCOURSE='BIOLOGY 171')
{

  #select students in their first Biology class
  sc <- sc %>% filter(crs_termcd >= crse_termcd_limit & crs_name %in% BIOCOURSE)
  sc <- sc %>% mutate(crs_name='INTRO')
  
  #just the SAI
  kk1 <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                             COURSE='INTRO',INTRO,TERM='FA 2010',
                             model=as.formula(numgrade ~ opp_count),nohist=TRUE,aggregate=TRUE)
  ##then the SAI + GPAO
  kk2 <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                             COURSE='INTRO',INTRO,TERM='FA 2010',
                             model=as.formula(numgrade ~ opp_count+gpao),nohist=TRUE,aggregate=TRUE)
  ## then SAI+GPAO+HSGPA+ACT_MATH
  kk3 <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                             COURSE='INTRO',INTRO,TERM='FA 2010',
                             model=as.formula(numgrade ~ opp_count+gpao+mathsr+hsgpa),nohist=TRUE,aggregate=TRUE)
  
   out <- list(kk1[[2]],kk1[[5]],kk2[[5]],kk3[[5]])                          
  
  return(out)
  
}

run_courses_and_plot <- function(data)
{
  clist <- c('BIOLOGY 171','BIOLOGY 172','BIOLOGY 173')
  
  for (i in 1:3)
  {
    print('*********')
    print(clist[i])
    kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=clist[i])
    p <- kk[[1]] %>% filter(opp %in% c('0','1','2','3')) %>% mutate(se=sd_grade/sqrt(N)) %>% 
              ggplot(aes(x=opp,y=mean_grade,size=N))+geom_point()+
              geom_errorbar(aes(ymin=mean_grade-se,ymax=mean_grade+se,size=1))+ggtitle(clist[i])+ylim(2,4)
    print(p)
    print(kk[[2]])
    print(kk[[3]])
    print(kk[[4]])
    
    
  }
  
  
}