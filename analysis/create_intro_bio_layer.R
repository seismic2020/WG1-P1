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
  
    
  kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                             COURSE='INTRO',INTRO,TERM='FA 2010',
                             model=as.formula(numgrade ~ ui_firstgen+ui_female+ui_urm+ui_li+
                                                ui_fem_urm+ui_fg_urm+ui_li_urm+ui_none),nohist=TRUE,
                             aggregate=TRUE) #<-- this aggregates all terms!!
  
  #ui_fg_fem+ui_fg_li+ui_fem_li
  #ui_fg_fem_li+ui_fg_fem_urm+ui_fg_urm_li+ui_urm_li_fem+ui_quad
  
  return(kk)
  
}