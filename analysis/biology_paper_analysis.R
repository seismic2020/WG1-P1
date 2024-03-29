#This runs as a wrapper, calling grade_penalty_wg1_p1.R
#
#It takes the same two tables as inputs.
#
#The Data Model has been updated to include a flag "is_stem", 
#to indicate that a course is considered STEM by the user at
#their institution. 
#
#INPUTS:
#sr, sc: the student course and student record tables
#crse_termcd_limit: The lower limit for TERM_CD of the cohort to include. 
#                   It includes this term and all those after.
#https://docs.google.com/document/d/18uwBhFWCjt-mItc9Es-0ZcJo7uupOGEpDgjPgFoxjcA/edit
#
##################################
biology_paper_analysis <- function(sr,sc,crse_termcd_limit=1760,top10=TRUE)
{
    #select students in intro Biology
    sc <- sc %>% filter(crs_sbj == 'BIOLOGY' & crs_catalog == 171 &
                        crs_component == 'LEC' & crs_termcd >= crse_termcd_limit)
    
    
    #And go for it.
    #kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
    #                           COURSE='BIOLOGY 171',TERM='FA 2010',
    #                           model=as.formula(numgrade ~ ui_firstgen+ui_female+ui_urm+ui_li+
    #                                            ui_fem_urm+ui_fg_urm+ui_li_urm+ui_none),nohist=TRUE,
    #                           aggregate=TRUE) #<-- this aggregates all terms!!
    #ui_fg_fem+ui_fg_li+ui_fem_li
    #ui_fg_fem_li+ui_fg_fem_urm+ui_fg_urm_li+ui_urm_li_fem+ui_quad
    
    #Question 1.1.2) - grade outcoome vs. advantages
    kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                                COURSE='BIOLOGY 171',TERM='FA 2010',
                                model=as.formula(numgrade ~ opp_count),nohist=TRUE,aggregate=TRUE) 
    
    #Question 1.1.2) - grade outcoome vs. advantages with controls
    kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                               COURSE='BIOLOGY 171',TERM='FA 2010',
                               model=as.formula(numgrade ~ opp_count+hsgpa+gpao),nohist=TRUE,aggregate=TRUE) 
    
    #Question 2.1)
    
    
    
    return(kk)
  
}
