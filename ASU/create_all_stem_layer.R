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
#top10: let it the code try to identify and run on your top 10 courses by 
#       enrollment of these students. WARNING: for reasons unknown the
#       join required is super slow.
##################################
create_all_stem_layer <- function(sr,sc,crse_termcd_limit=2097,top10=FALSE)
{
    #select students in their first term, only STEM courses.
    sc <- sc %>% filter(enrl_from_cohort <= 3 & is_stem == 1 & 
                        crs_component == 'LEC' & crs_termcd >= crse_termcd_limit)
    
    if (top10 == TRUE)
    {
      sc_count <- sc %>% group_by(crs_name) %>% tally() %>% top_n(10) %>% ungroup()
      sc <- sc_count %>% left_join(sc)
    }
    
    #Random sample one course per student
    sc <- sc %>% group_by(st_id) %>% sample_n(1) %>% ungroup()
    
    #Rename the course STEM
    sc <- sc %>% mutate(crs_name='STEM')
    
    #And go for it.
    kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                               COURSE='STEM',TERM=2107,
                               model=as.formula(numgrade ~ ui_firstgen+ui_female+ui_urm+ui_li+
                                                ui_fem_urm+ui_fg_urm+ui_li_urm+ui_none),nohist=TRUE,
                               aggregate=TRUE) #<-- this aggregates all terms!!
    #ui_fg_fem+ui_fg_li+ui_fem_li
    #ui_fg_fem_li+ui_fg_fem_urm+ui_fg_urm_li+ui_urm_li_fem+ui_quad
    
    return(kk)
  
}
