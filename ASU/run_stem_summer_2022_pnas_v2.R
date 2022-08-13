#This runs as a wrapper, calling grade_penalty_wg1_p1.R
#
#It takes the same two tables as inputs.
#
#The Data Model has been updated to include a flag "is_stem", 
#to indicate that a course is considered STEM by the user at
#their institution. 
#
#SAMPLE
#Student sample: 
#               - non-int'l, non-transfer - institution must remove these, the code will not do it. 
#               - from these, consider only students in first STEM course. Code will do this (but seem STEM course notes below)
#               - create top 10 enrollment list from only first year students. Students will be chosen from these courses.
#Course sample - first, code will remove courses instances with a 'W' received - we can't do grade point calcs with this
#        - institutions need to select their STEM subjects. As a reminder, we take STEM = (MATH,PHYSICS,CHEM,BIO,ENGIN,STATS).
#          Please set the is_stem flag in the course table = 1.
#        - we're using ONLY lecture format courses. The code will do this as long as the crs_component == 'LEC'
#We'll assume institutions are removing "shortened" or low-use terms from their courses tables so that 
#   - students that are taking their first STEM course in a term like this are excluded.
#   - and we can use "enrl_from_cohort" (see Data Model) flag to select those < 1. That is, those taking their first stem course in 
#     in one of the Fall/Spring semesters where enrollments are high.
#   - only considering Fall 2009-Fall 2019 courses.
#Finally, students taking more than one STEM course in their first STEM-taking semester will have a course randomly sampled, guaranteeing 
#that we use only one course per student
#
#Updated Analysis:
#1) overall counts by SAI of domestic, non-transfer students 
#2) mean STEM course grade anomaly by SAI. This uses the student/course sampling described above.
#3) t-test of mean grade anomaly for SAI = (0-3) vs. SAI=4; SAI = (0,3) are aggregated.
#4) Linear regression: grade anomaly ~ SAI, where SAI is the indep variable
#5) sample SAI and demographics of final sample used in analysis; note that this is NOT that same as (1)
#
#INPUTS:
#sr, sc: the student course and student record tables
#crse_termcd_limit: The lower limit for TERM_CD of the cohort to include. 
#                   It includes this term and all those after.
#top10: let it the code try to identify and run on your top 10 courses by 
#       enrollment of these students. WARNING: for reasons unknown the
#       join required is super slow.
##################################
run_stem_summer_2022_pnas_v2 <- function(sr,sc,crse_termcd_lower_limit=2097,crse_termcd_upper_limit=2197,top10=TRUE)
{
  
   #These scripts will need to be sourced wherever they are on your machine.  
   source("grade_penalty_wg1_p1.R")
   source("grade_penalty_functions.R")

   #to accomplish (1), we will consider ALL students that entered between Fall 2009 and Fall 2019.
   #This should be all we need to do to define the sample, because transfers and int'l should already be removed,
   #and our baseline sample is Fall 2009-Fall 2019 entering cohorts, which should already be defined by each institutions
   #
   #This function comes from 'grade_penalty_functions.R'
   all_sai <- add_ME_coding(sr)
   stats   <- all_sai %>% group_by(sai,opp) %>% tally()
  
    #define the analysis sample
    sc <- sc %>% filter(enrl_from_cohort < 10 & #those taking a course sometime in their first year
    											# UNIQUE ASU CHANGE -- term codes are 4 digits and admit
    											# terms are always summer
                        is_stem == 1 &           #courses must be STEM of some kind
                        crs_component == 'LEC' & #course must be lecture based
                        numgrade_w == 0 &        #we're explicitly excluding withdrawals!
                        crs_termcd >= crse_termcd_lower_limit & 
                        crs_termcd <= crse_termcd_upper_limit) #course must be in the term code range for Fall 2009-Fall 2019
    
    #because some students took their first STEM course in the fall and some in the spring, we have to make sure
    #we're properly accounting for that; students taking STEM courses in the Fall AND Spring should only have their
    #Fall courses counted.
    
    sc <- sc %>% group_by(st_id) %>% filter(crs_termcd==min(crs_termcd)) %>% ungroup()
    
    #get the top 10 STEM courses by enrollment of the sample that has passed our cuts thus far. 
    #This is over Fall 2009-Fall 2019, not term-by-term!
    if (top10 == TRUE)
    {
      sc_count <- sc %>% group_by(crs_name) %>% tally() %>% top_n(10) %>% ungroup()
      sc <- sc_count %>% left_join(sc)
      print('Top 10 courses')
      View(sc_count)
    }
    
    #Random sample one course per student.
    sc <- sc %>% group_by(st_id) %>% sample_n(1) %>% ungroup()
    
    #Rename the course STEM just to make the code happy.
    sc <- sc %>% mutate(crs_name='STEM')
    
    #And go for it. Most of what we need is in this orginal function
    kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                               COURSE='STEM',TERM='FA 2010',
                               model=as.formula(grade_penalty ~ sai),nohist=TRUE,
                               aggregate=TRUE) #<-- this aggregates all terms!!
    #ui_fg_fem+ui_fg_li+ui_fem_li
    #ui_fg_fem_li+ui_fg_fem_urm+ui_fg_urm_li+ui_urm_li_fem+ui_quad
    
    #For Analysis (3), we need to 
    
    #lots of list-passing here. taking everything apart and putting it together again.
    #Analysis outputs
    
    output <- list(stats,kk[[2]],kk[[3]],kk[[4]])
    
    #output follows the analysis questions above.
    #analysis 
    #question 1 overall demographics: output[[1]]
    #question 2 grade anomalies by sai: output[[2]]
    #question 3 t-test of low and hi sai: output[[3]]
    #question 4 regression, grade_penalty ~ sai: output[[4]]
    #question 5: student sample sai: output[[2]]

    return(output)
  
}
