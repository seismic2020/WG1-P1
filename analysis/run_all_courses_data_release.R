###########
# PURPOSE: 
# Wrap up the grade_penalty code to output one big table, one row per course. It takes the entire 
# student and course tables given, computes and orders the courses by enrollment, then returns the top N
# by enrollment. As part of the "Data Release" model, for demographic groups and combinations thereof, 
# it computes Ns, grade mean and sd, DFW rates, grade anomaly, and demographic diversity totalling out to 
# over 100 variables. It *only* includes lectures at this point.
#
# INPUT: see https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021
#       sc - table of COURSE LEVEL VARIABLES 
#       sr - table of STUDENT LEVEL VARIABLES 
#       (Note that this is just the SEISMIC data model we've been using all along.)
#       crse_term_cd_limit - the first term to begin aggregation of terms
#       mask - if set to true, for any groups with < 6 students, 
#              all observations for that groups will be masked and set to NA. If <= 10 students,
#              only the Ns will be masked.
#       top - set this to the number of courses to keep, ranked from higher to lower by the 
#             by the total enrollment.
#       by_term - if set to true, this will take the *top* enrolled courses each and return a table with the stats 
#                 for these courses by term instead of in aggregate.
# Dependencies (compile these first):
#       library(tidyverse)
#       grade_penalty_functions.R
#       grade_penalty_wg1_p1.R
###########

run_all_courses_data_release <- function(sr,sc,crse_termcd_limit=1760,mask=FALSE,top=250,by_term=FALSE)
{
  #get lectures within the range of terms we want
  sc <- sc %>% filter(crs_component == 'LEC' & crs_termcd >= crse_termcd_limit)
  
  #get the top N courses by total enrollment over this time.
  if (by_term == FALSE){sc <- sc %>% mutate(crs_term='ALL')}

  sc_count <- sc %>% group_by(crs_name,crs_term) %>% tally() %>% group_by(crs_term) %>% top_n(top) %>% ungroup()
  top <- dim(sc_count)[1]
  sc <- sc %>% left_join(sc_count)
  
  
  #now loop over the courses
  for (i in 1:top)
  {
    #print(pull(sc_count,crs_name)[i])
    #print(pull(sc_count,crs_term)[i])
    kk <- grade_penalty_wg1_p1(sr,sc, #%>% drop_na(gpao,numgrade), # commenting this out keeps the W's
                               COURSE=pull(sc_count,crs_name)[i],TERM=pull(sc_count,crs_term)[i],
                               model=as.formula(numgrade ~ ui_firstgen+ui_female+ui_urm+ui_li+
                                                  ui_fem_urm+ui_fg_urm+ui_li_urm+ui_none),nohist=TRUE,
                               aggregate=FALSE) 
   
    #extract what we need from the grade penalty structure.
    cname <- pull(sc_count,crs_name)[i]
    term  <- pull(sc_count,crs_term)[i]
    res   <- kk[[3]] %>%  pivot_wider(names_from=GROUP,values_from=c(-GROUP))
    res   <- res %>% add_column(COURSE=cname,.before='N_ALL') %>% 
                     add_column(TERM=term,.after='COURSE') %>%
                     add_column(SIMP_DIV=kk[[6]],.after='N_ALL')
    
    #print the course we just finished out to screen
    print(cname)
    
    #append the new results onto the existing table unless the table hastn' been made yet.
    if (i == 1)
    {
      out <- res
    }
    if (i > 1)
    {
      out <- bind_rows(out,res)
      
    }
  }
  
  #finally run the whole data set and paste it on the end. Had to pull this out of the loop
  #to run. The steps here are identical to what's in the loop.
  sc <- sc %>% mutate(crs_name='STEM') %>% group_by(st_id) %>% sample_n(1) %>% ungroup()
  kk <- grade_penalty_wg1_p1(sr,sc %>% drop_na(gpao,numgrade),
                             COURSE='STEM',TERM='ALL',
                             model=as.formula(numgrade ~ ui_firstgen+ui_female+ui_urm+ui_li+
                                                ui_fem_urm+ui_fg_urm+ui_li_urm+ui_none),nohist=TRUE,aggregate=TRUE) 
  res   <- kk[[3]] %>% pivot_wider(names_from=GROUP,values_from=c(-GROUP))
  cname <- 'ALL DATA'
  res   <- res %>% add_column(COURSE=cname,.before='N_ALL') %>% add_column(SIMP_DIV=kk[[6]],.after='N_ALL')
  out   <- bind_rows(out,res) %>% arrange(desc(N_ALL))
  
  #get rid of the median and MAD to reduce column bloat.
  out <- out %>% select(-contains(c('mad','med')))
  
  #now if we're going to share this, we need to mask low counts
  if (mask==TRUE){out <- mask_small_n(out)}
  
  return(out)
  
  
}


mask_small_n <- function(input)
{
  ncrse <- dim(input)[1]
  
  for (k in 1:ncrse)
  {
    data     <- input[k,]
    allnames <- names(data)
    rnames <- names(data)[grepl('^N_',allnames)]
  
    for (i in 1:length(rnames))
    {
      e <- names(data) == rnames[i]
      f <- which(data[,e] < 11)
      g <- which(data[,e] < 5)
    
      data[f,e] <- NA

      if (length(g) > 0)
      {
        sub  <- str_sub(rnames[i],3)
        h    <- grep(sub,allnames)
        data[,h] <- NA
      }
    }
    
    input[k,] <- data
    
  }
  return(input)
  
}  
  
