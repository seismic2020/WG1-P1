###########
# PURPOSE: 
# Wrap up the grade_penalty code to output one big table, one row per course. It takes the entire 
# student and course tables given, computes and orders the courses by enrollment, then returns the top N
# by enrollment. As part of the "Data Release" model, for demographic groups and combinations thereof, 
# it computes Ns, grade mean and sd, DFW rates, grade anomaly, and demographic diversity totalling out to 
# over 100 variables.
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
###########

run_all_courses_data_release <- function(sr,sc,crse_termcd_limit=1760,mask=FALSE,top=250)
{
  sc <- sc %>% filter(crs_component == 'LEC' & crs_termcd >= crse_termcd_limit)
  sc_count <- sc %>% group_by(crs_name) %>% tally() %>% top_n(top) %>% ungroup()
  sc <- sc %>% left_join(sc_count)
  
  for (i in 1:top)
  {
    kk <- grade_penalty_wg1_p1(sr,sc, #%>% drop_na(gpao,numgrade), # commenting this out keeps the W's
                               COURSE=pull(sc_count,crs_name)[i],TERM='ALL',
                               model=as.formula(numgrade ~ ui_firstgen+ui_female+ui_urm+ui_li+
                                                  ui_fem_urm+ui_fg_urm+ui_li_urm+ui_none),nohist=TRUE,
                               aggregate=TRUE) 
    
    cname <- pull(sc_count,crs_name)[i]
    
    res   <- kk[[3]] %>%  pivot_wider(names_from=GROUP,values_from=c(-GROUP))
    
    res   <- res %>% add_column(COURSE=cname,.before='N_ALL') %>% add_column(SIMP_DIV=kk[[6]],.after='N_ALL')
    print(cname)
    
    if (i == 1)
    {
      out <- res
    }
    if (i > 1)
    {
      out <- bind_rows(out,res)
      
    }
  }
  
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
  
