create_all_stem_layer <- function(sr,sc)
{
    sc <- sc %>% filter(enrl_from_cohort == 0.5 & is_stem == 1 & crs_term == 'FA 2016')
    sc <- sc %>% group_by(st_id) %>% sample_n(1) 
    
    
    
    return(sc)
  
}