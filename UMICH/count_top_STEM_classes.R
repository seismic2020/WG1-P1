count_top_STEM_classes <- function(sr,sc)
{
  library(tidyverse)
  #get all STEM graduators
  sr <- sr %>% filter((current_major == 'S' | current_major == 'E'))
  sc <- sc %>% filter(enrl_from_cohort < 1.0)
  
  full <- sc %>% left_join(sr,by='st_id')
  full <- full %>% group_by(crs_name,crs_sbj,crs_catalog) %>% summarize(N=n()) 
  full <- label_stem_courses(full)
  
  return(full)  
  
}

label_stem_courses <- function(full)
{
  STEM <- c('AERO','AEROSP','ANAT','ANATOMY','ANESTH','AOSS','APPPHYS','ASTRO','AUTO',
             'BIOINF','BIOLCHEM','BIOLOGY','BIOMATLS','BIOMEDE','BIOPHYS','BIOSTAT',
             'BOTANY','CANCBIO','CEE','CHE','CHEM','CHEMBIO','CLIMATE','CMPLXSYS','CMPTRSC', #COGSCI
             'CS','EARTH','EEB','EECS','ENGR','ENSCEN','ENVIRON','ENVRNSTD','EPID','ESENG',
             'GEOSCI','HUMGEN','IOE',
             'MACROMOL','MATH','MATSCIE','MCDB','MECHENG','MEDCHEM','MEMS','MFG','MICROBIOL',
             'NAVARCH','MILSCI','NAVSCI','NERS','NEUROL','NEUROSCI',
             'PHARMACY','PHARMADM','PHARMCEU','PHARMCHM','PHARMCOG','PHARMSCI','PHYSICS','PHYSIOL',
             'PIBS','PUBHLTH', #PYSCH
             'RADIOL','SI','STATS','SPACE','ZOOLOGY')
  STEM <- tibble(crs_sbj=STEM,IS_STEM=1)
  
  full <- full %>% left_join(STEM)
  full <- full %>% replace_na(list(IS_STEM=0))
  return(full)
  
}