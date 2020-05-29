wg1_p1_rename_umich_seismic <- function(sr,sc)
{
   #sc <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20200129.tab")
   #sr <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20200129.tab") 
   
   source('/Users/bkoester/Google Drive/code/SEISMIC/SEISMIC2020/WG1-P1/UMICH/term_count.R')
  
   sc <- sc %>% filter(grepl("^U",PRMRY_CRER_CD) & !grepl("S",TERM_SHORT_DES) & 
                         !grepl("M",TERM_SHORT_DES) & TERM_CD >= 1210) 
   sr <- sr %>% filter(FIRST_TERM_ATTND_CD >= 1210)
  
   sc <- term_count(sr,sc)
   sc$SUM <- sc$TERMYR/2.0-0.5
   
   sr <- sr %>% mutate(LI=0)
   sc <- sc %>% mutate(WD=0,RT=0,SEM=0,BLANK=NA)
   sc$WD[which(sc$CRSE_GRD_OFFCL_CD == 'W')] <- 1
   sc$SEM[grep('S',sc$TERM_SHORT_DES)] <- 1
   
   sr$LI[which(sr$MEDINC < 50000)] <- 1
   
   
   sr_names <- c("st_id"="STDNT_ID",
                "firstgen"="FIRST_GEN",
                "ethniccode"="STDNT_ETHNC_GRP_SHORT_DES",
                "ethniccode_cat"="STDNT_DMSTC_UNDREP_MNRTY_CD",
                "female"="STDNT_GNDR_SHORT_DES",
                "famincome"="MEDINC",
                "lowincomflag"="LI",
                "transfer"="TRANSFER",
                "international"="STDNT_INTL_IND",
                "us_hs"="HS_PSTL_CD",
                "cohort"="FIRST_TERM_ATTND_SHORT_DES",
                "englsr"="MAX_ACT_ENGL_SCR",
                "mathsr"="MAX_ACT_MATH_SCR",
                "hsgpa"="HS_GPA",
                "current_major"="DIVISION")
   
   
      sc_names <- c("st_id"="STDNT_ID",
                    "crs_sbj"="SBJCT_CD",
                "crs_catalog"="CATLG_NBR",
                "crs_name"="CRSE_ID_CD",
                "numgrade"="GRD_PNTS_PER_UNIT_NBR",
                "numgrade_w"="WD",
                "crs_retake"="RT",
                "crs_term"="TERM_SHORT_DES",
                "crs_termcd"="TERM_CD",
                "summer_crs"="SEM",
                "enrl_from_cohort"="SUM",
                "gpao"="EXCL_CLASS_CUM_GPA",
                "begin_term_cum_gpa"="BOT_GPA",
                "crs_credits"="UNITS_ERND_NBR",
                "instructor_name"="BLANK",
                "crs_component"="CRSE_CMPNT_CD",
                "class_number"="CLASS_NBR",
                "aptaker"="BLANK",
                "apskipper"="BLANK",
                "tookcourse"="BLANK",
                "apyear"="BLANK",
                "apscore"="BLANK") 
      
      sr <- sr  %>% select(sr_names)
      sc <- sc  %>% select(sc_names)
      
      sr$ethniccode_cat <- 1
      sr$ethniccode_cat[sr$ethniccode == 'White' | sr$ethniccode == 'Not Indic'] <- 0
      sr$ethniccode_cat[sr$ethniccode == 'Asian'] <- 2
      
      e1 <- which(sr$female == 'Female')
      e0 <- which(sr$female == 'Male')
      
      sr$female <- 2 
      sr$female[e1] <- 1
      sr$female[e0] <- 0
      
      sc$crs_name <- str_c(sc$crs_sbj,sc$crs_catalog,sep=" ")
      sc <- flag_stem(sc)
      
    return(list(sr,sc))
  
}

flag_stem <- function(sc)
{
   #flag the stem courses
   clist <- c('AERO','AEROSP','ANAT','ANATOMY','ANESTH','AOSS','APPPHYS','ASTRO','AUTO',
              'BIOINF','BIOLCHEM','BIOLOGY','BIOMATLS','BIOMEDE','BIOPHYS','BIOSTAT',
              'BOTANY','CANCBIO','CEE','CHE','CHEM','CHEMBIO','CLIMATE','CMPLXSYS','CMPTRSC', #COGSCI
              'CS','EARTH','EEB','EECS','ENGR','ENSCEN','ENVIRON','ENVRNSTD','EPID','ESENG',
              'GEOSCI','HUMGEN','IOE',
              'MACROMOL','MATH','MATSCIE','MCDB','MECHENG','MEDCHEM','MEMS','MFG','MICROBIOL',
              'NAVARCH','MILSCI','NAVSCI','NERS','NEUROL','NEUROSCI',
              'PHARMACY','PHARMADM','PHARMCEU','PHARMCHM','PHARMCOG','PHARMSCI','PHYSICS','PHYSIOL',
              'PIBS','PUBHLTH', #PYSCH
              'RADIOL','SI','STATS','SPACE','ZOOLOGY')
   
   #clist <- c('MATH','PHYSICS','CHEM','BIOLOGY','STATS')
   
   ncrse        <- dim(sc)[1]
   is_stem  <- mat.or.vec(ncrse,1)
   e            <- sc$crs_sbj %in% clist
   is_stem[e]   <- 1
   data          <- as_tibble(data.frame(sc,is_stem))
   return(data)
}
   