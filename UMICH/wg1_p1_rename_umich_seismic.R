wg1_p1_rename_umich_seismic <- function(sr,sc)
{
   source('/Users/bkoester/Google Drive/code/Mellon/TOF/term_count.R')
  
   sc <- sc %>% filter(grepl("^U",PRMRY_CRER_CD) & !grepl("S",TERM_SHORT_DES) & 
                         !grepl("M",TERM_SHORT_DES) & TERM_CD >= 1810) 
   sr <- sr %>% filter(FIRST_TERM_ATTND_CD >= 1810)
  
   sc <- term_count(sr,sc)
   sc$SUM <- sc$TERMYR/2.0
   View(sc)
   
   sr <- sr %>% mutate(LI=0)
   sc <- sc %>% mutate(WD=0,RT=0,SEM=0,BLANK=NA)
   sc$WD[which(sc$CRSE_GRD_OFFCL_CD == 'W')] <- 1
   sc$SEM[grep('S',sc$TERM_SHORT_DES)] <- 1
   
   sr$LI[which(sr$MEDINC < 40000)] <- 1
   
   
   sr_names <- c("st_id"="STDNT_ID",
                "firstgen"="FIRST_GEN",
                "ethniccode"="STDNT_ETHNC_GRP_SHORT_DES",
                "ethniccode_cat"="STDNT_ETHNC_GRP_SHORT_DES",
                "gender"="STDNT_GNDR_SHORT_DES",
                "famincome"="MEDINC",
                "lowincomflag"="LI",
                "transfer"="TRANSFER",
                "international"="STDNT_INTL_IND",
                "us_hs"="STDNT_INTL_IND",
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
                "crs_term"="TERM_CD",
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
      
    return(list(sr,sc))
  
}