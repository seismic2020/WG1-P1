#tables assumed clean and samples assumed already defined
#--------
#ARGUMENTS to course_report():
#sr: the student record table
#sc: hte student course table
#CRSE_SUBJECT: character field for the course subject
#CRSE_NUMBER:  character field for the course catalog number
#BY_TERM    : Currently unused, stay tuned for future releases
#RETURN_TABLE: One of "CRER","MAJ","DEMO", or "PRIV", returns tabulated statistics for one of
#              ADMSSN_CRER_CD, DGR_1_MAJOR_1_DES, demography, or privelge (respectively).
#--------
#Example workflow in R session
#1) load tidyverse, read in the data tables in your R session (insert your own path), then
#   compile this function.
#> 
#> library(tidyverse)
#> sr <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/student_record.tsv")
#> sc <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/student_course.tsv")
#> source('~/Box Sync/SEISMIC/SEISMIC/SEISMIC/course_report.R')
#
#2) check that all the columns are present and the types are correct in your R session with
#"check tables", which verbosely assesses the sc and sr table to determine if the 
#columns are there and the data types are correct.
#> 
#> temp <- check_tables(sc,sr)
#  sometimes you can coerce a datatype if it's not working as follows:
#> sc <- sc %>% mutate(CATLG_NBR = as.character(CATLG_NBR))
#
#3) Choose a course by subject and catalog number, the table to type to return:
#>
#> maj <- course_report(sr,sc,CRSE_SUBJECT='CHEM',CRSE_NUMBER="210",RETURN_TABLE="MAJ")
#> print(maj)
#  This will create Venn diagrams in your plot window, and the print() statment
#  outputs the table to the console.
#
#4) Get fancier and hoose a course and subject, but require that the report be 
#   specific for "Freshman" in term 1810.
#>
#> maj <- course_report(sr %>% filter(FIRST_ENTRY_TYPE == 'Freshman'),sc %>% filter(TERM_CD == 1810),
#                       CRSE_SUBJECT='CHEM',CRSE_NUMBER="210",RETURN_TABLE="MAJ")
#print(maj)
#--------------------
course_report <- function(sr,sc,CRSE_SUBJECT='PHYSICS',CRSE_NUMBER = "140",BY_TERM=FALSE,
                          RETURN_TABLE='CRER')
{
  require(colorfulVennPlot)
  require(tidyverse)
  require(xtable)
  require(RColorBrewer)
  
  sc <- sc %>% filter(SBJCT_CD == CRSE_SUBJECT & CATLG_NBR == as.character(CRSE_NUMBER))
  sr <- sr %>% select(c(STDNT_ID,STDNT_GNDR_SHORT_DES,STDNT_UNDREP_MNRTY_CD,FIRST_GENERATION,ADMSSN_CRER_CD,
                        DGR_1_MAJOR_1_DES,PRNT_MAX_ED_LVL_DES,EST_GROSS_FAM_INC_CD))
  
  sc <- left_join(sc,sr)
  
  crer  <- sc %>% group_by(ADMSSN_CRER_CD) %>% tally() %>% arrange(desc(n)) %>% top_n(10)
  maj   <- sc %>% group_by(DGR_1_MAJOR_1_DES) %>% tally() %>% arrange(desc(n)) %>% top_n(10)
  demo  <- basic_demography(sc)
  priv  <- privilege_summary(sc)
  
  return(maj)
  
}

#recodes demographic variables and summary statistics for a course by term if desired, then
#creates the summary table that is returned to course_peport()
#Called by course_report().
#
basic_demography <- function(sc,BY_TERM=FALSE)
{
  sc$STDNT_GNDR_SHORT_DES <- sc$STDNT_GNDR_SHORT_DES %>% replace_na('GNDR_NA')
  sc <- sc %>% mutate(STDNT_UNDREP_MNRTY_CD = case_when(STDNT_UNDREP_MNRTY_CD == 0 ~ 'non-URM',
                                                       STDNT_UNDREP_MNRTY_CD == 1 ~ 'URM'))
  sc$STDNT_UNDREP_MNRTY_CD <- sc$STDNT_UNDREP_MNRTY_CD %>% replace_na('URM_NA')
  sc <- sc %>% mutate(FIRST_GENERATION = case_when(FIRST_GENERATION == 0 ~ 'non-FG',
                                            FIRST_GENERATION == 1 ~ 'FG'))
  sc$FIRST_GENERATION <- sc$FIRST_GENERATION %>% replace_na('FG_NA')
  
  if (BY_TERM==TRUE){sc <- sc %>% group_by(TERM_CD,TERM_SHORT_DES) %>% arrange(TERM_CD)}
  
  total <- sc %>% group_by(TERM_CD,TERM_SHORT_DES) %>% summarize(N=n())  %>% ungroup()
  gen   <- sc %>% group_by(TERM_CD,TERM_SHORT_DES,STDNT_GNDR_SHORT_DES) %>% 
    summarize(N=n()) %>% spread(STDNT_GNDR_SHORT_DES,N) %>% ungroup() %>% select(-TERM_SHORT_DES)
  min   <- sc %>% group_by(TERM_CD,TERM_SHORT_DES,STDNT_UNDREP_MNRTY_CD) %>% 
    summarize(N=n()) %>% spread(STDNT_UNDREP_MNRTY_CD,N) %>% ungroup() %>% select(-TERM_SHORT_DES)
  fg    <- sc %>% group_by(TERM_CD,TERM_SHORT_DES,FIRST_GENERATION) %>% 
    summarize(N=n()) %>% spread(FIRST_GENERATION,N) %>% ungroup() %>% select(-TERM_SHORT_DES)
  #print(gen)
  temp  <- left_join(total,gen,by='TERM_CD') %>% left_join(min,by='TERM_CD') %>% left_join(fg,by='TERM_CD')
  
  
  return(temp)
  
  
}

#######
#creates the Venn diagrams
#Called by course_report().
#
privilege_summary <- function(sr)
{
  library(colorfulVennPlot)
  library(RColorBrewer)
  
  
    sr <- sr %>% mutate(PRIVILEGE = 4)
    sr$PRIVILEGE[which(sr$STDNT_GNDR_SHORT_DES == 'Female')] <- sr$PRIVILEGE[which(sr$STDNT_GNDR_SHORT_DES == 'Female')]-1
    sr$PRIVILEGE[which(sr$STDNT_UNDREP_MNRTY_CD == 1)] <- sr$PRIVILEGE[which(sr$STDNT_UNDREP_MNRTY_CD == 1)]-1
    sr$PRIVILEGE[which(sr$FIRST_GENERATION == 1)] <- sr$PRIVILEGE[which(sr$FIRST_GENERATION == 1)]-1
    
    temp <- sr %>% mutate(GENDER = case_when(STDNT_GNDR_SHORT_DES == 'Female' ~ 1,
                                             STDNT_GNDR_SHORT_DES == 'Male' ~ 0),
                          INCOME = case_when(EST_GROSS_FAM_INC_CD <= 50 & EST_GROSS_FAM_INC_CD != 0 ~ 1,
                                             EST_GROSS_FAM_INC_CD > 50 ~ 0),
                          URM   = STDNT_UNDREP_MNRTY_CD) %>% 
                          select(GENDER,URM,FIRST_GENERATION,INCOME) 
    
    bigN <- dim(temp %>% drop_na())[1]
    
    temp4d <- temp %>% group_by(GENDER,URM,FIRST_GENERATION,INCOME) %>% drop_na() %>% tally()
    temp3d <- temp %>% group_by(GENDER,URM,FIRST_GENERATION) %>% drop_na() %>% tally()
    
    #temp4d <- signif(temp4d/bigN,3)
    #temp3d <- signif(temp3d/bigN,3)
    
    #default ordering that we need to handle missing combinations.
    def4 <- c("0001","0010","0011","0100","0101","0110","0111","1000",
              "1001","1010","1011","1100","1101","1110","1111")
    nms <- str_c(temp4d$GENDER,temp4d$URM,temp4d$FIRST_GENERATION,temp4d$INCOME,sep="")[-1]
    i1  <- match(nms,def4)
    vec <- mat.or.vec(length(def4),1)
    vec[i1] <-  as.vector(temp4d$n[-1])
    names(vec) <- def4
    labels=c('GENDER','URM','FIRST_GENERATION','INCOME')
    par(xpd=TRUE)
    plot.new()
    plotVenn4d(vec,labels=labels,Colors=c(brewer.pal(7,'Blues'),brewer.pal(8,'Blues')))
    
    
    def3 <- c("001","010","011","100","101","110","111")
    nms <- str_c(temp3d$GENDER,temp3d$URM,temp3d$FIRST_GENERATION,sep="")[-1]
    i1  <- match(nms,def3)
    vec <- mat.or.vec(length(def3),1)
    vec[i1] <-  as.vector(temp3d$n[-1])
    
    names(vec) <- def3
    labels=c('GENDER','URM','FIRST_GENERATION')
    par(xpd=TRUE)
    plot.new()
    plotVenn3d(vec,labels=labels,Colors=brewer.pal(7,'Blues'))
    par(xpd=TRUE)
    
   return(temp)
  
}
###############
#not called by course_report. diagnostic allowing the user to test table formats before 
#running the course reports.
#return to main level if tables are missing info or not properly formatted
#
check_tables <- function(sc,sr)
{
  #the types/names of the existing tables
  types_sc      <- t(sc %>% summarize_all(class))
  req_names_sc <- rownames(types_sc)
  req_types_sc <- types_sc[,1]
  types_sc <- tibble(req_names_sc,req_types_sc)
  
  types_sr      <- t(sr %>% summarize_all(class))
  req_names_sr <- rownames(types_sr)
  req_types_sr <- types_sr[,1]
  types_sr <- tibble(req_names_sr,req_types_sr)
  
  #names(types_sc) <- c("req_names_sc","req_types_sc")
  #names(types_sr) <- c("req_names_sr","req_types_sr")
  
  req_names_sc  <- c('STDNT_ID','TERM_CD','TERM_SHORT_DES','GRD_PNTS_PER_UNIT_NBR',
                     'UNITS_ERND_NBR','EXCL_CLASS_CUM_GPA','SBJCT_CD','CATLG_NBR')
  req_types_sc  <- c('numeric','numeric','character','numeric',
                     'numeric','numeric','character','character')
  
  req_names_sr  <- c('STDNT_ID','ADMSSN_CRER_CD','DGR_1_MAJOR_1_DES',
                     'FIRST_TERM_ATTND_CD','FIRST_TERM_ATTND_SHORT_DES',
                     'STDNT_GNDR_SHORT_DES','STDNT_UNDREP_MNRTY_CD','FIRST_GENERATION','EST_GROSS_FAM_INC_CD',
                     'HS_GPA','MAX_ACT_MATH_SCR','MAX_ACT_ENGL_SCR')
  req_types_sr  <- c('numeric','character','character',
                     'numeric','character',
                     'character','numeric','numeric','numeric',
                     'numeric','numeric','numeric')
  types_tbl_sc <- tibble(req_names_sc,req_types_sc)
  types_tbl_sr <- tibble(req_names_sr,req_types_sr)
  
  #now check our tables against what is expected"
  i1_sr_types <- left_join(types_tbl_sr,types_sr,by='req_names_sr')
  i1_sc_types <- left_join(types_tbl_sc,types_sc,by='req_names_sc')
  
  #run the checks on names
  missing_name_sr <- i1_sr_types %>% filter(is.na(req_types_sr.y)) %>% pull(req_names_sr)
  if (length(missing_name_sr) == 0){print('no missing student record columns')}
  if (length(missing_name_sr) >  0)
  {
    print('missing student record columns:')
    print(missing_name_sr)
    return()
  }
  missing_name_sc <- i1_sc_types %>% filter(is.na(req_types_sc.y)) %>% pull(req_names_sc)
  if (length(missing_name_sc) == 0){print('no missing student course columns')}
  if (length(missing_name_sc) >  0)
  {
    print('missing student course columns:')
    print(missing_name_sr)
    return()
  }
  
  #now the checks on types
  View(i1_sr_types)
  missing_types_sr <- i1_sr_types %>% filter(req_types_sr.x != req_types_sr.y) %>% pull(req_names_sr)
  if (length(missing_types_sr) == 0){print('student record columns types are good')}
  if (length(missing_types_sr) >  0)
  {
    print('following student record types are incorrect:')
    print(missing_types_sr)
    return()
  }
  missing_types_sc <- i1_sc_types %>% filter(req_types_sc.x != req_types_sc.y) %>% pull(req_names_sc)
  if (length(missing_types_sc) == 0){print('student record columns types are good')}
  if (length(missing_types_sc) >  0)
  {
    print('following student course types are incorrect:')
    print(missing_types_sc)
    return()
  }
}

