create_term_network <- function(sr,sc,weighted=FALSE,maxN=10000,TERM_CD_TEST=2010)
{
 library(tidyverse)
 library(igraph)
  
  #sc <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20190924.tab")
  #sr <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20190924.tab") 
  
  #get the student IDs and Majors
  sr <- sr %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES) 
  
  #select the term, only Undergrads, courses with > 1 student.
  sc <- sc %>% filter(TERM_CD == TERM_CD_TEST)
  sc <- sc %>% group_by(CLASS_NBR) %>% mutate(N=n()) %>% 
               filter(N > 1 & N < maxN & grepl("^U",PRMRY_CRER_CD)) %>%
        mutate(CRSE_IND=group_indices()) %>% ungroup() %>% 
        select(STDNT_ID,SBJCT_CD,CATLG_NBR,CLASS_NBR,CRSE_IND,
               UNITS_ERND_NBR,ACAD_LVL_BOT_SHORT_DES,N)
  
  sc <- sc %>% left_join(sr) %>% group_by(STDNT_ID) %>% 
               mutate(STDNT_IND=group_indices()) %>% ungroup()
  
  ncrse <- max(sc$CRSE_IND)
  nstd  <- max(sc$STDNT_IND)
  
  CIND       <- unique(sc$CRSE_IND)
  adjmtx     <- mat.or.vec(nstd,nstd) #Matrix(nrow=nstd,ncol=nstd,sparse=TRUE)
  STDNT_ID   <- mat.or.vec(nstd,1)
  MAJOR      <- STDNT_ID
  ACAD_LEVEL <- STDNT_ID
  STDNT_MIND  <- STDNT_ID
  
  
  for (i in 1:ncrse)
  {
     
    c   <- which(sc$CRSE_IND == CIND[i])
    hrs <- sc$UNITS_ERND_NBR[c[1]]
    if (weighted == FALSE){hrs <- 1}
    ncstd <- length(c)
    print(str_c(c(i,' of ',ncrse))) 
    
    for (j in 1:ncstd)
    {
      STDNT_MIND[sc$STDNT_IND[c[j]]] <- sc$STDNT_IND[c[j]]
      STDNT_ID[sc$STDNT_IND[c[j]]] <- sc$STDNT_ID[c[j]]
      MAJOR[sc$STDNT_IND[c[j]]] <- sc$UM_DGR_1_MAJOR_1_DES[c[j]]
      ACAD_LEVEL[sc$STDNT_IND[c[j]]] <- sc$ACAD_LVL_BOT_SHORT_DES[c[j]]
      adjmtx[sc$STDNT_IND[c[j]],sc$STDNT_IND[c]] <- adjmtx[sc$STDNT_IND[c[j]],sc$STDNT_IND[c]]+hrs
    }
  }
  
  print('computing graph from adjacency matrix')
  net <- graph_from_adjacency_matrix(adjmtx)
  DEGREE <- degree(net)/2.
  out <- tibble(STDNT_ID,STDNT_MIND,DEGREE,MAJOR,ACAD_LEVEL)
  
  return(out)
  
  
}

run_several <- function(sr,sc)
{
  aa1 <- create_term_network(sr,sc,TERM_CD_TEST=2010)
  aa2 <- create_term_network(sr,sc,weighted=TRUE,TERM_CD_TEST=2010)
  aa3 <- create_term_network(sr,sc,maxN=50,TERM_CD_TEST=2010)
  aa4 <- create_term_network(sr,sc,maxN=50,weighted=TRUE,TERM_CD_TEST=2010)
  
  aa5 <- create_term_network(sr,sc,TERM_CD_TEST=2210)
  aa6 <- create_term_network(sr,sc,weighted=TRUE,TERM_CD_TEST=2210)
  aa7 <- create_term_network(sr,sc,maxN=50,TERM_CD_TEST=2210)
  aa8 <- create_term_network(sr,sc,maxN=50,weighted=TRUE,TERM_CD_TEST=2210)
  
  
  write_tsv(aa1,'~/Desktop/degree_FA2014_ALL.txt')
  write_tsv(aa2,'~/Desktop/degree_FA2014_weighted.txt')
  write_tsv(aa3,'~/Desktop/degree_FA2014_50.txt')
  write_tsv(aa4,'~/Desktop/degree_FA2014_50_weighted.txt')
  write_tsv(aa5,'~/Desktop/degree_FA2018_ALL.txt')
  write_tsv(aa6,'~/Desktop/degree_FA2018_weighted.txt')
  write_tsv(aa7,'~/Desktop/degree_FA2018_50.txt')
  write_tsv(aa8,'~/Desktop/degree_FA2018_50_weighted.txt')
  
  out <- list(aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8)
  
  return(out)
}

join_all_runs <- function(out)
{
  library(ggExtra)
  temp1 <- full_join(out[[1]] %>% select(-STDNT_MIND),out[[2]] %>% select(-STDNT_MIND),
                    by=c("STDNT_ID","MAJOR","ACAD_LEVEL"))
  temp1 <- full_join(temp1,out[[3]] %>% select(-STDNT_MIND),
                    by=c("STDNT_ID","MAJOR","ACAD_LEVEL"))
  temp1 <- full_join(temp1,out[[4]] %>% select(-STDNT_MIND),
                    by=c("STDNT_ID","MAJOR","ACAD_LEVEL"))
  
  names(temp1) <- c('STDNT_ID','DEGREE_ALL','MAJOR','ACAD_LEVEL','DEGREE_ALL_WEIGHTED',
                 'DEGREE_50','DEGREE_50_WEIGHTED')
  
  temp1 <- temp1 %>% replace_na(list(DEGREE_50=0,DEGREE_50_WEIGHTED=0))
  
  pp <- ii %>% group_by(MAJOR,ACAD_LEVEL) %>% 
               summarize(N=n(),DALL=median(DEGREE_ALL),
                               DALLW=median(DEGREE_ALL_WEIGHTED),
                               D50=median(DEGREE_50),
                               D50W=median(DEGREE_50_WEIGHTED))
  
  #p <- temp1 %>% pivot_longer(c('DEGREE_ALL','DEGREE_ALL_WEIGHTED',
  #                      'DEGREE_50','DEGREE_50_WEIGHTED'),
  #                    names_to='DEG',values_to='DEGREE') %>% 
  #           ggplot(aes(x=DEGREE))+
  #           geom_histogram()+facet_wrap(~DEG)
  
  
  return(temp1)
  
}



