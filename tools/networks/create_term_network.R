#This assembles student co-enrollment networks for a single term
create_term_network <- function(sr,sc,weighted=FALSE,maxN=10000,TERM_CD_TEST='FA 2014',name='junk',keep=FALSE)
{
 library(tidyverse)
 library(igraph)
  
  #sc <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20190924.tab")
  #sr <- read_tsv("/Users/bkoester/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20190924.tab") 
  
  #get the student IDs and Majors
  sr <- sr %>% select(STDNT_ID,UM_DGR_1_MAJOR_1_DES) 
  
  #select the term, only Undergrads, courses with > 1 student.
  sc <- sc %>% filter(TERM_SHORT_DES == TERM_CD_TEST)
  sc <- sc %>% group_by(CLASS_NBR) %>% mutate(N=n()) %>% 
               filter(N > 1 & N < maxN & grepl("^U",PRMRY_CRER_CD)) %>%
        mutate(CRSE_IND=group_indices()) %>% ungroup() %>% 
        select(STDNT_ID,SBJCT_CD,CATLG_NBR,CLASS_NBR,CRSE_IND,
               UNITS_ERND_NBR,ACAD_LVL_BOT_SHORT_DES,PRMRY_CRER_CD,N)
  
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
  PRMRY_CRER_CD <- STDNT_ID
  
  
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
      PRMRY_CRER_CD[sc$STDNT_IND[c[j]]] <- sc$PRMRY_CRER_CD[c[j]]
    }
  }
  
  print('computing graph from adjacency matrix')
  net <- graph_from_adjacency_matrix(adjmtx)
  DEGREE <- degree(net)/2.
  out <- tibble(STDNT_ID,STDNT_MIND,DEGREE,MAJOR,ACAD_LEVEL,PRMRY_CRER_CD)
  
  if (keep == TRUE)
  {
    ll <- kstep_centrality(net)
    write_tsv(out,str_c('~/Desktop/out_',name,'.tab',sep=""))
    write_tsv(data.frame(ll), str_c('~/Desktop/ksteps_',name,'.tab',sep=""))
  }
  return(list(out,net,adjmtx))
  
  
}

#takes output of "create_term_network.R" compute the k-step centrality up to k = 5, like the Cornell folks.
#Specifically, it takes the igraph "net" object from that function.
kstep_centrality <- function(net)
{
  #a <- shortest_paths(net,1)  
  #nnodes <- 100
  nnodes <- length(V(net))
  out <- mat.or.vec(1000,5)
  print(Sys.time())
  for (i in 1:1000)
  {
    if (i %% 100 == 0){print(i)}
    a <- distances(net,i,weights=NA,mode='all')
    out[i,1] <- length(which(a == 1))
    out[i,2] <- length(which(a <= 2))
    out[i,3] <- length(which(a <= 3))
    out[i,4] <- length(which(a <= 4))
    out[i,5] <- length(which(a <= 5))
  }
  print(Sys.time())
  return(out/nnodes)
}

#Compute degree distributions withing and between schools.
compute_degrees <- function(lsa,adjmtx)
{
  CRER_CD_LVL <- unique(lsa$PRMRY_CRER_CD)
  ncrer <- length(CRER_CD_LVL)
  CRER_WITHIN  <- mat.or.vec(ncrer,1)
  CRER_BETWEEN <- CRER_WITHIN
  
  degmtx <- mat.or.vec(dim(lsa)[1],ncrer*2)
  cts <-1 
  for (i in 1:ncrer)
  {
    print(CRER_CD_LVL[i])
    sch    <- which(lsa$PRMRY_CRER_CD == CRER_CD_LVL[i])
    subsch <- adjmtx
    subsch[-sch,] <- 0
    subsch[,-sch] <- 0
    schall <- adjmtx
    schall[-sch,] <- 0
    
    netsubsch    <- graph_from_adjacency_matrix(subsch,mode='upper')
    netsuball    <- graph_from_adjacency_matrix(schall,mode='upper')
    
    degmtx[,cts]   <- degree(netsubsch)
    degmtx[,cts+1] <- degree(netsuball)
        
    cts <- cts + 2
  
  }
  
  return(list(CRER_CD_LVL,degmtx))
  
}

#A wrapper to investigate variation in the degree distribution among terms
run_terms <- function(sr,sc)
{
  net <- create_term_network(sr,sc,TERM_CD_TEST='FA 2014',name='w_FA2014_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='WN 2014',name='w_WN2014_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='FA 2015',name='w_FA2015_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='WN 2015',name='w_WN2015_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='FA 2016',name='w_FA2016_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='WN 2016',name='w_WN2016_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='FA 2017',name='w_FA2017_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='WN 2017',name='w_WN2017_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='FA 2018',name='w_FA2018_ALL',weighted=TRUE)
  net <- create_term_network(sr,sc,TERM_CD_TEST='WN 2018',name='w_WN2018_ALL',weighted=TRUE)
  

}

#A wrapper to investigate variation in the degree distribution among terms
plot_terms <- function()
{
 dir <- '~/Desktop/'
 terms <- c('FA2014','WN2014','FA2015','WN2015','FA2016','WN2016',
            'FA2017','WN2017','FA2018','WN2018')
 nterms <- length(terms)
 ksteps <- mat.or.vec(5,nterms)
 meddeg <- mat.or.vec(1,nterms)
 
 for (i in 1:nterms)
 {
    datak <- read.delim(str_c('~/Desktop/ksteps_',terms[i],'_ALL.tab',sep=""))
    datao <- read.delim(str_c('~/Desktop/out_',terms[i],'_ALL.tab',sep=""))

    for (j in 1:5)
    {
      ksteps[1,i] <- mean(datak[,1],na.rm=TRUE)
      ksteps[2,i] <- mean(datak[,2],na.rm=TRUE)
      ksteps[3,i] <- mean(datak[,3],na.rm=TRUE)
      ksteps[4,i] <- mean(datak[,4],na.rm=TRUE)
      ksteps[5,i] <- mean(datak[,5],na.rm=TRUE)
    
      meddeg[1,i] <- median(datao$DEGREE,na.rm=TRUE)
    
    }
  
 }
 
 return(meddeg)
 
}

run_several_old <- function(sr,sc)
{
  aa1 <- create_term_network(sr,sc,TERM_CD_TEST=2010)
  aa2 <- create_term_network(sr,sc,weighted=TRUE,TERM_CD_TEST=2010)
  aa3 <- create_term_network(sr,sc,maxN=50,TERM_CD_TEST=2010)
  aa4 <- create_term_network(sr,sc,maxN=50,weighted=TRUE,TERM_CD_TEST=2010)
  
  aa5 <- create_term_network(sr,sc,TERM_CD_TEST=2210)
  aa6 <- create_term_network(sr,sc,weighted=TRUE,TERM_CD_TEST=2210)
  aa7 <- create_term_network(sr,sc,maxN=50,TERM_CD_TEST=2210)
  aa8 <- create_term_network(sr,sc,maxN=50,weighted=TRUE,TERM_CD_TEST=2210)
  
  
  write_tsv(aa1,'~/Desktop/degree_FA2014_ALL_CRSE_ID.txt')
  write_tsv(aa2,'~/Desktop/degree_FA2014_weighted_CRSE_ID.txt')
  write_tsv(aa3,'~/Desktop/degree_FA2014_50_CRSE_ID.txt')
  write_tsv(aa4,'~/Desktop/degree_FA2014_50_weighted_CRSE_ID.txt')
  write_tsv(aa5,'~/Desktop/degree_FA2018_ALL_CRSE_ID.txt')
  write_tsv(aa6,'~/Desktop/degree_FA2018_weighted_CRSE_ID.txt')
  write_tsv(aa7,'~/Desktop/degree_FA2018_50_CRSE_ID.txt')
  write_tsv(aa8,'~/Desktop/degree_FA2018_50_weighted_CRSE_ID.txt')
  
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
  
  pp <- temp1 %>% group_by(MAJOR,ACAD_LEVEL) %>% 
               summarize(N=n(),DALL=median(DEGREE_ALL),
                               DALLW=median(DEGREE_ALL_WEIGHTED),
                               D50=median(DEGREE_50),
                               D50W=median(DEGREE_50_WEIGHTED))
  
  #p <- temp1 %>% pivot_longer(c('DEGREE_ALL','DEGREE_ALL_WEIGHTED',
  #                      'DEGREE_50','DEGREE_50_WEIGHTED'),
  #                    names_to='DEG',values_to='DEGREE') %>% 
  #           ggplot(aes(x=DEGREE))+
  #           geom_histogram()+facet_wrap(~DEG)
  
  
  return(pp)
  
}



