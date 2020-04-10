grade_anomaly_explore <- function(sr,sc,CRSE_SUBJECT='PHYSICS',CRSE_NUMBER = 140,CRSE_TERM=1.0,cluster_level=3.5)
{
  source("/Users/bkoester/Google Drive/code/Mellon/TOF/term_count.R")
  source('~/Box Sync/SEISMIC/SEISMIC/SEISMIC/basic_compute_pairwise.R')
  
  require(tidyverse)
  #sr <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/student_record.tsv")
  #sc <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/student_course.tsv")
  
  sc <- term_count(sr,sc)
  
  CRSE <- str_c(CRSE_SUBJECT,CRSE_NUMBER)
  
  print('computing grade anomaly statistics, course-taking similarities')
  jj <- single_course_explore(sr,sc,CRSE,CRSE_TERM=CRSE_TERM,PRE_TERM=CRSE_TERM,
                              CLPLOT=TRUE,cluster_level=cluster_level)
  
  clusters <- cluster_summary(jj)
  View(clusters[[1]])
  
  sim    <- quantile(jj[[1]]$SIM,probs=seq(1,9)/10)
  gndr   <- mat.or.vec(length(sim),1)
  gndrse <- gndr 
  full   <- gndr
  fullse <- gndr
  gpd    <- gndr
  gpdse  <- gndr
  
  for (i in 1:length(sim))
  {
    #print(sim[i])
    pp <- similarity_gender(jj,SIMCUT=sim[i])
    gndr[i]   <- pp[1]
    gndrse[i] <- pp[2]
    
  }
  
  #now get the regular results, no SIMILARITY or clustering
  rf <- summary(glm(CRSE_GRADE ~ EXCL_CRSE_GPA + STDNT_UNDREP_MNRTY_CD+
                      STDNT_GNDR_SHORT_DES+MAX_ACT_MATH_SCR+HS_GPA,
                    data=jj[[2]]))
  full[]   <- rf$coefficients[4,1]
  fullse[] <- rf$coefficients[4,2]
  
  #and then by cluster
  kk <- jj[[2]] %>% group_by(CL) %>% nest() %>% 
    mutate(model=map(data,reg_func))
  GNDR_BY_CL <- kk %>% select(CL,model)
  
  
  gpdt  <- jj[[2]] %>% group_by(STDNT_GNDR_SHORT_DES) %>% 
    summarize(AGA=mean(CRSE_GRADE-EXCL_CRSE_GPA),SE=sd(CRSE_GRADE-EXCL_CRSE_GPA)/sqrt(n()))
  gpd[]   <- gpdt$AGA[2]-gpdt$AGA[1]
  gpdse[] <- sqrt(gpdt$SE[1]^2+gpdt$SE[2]^2.)          
  
  aa <- tibble(sim,gndr,gndrse,full,fullse,gpd,gpdse)
  
  print(aa %>% ggplot(aes(x=sim,y=gndr))+geom_point()+
          geom_errorbar(aes(ymin=gndr-gndrse,ymax=gndr+gndrse))+
          ggtitle(str_c(CRSE,': YEAR = ',CRSE_TERM)))
  

  vec <- unlist(GNDR_BY_CL)
  GNDR_BY_CL <- ((matrix(vec,ncol=3,nrow=(length(vec)/3))))
  GNDR_BY_CL <- data.frame(GNDR_BY_CL)
  names(GNDR_BY_CL) <- c("CL","GNDR","SE")
  GNDR_BY_CL <- tibble(CL=GNDR_BY_CL$CL,GNDR=GNDR_BY_CL$GNDR,SE=GNDR_BY_CL$SE)
  
  print(GNDR_BY_CL %>% ggplot(aes(x=CL,y=GNDR))+geom_point()+
          geom_errorbar(aes(ymin=GNDR-SE,ymax=GNDR+SE))+
          ggtitle(str_c(CRSE,': YEAR = ',CRSE_TERM,', By CRSEs Taken Cluster')))
  
  return(GNDR_BY_CL)
  
}

#Compute clustering and pairwise similarity statistics for 
#assessing the effect of previous course load on AGA/GPAO.
single_course_explore <- function(sr,sc,CRSE='PHYSICS 140',CRSE_TERM=1.0,PRE_TERM=1.0,cluster_level=3.5,CLPLOT=FALSE)
{
  
  #1) find all students that took this course and get all their other courses
  ID <- sc %>% mutate(CRSE_ID_CD=str_c(SBJCT_CD,CATLG_NBR,sep=" ")) %>% 
               filter(CRSE_ID_CD == CRSE & TERM_CD > 1560 & TERMYR == CRSE_TERM & UNITS_ERND_NBR > 1.0) %>% 
               mutate(EXCL_CRSE_GPA=EXCL_CLASS_CUM_GPA,CRSE_GRADE=GRD_PNTS_PER_UNIT_NBR) %>% 
               select(STDNT_ID,EXCL_CRSE_GPA,CRSE_GRADE) %>% distinct()
  
  sc <- sc %>% right_join(ID,by='STDNT_ID') %>% filter(TERMYR <= PRE_TERM)
  
  
  sr <- sr %>% filter(FIRST_TERM_ATTND_CD >= 1560) %>% 
        right_join(ID,by='STDNT_ID') %>% drop_na()
  
  #2) get course similarities
  keep <- c('STDNT_GNDR_SHORT_DES','STDNT_UNDREP_MNRTY_CD','MAX_ACT_MATH_SCR',
            'HS_GPA','EXCL_CRSE_GPA','CRSE_GRADE')
  sim <- basic_compute_pairwise(sc,sr,keep_cols=keep,MIN_COURSE_SIZE=10,
                                CLUSTERING = TRUE, cluster_level = cluster_level,CLUSTER_DIAG=CLPLOT)
  
  #mas <- sr %>% left_join(sim[[2]])
  
  cl_counts <- sim[[2]] %>% left_join(sc)#,by='STDNT_ID')
  
  a1 <- cl_counts %>% filter(CL == 1) %>% mutate(CRSE=str_c(SBJCT_CD,CATLG_NBR)) %>% 
        mutate(N=n()) %>% group_by(CRSE) %>% summarize(ff=n()/N[1]) %>% 
        distinct(CRSE,.keep_all=TRUE)
 
  
  #sim[[2]] <- sim[[2]] %>% select(c(ID,PAIR,CL))
  out <- sim[[1]]  %>% select(c(ID,PAIR,SIM)) %>%  
                       left_join(sim[[2]],by=c("ID"="STDNT_ID")) %>% 
                       left_join(sim[[2]],by=c("PAIR"="STDNT_ID"))
  
  #temp <- out %>% filter(CL.x == CL.y & SIM <= 1.0) %>% distinct(PAIR)
  #cl_counts <- cl_counts %>% right_join(temp,by=c("STDNT_ID"="PAIR"))
  
  return(list(out,cl_counts))
  
}

#run a basic regression on pairs with SIMCUT similarity or better.
#cluster is included in the regression!
similarity_gender <- function(jj,SIMCUT=1.0)
{
  
  temp <- jj[[1]] %>% filter(CL.x == CL.y & SIM >= SIMCUT) %>% distinct(PAIR)
  cl_counts <- jj[[2]] %>% right_join(temp,by=c("STDNT_ID"="PAIR")) %>% 
               distinct(STDNT_ID,.keep_all=TRUE)
  
  r <- (summary(glm(CRSE_GRADE ~ EXCL_CRSE_GPA + STDNT_UNDREP_MNRTY_CD+
                STDNT_GNDR_SHORT_DES+MAX_ACT_MATH_SCR+HS_GPA+as.factor(CL),
              data=cl_counts)))
  
  #print(r)
  return(c(r$coefficients[4,1],r$coefficients[4,2]))
  
}

#returns top courses within clusters for diagnostics
cluster_summary <- function(jj)
{
  #print(jj[[1]] %>% filter(CL.x == CL.y) %>% group_by(CL.x) %>% summarize(mean(SIM)))
  ncl <- dim(jj[[2]] %>% group_by(CL) %>% tally())[1]
  jj[[2]] <- jj[[2]] %>% mutate(CRSE=str_c(SBJCT_CD,CATLG_NBR))
  
  
  for (i in 1:ncl)
  {
    tempF <- jj[[2]] %>% filter(CL == i & STDNT_GNDR_SHORT_DES == 'Female') %>%  
      mutate(N=n()) %>% group_by(CRSE) %>% summarize(FF=n()/N[1]) %>% arrange(desc(FF)) %>%
      filter(FF > 0.01)
    
    tempM <- jj[[2]] %>% filter(CL == i & STDNT_GNDR_SHORT_DES == 'Male') %>%  
      mutate(N=n()) %>% group_by(CRSE) %>% summarize(FF=n()/N[1]) %>% arrange(desc(FF)) %>%
      filter(FF > 0.01)
    
    tempA  <- jj[[2]] %>% filter(CL == i) %>%  
            mutate(N=n()) %>% group_by(CRSE) %>% summarize(FF=n()/N[1]) %>% arrange(desc(FF)) %>%
            filter(FF > 0.01)
    
    if (i == 1) {crseF <- tempF; crseM <- tempM;crseA <- tempA}
    if (i > 1)  
    {
      crseF <- crseF %>% full_join(tempF,by='CRSE')
      crseM <- crseM %>% full_join(tempM,by='CRSE')
      crseA <- crseA %>% full_join(tempA,by='CRSE')
    }
  }
  
  names(crseA) <- c('CRSE',str_c('cluster',seq(1,ncl,)))
  
  return(list(crseA,crseF,crseM))
}
  
#basic regression function (no cluster covariate)
#good for assesing gender effect within cluster, or gender effect controlled-for-stuff
reg_func <- function(df)
{
  f <- as.formula(CRSE_GRADE ~ EXCL_CRSE_GPA + STDNT_UNDREP_MNRTY_CD+
                    STDNT_GNDR_SHORT_DES+MAX_ACT_MATH_SCR+HS_GPA)
  
  r <- summary(glm(f,data=as.data.frame(df)))
  e <- grepl('GNDR',rownames(r$coefficients))
  est <- c(r$coefficients[e,1],r$coefficients[e,2])
  return(est)
  
} 
  
  