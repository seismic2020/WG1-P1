#source this before running "grade_penalty_wg1_p1.R"
#this contains all the functions for computing statisics and making plots
grade_penalty_functions <- function()
{
  
}

#this return a simple table of counts by various categories for a table passed to it.
  demographic_summary <- function(data)
  {
    #single variables
    N_FEMALE <- length(which(data$gender == 1))
    N_MALE   <- length(which(data$gender == 0))
    N_GEN_UNK<- length(which(data$gender == 2))
    N_GEN_NA <- length(which(is.na(data$gender)))
    
    N_ETH_0  <- length(which(data$ethniccode_cat == 0))
    N_ETH_1  <- length(which(data$ethniccode_cat == 1))
    N_ETH_2  <- length(which(data$ethniccode_cat == 2))
    N_ETH_NA <- length(which(is.na(data$ethniccode_cat)))
    
    N_LI_0  <- length(which(data$lowincomflag == 0))
    N_LI_1  <- length(which(data$lowincomflag == 1))
    N_LI_NA <- length(which(is.na(data$lowincomflag)))
    
    N_FG_0  <- length(which(data$firstgen == 0))
    N_FG_1  <- length(which(data$firstgen == 1))
    N_FG_NA <- length(which(is.na(data$firstgen )))
    
    out <- tibble(N_FEMALE,N_MALE,N_GEN_UNK,N_GEN_NA,
                  N_ETH_0,N_ETH_1,N_ETH_2,N_ETH_NA,
                  N_LI_0,N_LI_1,N_LI_NA,
                  N_FG_0,N_FG_1,N_FG_NA)
    return(out)
    
  }
  
  #this defines and adds the molinaro coding to a table.
  #add the molinaro coding
  add_molinaro_coding <- function(sr)
  {
    
    sr <- sr %>% mutate(opp='Other',opp_count=3)
    
    sr$opp[which(sr$firstgen == 1)] <- "FG"
    sr$opp[which(sr$ethniccode_cat == 1)] <- "URM"
    sr$opp[which(sr$lowincomflag == 1)] <- "LI"
    
    sr$opp[which(sr$firstgen == 1 & sr$lowincomflag == 1)] <- "FG&LI"
    sr$opp[which(sr$ethniccode_cat == 1 & sr$lowincomflag == 1)] <- "URM&LI"
    sr$opp[which(sr$firstgen == 1 & sr$ethniccode_cat == 1)] <- "URM&FG"
    
    sr$opp[which(sr$ethniccode_cat == 0 &
                   sr$lowincomflag == 0 &
                   sr$firstgen == 0)] <- "NotTriple"
    
    sr$opp[which(sr$ethniccode_cat == 1 &
                   sr$lowincomflag == 1 &
                   sr$firstgen == 1)] <- "Triple"
    sr$opp_count[which(sr$opp == 'FG' | sr$opp == 'URM' | sr$opp == 'LI')] <- 2
    sr$opp_count[which(sr$opp == 'FG&LI' | sr$opp == 'URM&LI' | sr$opp == 'URM&FG')] <- 1
    sr$opp_count[which(sr$opp == 'Triple')] <- 0
    
    
    return(sr)
  }
  
  #this computes summary statistics for the molinaro coding.
  summarize_molinaro_statistics <- function(sr)
  {
    out1 <- compute_column_group_statistics(sr %>% group_by(opp))
    out2 <- compute_column_group_statistics(sr %>% group_by(opp_count))
    out2 <- out2 %>% mutate(opp=as.character(opp_count)) %>% select(-opp_count)
    out  <- bind_rows(out1,out2) 
  }
  
  #this computes and adds the fiorini coding to a table.
  add_fiorini_coding <- function(sr)
  {
    sr <- sr %>% mutate(ui_firstgen=0,ui_gender=0,ui_urm=0,
                        ui_fg_gen=0,ui_gen_urm=0,ui_fg_urm=0,ui_triple=0,ui_none=0)
    
    sr$ui_firstgen[which(sr$firstgen == 1)] <- 1
    sr$ui_gender[which(sr$gender == 1)] <- 1
    sr$ui_urm[which(sr$ethniccode_cat == 1)] <- 1
    sr$ui_fg_gen[which(sr$firstgen == 1 & sr$gender == 1)] <- 1
    sr$ui_fg_urm[which(sr$firstgen == 1 & sr$ethniccode_cat == 1)] <- 1
    sr$ui_gen_urm[which(sr$gender == 1 & sr$ethniccode_cat == 1)] <- 1
    sr$ui_triple[which(sr$gender == 1 & sr$ethniccode_cat == 1 & sr$firstgen== 1)] <- 1  
    sr$ui_none[which(sr$gender == 0 & sr$ethniccode_cat != 1 & sr$firstgen != 1)] <- 1
    
    return(sr)
    
  }
  
  #this compute the fiorini statistics.
  summarize_fiorini_statistics <- function(sr)
  {
    
    UI_FG      <- compute_column_group_statistics(sr %>% group_by(ui_firstgen)) %>% slice(2) %>% 
      select(-ui_firstgen) %>%  mutate(GROUP='FG')
    UI_GENDER  <- compute_column_group_statistics(sr %>% group_by(ui_gender)) %>% slice(2) %>% 
      select(-ui_gender) %>%  mutate(GROUP='FEM')
    UI_URM     <- compute_column_group_statistics(sr %>% group_by(ui_urm)) %>% slice(2)  %>% 
      select(-ui_urm) %>%  mutate(GROUP='URM')
    UI_FG_GEN  <- compute_column_group_statistics(sr %>% group_by(ui_fg_gen)) %>% slice(2) %>% 
      select(-ui_fg_gen) %>%  mutate(GROUP='FG_FEM')
    UI_GEN_URM <- compute_column_group_statistics(sr %>% group_by(ui_gen_urm)) %>% slice(2) %>% 
      select(-ui_gen_urm) %>%  mutate(GROUP='FEM_URM')
    UI_FG_URM  <- compute_column_group_statistics(sr %>% group_by(ui_fg_urm)) %>% slice(2) %>% 
      select(-ui_fg_urm) %>%  mutate(GROUP='FG_URM')
    UI_TRIPLE  <- compute_column_group_statistics(sr %>% group_by(ui_triple)) %>% slice(2) %>% 
      select(-ui_triple) %>%  mutate(GROUP='TRIPLE')
    UI_NONE    <- compute_column_group_statistics(sr %>% group_by(ui_none)) %>% slice(2) %>% 
      select(-ui_none) %>%  mutate(GROUP='NONE')
    
    
    out <- bind_rows(UI_FG,UI_GENDER,UI_URM,UI_FG_GEN,UI_GEN_URM,UI_FG_URM,UI_TRIPLE,UI_NONE)
    out <- out  %>% select(GROUP,N,mean_grade,sd_grade,med_grade,mad_grade,mn_ga,sd_ga,med_ga,mad_ga)
    return(out)
  }
  
  #called the molinaro/fiorini statistics routines
  #the group_by command has to be run before passing the data frame.
  compute_column_group_statistics <- function(data)
  {
    res   <- data %>% summarize(N=n(),mean_grade=mean(numgrade),sd_grade=sd(numgrade),
                                med_grade=median(numgrade),  mad_grade=mad(numgrade),
                                mn_ga=mean(numgrade-gpao),sd_ga=sd(numgrade-gpao),
                                med_ga=median(numgrade-gpao),mad_ga=mad(numgrade-gpao))
    return(res)
  }
  
  #make a grade-gpao plots the ethnicity categories
  make_eth_grade_gpao_plot <- function(data)
  {
    bin  <- cut_width(data$gpao,0.33) #make bins in GPAO every 0.33 points for plotting
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin)  
    
    datap <- gpao.binned.urm(data)
    
    datap <- datap %>% mutate(ETH=as.character(ethniccode_cat))
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=ETH)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=ETH),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": ETHNIC_CODE"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
  }
  
  #make a grade-gpao plots the gender categories
  make_gender_grade_gpao_plot <- function(data)
  {
    bin  <- cut_width(data$gpao,0.33)
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin)  
    
    datap <- gpao.binned.gndr(data)
    datap <- datap %>% mutate(GNDR=as.character(gender))
    
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=GNDR)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=GNDR),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": Gender"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    
  }
  
  #make a grade-gpao plots the firstgen categories
  make_firstgen_grade_gpao_plot <- function(data)
  {
    bin  <- cut_width(data$gpao,0.33)
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin)  
    
    datap <- gpao.binned.first.gen(data)
    datap <- datap %>% mutate(FG=as.character(firstgen))
    
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=FG)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=FG),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": First Gen"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    
  }
  
  #make a grade-gpao plots the lowinc categories
  make_lowinc_grade_gpao_plot <- function(data)
  {
    bin  <- cut_width(data$gpao,0.33)
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin)  
    
    datap <- gpao.binned.lowinc(data)
    datap <- datap %>% mutate(LI=as.character(lowincomflag))
    
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=LI)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=LI),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": Low Income"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    
  }
  
  #compute statistics by bin/gender
  gpao.binned.gndr <- function(data)
  {
    #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
    tt <- data %>% group_by(gender,bin) %>% 
      summarise(mnGRD=signif(mean(numgrade),3),seGRD=signif(sd(numgrade,na.rm=TRUE)/sqrt(n()),3))
    maxSE <- max(tt$seGRD,na.rm=TRUE)
    tt <- tt %>% replace_na(list(seGRD=maxSE))
    tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
    return(tt)
  }
  
  #compute statistics by bin/ethniicty
  gpao.binned.urm <- function(data)
  {
    tt <- data %>% group_by(ethniccode_cat,bin) %>% 
      summarise(mnGRD=signif(mean(numgrade),3),seGRD=signif(sd(numgrade,na.rm=TRUE)/sqrt(n()),3))
    maxSE <- max(tt$seGRD,na.rm=TRUE)
    tt <- tt %>% replace_na(list(seGRD=maxSE))
    tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
    return(tt)
  }
  
  #compute statistics by bin/firstgen
  gpao.binned.first.gen <- function(data)
  {
    #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
    tt <- data %>% group_by(firstgen,bin) %>% 
      summarise(mnGRD=signif(mean(numgrade),3),seGRD=signif(sd(numgrade,na.rm=TRUE)/sqrt(n()),3))
    maxSE <- max(tt$seGRD,na.rm=TRUE)
    tt <- tt %>% replace_na(list(seGRD=maxSE))
    tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
    return(tt)
  }
  
  #compute statistics by bin/lowinc
  gpao.binned.lowinc <- function(data)
  {
    #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
    tt <- data %>% group_by(lowincomflag,bin) %>% 
      summarise(mnGRD=signif(mean(numgrade),3),seGRD=signif(sd(numgrade,na.rm=TRUE)/sqrt(n()),3))
    maxSE <- max(tt$seGRD,na.rm=TRUE)
    tt <- tt %>% replace_na(list(seGRD=maxSE))
    tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
    return(tt)
  }
  
  