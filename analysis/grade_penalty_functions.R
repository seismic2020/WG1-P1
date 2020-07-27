#source this before running "grade_penalty_wg1_p1.R"
#this contains all the functions for computing statisics and making plots
grade_penalty_functions <- function()
{
  
}

#this return a simple table of counts by various categories for a table passed to it.
  demographic_summary <- function(data)
  {
    #single variables
    N_FEMALE <- length(which(data$female == 1))
    N_MALE   <- length(which(data$female == 0))
    N_GEN_UNK<- length(which(data$female == 2))
    N_GEN_NA <- length(which(is.na(data$female)))
    
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
    
    N_TR_0  <- length(which(data$transfer == 0))
    N_TR_1  <- length(which(data$transfer == 1))
    N_TR_NA <- length(which(is.na(data$transfer )))
    
    N_INT_0  <- length(which(data$international == 0))
    N_INT_1  <- length(which(data$international == 1))
    N_INT_NA <- length(which(is.na(data$international )))
    
    out <- tibble(N_FEMALE,N_MALE,N_GEN_UNK,N_GEN_NA,
                  N_ETH_0,N_ETH_1,N_ETH_2,N_ETH_NA,
                  N_LI_0,N_LI_1,N_LI_NA,
                  N_FG_0,N_FG_1,N_FG_NA,
                  N_TR_0,N_TR_1,N_TR_NA,
                  N_INT_0,N_INT_1,N_INT_NA)
    return(out)
    
  }
  
  #this defines and adds the mutually exclusive coding to a table.
  #add the mutually exclusive coding
  add_ME_coding <- function(sr)
  {
    
    sr <- sr %>% mutate(opp='Other',opp_count=3)
    
    sr$opp[which(sr$firstgen == 1)] <- "FG"
    sr$opp[which(sr$ethniccode_cat == 1)] <- "URM"
    sr$opp[which(sr$lowincomflag == 1)] <- "LI"
    sr$opp[which(sr$female == 1)] <- "FEM"
    
    sr$opp[which(sr$firstgen == 1 & sr$lowincomflag == 1)]       <- "FG&LI"
    sr$opp[which(sr$ethniccode_cat == 1 & sr$lowincomflag == 1)] <- "URM&LI"
    sr$opp[which(sr$firstgen == 1 & sr$ethniccode_cat == 1)]     <- "URM&FG"
    sr$opp[which(sr$firstgen == 1 & sr$female == 1)]             <- "FG&FEM"
    sr$opp[which(sr$ethniccode_cat == 1 & sr$female == 1)]       <- "URM&FEM"
    sr$opp[which(sr$lowincomflag == 1 & sr$female == 1)]       <- "LI&FEM"
    
    
    sr$opp[which(sr$firstgen == 1 & sr$lowincomflag == 1 & sr$female == 1)]           <- "FG&LI&FEM"
    sr$opp[which(sr$ethniccode_cat == 1 & sr$lowincomflag == 1 & sr$female == 1)]     <- "URM&LI&FEM"
    sr$opp[which(sr$firstgen == 1 & sr$ethniccode_cat == 1 & sr$female == 1)]         <- "URM&FG&FEM"
    sr$opp[which(sr$firstgen == 1 & sr$ethniccode_cat == 1 & sr$lowincomflag == 1)]   <- "URM&FG&LI"
    
    sr$opp[which(sr$ethniccode_cat == 0 &
                   sr$lowincomflag == 0 &
                   sr$firstgen == 0 &   
                   sr$female   == 0)] <- "NotQuad"

    sr$opp[which(sr$ethniccode_cat == 1 &
                   sr$lowincomflag == 1 &
                   sr$firstgen == 1 & 
                   sr$female == 1)] <- "Quad"
    sr$opp_count[which(sr$opp == 'FG' | sr$opp == 'URM' | sr$opp == 'LI' | sr$opp == 'FEM')] <- 3
    sr$opp_count[which(sr$opp == 'FG&LI' | sr$opp == 'URM&LI' | sr$opp == 'URM&FG' | 
                       sr$opp == 'LI&FEM' | sr$opp == 'FG&FEM' | sr$opp == 'URM&FEM')] <- 2
    sr$opp_count[which(sr$opp == 'FG&LI&FEM' | sr$opp == 'URM&LI&FEM' | 
                         sr$opp == 'URM&FG&FEM' | sr$opp == 'URM&FG&LI')] <- 1
    
    sr$opp_count[which(sr$opp == 'Quad')] <- 0
    
    
    return(sr)
  }
  
  #this computes summary statistics for the mututally exclusivecoding.
  summarize_ME_statistics <- function(sr)
  {
    out1 <- compute_column_group_statistics(sr %>% group_by(opp))
    out2 <- compute_column_group_statistics(sr %>% group_by(opp_count))
    out2 <- out2 %>% mutate(opp=as.character(opp_count)) %>% select(-opp_count)
    out  <- bind_rows(out1,out2) 
  }
  
  #this computes and adds the non mutatually exclusive coding to a table.
  add_nonME_coding <- function(sr)
  {
    sr <- sr %>% mutate(ui_firstgen=0,ui_female=0,ui_urm=0,ui_li=0,
                        ui_fg_fem=0,ui_fem_urm=0,ui_fg_urm=0,ui_fg_li=0,ui_li_urm=0,ui_fem_li=0,
                        ui_fg_fem_li=0,ui_fg_fem_urm=0,ui_fg_urm_li=0,ui_urm_li_fem=0,
                        ui_quad=0,ui_none=0)
    sr$ui_firstgen[which(sr$firstgen == 1)] <- 1
    sr$ui_female[which(sr$female == 1)] <- 1
    sr$ui_urm[which(sr$ethniccode_cat == 1)] <- 1
    sr$ui_li[which(sr$lowincomflag == 1)] <- 1
    
    sr$ui_fg_fem[which(sr$firstgen == 1 & sr$female == 1)] <- 1
    sr$ui_fg_urm[which(sr$firstgen == 1 & sr$ethniccode_cat == 1)] <- 1
    sr$ui_fem_urm[which(sr$female == 1 & sr$ethniccode_cat == 1)] <- 1
    sr$ui_fg_li[which(sr$firstgen == 1 & sr$lowincomflag == 1)] <- 1
    sr$ui_li_urm[which(sr$lowincomflag == 1 & sr$ethniccode_cat == 1)] <- 1
    sr$ui_fem_li[which(sr$female == 1 & sr$lowincomflag == 1)] <- 1
    
    sr$ui_fg_fem_li[which(sr$firstgen == 1 & sr$female == 1 & sr$lowincomflag == 1)] <- 1
    sr$ui_fg_fem_urm[which(sr$firstgen == 1 & sr$ethniccode_cat == 1 & sr$female == 1)] <- 1
    sr$ui_fg_urm_li[which(sr$female == 1 & sr$ethniccode_cat == 1 & sr$lowincomflag == 1)] <- 1
    sr$ui_urm_li_fem[which(sr$ethniccode_cat == 1 & sr$lowincomflag == 1 & sr$female == 1)] <- 1
    
    
    sr$ui_quad[which(sr$female == 1 & sr$ethniccode_cat == 1 & 
                       sr$firstgen== 1 & sr$lowincomflag == 1)] <- 1  
    sr$ui_none[which(sr$female == 0 & sr$ethniccode_cat != 1 & sr$firstgen != 1 & sr$lowincomflag != 1)] <- 1
    
    return(sr)
    
  }
  
  #this compute the fiorini statistics.
  summarize_nonME_statistics <- function(sr)
  {
    UI_FG      <- compute_column_group_statistics(sr %>% group_by(ui_firstgen)) %>% slice(2) %>% 
      select(-ui_firstgen) %>%  mutate(GROUP='FG')
    UI_female  <- compute_column_group_statistics(sr %>% group_by(ui_female)) %>% slice(2) %>% 
      select(-ui_female) %>%  mutate(GROUP='FEM')
    UI_URM     <- compute_column_group_statistics(sr %>% group_by(ui_urm)) %>% slice(2)  %>% 
      select(-ui_urm) %>%  mutate(GROUP='URM')
    UI_LI      <- compute_column_group_statistics(sr %>% group_by(ui_li)) %>% slice(2)  %>% 
      select(-ui_li) %>%  mutate(GROUP='LI')
    
    UI_FG_GEN  <- compute_column_group_statistics(sr %>% group_by(ui_fg_fem)) %>% slice(2) %>% 
      select(-ui_fg_fem) %>%  mutate(GROUP='FG_FEM')
    UI_GEN_URM <- compute_column_group_statistics(sr %>% group_by(ui_fem_urm)) %>% slice(2) %>% 
      select(-ui_fem_urm) %>%  mutate(GROUP='FEM_URM')
    UI_FG_URM  <- compute_column_group_statistics(sr %>% group_by(ui_fg_urm)) %>% slice(2) %>% 
      select(-ui_fg_urm) %>%  mutate(GROUP='FG_URM')
    UI_FG_LI  <- compute_column_group_statistics(sr %>% group_by(ui_fg_li)) %>% slice(2) %>% 
      select(-ui_fg_li) %>%  mutate(GROUP='FG_LI')
    UI_LI_URM <- compute_column_group_statistics(sr %>% group_by(ui_li_urm)) %>% slice(2) %>% 
      select(-ui_li_urm) %>% mutate(GROUP='LI_URM')
    UI_GEN_LI  <- compute_column_group_statistics(sr %>% group_by(ui_fem_li)) %>% slice(2) %>% 
      select(-ui_fem_li) %>%  mutate(GROUP='GEN_LI')
    
    UI_FG_GEN_LI  <- compute_column_group_statistics(sr %>% group_by(ui_fg_fem_li)) %>% slice(2) %>% 
      select(-ui_fg_fem_li) %>%  mutate(GROUP='FG_FEM_LI')
    UI_FG_GEN_URM <- compute_column_group_statistics(sr %>% group_by(ui_fg_fem_urm)) %>% slice(2) %>% 
      select(-ui_fg_fem_urm) %>%  mutate(GROUP='FG_FEM_URM')
    UI_FG_URM_LI  <- compute_column_group_statistics(sr %>% group_by(ui_fg_urm_li)) %>% slice(2) %>% 
      select(-ui_fg_urm_li) %>%  mutate(GROUP='FG_URM_LI')
    UI_URM_LI_GEN  <- compute_column_group_statistics(sr %>% group_by(ui_urm_li_fem)) %>% slice(2) %>% 
      select(-ui_urm_li_fem) %>%  mutate(GROUP='URM_LI_GEN')
    
    
    UI_QUAD  <- compute_column_group_statistics(sr %>% group_by(ui_quad)) %>% slice(2) %>% 
      select(-ui_quad) %>%  mutate(GROUP='QUAD')
    UI_NONE    <- compute_column_group_statistics(sr %>% group_by(ui_none)) %>% slice(2) %>% 
      select(-ui_none) %>%  mutate(GROUP='NONE')
    
    
    out <- bind_rows(UI_FG,UI_female,UI_URM,UI_LI,
                     UI_FG_GEN,UI_GEN_URM,UI_FG_URM,
                     UI_FG_LI,UI_LI_URM,UI_GEN_LI,
                     UI_FG_GEN_LI,UI_FG_GEN_URM,UI_FG_URM_LI,UI_URM_LI_GEN,
                     UI_QUAD,UI_NONE)
    out <- out  %>% select(GROUP,N,mean_grade,sd_grade,med_grade,mad_grade,mn_ga,sd_ga,med_ga,mad_ga)
    return(out)
  }
  
  #called the molinaro/fiorini statistics routines
  #the group_by command has to be run before passing the data frame.
  compute_column_group_statistics <- function(data)
  {
    res   <- data %>% summarize(N=n(),mean_grade=mean(numgrade,na.rm=TRUE),sd_grade=sd(numgrade,na.rm=TRUE),
                                med_grade=median(numgrade,na.rm=TRUE),  mad_grade=mad(numgrade,na.rm=TRUE),
                                mn_ga=mean(numgrade-gpao,na.rm=TRUE),sd_ga=sd(numgrade-gpao,na.rm=TRUE),
                                med_ga=median(numgrade-gpao,na.rm=TRUE),mad_ga=mad(numgrade-gpao,na.rm=TRUE))
    return(res)
  }
  
  #make a grade-gpao plots the ethnicity categories
  make_eth_grade_gpao_plot <- function(data,nohist=TRUE)
  {
    if (nohist == FALSE)
    {
      print(names(data))
      p <- data %>% mutate(ETH=as.factor(ethniccode_cat),diff=numgrade-gpao) %>% 
        drop_na(ETH,diff) %>%
        ggplot(aes(x=numgrade-gpao,fill=ETH))+
        geom_histogram()+xlab('Grade-GPAO')
      print(p)  
    }
    
    bin  <- cut_width(data$gpao,0.33) #make bins in GPAO every 0.33 points for plotting
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin) %>% drop_na(numgrade,gpao)
    
    datap <- gpao.binned.urm(data)
    
    datap <- datap %>% mutate(ETH=as.character(ethniccode_cat)) %>% drop_na(ETH) 
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=ETH)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=ETH),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      coord_cartesian(ylim = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": ETHNIC_CODE"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
  }
  
  #make a grade-gpao plots the female categories
  make_female_grade_gpao_plot <- function(data,nohist=TRUE)
  {
    if (nohist == FALSE)
    {
      p <- data %>% mutate(FEMALE=as.factor(female),diff=numgrade-gpao) %>% 
        drop_na(FEMALE,diff) %>%
        ggplot(aes(x=numgrade-gpao,fill=FEMALE))+
        geom_histogram()+xlab('Grade-GPAO')
      print(p)  
    }
    bin  <- cut_width(data$gpao,0.33)
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin) %>% drop_na(numgrade,gpao)
    
    datap <- gpao.binned.gndr(data)
    datap <- datap %>% mutate(GNDR=as.character(female)) %>% drop_na(GNDR) 
    
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=GNDR)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=GNDR),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      coord_cartesian(ylim = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": female"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    
  }
  
  #make a grade-gpao plots the firstgen categories
  make_firstgen_grade_gpao_plot <- function(data,nohist=TRUE)
  {
    if (nohist == FALSE)
    {
      p <- data %>% mutate(FG=as.factor(firstgen),diff=numgrade-gpao) %>% 
        drop_na(FG,diff) %>%
        ggplot(aes(x=numgrade-gpao,fill=FG))+
        geom_histogram()+xlab('Grade-GPAO')
      print(p)  
    }
    
    bin  <- cut_width(data$gpao,0.33)
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin) %>% drop_na(numgrade,gpao) 
    
    datap <- gpao.binned.first.gen(data)
    datap <- datap %>% mutate(FG=as.character(firstgen)) %>% drop_na(FG) 
    
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=FG)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=FG),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      coord_cartesian(ylim = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": First Gen"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    
  }
  
  #make a grade-gpao plots the lowinc categories
  make_lowinc_grade_gpao_plot <- function(data,nohist=TRUE)
  {
    if (nohist == FALSE)
    {
      p <- data %>% mutate(LI=as.factor(lowincomflag),diff=numgrade-gpao) %>% 
                    drop_na(LI,diff) %>%
                    ggplot(aes(x=numgrade-gpao,fill=LI))+
                   geom_histogram()+xlab('Grade-GPAO')
      print(p)  
    }
    
    bin  <- cut_width(data$gpao,0.33)
    bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
    data <- data %>% mutate(bin=bin) %>% drop_na(numgrade,gpao)  
    
    datap <- gpao.binned.lowinc(data)
    datap <- datap %>% mutate(LI=as.character(lowincomflag))%>% drop_na(LI) 
    
    dodge <- position_dodge(width=0.2)
    
    p <- datap %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD),color=LI)) + geom_point(position=dodge) + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=LI),position=dodge, width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE,se=FALSE)+
      geom_abline(intercept=0,slope=1)+
      coord_cartesian(ylim = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(data$crs_name[1],": Low Income"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    
  }
  
  #compute statistics by bin/female
  gpao.binned.gndr <- function(data)
  {
    #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
    tt <- data %>% group_by(female,bin) %>% 
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
  
  