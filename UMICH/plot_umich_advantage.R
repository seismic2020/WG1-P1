#In these plots, main argument is a list based on the SEISMIC formatting of variables.
#data <- list(sr,sc)
#Note that most of these are adapted to UMich courses.
# source('~/Google Drive/code/SEISMIC/SEISMIC2020/WG1-P1/analysis/create_intro_bio_layer.R')
# source('~/Google Drive/code/SEISMIC/SEISMIC2020/WG1-P1/analysis/create_all_stem_layer.R')
# source('~/Google Drive/code/SEISMIC/SEISMIC2020/WG1-P1/analysis/grade_penalty_wg1_p1.R')
# source('~/Google Drive/code/SEISMIC/SEISMIC2020/WG1-P1/analysis/grade_penalty_functions.R')
#

###########

plot_umich_advantage <- function(data)
{
  kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=c('BIOLOGY 171'))
  p1 <- kk[[2]] %>% filter(grepl('[A-Z]&.[A-Z]&',opp)) %>% ggplot(aes(x=opp,y=mean_grade))+geom_point()+
                     geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),ymax=mean_grade+sd_grade/sqrt(N)))+
                     ggtitle('BIOLOGY 171')+ylim(1.25,3.5)
  
  
  kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=c('PHYSICS 135','PHYSICS 140'))
  p2 <- kk[[2]] %>% filter(grepl('[A-Z]&.[A-Z]&',opp)) %>% ggplot(aes(x=opp,y=mean_grade))+geom_point()+
    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),ymax=mean_grade+sd_grade/sqrt(N)))+
    ggtitle('PHYSICS 135/140')+ylim(1.25,3.5)
  
  
  kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=c('CHEM 130'))
  p3 <- kk[[2]] %>% filter(grepl('[A-Z]&.[A-Z]&',opp)) %>% ggplot(aes(x=opp,y=mean_grade))+geom_point()+
    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),ymax=mean_grade+sd_grade/sqrt(N)))+
    ggtitle('CHEM 130')+ylim(1.25,3.5)
  
  
  kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=c('MATH 105','MATH 115'))
  p4 <- kk[[2]] %>% filter(grepl('[A-Z]&.[A-Z]&',opp)) %>% ggplot(aes(x=opp,y=mean_grade))+geom_point()+
    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),ymax=mean_grade+sd_grade/sqrt(N)))+
    ggtitle('MATH 105/115')+ylim(1.25,3.5)
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
}

#run the intersection plots for all large courses.
big_wrapper <- function(data)
{
  #qq <- plot_example_adv(data,clist=c('MATH 115'),title='ALL_STEM, FIRST YEAR',ALL_STEM=TRUE)
  
  data[[2]] <- data[[2]] %>% filter(crs_termcd >= 1760) %>% 
               group_by(crs_name) %>% mutate(N=n()) %>% filter(N > 920)
  
  data[[2]] <- data[[2]] %>% filter(crs_component == 'LEC' & crs_termcd >= 1760)
  sc_count  <- data[[2]] %>% group_by(crs_name) %>% tally() %>% top_n(250) %>% ungroup()
  data[[2]]        <- data[[2]] %>% left_join(sc_count)
  
  #ncrse <- length(!duplicated(data[[2]]$crs_name))
  clist <- data[[2]]$crs_name[!duplicated(data[[2]]$crs_name)]
  clist <- clist[!is.na(clist)]
  ncrse <- length(clist)
  
  for (i in 1:ncrse)
  {
    print(clist[i])
    if (!is.na(clist[i]))
    {
      
     pdf(str_c('~/Desktop/advantage_plots/',clist[i],'.pdf',sep=""),width=11,height=7)
       qq <- plot_example_adv(data,clist=clist[i],title=clist[i])
     dev.off()
    
      dd <- class_setup_heatmap(data,clist=clist[i])
      dd <- dd %>% select(opp,mean_grade,GROUP,N) %>% mutate(COURSE = clist[i])
    
      if (i == 1){temp <- dd}
      if (i > 1){temp <- temp %>% bind_rows(dd)}
    }
  }
  
  return(temp)
  
}


plot_example_adv <- function(data,clist=c('CHEM 130'),title='NONE',ALL_STEM=FALSE)
{

  kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=clist)
  
  if (ALL_STEM == TRUE){kk <- create_all_stem_layer(data[[1]],data[[2]])}
  #kk <- setup_and_order_model_2(kk)
  kku <- setup_and_order_model_urm(kk)
  kkf <- setup_and_order_model_fg(kk)
  kkl <- setup_and_order_model_li(kk)
  kkfem <- setup_and_order_model_fem(kk)
  
  p1 <- kku[[2]] %>% 
                    ggplot(aes(x=opp,y=mean_grade,color=GROUP,width=0.2,label=NAME))+geom_point()+
                    geom_text(hjust=-0.1,size=2.5)+
                    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),
                                      ymax=mean_grade+sd_grade/sqrt(N)))+
                    ggtitle(title)+scale_y_continuous(limits=c(2,4),oob=scales::squish)
  print(p1)
  
  p1 <- kkf[[2]] %>% 
    ggplot(aes(x=opp,y=mean_grade,color=GROUP,width=0.2,label=NAME))+geom_point()+
    geom_text(hjust=-0.1,size=2.5)+
    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),
                      ymax=mean_grade+sd_grade/sqrt(N)))+
    ggtitle(title)+scale_y_continuous(limits=c(2,4),oob=scales::squish)
  print(p1)
  
  p1 <- kkl[[2]] %>% 
    ggplot(aes(x=opp,y=mean_grade,color=GROUP,width=0.2,label=NAME))+geom_point()+
    geom_text(hjust=-0.1,size=2.5)+
    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),
                      ymax=mean_grade+sd_grade/sqrt(N)))+
    ggtitle(title)+scale_y_continuous(limits=c(2,4),oob=scales::squish)
  print(p1)
  
  p1 <- kkfem[[2]] %>% 
    ggplot(aes(x=opp,y=mean_grade,color=GROUP,width=0.2,label=NAME))+geom_point()+
    geom_text(hjust=-0.1,size=2.5)+
    geom_errorbar(aes(ymin=mean_grade-sd_grade/sqrt(N),
                      ymax=mean_grade+sd_grade/sqrt(N)))+
    ggtitle(title)+scale_y_continuous(limits=c(2,4),oob=scales::squish)
  print(p1)
  
  
}

setup_and_order_model_1 <- function(kk)
{
  kk[[2]] <- kk[[2]] %>% filter(opp =='URM' | opp == 'URM&FEM' | 
                                opp == 'URM&LI&FEM' | opp == 'Quad' |
                                opp =='0' | opp == '1' | 
                                opp == '2' | opp == '3')  
  kk[[2]] <- kk[[2]] %>% mutate(GROUP='Full Index')
  
  kk[[2]]$GROUP[which(nchar(kk[[2]]$opp) > 1)] <- 'MODEL'
  
  kk[[2]]$opp[which(kk[[2]]$opp == '3')] <- 'URM'
  kk[[2]]$opp[which(kk[[2]]$opp == '2')] <- 'URM&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '1')] <- 'URM&LI&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '0')] <- 'Quad'
  
  kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c("URM", 'URM&FEM','URM&LI&FEM','Quad'))
  
  
  
  return(kk)
}

setup_and_order_model_2 <- function(kk)
{
  
  kk[[2]] <- kk[[2]] %>% filter(opp != 'Other' & opp != 'NotQuad' & opp != 4)  
  kk[[2]] <- kk[[2]] %>% mutate(GROUP='non-model')
  kk[[2]] <- kk[[2]] %>% mutate(NAME=opp)
  
  #set the group names
  #these are what we actually want to test
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'URM')] <- 'MODEL'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'URM&FEM')] <- 'MODEL'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'URM&LI&FEM')] <- 'MODEL'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'Quad')] <- 'MODEL'
  
  kk[[2]]$GROUP[which(kk[[2]]$opp == '3')] <- 'Full Index'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '2')] <- 'Full Index'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '1')] <- 'Full Index'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '0')] <- 'Full Index'
  
  #label the others for ordering
  kk[[2]]$opp[which(kk[[2]]$opp == '3')] <- 3 #'URM'
  kk[[2]]$opp[which(kk[[2]]$opp == '2')] <- 2 #'URM&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '1')] <- 1 #'URM&LI&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '0')] <- 0 #'Quad'
  
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) <= 3 & nchar(kk[[2]]$NAME) > 1)] <- 3 #'URM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 3 & nchar(kk[[2]]$opp) <= 7)] <- 2  #'URM&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 7)] <- 1  #'URM&LI&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) == 4)] <- 0 #'Quad'
  
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c('3', '2','1','0'))
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c("URM", 'URM&FEM','URM&LI&FEM','Quad'))
  kk[[2]] <- kk[[2]] %>% mutate(opp=as.numeric(opp))
  
  kk[[2]]$opp[kk[[2]]$GROUP == 'non-model'] <- kk[[2]]$opp[kk[[2]]$GROUP == 'non-model']+0.25
  kk[[2]]$opp[kk[[2]]$GROUP == 'Full Index'] <- kk[[2]]$opp[kk[[2]]$GROUP == 'Full Index']-0.1
  
  return(kk)
}

setup_and_order_model_urm <- function(kk)
{
  kk[[2]] <- kk[[2]] %>% filter(opp != 'Other' & opp != 'NotQuad' & opp != 4)  
  kk[[2]] <- kk[[2]] %>% mutate(GROUP='non-model')
  kk[[2]] <- kk[[2]] %>% mutate(NAME=opp)
  
  #set the group names
  #these are what we actually want to test
  kk[[2]]$GROUP[which(grepl('URM',kk[[2]]$opp))] <- 'URM MODEL'
  kk[[2]]$GROUP[which(!grepl('URM',kk[[2]]$opp))] <- 'NONE'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'Quad')] <- 'URM MODEL'
  
  
  kk[[2]]$GROUP[which(kk[[2]]$opp == '3')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '2')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '1')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '0')] <- 'AI'
  kk[[2]] <- kk[[2]] %>% filter(GROUP != 'NONE')
  
  
  #label the others for ordering
  kk[[2]]$opp[which(kk[[2]]$opp == '3')] <- 3 #'URM'
  kk[[2]]$opp[which(kk[[2]]$opp == '2')] <- 2 #'URM&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '1')] <- 1 #'URM&LI&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '0')] <- 0 #'Quad'
  
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) <= 3 & nchar(kk[[2]]$NAME) > 1)] <- 3 #'URM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 3 & nchar(kk[[2]]$opp) <= 7)] <- 2  #'URM&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 7)] <- 1  #'URM&LI&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) == 4)] <- 0 #'Quad'
  
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c('3', '2','1','0'))
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c("URM", 'URM&FEM','URM&LI&FEM','Quad'))
  kk[[2]] <- kk[[2]] %>% mutate(opp=as.numeric(opp))
  kk[[2]]$opp[kk[[2]]$GROUP == 'AI'] <- kk[[2]]$opp[kk[[2]]$GROUP == 'AI']-0.1
  
  return(kk)
  
}

setup_and_order_model_fg <- function(kk)
{
  kk[[2]] <- kk[[2]] %>% filter(opp != 'Other' & opp != 'NotQuad' & opp != 4)  
  kk[[2]] <- kk[[2]] %>% mutate(GROUP='non-model')
  kk[[2]] <- kk[[2]] %>% mutate(NAME=opp)
  
  #set the group names
  #these are what we actually want to test
  kk[[2]]$GROUP[which(grepl('FG',kk[[2]]$opp))] <- 'FG MODEL'
  kk[[2]]$GROUP[which(!grepl('FG',kk[[2]]$opp))] <- 'NONE'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'Quad')] <- 'FG MODEL'
  
  
  kk[[2]]$GROUP[which(kk[[2]]$opp == '3')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '2')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '1')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '0')] <- 'AI'
  kk[[2]] <- kk[[2]] %>% filter(GROUP != 'NONE')
  
  
  #label the others for ordering
  kk[[2]]$opp[which(kk[[2]]$opp == '3')] <- 3 #'URM'
  kk[[2]]$opp[which(kk[[2]]$opp == '2')] <- 2 #'URM&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '1')] <- 1 #'URM&LI&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '0')] <- 0 #'Quad'
  
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) <= 3 & nchar(kk[[2]]$NAME) > 1)] <- 3 #'URM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 3 & nchar(kk[[2]]$opp) <= 7)] <- 2  #'URM&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 7)] <- 1  #'URM&LI&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) == 4)] <- 0 #'Quad'
  
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c('3', '2','1','0'))
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c("URM", 'URM&FEM','URM&LI&FEM','Quad'))
  kk[[2]] <- kk[[2]] %>% mutate(opp=as.numeric(opp))
  kk[[2]]$opp[kk[[2]]$GROUP == 'AI'] <- kk[[2]]$opp[kk[[2]]$GROUP == 'AI']-0.1
  
  return(kk)
  
}

setup_and_order_model_fem <- function(kk)
{
  kk[[2]] <- kk[[2]] %>% filter(opp != 'Other' & opp != 'NotQuad' & opp != 4)  
  kk[[2]] <- kk[[2]] %>% mutate(GROUP='non-model')
  kk[[2]] <- kk[[2]] %>% mutate(NAME=opp)
  
  #set the group names
  #these are what we actually want to test
  kk[[2]]$GROUP[which(grepl('FEM',kk[[2]]$opp))] <- 'FEM MODEL'
  kk[[2]]$GROUP[which(!grepl('FEM',kk[[2]]$opp))] <- 'NONE'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'Quad')] <- 'FEM MODEL'
  
  
  kk[[2]]$GROUP[which(kk[[2]]$opp == '3')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '2')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '1')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '0')] <- 'AI'
  kk[[2]] <- kk[[2]] %>% filter(GROUP != 'NONE')
  
  
  #label the others for ordering
  kk[[2]]$opp[which(kk[[2]]$opp == '3')] <- 3 #'URM'
  kk[[2]]$opp[which(kk[[2]]$opp == '2')] <- 2 #'URM&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '1')] <- 1 #'URM&LI&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '0')] <- 0 #'Quad'
  
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) <= 3 & nchar(kk[[2]]$NAME) > 1)] <- 3 #'URM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 3 & nchar(kk[[2]]$opp) <= 7)] <- 2  #'URM&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 7)] <- 1  #'URM&LI&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) == 4)] <- 0 #'Quad'
  
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c('3', '2','1','0'))
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c("URM", 'URM&FEM','URM&LI&FEM','Quad'))
  kk[[2]] <- kk[[2]] %>% mutate(opp=as.numeric(opp))
  kk[[2]]$opp[kk[[2]]$GROUP == 'AI'] <- kk[[2]]$opp[kk[[2]]$GROUP == 'AI']-0.1
  
  return(kk)
  
}

setup_and_order_model_li <- function(kk)
{
  kk[[2]] <- kk[[2]] %>% filter(opp != 'Other' & opp != 'NotQuad' & opp != 4)  
  kk[[2]] <- kk[[2]] %>% mutate(GROUP='non-model')
  kk[[2]] <- kk[[2]] %>% mutate(NAME=opp)
  
  #set the group names
  #these are what we actually want to test
  kk[[2]]$GROUP[which(grepl('LI',kk[[2]]$opp))] <- 'LI MODEL'
  kk[[2]]$GROUP[which(!grepl('LI',kk[[2]]$opp))] <- 'NONE'
  kk[[2]]$GROUP[which(kk[[2]]$opp== 'Quad')] <- 'LI MODEL'
  
  
  kk[[2]]$GROUP[which(kk[[2]]$opp == '3')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '2')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '1')] <- 'AI'
  kk[[2]]$GROUP[which(kk[[2]]$opp == '0')] <- 'AI'
  kk[[2]] <- kk[[2]] %>% filter(GROUP != 'NONE')
  
  
  #label the others for ordering
  kk[[2]]$opp[which(kk[[2]]$opp == '3')] <- 3 #'URM'
  kk[[2]]$opp[which(kk[[2]]$opp == '2')] <- 2 #'URM&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '1')] <- 1 #'URM&LI&FEM'
  kk[[2]]$opp[which(kk[[2]]$opp == '0')] <- 0 #'Quad'
  
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) <= 3 & nchar(kk[[2]]$NAME) > 1)] <- 3 #'URM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 3 & nchar(kk[[2]]$opp) <= 7)] <- 2  #'URM&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) > 7)] <- 1  #'URM&LI&FEM'
  kk[[2]]$opp[which(nchar(kk[[2]]$NAME) == 4)] <- 0 #'Quad'
  
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c('3', '2','1','0'))
  #kk[[2]]$opp <- factor(kk[[2]]$opp,levels = c("URM", 'URM&FEM','URM&LI&FEM','Quad'))
  kk[[2]] <- kk[[2]] %>% mutate(opp=as.numeric(opp))
  kk[[2]]$opp[kk[[2]]$GROUP == 'AI'] <- kk[[2]]$opp[kk[[2]]$GROUP == 'AI']-0.1
  
  return(kk)
  
}

class_setup_heatmap <- function(data,clist='BIOLOGY 171')
{
  data[[1]] <- data[[1]] %>% filter(international == 0)
  kk <- create_intro_bio_layer(data[[1]],data[[2]],BIOCOURSE=clist)
  kk[[2]] <- kk[[2]] %>% mutate(GROUP=case_when(opp == 'FEM' ~ '3', 
                                          opp == 'URM' ~ '3',
                                          opp == 'FG' ~ '3',
                                          opp == 'LI' ~ '3',
                                          opp == 'FG&FEM' ~ '2', 
                                          opp == 'URM&FEM' ~ '2',
                                          opp == 'FG&LI' ~ '2',
                                          opp == 'LI&FEM' ~ '2',
                                          opp == 'URM&LI' ~ '2', 
                                          opp == 'URM&FG' ~ '2',
                                          opp == 'FG&LI&FEM' ~ '1',
                                          opp == 'URM&FG&FEM' ~ '1',
                                          opp == 'URM&FG&LI' ~ '1',
                                          opp == 'URM&LI&FEM' ~ '1',
                                          opp == 'Quad' ~ '0',
                                          opp == '0' ~ '0',
                                          opp == '1' ~ '1',
                                          opp == '2' ~ '2',
                                          opp == '3' ~ '3'))
                                          
             
  #ll <- data[[2]] %>% select(opp,GROUP,mean_grade) %>% arrange(GROUP) %>% drop_na() %>% 
  #  pivot_wider(names_from=GROUP,values_from=mean_grade)
  #ll <- as.matrix(ll)
  #rnames <- ll$opp[,1]
  #cnames <- names(ll[1,-1])
  #ll <- as.matrix(ll[-1,-1])            
  
  return(kk[[2]])                                        
                                          
}

#this takes output from big_wrapper for analysis
opp_heatmap <- function(data)
{
  library(gplots)
  library(heatmaply)
  #data$mean_grade[which(data$N < 10)] <- NA
  st <- data %>% select(COURSE,opp,mean_grade) %>% pivot_wider(names_from='COURSE',values_from='mean_grade')
  t  <- pull(st,opp)
  st <- st %>% select(-opp)
  jj <- data.matrix(st,rownames.force = TRUE) 
  row.names(jj) <- t
  st <- jj[c(1:17),] 
  
  #create a cellnote matrix with N's instead of the deault means.
  nt <- data %>% select(COURSE,opp,N) %>% pivot_wider(names_from='COURSE',values_from='N')
  
  t  <- pull(nt,opp)
  nt <- nt %>% select(-opp)
  jj <- data.matrix(nt,rownames.force = TRUE) 
  row.names(jj) <- t
  nt <- jj[c(1:17),] 
  
  #i think this needs to be run at the command line to work.
  #heatmaply(st)
  
  
  return(list(st,nt))
}