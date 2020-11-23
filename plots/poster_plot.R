require(tidyverse)
require(rstudioapi)

#use the RStudio API to set the working directory to the folder in which this script lives
#this will be used later for saving the output PDF and PNG files in the same folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read in the data, and compute the standard error while we're at it
modelout2 <- read_csv('modelout2.csv') %>%
  mutate(se_grade = sd_grade/sqrt(N)) %>%
  mutate(opp = case_when(
    opp == 'NotQuad' ~ '4',
    TRUE ~ opp
  ))

poster_plot <- function(ymin = 1.75, ymax = 3.25, ybreak = 0.25, sizemin = 4, sizemax = 8) {
  return(
    modelout2 %>%
      filter(opp %in% c('0','1','2','3', '4')) %>%
      mutate(INST = case_when(
        INST == 'A' ~ 'E',
        INST == 'B' ~ 'A',
        INST == 'E' ~ 'F',
        INST == 'F' ~ 'B',
        TRUE ~ INST
      )) %>%
      ggplot(aes(x = opp, y = mean_grade, group = INST, color = INST, size = N)) +
      geom_point(aes(size = N), position = position_dodge2(width = 0.2)) +
      geom_errorbar(aes(ymin = mean_grade - se_grade, ymax = mean_grade + se_grade), size = 1, color = "black", width = 0.2, position = position_dodge(width = 0.2), show.legend = FALSE) +
      geom_errorbar(aes(ymin = mean_grade - se_grade, ymax = mean_grade + se_grade), size = 1, alpha = 0.65, width = 0.2, position = position_dodge(width = 0.2), show.legend = FALSE) +
      scale_y_continuous(breaks = seq(ymin, ymax, by = ybreak)) +
      coord_cartesian(ylim = c(ymin, ymax)) +
      scale_size_continuous(range = c(sizemin, sizemax), breaks = seq(10000, 60000, by=10000)) +
      theme_minimal() +
      theme(
        text = element_text(size = 30),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align = 0.5
      ) +
      labs(x = 'Advantage Index', y = 'Mean Grade', title = 'Mean Grade by Advantage Index',
           color = 'Institution', group = 'Institution') +
      guides(color = guide_legend(override.aes = list(size=5)))
  )
}

advantage_plot <- poster_plot(ymin=1.8, ymax=3.2, ybreak=0.2)

#creates a PDF of the plot in the same directory as this script with the example plots
pdf('poster_plot.pdf', width = 12.85, height = 8.65)

advantage_plot

dev.off()

#creates a PNG of the plot in the same directory as this script with the example plots
ggsave('poster_plot.png', advantage_plot, width = 12, height = 8, dpi = 400)
