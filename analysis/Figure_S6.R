library('tidyverse')
library('ggplot2')
library('viridis')
library('scales')

# helper 

theme_black = function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_line(colour = "white"),  
      axis.text.x = element_text(color = "white",margin = margin(2, 2, 2, 2),size=8),  
      axis.text.y = element_text(color = "white",hjust=1,margin = margin(2, 2, 2, 2)),  
      axis.ticks = element_line(color = "white"),  
      axis.title.x = element_text(size = base_size, color = "white"),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90,margin = margin(0, 10, 0, 0)),  
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_blank(),  
      panel.border = element_blank(),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
#       panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "black", color = "white"),  
      strip.text.x = element_text( color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

# load

context_dat <- read.csv("data/context_counts_subject_level.csv")

# tidy

context_dat$context <- gsub("HomeBook-reading","Home: book reading",context_dat$context)
context_dat$context <- gsub("HomeInterview/Unstructured","Home: interview/unstructured",context_dat$context)
context_dat$context <- gsub("HomeNaN","Home: unreported",context_dat$context)
context_dat$context <- gsub("HomeOther","Home: other",context_dat$context)
context_dat$context <- gsub("HomeUnstructured","Home: unstructured",context_dat$context)
context_dat$context <- gsub("LabOther","Lab: other",context_dat$context)
context_dat$context <- gsub("LabTabletop play","Lab: tabletop play",context_dat$context)
context_dat$context <- gsub("LabInterview/Unstructured","Lab: interview/unstructured",context_dat$context)
context_dat$context <- gsub("LabUnstructured","Lab: unstructured",context_dat$context)
context_dat$context <- gsub("NaNNaN","Unreported",context_dat$context)
context_dat$context <- gsub("OtherUnstructured","Other: unstructured",context_dat$context)

unique(context_dat$context)

context_summary <- context_dat %>%
                    group_by(Language_name) %>%
                    mutate(percent = count / sum(count) * 100)

context_summary[order(context_summary$Language_name),]

context_summary$context <- factor(context_summary$context, levels = c("Home: book reading",
                                                                      "Home: interview/unstructured",
                                                                      "Home: unstructured",
                                                                      "Home: other",
                                                                      "Home: unreported",
                                                                      "Lab: tabletop play",
                                                                      "Lab: interview/unstructured",
                                                                      "Lab: unstructured",
                                                                      "Lab: other",
                                                                      "Other: unstructured",
                                                                      "Unreported"))

context_summary <- context_summary %>% mutate(row = case_when(
                    Language_name=="Croatian" ~ 0,
                    Language_name=="English" ~ 0,
                    Language_name=="Estonian" ~ 0,
                    Language_name=="French" ~ 0,
                    Language_name=="German" ~ 0,
                    Language_name=="Japanese" ~ 0,
                    Language_name=="Korean" ~ 0,
                    # Language_name=="Mandarin" ~ 1,
                    Language_name=="Norwegian" ~ 1,
                    Language_name=="Persian" ~ 1,
                    # Language_name=="Polish" ~ 1,
                    Language_name=="Portuguese" ~ 1,
                    Language_name=="Spanish" ~ 1,
                    Language_name=="Swedish" ~ 1,
                    Language_name=="Tseltal" ~ 1)) %>%
                filter(Language_name!="Polish" & Language_name!="Mandarin")

# Assuming context_summary is your data frame
context_summary <- rbind(context_summary,
                         data.frame(X=19,
                                    Language_name = " ",
                                    percent = 0, 
                                    context = "Unreported",  # or any context, won't be shown
                                    row = 1))

context_summary <- context_summary %>%
  mutate(context = factor(context, 
                          levels = c("Home: book reading",
                                     "Home: interview/unstructured",
                                     "Home: unstructured",
                                     "Home: other",
                                     "Home: unreported",
                                     "Lab: interview/unstructured",
                                     "Lab: unstructured",
                                     "Other: unstructured",
                                     "Unreported")),
         Language_name = factor(Language_name, 
                                levels = c("Croatian", "English", "Estonian", 
                                           "French", "German", "Japanese", 
                                           "Korean", "Norwegian", "Persian", 
                                           "Portuguese", "Spanish", "Swedish", 
                                           "Tseltal", " ")))

context_summary

# viz

p <- ggplot() +
     geom_bar(aes(y = percent, x = Language_name, fill = context),
              data = context_summary,
              stat="identity") +
    facet_wrap(~row, ncol=1, scales='free', drop = FALSE) +
    scale_fill_manual(values = c(
                       "Home: book reading" = "#5E4FA2",
                       "Home: interview/unstructured" = "#3388BD",
                       "Home: unstructured" = "#66C2A4",
                       "Home: other" = "#ABDDA4",
                       "Home: unreported" = "#E6F598",
                       "Lab: interview/unstructured" = "#FEE08B",
                       "Lab: unstructured" = "#FDAE61",
                       "Other: unstructured" = "#F46D43",
                       "Unreported" = "#D53E4F"),
                      name = "Recording context") +
    labs(y = "Percentage of transcripts",
         x = "") +
    theme_classic() +
    # theme_black() +
    scale_x_discrete(position = "top") +
    theme(text = element_text(size=11),
          axis.text.x = element_text(colour="black",vjust = 0.5, hjust = 0.5),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(colour="black"),
          strip.text.x = element_blank(),
          legend.key.size = unit(4, 'mm'),
          legend.background = element_rect(fill="white",
                                           size=0, linetype="dotted",
                                           colour = "white"))

# ggsave("../figures/Figure_2_black.pdf", width = 7, height = 2.4,dpi=1200)
ggsave("figures/Figure_S6.pdf", width = 7, height = 2.4,dpi=1200)
