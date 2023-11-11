library("RColorBrewer")
library('tidyverse')
library('ggplot2')
library('scales')

context_dat <- read.csv("data/context_counts_subject_level.csv")

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
                    Language_name=="Mandarin" ~ 1,
                    Language_name=="Norwegian" ~ 1,
                    Language_name=="Persian" ~ 1,
                    Language_name=="Polish" ~ 1,
                    Language_name=="Portuguese" ~ 1,
                    Language_name=="Spanish" ~ 1,
                    Language_name=="Swedish" ~ 1))

num_colors <- length(unique(context_summary$context))
spectral_colors <- brewer.pal(num_colors + 1, "Spectral")
reversed_palette <- rev(spectral_colors)

p <- ggplot() +
        geom_bar(aes(y = percent, x = Language_name, fill = context),
                data = context_summary,
                stat="identity") +
        facet_wrap(~row, ncol=1, scales='free') +
        # scale_fill_brewer(name = "Recording context", palette = "Spectral", direction = -1) +
        scale_fill_manual(name = "Recording context", values = reversed_palette) +
        labs(y = "Percentage of transcripts",
            x = "") +
        theme_classic() +
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

ggsave("figures/Figure_S5.pdf", width = 7, height = 2.4, dpi=1200)
