library("kableExtra")
library("tidyverse")
library("cowplot")
library("repr")

# ---- load data

language_labels <- read_csv("data/language_labels.csv") %>%
  rename(language = Language)

geo_dat <- read_csv("data/langs_and_countries.csv") %>%
  right_join(language_labels, by = "language")

# ---- visualize

options(repr.plot.width=11.7, repr.plot.height=6.2)

geo_dat <- geo_dat %>%
    mutate(
      #      lang_fam_abbrv = recode(Language_Family,
      #                              `Indo-European` = "I-E",
      #                              `Uralic` = "U",
      #                              `Japonic` = "J",
      #                              `Koreanic` = "K",
      #                              `Sino-Tibetan` = "S-T"),
           Language_label = Language_name,
           Language_label = paste0(Language_label)) #," (",lang_fam_abbrv,")"))

mp <- NULL

mapWorld <- borders("world",
                    # colour = "white",
                    colour="gray50", # with white background
                    fill="gray75") # create a layer of borders

mp <- ggplot(data=geo_dat) + mapWorld

mp <- mp +
      geom_point(aes(longitude, latitude, color = Language_label), size=3) +
      # theme_black() +
      theme_void() +
      theme(text = element_text(size=16),
            #plot.background = element_rect(fill = "black"),
            legend.title = element_blank(),
            legend.position = c(0.91, 0.55),
            legend.background = element_rect(fill=alpha("white",0.90),
                                             size=0, linetype="dotted",
                                             colour = "white"),
            legend.text=element_text(size=20,colour="black"))
            
ggsave("figures/geo_dat.pdf", width = 11.7, height = 6.2,dpi = 1200, scale = 1.2)
