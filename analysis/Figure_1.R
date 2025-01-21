library("tidyverse")
library("cowplot")
library("magick")
library("ggtree")
library("here")
library("ape")

# ---- load data

tree <- read.tree(here("data","language_tree.nwk"))

language_labels <- read_csv("data/language_labels.csv") %>%
  rename(language = Language)

geo_dat <- read_csv("data/langs_and_countries.csv") %>%
  right_join(language_labels, by = "language")

# ---- visualize

# C-NC demo graphic

pdf_path <- here("figures", "C_NC_demo.pdf")

p0 <- image_read_pdf(pdf_path, density = 1200) %>%
  ggdraw() +
  draw_image(image_read_pdf(pdf_path, density = 1200))

# tree

p1 <- ggtree(tree, branch.length = "none") +
  geom_tiplab(size = 2.5, hjust = 0, nudge_x = 0.1) +  # Tip labels with colors
  geom_tippoint(aes(color = label), size = 3) +              # Colored dots for tips
  geom_text2(aes(subset = !isTip, label = label),
             vjust = -0.5, hjust = 1.1, size = 2.5) +  # Family labels moved upward
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 5, r = 0, b = 5, l = 0)) +
  coord_cartesian(xlim = c(0, 2.7))

# ggsave(here("figures", "phylogenetic_tree.png"), width = 6.5/1.618, height = 6.5, dpi = 1200)

# map

geo_dat <- geo_dat %>%
    mutate(Language_label = Language_name,
           Language_label = paste0(Language_label))

mp <- NULL

mapWorld <- borders("world",
                    # colour = "white",
                    colour="gray50", # with white background
                    fill="gray75") # create a layer of borders

mp <- ggplot(data=geo_dat) + mapWorld

p2 <- mp +
      geom_point(aes(longitude, latitude, color = Language_label), size=3) +
      # theme_black() +
      theme_void() +
      theme(text = element_text(size = 16),
            legend.position = "none",
            plot.background = element_rect(fill = "aliceblue", color = NA),
            plot.margin = margin(t = 5, r = 0, b = 5, l = 0)) +
      coord_cartesian(ylim = c(-55, 90), xlim = c(-160, 175))

# combined plots

plot_grid(
  p0,
  plot_grid(p1 + theme(legend.position = "none"), p2,                                    
            ncol = 2, rel_widths = c(0.33, 0.67),         
            labels = c("B", ""), align = "v"),
  ncol = 1,  
  rel_heights = c(1.2, .8),                            
  labels = c("A", "")                      
)

# ---- save Figure 1

width <- 6.5
golden_ratio <- 1.618
# height <- width / golden_ratio
height <- 7.5

ggsave(here("figures", "Figure_1.pdf"), width = width, height = height, dpi = 1200)
