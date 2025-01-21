library('tidyverse')
library('patchwork')
library('cowplot')
library('ggplot2')
library('repr')

rand_lex_sumstats <- read_csv("data/rand_lex_sumstats.csv")
rand_mlu_sumstats <- read_csv("data/rand_mlu_sumstats.csv")
rand_swu_sumstats <- read_csv("data/rand_swu_sumstats.csv")

# ---- Figure 2

options(repr.plot.width=6.47, repr.plot.height=7.3)

wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

xlabs <- c("C", "NC")
font_sz = 11

# Define label data function
create_label_data <- function(languages, y_value, label) {
  labels <- tibble(
    Language_name = languages,
    contingent = 1.5,
    y = y_value,
    label = label
  )
  return(labels)
}

# Define a list of languages
languages <- c("German", "English", "Estonian", "Persian", "French", "Croatian",
               "Japanese", "Korean", "Norwegian", "Portuguese", "Spanish",
               "Swedish", "Tseltal")

lex_sig3_languages <- c("German", "English", "French", "Croatian",
                        "Japanese", "Korean", "Persian")

lex_sig2_languages <- c("Estonian", "Portuguese")

lex_ns_languages <- c("Norwegian", "Spanish", "Swedish", "Tseltal")

swu_sig3_languages <- c("German", "English", "French", "Croatian", "Japanese",
                        "Korean", "Norwegian", "Portuguese", "Spanish",
                        "Swedish", "Tseltal")

swu_sig2_languages <- c("Persian")

swu_trend_languages <- c("Estonian")

# Create label data for each plot
lex_labels_sig3 <- create_label_data(lex_sig3_languages, 250, "***")
lex_labels_sig2 <- create_label_data(lex_sig2_languages, 250, "**")
lex_labels_ns <- create_label_data(lex_ns_languages, 285, "ns")

mlu_labels_sig <- create_label_data(languages, 5.75, "***")

swu_labels_sig3 <- create_label_data(swu_sig3_languages, 0.57, "***")
swu_labels_sig2 <- create_label_data(swu_sig2_languages, 0.57, "**")
swu_labels_trend <- create_label_data(swu_trend_languages, 0.64, "^")

# Plot 1: Lexical Diversity
p1 <- ggplot(rand_lex_sumstats, aes(x = contingent, y = sums, color = Language_name)) +
  stat_summary(fun.y = mean, geom = "point", shape = 19, size = 1.75) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size = 1.25, width = .5) +
  facet_wrap(. ~ Language_name, ncol = 7) +
  geom_text(data = lex_labels_sig3, aes(label = label, y = y), size = 6, color = "black") +
  geom_text(data = lex_labels_sig2, aes(label = label, y = y), size = 6, color = "black") +
  geom_text(data = lex_labels_ns, aes(label = label, y = y), size = 2.5, color = "black",fontface = "italic") +
  coord_cartesian(ylim = c(0, 300)) +
  labs( y = "Number of Unique Words", x = "") +
  theme_classic() +
  scale_x_discrete(labels = xlabs) +
  # theme_black() +
  theme(text = element_text(size = font_sz),
        axis.text.x = element_text(vjust = 0.5, hjust = .5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_text(vjust=.25),
        axis.ticks.length = unit(-2.5, "pt"),
        legend.position = "none")

ggsave("figures/Figure_2A.pdf", width = 6.47, height = 2.4, dpi = 1200)

# Plot 2: Mean Length of Utterance
p2 <- ggplot(rand_mlu_sumstats, aes(x = contingent, y = means, color = Language_name)) +
  stat_summary(fun.y = mean, geom = "point", shape = 19, size = 1.75) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size = 1.25, width = .5) +
  facet_wrap(. ~ Language_name, ncol = 7) +
  geom_text(data = mlu_labels_sig, aes(label = label, y = y), size = 6, color = "black") +
  coord_cartesian(ylim = c(0, 7)) +
  labs(y = "Mean Length of Utterance\nin Words", x = "") +
  theme_classic() +
  scale_x_discrete(labels = xlabs) +
  # theme_black() +
  theme(text = element_text(size = font_sz),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_text(vjust=-2.5),
        axis.ticks.length = unit(-2.5, "pt"),
        legend.position = "none")

ggsave("figures/Figure_2B.pdf", width = 6.47, height = 2.4, dpi = 1200)

# Plot 3: Proportion Single Word Utterances
p3 <- ggplot(rand_swu_sumstats, aes(x = contingent, y = means, color = Language_name)) +
  stat_summary(fun.y = mean, geom = "point", shape = 19, size = 1.75) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size = 1.25, width = .5) +
  facet_wrap(. ~ Language_name, ncol = 7) +
  geom_text(data = swu_labels_sig3, aes(label = label, y = y), size = 6, color = "black") +
  geom_text(data = swu_labels_sig2, aes(label = label, y = y), size = 6, color = "black") +
  geom_text(data = swu_labels_trend, aes(label = label, y = y), size = 3, color = "black") +
  coord_cartesian(ylim = c(0, .7)) +
  labs(y = "Prop. Single Word\nUtterances", x = "") +
  theme_classic() +
  scale_x_discrete(labels = xlabs) +
  # theme_black() +
  theme(text = element_text(size = font_sz),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks.length = unit(-2.5, "pt"),
        legend.position = "none")

ggsave("figures/Figure_2C.pdf", width = 6.47, height = 2.4, dpi = 1200)

# Combine plots
p <- plot_grid(
  p1, p2, p3,
  ncol = 1,
  labels = c("A", "B", "C"),
  align = "v"
)

ggsave("figures/Figure_2.pdf", width = 6.47, height = 7.3, dpi = 1200)


# ---- Black theme:

%%R

theme_black = function(base_size = 11, base_family = "") {
 
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
 
    theme(
      # Specify axis options
      axis.line = element_line(colour = "white"),  
      axis.text.x = element_text(color = "white",margin = margin(2, 2, 2, 2)),  
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
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
 
    )
 
}
