library("tidyverse")
library("cowplot")

data_lexdiv <- read_csv("data/rand_lexdiv_signtest.csv")
data_mlu <- read_csv("data/rand_mlu_signtest.csv")
data_swu <- read_csv("data/rand_swu_signtest.csv")

# ---- Descriptive statistics

data_lexdiv %>%
  summarise(
    Mean = mean(prop, na.rm = TRUE),
    StdDev = sd(prop, na.rm = TRUE)
  )

data_mlu %>%
  summarise(
    Mean = mean(prop, na.rm = TRUE),
    StdDev = sd(prop, na.rm = TRUE)
  )

data_swu %>%
  summarise(
    Mean = mean(prop, na.rm = TRUE),
    StdDev = sd(prop, na.rm = TRUE)
  )

# ---- Plot

label_custom <- function(x) {
  # Create a custom labeling function
  ifelse(x == 1, "1", ifelse(x == 0, "0", sprintf(".%s", substr(format(x), 3, 4))))
}

# Create a plotting function
plot_measure <- function(data, measure_label) {
  ggplot(data, aes(x = Language_name, y = prop, color = Language_name)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
    labs(x = "", 
         y = expression(atop("Prop. transcripts with", "" * Delta * " in simplified direction")),
         title = measure_label) +
    scale_y_continuous(limits = c(0, 1), labels = label_custom) +
    theme_classic() +
    theme(text = element_text(size=11),
          axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
          axis.text.y = element_text(colour = "black",),
          legend.position = "none",
          plot.title = ggtext::element_markdown(size = 10),
          panel.background = element_rect(fill = "gray"),
          plot.margin = margin(6, -12, 6, 0))
}

# Create each plot individually
plot_a <- plot_measure(data_lexdiv, "Number of Unique Words")
plot_b <- plot_measure(data_mlu, "Mean Length of Utterance in Words") + ylab(NULL)
plot_c <- plot_measure(data_swu, "Single Word Utterances") + ylab(NULL)

plot_grid(plot_a, plot_b, plot_c,
          align = 'vh', nrow = 1,
          labels = c("A", "B", "C"),
          hjust = -3, vjust = 1.3)

ggsave("figures/sign_test_across_measures.pdf", width = 10, height = 2.6, dpi=1200)

