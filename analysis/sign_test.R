library("tidyverse")

all_data <- map_df(c("data/rand_lexdiv_signtest.csv", 
                     "data/rand_mlu_signtest.csv", 
                     "data/rand_swu_signtest.csv"), 
                   read_csv)

ggplot(all_data, aes(x = Language_name, y = prop, color = Language_name)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  facet_wrap(~ measure, scales = "free_x") +
  labs(x = "", 
       y = expression(atop("Proportion of transcripts", "with " * Delta * " in simplified direction"))) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "gray"))

ggsave("figures/sign_test_across_measures.pdf", width = 11.7, height = 3.1)
