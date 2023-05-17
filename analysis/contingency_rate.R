library("cowplot")
library("tidyverse")
library("kableExtra")

rand_dat <- read_csv("../data/rand_dat_inc_master.csv")

# ---- rate at which child vocalizations elicited caregiver speech

elic_resp <- rand_dat %>%
    filter(caregiver != "other") %>%
    select(transcript_id, target_child_id, Language_name, caregiver, media_start, contingent) %>% 
    group_by(transcript_id) %>% 
    mutate(elicited_response = lead(contingent)) %>%
    ungroup() %>% 
    filter(caregiver == "target_child") %>% 
    select(-c(contingent))

cont_rate <- elic_resp %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(elicited_response, na.rm = TRUE))

cont_rate_means <- cont_rate %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p1 <- cont_rate %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cont_rate_means,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    facet_wrap(. ~ Language_name, ncol = 7) +
    geom_text(data=cont_rate_means,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .25,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = "Child vocalization contingent caregiver speech elicitation rate") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

# ---- rate at caregiver speech was produced contingently on child vocalization

rand_cg_dat <- read_csv("../data/rand_dat_inc_master.csv")

cg_cont_rate <- rand_cg_dat %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(contingent))

cg_cont_rate_means <- cg_cont_rate %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p2 <- cg_cont_rate %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cg_cont_rate_means,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    facet_wrap(. ~ Language_name, ncol = 7) +
    geom_text(data=cg_cont_rate_means,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .1,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = "Proportion of caregiver utterances which were contingent on child vocalizations") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

plot_grid(p1, p2, ncol = 1, labels = c("A", "B"))

ggsave("../figures/contingency_rates.pdf", width = 6.47, height = 5, dpi = 1200)

# ---- Rollins corpus contingency rates

rand_dat_rollins <- rand_dat %>% 
    filter(corpus_name == "Rollins")

# ---- rate at which child vocalizations elicited Rollins caregiver speech

elic_resp_rollins <- rand_dat_rollins %>%
    filter(caregiver != "other") %>%
    select(transcript_id, target_child_id, Language_name, caregiver, media_start, contingent) %>% 
    group_by(transcript_id) %>% 
    mutate(elicited_response = lead(contingent)) %>%
    ungroup() %>% 
    filter(caregiver == "target_child") %>% 
    select(-c(contingent))

cont_rate_rollins <- elic_resp_rollins %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(elicited_response, na.rm = TRUE))

cont_rate_means_rollins <- cont_rate_rollins %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p3 <- cont_rate_rollins %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cont_rate_means_rollins,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    geom_text(data=cont_rate_means_rollins,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .05,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = str_wrap("Child vocalization contingent caregiver speech elicitation rate",48)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

# ---- rate at Rollins caregiver speech was produced contingently on child vocalization

rand_cg_dat_rollins <- rand_cg_dat %>%
    filter(corpus_name == "Rollins")

cg_cont_rate_rollins <- rand_cg_dat_rollins %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(contingent))

cg_cont_rate_means_rollins <- cg_cont_rate_rollins %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p4 <- cg_cont_rate_rollins %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cg_cont_rate_means_rollins,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    geom_text(data=cg_cont_rate_means_rollins,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .025,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = str_wrap("Proportion of caregiver utterances which were contingent on child vocalizations",48)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

plot_col <- plot_grid(p3, p4, ncol = 1, labels = c("A", "B"))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Rollins corpus (Lab: unstructured context)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, plot_col,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

ggsave("../figures/contingency_rates_Rollins.pdf", width = 4.5, height = 3.5, dpi = 1200)

# ---- Lund corpus contingency rates

rand_dat_lund <- rand_dat %>% 
    filter(corpus_name == "Lund")

# ---- rate at which child vocalizations elicited Lund caregiver speech

elic_resp_lund <- rand_dat_lund %>%
    filter(caregiver != "other") %>%
    select(transcript_id, target_child_id, Language_name, caregiver, media_start, contingent) %>% 
    group_by(transcript_id) %>% 
    mutate(elicited_response = lead(contingent)) %>%
    ungroup() %>% 
    filter(caregiver == "target_child") %>% 
    select(-c(contingent))

cont_rate_lund <- elic_resp_lund %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(elicited_response, na.rm = TRUE))

cont_rate_means_lund <- cont_rate_lund %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p5 <- cont_rate_lund %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cont_rate_means_lund,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    geom_text(data=cont_rate_means_lund,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .05,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = str_wrap("Child vocalization contingent caregiver speech elicitation rate",48)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

# ---- rate at Lund caregiver speech was produced contingently on child vocalization

rand_cg_dat_lund <- rand_cg_dat %>%
    filter(corpus_name == "Lund")

cg_cont_rate_lund <- rand_cg_dat_lund %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(contingent))

cg_cont_rate_means_lund <- cg_cont_rate_lund %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p6 <- cg_cont_rate_lund %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cg_cont_rate_means_lund,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    geom_text(data=cg_cont_rate_means_lund,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .025,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = str_wrap("Proportion of caregiver utterances which were contingent on child vocalizations",48)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

plot_col <- plot_grid(p5, p6, ncol = 1, labels = c("A", "B"))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Lund corpus (Swedish vintage corpus)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, plot_col,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

ggsave("../figures/contingency_rates_Lund.pdf", width = 4.5, height = 3.5, dpi = 1200)

# ---- Yamaguchi, & Lyon (recent French corpus)

rand_dat_YamLyon <- rand_dat %>%
    filter(corpus_name == "Yamaguchi" | corpus_name == "Lyon")

# ---- rate at which child vocalizations elicited Lund caregiver speech

elic_resp_YamLyon <- rand_dat_YamLyon %>%
    filter(caregiver != "other") %>%
    select(transcript_id, target_child_id, Language_name, caregiver, media_start, contingent) %>% 
    group_by(transcript_id) %>% 
    mutate(elicited_response = lead(contingent)) %>%
    ungroup() %>% 
    filter(caregiver == "target_child") %>% 
    select(-c(contingent))

cont_rate_YamLyon <- elic_resp_YamLyon %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(elicited_response, na.rm = TRUE))

cont_rate_means_YamLyon <- cont_rate_YamLyon %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p7 <- cont_rate_YamLyon %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cont_rate_means_YamLyon,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    geom_text(data=cont_rate_means_YamLyon,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .05,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = str_wrap("Child vocalization contingent caregiver speech elicitation rate",48)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

# ---- rate at Lund caregiver speech was produced contingently on child vocalization

rand_cg_dat_YamLyon <- rand_cg_dat %>%
    filter(corpus_name == "Yamaguchi" | corpus_name == "Lyon")

cg_cont_rate_YamLyon <- rand_cg_dat_YamLyon %>%
    group_by(transcript_id, Language_name) %>% 
    summarize(contingency_rate = mean(contingent))

cg_cont_rate_means_YamLyon <- cg_cont_rate_YamLyon %>% 
    group_by(Language_name) %>% 
    summarize(avg_contingency_rate = round(mean(contingency_rate),2))

p8 <- cg_cont_rate_YamLyon %>% ggplot(aes(fill = Language_name)) +
    geom_histogram(aes(contingency_rate)) +
    geom_vline(data=cg_cont_rate_means_YamLyon,
               aes(xintercept=avg_contingency_rate),
               color="black", linetype="dashed", size=.6) +
    geom_text(data=cg_cont_rate_means_YamLyon,
              aes(label = round(avg_contingency_rate, digits = 2),
                  x = avg_contingency_rate + .025,
                  y = 15), size = 3) +
    coord_cartesian(ylim = c(0,25)) +
    labs(y = "Count",
         x = str_wrap("Proportion of caregiver utterances which were contingent on child vocalizations",48)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = .5),
          axis.ticks.length = unit(-2.5, "pt"),
          legend.position = "none")

plot_col <- plot_grid(p7, p8, ncol = 1, labels = c("A", "B"))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Lund corpus (French recent corpus)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, plot_col,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

ggsave("../figures/contingency_rates_Lund.pdf", width = 4.5, height = 3.5, dpi = 1200)
