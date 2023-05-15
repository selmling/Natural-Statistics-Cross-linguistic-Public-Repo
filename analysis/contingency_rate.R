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
