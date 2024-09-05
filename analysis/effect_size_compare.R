library("tidyverse")
library("cowplot")

# ---- Load and format data

file_list <- c("data/rand_lexdiv_stats.csv", "data/maxvoc_lexdiv_stats.csv", "data/maxturn_lexdiv_stats.csv",
               "data/rand_mlu_stats.csv", "data/maxvoc_mlu_stats.csv", "data/maxturn_mlu_stats.csv",
               "data/rand_swu_stats.csv", "data/maxvoc_swu_stats.csv", "data/maxturn_swu_stats.csv")

effect_size_dat <- file_list %>% map_df(~read_csv(.)) %>%
    mutate(Language = str_split_fixed(`Language (n)`, " ", n = 2)[, 1],
           `Effect size abs` = abs(`Effect size`),
           `CI+` = abs(`CI+`),
           `CI-` = abs(`CI-`),
           sample = recode(sample, "rand" = "random"))

# drop Mandarin & Polish (n<2)
# drop Tseltal and Persian (no random 10-min samples to compare against)
effect_size_dat <- effect_size_dat  %>%
    filter(!Language %in% c("Mandarin", "Polish", "Tseltal", "Persian"))

effect_size_dat_lexdiv <- effect_size_dat %>%
    filter(measure == "lexdiv")

effect_size_dat_mlu <- effect_size_dat %>%
    filter(measure == "mlu")

effect_size_dat_swu <- effect_size_dat %>%
    filter(measure == "swu")


# ---- plot and export pdf

p1 <- ggplot(effect_size_dat_lexdiv, aes(x = sample, y = `Effect size abs`, color = sample)) +
  geom_linerange(aes(x = sample, y = `Effect size abs`, ymin = 0, ymax = `Effect size abs`),
               position = position_dodge(width = .7)) +
  lims(y = c(0, 3.5)) +
  geom_point(size = 2, shape = 21, stroke = 2,
             position = position_dodge(width = .7)) +
  facet_wrap(~ Language, ncol = 1) +
  labs(tag = "A",
       title = "Lexical Diversity",
       x = "",
       y = "Effect size (abs. value)") +
  coord_flip() +
  theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

p2 <- ggplot(effect_size_dat_mlu, aes(x = sample, y = `Effect size abs`, color = sample)) +
  geom_linerange(aes(x = sample, y = `Effect size abs`, ymin = 0, ymax = `Effect size abs`),
               position = position_dodge(width = .7)) +
  lims(y = c(0, .5)) +
  geom_point(size = 2, shape = 21, stroke = 2,
             position = position_dodge(width = .7)) +
  facet_wrap(~ Language, ncol = 1) +
  labs(tag = "B",
       title = "Mean Length of Utterance",
       x = "",
       y = "Effect size (abs. value)") +
  coord_flip() +
  theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

p3 <- ggplot(effect_size_dat_swu, aes(x = sample, y = `Effect size abs`, color = sample)) +
  geom_linerange(aes(x = sample, y = `Effect size abs`, ymin = 0, ymax = `Effect size abs`),
               position = position_dodge(width = .7)) +
  lims(y = c(0, .5)) +
  geom_point(size = 2, shape = 21, stroke = 2,
             position = position_dodge(width = .7)) +
  facet_wrap(~ Language, ncol = 1) +
  labs(tag = "C",
       title = "Proportion of Single-Word Utterances",
       x = "",
       y = "Effect size (abs. value)") +
  coord_flip() +
  theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

prow <- plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    p3 + theme(legend.position = "none"),
    nrow = 1
)

legend <- get_legend(
    p1 +  guides(color = guide_legend(reverse = TRUE))
)

g <- plot_grid(prow, legend, rel_widths = c(3, .4))

ggsave(file="figures/effect_size_compare.pdf", plot=g, width = 10.5, height = 10.5, dpi=1200)

# ---- Summaries

# Logic: we want to know if effect sizes get bigger or smaller compared to random sampling.
#        Thus, we'll subtract other two effect sizes from those obtained under random sampling.
#        If the difference is positive (RS=.3 and MVS=.2), then random sampling had a bigger effect size.
#        If the difference is negative (RS=.3 and MTS=.5), then random sampling had a smaller effect size.


effect_size_dat_lexdiv_diffs <- effect_size_dat_lexdiv %>%
  select(Language, `Effect size`, sample) %>%
  mutate(`Effect size` = abs(`Effect size`)) %>%
  spread(key = sample, value = `Effect size`) %>%
  mutate(diff_maxvoc_random = `random` - `maxvoc`,
         diff_maxturn_random = `random` - `maxturn`)

eff_size_lexdiv_diff_summary <- effect_size_dat_lexdiv_diffs %>%
  summarize(
    median_diff_maxvoc_random = median(diff_maxvoc_random, na.rm = TRUE),
    sd_diff_maxvoc_random = sd(diff_maxvoc_random, na.rm = TRUE),
    min_diff_maxvoc_random = min(diff_maxvoc_random, na.rm = TRUE),
    max_diff_maxvoc_random = max(diff_maxvoc_random, na.rm = TRUE),
    median_diff_maxturn_random = median(diff_maxturn_random, na.rm = TRUE),
    sd_diff_maxturn_random = sd(diff_maxturn_random, na.rm = TRUE),
    min_diff_maxturn_random = min(diff_maxturn_random, na.rm = TRUE),
    max_diff_maxturn_random = max(diff_maxturn_random, na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  separate(measurement, into = c("type", "metric"), sep = "_", extra = "merge") %>%
  pivot_wider(
    names_from = "metric", 
    values_from = "value"
  ) %>% 
  mutate(measure = "Lexical diversity")

effect_size_dat_mlu_diffs <- effect_size_dat_mlu %>%
  select(Language, `Effect size`, sample) %>%
  mutate(`Effect size` = abs(`Effect size`)) %>%
  spread(key = sample, value = `Effect size`) %>%
  mutate(diff_maxvoc_random = `random` - `maxvoc`,
         diff_maxturn_random = `random` - `maxturn`)

eff_size_mlu_diff_summary <- effect_size_dat_mlu_diffs %>%
  summarize(
    median_diff_maxvoc_random = median(diff_maxvoc_random, na.rm = TRUE),
    sd_diff_maxvoc_random = sd(diff_maxvoc_random, na.rm = TRUE),
    min_diff_maxvoc_random = min(diff_maxvoc_random, na.rm = TRUE),
    max_diff_maxvoc_random = max(diff_maxvoc_random, na.rm = TRUE),
    median_diff_maxturn_random = median(diff_maxturn_random, na.rm = TRUE),
    sd_diff_maxturn_random = sd(diff_maxturn_random, na.rm = TRUE),
    min_diff_maxturn_random = min(diff_maxturn_random, na.rm = TRUE),
    max_diff_maxturn_random = max(diff_maxturn_random, na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  separate(measurement, into = c("type", "metric"), sep = "_", extra = "merge") %>%
  pivot_wider(
    names_from = "metric", 
    values_from = "value"
  ) %>% 
  mutate(measure = "MLU")

effect_size_dat_swu_diffs <- effect_size_dat_swu %>%
  select(Language, `Effect size`, sample) %>%
  mutate(`Effect size` = abs(`Effect size`)) %>%
  spread(key = sample, value = `Effect size`) %>%
  mutate(diff_maxvoc_random = `random` - `maxvoc`,
         diff_maxturn_random = `random` - `maxturn`)

eff_size_swu_diff_summary <- effect_size_dat_swu_diffs  %>%
  summarize(
    median_diff_maxvoc_random = median(diff_maxvoc_random, na.rm = TRUE),
    sd_diff_maxvoc_random = sd(diff_maxvoc_random, na.rm = TRUE),
    min_diff_maxvoc_random = min(diff_maxvoc_random, na.rm = TRUE),
    max_diff_maxvoc_random = max(diff_maxvoc_random, na.rm = TRUE),
    median_diff_maxturn_random = median(diff_maxturn_random, na.rm = TRUE),
    sd_diff_maxturn_random = sd(diff_maxturn_random, na.rm = TRUE),
    min_diff_maxturn_random = min(diff_maxturn_random, na.rm = TRUE),
    max_diff_maxturn_random = max(diff_maxturn_random, na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  separate(measurement, into = c("type", "metric"), sep = "_", extra = "merge") %>%
  pivot_wider(
    names_from = "metric", 
    values_from = "value"
  ) %>% 
  mutate(measure = "SWU")

eff_size_diff_summary <- list(eff_size_lexdiv_diff_summary, eff_size_mlu_diff_summary, eff_size_swu_diff_summary) %>% 
    reduce(bind_rows)

# Small effect size: d = 0.2
# Medium effect size: d = 0.5
# Large effect size: d = 0.8


