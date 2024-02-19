library("tidyverse")
library("plotrix")

pitch_dat <- read_csv("data/master_pitch.csv") %>% 
       mutate(rangeF0 = maxF0 - minF0)

pitch_dat_sum <- pitch_dat %>%
    mutate(rangeF0 = maxF0 - minF0,
           Contingency = recode(Contingency,
           `Non-contingent` = "Noncontingent")) %>%
    group_by(Language, Contingency) %>%
    summarize(avgF0 = mean(meanF0, na.rm = TRUE),
              seF0 = std.error(meanF0, na.rm = TRUE),
              avg_stdevF0 = mean(stdevF0, na.rm = TRUE),
              se_stdevF0 = std.error(stdevF0, na.rm = TRUE),
              avg_rangeF0 = mean(rangeF0, na.rm = TRUE),
              se_rangeF0 = std.error(rangeF0, na.rm = TRUE),
              N = n())

# visualized

# ---- correct colors

ggplot(pitch_dat_sum, aes(x = Contingency, y = avgF0, fill = Language)) +
  geom_bar(stat = "identity",
           position = position_dodge(),alpha = .7) +
  geom_errorbar(aes(ymin = avgF0 - seF0,
                    ymax = avgF0 + seF0,
                    color = Language), width = .35,
                    size = 2,
                    position = position_dodge(.9)) +
  facet_grid(cols = vars(Language)) +
  scale_color_manual(values=c("#E48900", "#C59A00", "#99A801", "#00BC57", "#00C094",
                              "#05A3FF", "#DF70F8", "#FB61D7")) +
  scale_fill_manual(values=c("#E48900", "#C59A00", "#99A801", "#00BC57", "#00C094",
                              "#05A3FF", "#DF70F8", "#FB61D7")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 350)) +
  scale_x_discrete(labels = c("C", "NC")) +
  labs(y = "Mean F0") +
  theme_classic() +
  theme(text = element_text(size = 20),
        legend.position = "None")

ggsave("figs/mean_pitch_diffs.pdf", width = 11, height = 6.180, dpi = 1200)

# ggplot(pitch_dat_sum, aes(x = Contingency, y = avg_stdevF0, fill = Language)) +
#   geom_bar(stat = "identity", color = "black",
#            position = position_dodge()) +
#   geom_errorbar(aes(ymin = avg_stdevF0 - se_stdevF0, ymax = avg_stdevF0 + se_stdevF0), width = .2,
#                  position = position_dodge(.9)) +
#   facet_grid(cols = vars(Language)) +
#   scale_x_discrete(labels = c("C", "NC")) +
#   labs(y = "Mean F0 SD") +
#   theme_classic()

# ---- stats

library("lme4")
library("emmeans")

# run a linear or mixed model depending on repeated measures

run_lm <- function(df, dependent_var, subject_var = "subID") {
  if (length(unique(df[[subject_var]])) > 1) {
    formula_str <- paste(dependent_var, "~ Contingency + (1 |", subject_var, ")")
    model_formula <- as.formula(formula_str)
    m <- lmer(model_formula, data = df, REML = FALSE)
  } else {
    formula_str <- paste(dependent_var, "~ Contingency")
    model_formula <- as.formula(formula_str)
    m <- lm(model_formula, data = df)
  }
  emm <- emmeans(m, ~ Contingency)
  return(emm)
}

# make a table from summary

table_maker <- function(data) { data %>%
    select(Language, estimate, SE, statistic, p.value) %>%
    `colnames<-`(c("Language", "Estimate", "SE", "Test statistic", "p-value")) %>%
    mutate_at(vars(-c(`p-value`,Language)), round,2) %>%
    mutate(`p-value` = format(round(`p-value`,4),nsmall=4)) %>%
    mutate(`p-value` = gsub("0.0000","<.0001",`p-value`)) %>%
    arrange(`Language`)
    }

# pitch range

rangeF0_test <- pitch_dat %>%
  group_by(Language) %>%
  nest() %>%
  mutate(
    fit = map(data, ~run_lm(.x, "rangeF0", "subID")),
    contrasts = map(fit, ~summary(contrast(.x, method = "pairwise")))
  ) %>%
  select(Language, contrasts) %>% 
  unnest() %>%
  mutate(statistic = coalesce(`t.ratio`), .before = p.value)

rangeF0_stats_table <- table_maker(rangeF0_test)

range_descriptives <- pitch_dat %>%
  group_by(Language, Contingency) %>%
  summarize(Mean = mean(rangeF0, na.rm = TRUE),
            SD = sd(rangeF0, na.rm = TRUE)) %>% 
  mutate_at(vars(-c(Language, Contingency)), round,2) %>% 
  ungroup() %>%
  mutate(Mean_SD = paste(Mean, "(", SD, ")")) %>%
  select("Language", "Contingency", "Mean_SD") %>% 
  pivot_wider(names_from = Contingency, values_from = Mean_SD) %>%
  rename(`Contingent Mean (SD)` = Contingent, `Non-contingent Mean (SD)` = `Non-contingent`)
       
rangeF0_stats_table %>% 
       left_join(range_descriptives, by = "Language") %>%
       select(Language, `Contingent Mean (SD)`, `Non-contingent Mean (SD)`, everything()) %>% 
       kbl("pipe")

# mean range

meanF0_test <- pitch_dat %>%
  group_by(Language) %>%
  nest() %>%
  mutate(
    fit = map(data, ~run_lm(.x, "meanF0", "subID")),
    contrasts = map(fit, ~summary(contrast(.x, method = "pairwise")))
  ) %>%
  select(Language, contrasts) %>% 
  unnest() %>%
  mutate(statistic = coalesce(`t.ratio`), .before = p.value)

meanF0_stats_table <- table_maker(meanF0_test)

mean_descriptives <- pitch_dat %>%
  group_by(Language, Contingency) %>%
  summarize(Mean = mean(meanF0, na.rm = TRUE),
            SD = sd(meanF0, na.rm = TRUE)) %>% 
  mutate_at(vars(-c(Language, Contingency)), round,2) %>% 
  ungroup() %>%
  mutate(Mean_SD = paste(Mean, "(", SD, ")")) %>%
  select("Language", "Contingency", "Mean_SD") %>% 
  pivot_wider(names_from = Contingency, values_from = Mean_SD) %>%
  rename(`Contingent Mean (SD)` = Contingent, `Non-contingent Mean (SD)` = `Non-contingent`)

meanF0_stats_table %>% 
       left_join(mean_descriptives, by = "Language") %>%
       select(Language, `Contingent Mean (SD)`, `Non-contingent Mean (SD)`, everything()) %>% 
       kbl("pipe")
