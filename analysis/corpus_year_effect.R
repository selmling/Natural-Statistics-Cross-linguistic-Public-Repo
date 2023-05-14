library("tidyverse")
library("kableExtra")
library("cowplot")
library("broom")

rand_dat_inc_cg_cc <- read_csv("../data/rand_dat_inc_master_cc_lexdiv.csv")
rand_dat_inc_cg_nc <- read_csv("../data/rand_dat_inc_master_nc_lexdiv.csv")

rand_dat_inc_cg <- rbind(rand_dat_inc_cg_cc, rand_dat_inc_cg_nc) %>%
  mutate(single_word_utterance = ifelse(num_tokens == 1, 1, 0)) %>%
  rename(uniqueness = uniquenss)

# ---- year of study
corpora_year <- read_csv("../data/corpora_year.csv") %>%
    rename(corpus_name = Corpora) %>%
    select(corpus_name, `Year collected`)

rand_dat_inc_cg <- rand_dat_inc_cg %>% left_join(corpora_year)

# ---- linguistic complexity summaries

lexdiv_sumstats <- rand_dat_inc_cg %>%
    select(target_child_id, transcript_id, target_child_age,
           `Year collected`, Language_name, contingent,
           uniqueness, num_tokens) %>%
  group_by(transcript_id, contingent, Language_name, `Year collected`) %>%
  summarise(variable = "result",
            types = sum(as.numeric(unlist(uniqueness))),
            tokens = sum(num_tokens),
            `Year collected` = unique(`Year collected`)) %>%
  pivot_wider(names_from = contingent,
              values_from = c(types, tokens))

mlu_sumstats <- rand_dat_inc_cg %>%
    select(target_child_id, transcript_id, target_child_age,
           `Year collected`, Language_name, contingent,
           num_tokens) %>%
  group_by(transcript_id, contingent, Language_name, `Year collected`) %>%
  summarise(mean = mean(num_tokens),
            `Year collected` = unique(`Year collected`)) %>%
  spread(contingent, mean) %>%
  mutate(diff = `non-contingent` - contingent)

swu_sumstats <- rand_dat_inc_cg %>%
  group_by(target_child_id, transcript_id, target_child_age,
           `Year collected`, Language_name, contingent,
           single_word_utterance) %>%
  group_by(transcript_id, contingent, Language_name, `Year collected`) %>%
  summarise(mean = mean(single_word_utterance),
            `Year collected` = unique(`Year collected`)) %>%
  spread(contingent, mean) %>%
  mutate(diff = `non-contingent` - contingent)

lexdiv_sumstats_long_types <- lexdiv_sumstats %>%
    select(transcript_id, Language_name, `Year collected`, types_contingent,
         `types_non-contingent`) %>%
    mutate(type_diff = `types_non-contingent` - types_contingent) %>%
    pivot_longer(cols = c(`types_non-contingent`, types_contingent)) %>%
    rename(Contingency = name,
         Types = value) %>%
    # avoid double counting transcripts:
    distinct(transcript_id, .keep_all = TRUE)

lexdiv_sumstats_long_types %>%
    group_by(Language_name) %>%
    count(`Year collected`) %>%
    kable("pipe")

# ---- scatterplots viz relation between Year collected and simplification effect

stroke <- 1
shape <- 21
size <- 2

p1 <- ggplot(lexdiv_sumstats_long_types, aes(color = Language_name)) +
  geom_point(aes(x = `Year collected`, y = type_diff),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color = FALSE) +
  stat_smooth(method = lm, size = 1.2, se = TRUE,
              aes(x = `Year collected`, y = type_diff),
              color = "black") +
  facet_wrap(. ~ Language_name, ncol = 7) +
  labs(y = "Difference in C & NC Lexical Diversity\n(higher is more simplified)",
       x = "Year collected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p2 <- ggplot(mlu_sumstats, aes(color = Language_name)) +
  geom_point(aes(x = `Year collected`, y = diff),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color = FALSE) +
  stat_smooth(method = lm, size = 1.2, se = TRUE,
              aes(x = `Year collected`, y = diff),
              color = "black") +
  facet_wrap(. ~ Language_name, ncol = 7) +
  labs(y = "Difference in C & NC MLUw\n(higher is more simplified)",
       x = "Year collected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p3 <- ggplot(swu_sumstats, aes(color = Language_name)) +
  geom_point(aes(x = `Year collected`, y = diff),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color = FALSE) +
  stat_smooth(method = lm, size = 1.2, se = TRUE,
              aes(x = `Year collected`, y = diff),
              color = "black") +
  facet_wrap(. ~ Language_name, ncol = 7) +
  labs(y = "Difference in C & NC SWU\n(lower is more simplified)",
       x = "Year collected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

g <- plot_grid(p1, p2, p3, ncol = 1, labels = c("A", "B", "C"))

ggsave("../figures/corpus_year.pdf", g, width = 8, height = 12)

# ---- statistical test

options(scipen = 999)

lexdiv_sumstats_long_types %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm(type_diff ~ `Year collected`,
                              data = .)),
         tidy = map(fit, tidy)) %>%
  unnest(cols = c(tidy)) %>%
  filter(term == "`Year collected`") %>%
  select(Language_name, estimate, p.value) %>%
  mutate(p.value = format(round(p.value, 4), nsmall = 4),
         p.value = gsub("0.0000", "<.0001", p.value)) %>%
  drop_na() %>%
  kable("pipe")

mlu_sumstats %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm(diff ~ `Year collected`,
                              data = .)),
         tidy = map(fit, tidy)) %>%
  unnest(cols = c(tidy)) %>%
  filter(term == "`Year collected`") %>%
  select(Language_name, estimate, p.value) %>%
  mutate(p.value = format(round(p.value, 4), nsmall = 4),
         p.value = gsub("0.0000", "<.0001", p.value)) %>%
  drop_na() %>%
  kable("pipe")

swu_sumstats %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm(diff ~ `Year collected`,
                              data = .)),
         tidy = map(fit, tidy)) %>%
  unnest(cols = c(tidy)) %>%
  filter(term == "`Year collected`") %>%
  select(Language_name, estimate, p.value) %>%
  mutate(p.value = format(round(p.value, 4), nsmall = 4),
         p.value = gsub("0.0000", "<.0001", p.value)) %>%
  drop_na() %>%
  kable("pipe")

# ---- Do child age and year collected correlate?

p4 <- rand_dat_inc_cg %>%
  select(target_child_id, transcript_id, target_child_age,
         `Year collected`, Language_name) %>%
  group_by(Language_name, `Year collected`, target_child_age) %>%
  summarize(Language_name = unique(Language_name),
            `Year collected` = unique(`Year collected`),
            target_child_age = unique(target_child_age)) %>%
  ggplot(aes(color = Language_name)) +
    geom_point(aes(x = `Year collected`, y = target_child_age),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
    guides(color = FALSE) +
    stat_smooth(method = lm, size = 1.2, se = TRUE,
              aes(x = `Year collected`, y = target_child_age),
              color = "black") +
    facet_wrap(. ~ Language_name, ncol = 7) +
  labs(y = "Child age",
       x = "Year collected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("../figures/corpus_year_x_child_age.pdf", p4, width = 8, height = 4)

# ---- statistical test

rand_dat_inc_cg %>%
  select(target_child_id, transcript_id, target_child_age,
         `Year collected`, Language_name) %>%
  group_by(Language_name, `Year collected`, target_child_age) %>%
  summarize(Language_name = unique(Language_name),
            `Year collected` = unique(`Year collected`),
            target_child_age = unique(target_child_age)) %>% 
  ungroup() %>% 
  group_by(Language_name) %>% 
  nest() %>%
  mutate(fit = map(data, ~ lm(target_child_age ~ `Year collected`,
                              data = .)),
         tidy = map(fit, tidy)) %>%
  unnest(cols = c(tidy)) %>%
  filter(term == "`Year collected`") %>%
  select(Language_name, estimate, p.value) %>%
  mutate(p.value = format(round(p.value, 4), nsmall = 4),
         p.value = gsub("0.0000", "<.0001", p.value)) %>%
  drop_na() %>%
  kable("pipe")

# ---- discretize year of collection

lexdiv_sumstats_long_types %>%
    group_by(Language_name) %>%
    count(`Year collected`) %>%
    kable("pipe")

lexdiv_sum_long_types_corp_yr <- lexdiv_sumstats_long_types %>%
  ungroup() %>%
  filter(Language_name %in% c("English", "French", "German",
                            "Japanese", "Swedish"))

corp_yr_medians <- lexdiv_sum_long_types_corp_yr %>%
  group_by(Language_name) %>%
  summarize(`Year collected` = unique(`Year collected`),
            median = median(`Year collected`))

lexdiv_sum_long_types_corp_yr <- lexdiv_sum_long_types_corp_yr %>%
  left_join(corp_yr_medians) %>%
  mutate(time_period = ifelse(`Year collected` > `median`,
  "recent", "vintage"))

lexdiv_sum_long_types_corp_yr %>%
        group_by(Language_name) %>%
        count(time_period) %>%
        kable("pipe")

# ---- plot by median split of corpus year

# lexical diversity

lexdiv_sumstats <- rand_dat_inc_cg %>%
    select(target_child_id, transcript_id, target_child_age,
           `Year collected`, Language_name, contingent,
           uniqueness, num_tokens) %>%
  filter(Language_name %in% c("English", "French", "German",
                            "Japanese", "Swedish")) %>% 
  group_by(transcript_id, contingent, Language_name, `Year collected`) %>%
  summarise(variable = "result",
            types = sum(as.numeric(unlist(uniqueness))),
            `Year collected` = unique(`Year collected`)) %>%
  left_join(corp_yr_medians) %>%
  mutate(time_period = ifelse(`Year collected` > `median`,
  "recent", "vintage"))

p1 <- ggplot(lexdiv_sumstats, aes(x = contingent, y = types,
             color = Language_name)) +
        stat_summary(fun.y = mean, geom = "point", shape = 19, size = 1.75) +
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     size = 1.25, width = .5) +
        facet_grid(time_period ~ Language_name) +
        coord_cartesian(ylim = c(0, 250)) +
        theme(axis.text.x = element_text(vjust = 0.5, hjust = .5),
              axis.ticks.length = unit(-2.5, "pt"),
              legend.position = "none")

# mluw

mlu_sumstats <- rand_dat_inc_cg %>%
  select(target_child_id, transcript_id, target_child_age,
         `Year collected`, Language_name, contingent,
         num_tokens) %>%
  filter(Language_name %in% c("English", "French", "German",
                            "Japanese", "Swedish")) %>% 
  group_by(transcript_id, contingent, Language_name, `Year collected`) %>%
  summarise(mean = mean(num_tokens),
            `Year collected` = unique(`Year collected`)) %>%
  left_join(corp_yr_medians) %>%
  mutate(time_period = ifelse(`Year collected` > `median`,
  "recent", "vintage"))

p2 <- ggplot(mlu_sumstats, aes(x = contingent, y = mean,
             color = Language_name)) +
        stat_summary(fun.y = mean, geom = "point", shape = 19, size = 1.75) +
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     size = 1.25, width = .5) +
        facet_grid(time_period ~ Language_name) +
        coord_cartesian(ylim = c(0, 6)) +
        theme(axis.text.x = element_text(vjust = 0.5, hjust = .5),
              axis.ticks.length = unit(-2.5, "pt"),
              legend.position = "none")

# swu

