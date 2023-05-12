library("tidyverse")
library("kableExtra")
library("cowplot")

rand_dat_inc_cg_cc <- read_csv("../data/rand_dat_inc_master_cc_lexdiv.csv")
rand_dat_inc_cg_nc <- read_csv("../data/rand_dat_inc_master_nc_lexdiv.csv")

rand_dat_inc_cg <- rbind(rand_dat_inc_cg_cc, rand_dat_inc_cg_nc) %>%
  mutate(single_word_utterance = ifelse(num_tokens == 1, 1, 0)) %>%
  rename(uniqueness = uniquenss)

# year of study
corpora_year <- read_csv("../data/corpora_year.csv") %>%
    rename(corpus_name = Corpora) %>%
    select(corpus_name, `Year collected`)

rand_dat_inc_cg <- rand_dat_inc_cg %>% left_join(corpora_year)

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

# scatterplots

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
  labs(y = "Difference in C & NC Lexical Diversity",
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
  labs(y = "Difference in C & NC MLUw",
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
  labs(y = "Difference in C & NC SWU",
       x = "Year collected") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

g <- plot_grid(p1, p2, p3, ncol = 1, labels = c("A", "B", "C"))

ggsave("../figures/corpus_year.pdf", g, width = 8, height = 12)

# Do child age and year collected correlate?

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
