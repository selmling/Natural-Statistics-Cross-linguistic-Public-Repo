library("kableExtra")
library("tidyverse")
library("ggridges")
library("cowplot")
library("broom")

# ---- Language competence measure 1

child_dat <- read_csv("../data/child_dat.csv")

# drop child blank utterances

child_dat_cln <- child_dat %>% 
  filter(!gloss == "") %>% 
  mutate(babble_count = str_count(gloss, "xxx") +
                        str_count(gloss, "yyy") +
                        str_count(gloss, "www"),
         word_count = num_tokens - babble_count,
         .after = num_tokens)

# word count and MLUw summary

child_word_dat_summary <- child_dat_cln %>% 
  group_by(Language_name, transcript_id, target_child_age) %>% 
  summarize(total_words = sum(word_count),
            MLUw = mean(word_count))

# vis check

child_word_dat_summary %>% 
  ggplot(aes(x=target_child_age, y=total_words, color=Language_name)) +
  geom_point() +
  geom_smooth(method = lm, color = "black") +
  facet_wrap(~Language_name)

# ---- language competence assignment

# 0-1 intelligible words = **babbling stage**

# >2 intelligible words, <1.5 MLUw = **single word stage**

# >1.5 MLUw = **multiword stage**
  
child_word_dat_summary <- child_word_dat_summary %>% 
  mutate(child_lang_cat = case_when(
          total_words <= 1 ~ "babbling",
          total_words > 1 & MLUw < 1.5 ~ "single word",
          MLUw >= 1.5 ~ "multiword"
  )) 

# vis check

g <- child_word_dat_summary %>%
  filter(Language_name != "Mandarin" & Language_name != "Polish") %>% # can't calculate density when n < 3
  mutate(child_lang_cat = factor(child_lang_cat, levels=c("babbling", "single word", "multiword"))) %>%
  ggplot(aes(x=target_child_age, y = child_lang_cat, fill=child_lang_cat)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7) +
  theme_ridges(center_axis_labels = TRUE) + 
  facet_wrap(~Language_name) +
  scale_fill_grey(name = "", start=1, end=0,
                  guide = guide_legend(reverse = TRUE)) +
  labs(y = "", x = "child age (months)") +
  theme(axis.ticks = element_blank())

g

ggsave("../figures/ling_comp_x_age.pdf", g, width = 9, height = 6)

# ---- Language competence measure 2 (continuous)

# proportion of the following utterance types:

# contains unintelligible token (i.e., babbling)

# single word utterances

# two-word combinations or more

child_dat_cln <- child_dat_cln %>% 
  mutate(child_utt_cat = case_when(
    babble_count > 0 ~ "babble",
    word_count == 1 & babble_count == 0 ~ "single word",
    word_count > 1 & babble_count == 0 ~ "multiword"
  ), .after = word_count)

child_word_dat_prop <- child_dat_cln %>% 
  group_by(Language_name, transcript_id, target_child_age, child_utt_cat) %>%
  summarize(n = n()) %>% 
  mutate(proportion = n / sum(n))

# vis check

library("wesanderson")

child_word_dat_prop %>%
  mutate(child_utt_cat = factor(child_utt_cat, levels=c("babble", "single word", "multiword"))) %>%
  ggplot(aes(x=target_child_age, y = proportion, color=child_utt_cat)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm",size = 2) +
  facet_wrap(~Language_name, ncol = 7) +
  scale_color_manual(name = "Child utterance types",
                     values = wes_palette("Zissou1", n = 3, type = "continuous")) +
  coord_cartesian(y = c(0, 1)) +
  labs(x = "Child age (months)",
       y = "Proportion of utterance type") +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("../figures/child_utt_prop_x_age.pdf", width = 12, height = 3.6)

# ---- difference score test as a function of language competence

# type difference as a function of continuous measure of language competency

# ---- load data

cdat <- read_csv("../data/rand_dat_inc_master_cc_lexdiv.csv")
ncdat <- read_csv("../data/rand_dat_inc_master_nc_lexdiv.csv")

dat <- rbind(cdat, ncdat) %>% 
  mutate(single_word_utterance = ifelse(num_tokens==1,1,0)) %>% 
  rename(uniqueness = uniquenss)

corpora_year <- read_csv("../data/corpora_year.csv") %>%
    rename(corpus_name = Corpora) %>%
    select(corpus_name, `Year collected`)

dat <- dat %>% left_join(corpora_year)

# ---- linguistic complexity contingent and non-contingent 

lexdiv_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
                Language_name, contingent, uniqueness, num_tokens,
                `Year collected`) %>% 
  group_by(target_child_id, transcript_id, contingent, Language_name) %>% 
  summarise(variable = 'result',
            types = sum(as.numeric(unlist(uniqueness))),
            tokens = sum(num_tokens),
            age = unique(target_child_age),
            year_collected = unique(`Year collected`)) %>%
  pivot_wider(names_from = contingent,
              values_from = c(types, tokens))

mlu_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
                Language_name, contingent, num_tokens,
                `Year collected`) %>% 
  group_by(transcript_id, contingent, Language_name) %>% 
  summarise(mean = mean(num_tokens),
            age = unique(target_child_age),
            year_collected = unique(`Year collected`)) %>%
  spread(contingent, mean) %>% 
  mutate(diff = `non-contingent` - contingent)

swu_sumstats <- dat %>%
  group_by(target_child_id, transcript_id, target_child_age,
           Language_name, contingent, single_word_utterance,
           `Year collected`) %>% 
  group_by(transcript_id, contingent, Language_name) %>% 
  summarise(mean = mean(single_word_utterance),
            age = unique(target_child_age),
            year_collected = unique(`Year collected`)) %>%
  spread(contingent, mean) %>% 
  mutate(diff = `non-contingent` - contingent)

# lexical diversity and total number of words

ggplot(lexdiv_sumstats_long_types,
       aes(x = age, y = type_diff)) + 
  geom_point() +
  facet_wrap(~ Language_name, ncol = 7) +
  stat_smooth(method = "lm", col = "red")