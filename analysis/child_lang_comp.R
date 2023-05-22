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

child_word_dat_prop_multiword <- child_word_dat_prop %>%
  filter(child_utt_cat == "multiword") %>%
  rename(prop_multiword = proportion)

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
              values_from = c(types, tokens)) %>%
  left_join(child_word_dat_prop_multiword, by=c("transcript_id", "Language_name"))

# wide to long
lexdiv_sumstats_long_types <- lexdiv_sumstats %>% 
  select(target_child_id, transcript_id, Language_name, age, types_contingent,
         `types_non-contingent`, prop_multiword, year_collected) %>% 
  mutate(type_diff = `types_non-contingent` - types_contingent) %>% 
  pivot_longer(cols = c(`types_non-contingent`, types_contingent)) %>% 
  rename(Contingency = name,
         Types = value)

mlu_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
                Language_name, contingent, num_tokens,
                `Year collected`) %>%
  group_by(transcript_id, contingent, Language_name) %>%
  summarise(mean = mean(num_tokens),
            age = unique(target_child_age),
            year_collected = unique(`Year collected`)) %>%
  spread(contingent, mean) %>%
  mutate(diff = `non-contingent` - contingent) %>%
  left_join(child_word_dat_prop_multiword, by=c("transcript_id", "Language_name"))

swu_sumstats <- dat %>%
  group_by(target_child_id, transcript_id, target_child_age,
           Language_name, contingent, single_word_utterance,
           `Year collected`) %>% 
  group_by(transcript_id, contingent, Language_name) %>%
  summarise(mean = mean(single_word_utterance),
            age = unique(target_child_age),
            year_collected = unique(`Year collected`)) %>%
  spread(contingent, mean) %>%
  mutate(diff = `non-contingent` - contingent) %>%
  left_join(child_word_dat_prop_multiword, by=c("transcript_id", "Language_name"))

# lexical diversity difference and child language competence

lexdiv_sumstats_long_types %>%
  drop_na(type_diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  ggplot(., aes(x = prop_multiword, y = type_diff)) +
    geom_point() +
    facet_wrap(~ Language_name, ncol = 7) +
    stat_smooth(method = "lm", col = "red")

# ---- linear models

# model functions
tp_diff_reg_fun <- function(df) tidy(lm(df$`type_diff` ~ df$prop_multiword + df$age + df$year_collected))

# number of unique words (types)

# vector for rows to remove
to_remove <- c("Mandarin", "Polish") # less than 3 observations

tp_diff_reg_nest <- lexdiv_sumstats_long_types %>%
  filter(!Language_name %in% to_remove) %>%
  drop_na(type_diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, tp_diff_reg_fun))

tp_diff_reg_pr <- tp_diff_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  mutate(p.adj = p.adjust(p.value, "holm"),
         sig = ifelse(p.adj <0.05, "Sig.", "Non Sig."),
         p.value = format(round(p.value,3),nsmall=4),
         p.value= gsub("0.0000","<.0001",p.value),
         p.adj = format(round(p.adj,3),nsmall=4),
         p.adj = gsub("0.0000","<.0001",p.adj),
         estimate = format(round(estimate,2))) %>% 
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Lexical diversity") %>%
  arrange(Language_name)

# model functions
mlu_diff_reg_fun <- function(df) tidy(lm(df$diff ~ df$prop_multiword + df$age + df$year_collected))

swu_diff_reg_fun <- function(df) tidy(lm(df$diff ~ df$prop_multiword + df$age + df$year_collected))

# mean length of utterance in words

mlu_sumstats %>%
  drop_na(diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  ggplot(., aes(x = prop_multiword, y = diff)) +
    geom_point() +
    facet_wrap(~ Language_name, ncol = 7) +
    stat_smooth(method = "lm", col = "red")

mlu_diff_reg_nest <- mlu_sumstats %>%
  filter(!Language_name %in% to_remove) %>%
  drop_na(diff) %>%
  group_by(Language_name) %>% 
  nest() %>% 
  mutate(model = map(data, mlu_diff_reg_fun))

mlu_diff_reg_pr <- mlu_diff_reg_nest %>% 
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  mutate(p.adj = p.adjust(p.value,method="holm"),
         sig = ifelse(p.adj <0.05, "Sig.", "Non Sig."),
         p.value = format(round(p.value,3),nsmall=4),
         p.value= gsub("0.0000","<.0001",p.value),
         p.adj = format(round(p.adj,3),nsmall=4),
         p.adj = gsub("0.0000","<.0001",p.adj),
         estimate = format(round(estimate,2))) %>% 
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>%
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Mean length of utterance in words") %>% 
  arrange(Language_name)

# proportion of single word utterances

swu_sumstats %>%
  drop_na(diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  ggplot(., aes(x = prop_multiword, y = diff)) +
    geom_point() +
    facet_wrap(~ Language_name, ncol = 7) +
    stat_smooth(method = "lm", col = "red")

swu_diff_reg_nest <- swu_sumstats %>%
  filter(!Language_name %in% to_remove) %>%
  drop_na(diff) %>%
  group_by(Language_name) %>% 
  nest() %>% 
  mutate(model = map(data, swu_diff_reg_fun))

swu_diff_reg_pr <- swu_diff_reg_nest %>% 
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%  
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  mutate(p.adj = p.adjust(p.value,method="holm"),
         sig = ifelse(p.adj <0.05, "Sig.", "Non Sig."),
         p.value = format(round(p.value,3),nsmall=4),
         p.value= gsub("0.0000","<.0001",p.value),
         p.adj = format(round(p.adj,3),nsmall=4),
         p.adj = gsub("0.0000","<.0001",p.adj),
         estimate = format(round(estimate,2))) %>% 
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>%
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Proportion single word utterances") %>% 
  arrange(Language_name)

# publication-ready table
table_S13 <- bind_rows(tp_diff_reg_pr, mlu_diff_reg_pr, swu_diff_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>%
  kbl(.) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Number of unique words", 1, 12) %>%
  pack_rows("Mean length of utterance in words", 13, 24) %>%
  pack_rows("Proportion single word utterances", 25, 36)

# ---- Does age of collection correlate with children's linguistic competence?

mlu_sumstats %>%
  drop_na(diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  ggplot(., aes(x = year_collected, y = prop_multiword)) +
    geom_point() +
    facet_wrap(~ Language_name, ncol = 7) +
    stat_smooth(method = "lm", col = "red")
