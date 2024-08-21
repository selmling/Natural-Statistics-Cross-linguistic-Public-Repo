library("ggnewscale")
library("kableExtra")
library("tidyverse")
library("ggridges")
library("cowplot")
library("broom")

# ---- Language proficiency measure 1

child_dat <- read_csv("data/child_dat.csv")

# Tseltal target child speech was not transcribed, thus not included in this analysis

TSE_child_data <- read_csv("data/TSE_dat.csv") %>%
    rename(transcript_id = sub,
           media_start = onset,
           media_end = offset,
           speaker_role = tier,
           gloss = cat) %>%
    mutate(speaker_role = case_when(
                speaker_role == "vcm@CHI" ~ "Target_Child",
                speaker_role == "FA1" ~ "Mother",
                TRUE ~ speaker_role),
            language = "tzh",
            corpus_name = "Casillas") %>%
    select(-1) %>% 
    arrange(transcript_id, media_start) %>%
    filter(speaker_role == "Target_Child") %>%
    left_join(age_dat, by = "transcript_id") %>%
    mutate(gloss = factor(gloss, levels = c("C", "N", "L", "Y", "U")))

# drop child blank utterances

child_dat_cln <- child_dat %>%
  filter(!gloss == "")

# drop kids who produced fewer than 5 non-blank utterances

excluders <- child_dat_cln %>%
  group_by(transcript_id) %>%
  summarise(utterance_count = n()) %>%
  filter(utterance_count < 5) %>%
  pull(transcript_id)

child_dat_cln <- child_dat_cln %>%
  filter(!transcript_id %in% excluders) %>%
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

# ---- language proficiency assignment

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

# ---- Language proficiency measure 2 (continuous)

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
  summarize(n = n(), .groups = 'drop') %>%
  complete(child_utt_cat, nesting(transcript_id, Language_name, target_child_age), fill = list(n = 0))%>% 
  group_by(transcript_id) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(Language_name, transcript_id, target_child_age, child_utt_cat, proportion)

# Tseltal

TSE_child_word_dat_prop <- TSE_child_data %>%
    group_by(transcript_id, target_child_age, gloss) %>%
    summarize(n = n(), .groups = "drop") %>%
    complete(gloss, nesting(transcript_id, target_child_age), fill = list(n = 0)) %>%
    group_by(transcript_id) %>%
    mutate(proportion = n / sum(n)) %>%
    arrange(transcript_id) %>%
    ungroup()

# vis check

library("wesanderson")

p01 <- child_word_dat_prop %>%
  filter(!Language_name %in% c("Mandarin", "Polish")) %>% 
  mutate(child_utt_cat = factor(child_utt_cat, levels=c("babble", "single word", "multiword"))) %>%
  ggplot(aes(x=target_child_age, y = proportion, color=child_utt_cat)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm",size = 2) +
  facet_wrap(~Language_name, ncol = 6) +
  scale_color_manual(name = "Child utterance types",
                     values = wes_palette("Zissou1", n = 3, type = "continuous")) +
  coord_cartesian(y = c(0, 1)) +
  labs(x = "Child age (months)",
       y = "Proportion of utterance type") +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 12))

p02 <- TSE_child_word_dat_prop %>%
  filter(!gloss %in% c("L", "Y", "U")) %>% # laughter, cry, unsure
  mutate(language = "Tseltal") %>%
  ggplot(aes(x = target_child_age, y = proportion, color = gloss)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm",size = 2) +
  coord_cartesian(y = c(0, 1),
                  xlim = c(0, 40)) +
  scale_x_continuous(breaks = seq(5, 35, by = 5)) +
  labs(x = "Child age (months)",
       y = "Proportion of\nutterance type") +
  scale_color_manual(name = "Child utterance types",
                     values = c("C" = "#3B9AB2", "N" = "#4E9A06"),
                     labels = c("C" = "canonical babble", "N" = "non-canonical babble")) +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = .70) +
  facet_wrap(~language)

plot_grid(p01, p02, ncol = 1, labels = c("", ""),
          rel_heights = c(2, 1))

ggsave("figures/child_utt_prop_x_age.pdf", width = 12, height = 6, dpi = 1200)

# ---- difference score test as a function of language competence

# type difference as a function of continuous measure of language competency

# ---- load data

child_word_dat_prop_multiword <- child_word_dat_prop %>%
  filter(child_utt_cat == "multiword") %>%
  rename(prop_multiword = proportion)

TSE_child_word_dat_prop_canonical <- TSE_child_word_dat_prop %>%
  filter(gloss == "C") %>%
  rename(prop_canonical = proportion,
         child_utt_cat = gloss)

cdat <- read_csv("data/rand_dat_inc_master_cc_lexdiv.csv")
ncdat <- read_csv("data/rand_dat_inc_master_nc_lexdiv.csv")

dat <- rbind(cdat, ncdat) %>% 
  mutate(single_word_utterance = ifelse(num_tokens==1,1,0))

# ---- linguistic complexity contingent and non-contingent 

lexdiv_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
                Language_name, contingent, uniqueness, num_tokens) %>%
                # `Year collected`
  group_by(target_child_id, transcript_id, contingent, Language_name) %>% 
  summarise(variable = 'result',
            types = sum(as.numeric(unlist(uniqueness))),
            tokens = sum(num_tokens),
            age = unique(target_child_age),
            ) %>%
  pivot_wider(names_from = contingent,
              values_from = c(types, tokens)) %>%
  left_join(child_word_dat_prop_multiword, by=c("transcript_id", "Language_name")) %>%
  filter(Language_name != "Tseltal") 

TSE_lexdiv_sumstats <- dat %>% 
  filter(Language_name == "Tseltal") %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
                Language_name, contingent, uniqueness, num_tokens) %>%
                # `Year collected`
  group_by(target_child_id, transcript_id, contingent) %>% 
  summarise(variable = 'result',
            types = sum(as.numeric(unlist(uniqueness))),
            tokens = sum(num_tokens),
            age = unique(target_child_age),
            ) %>%
  pivot_wider(names_from = contingent,
              values_from = c(types, tokens)) %>%
  left_join(TSE_child_word_dat_prop_canonical, by=c("transcript_id"))

# wide to long
lexdiv_sumstats_long_types <- lexdiv_sumstats %>% 
  select(target_child_id, transcript_id, Language_name, age, types_contingent,
         `types_non-contingent`, prop_multiword) %>% 
  mutate(type_diff = `types_non-contingent` - types_contingent) %>% 
  pivot_longer(cols = c(`types_non-contingent`, types_contingent)) %>% 
  rename(Contingency = name,
         Types = value)

TSE_lexdiv_sumstats_long_types <- TSE_lexdiv_sumstats %>% 
  select(target_child_id, transcript_id, age, types_contingent,
         `types_non-contingent`, prop_canonical) %>% 
  mutate(type_diff = `types_non-contingent` - types_contingent) %>% 
  pivot_longer(cols = c(`types_non-contingent`, types_contingent)) %>% 
  rename(Contingency = name,
         Types = value)

mlu_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
                Language_name, contingent, num_tokens) %>%
  group_by(transcript_id, contingent, Language_name) %>%
  summarise(mean = mean(num_tokens),
            age = unique(target_child_age)) %>%
  spread(contingent, mean) %>%
  mutate(diff = `non-contingent` - contingent) %>%
  left_join(child_word_dat_prop_multiword, by=c("transcript_id", "Language_name"))

swu_sumstats <- dat %>%
  group_by(target_child_id, transcript_id, target_child_age,
           Language_name, contingent, single_word_utterance) %>% 
  group_by(transcript_id, contingent, Language_name) %>%
  summarise(mean = mean(single_word_utterance),
            age = unique(target_child_age)) %>%
  spread(contingent, mean) %>%
  mutate(diff = `non-contingent` - contingent) %>%
  left_join(child_word_dat_prop_multiword, by=c("transcript_id", "Language_name"))

# ---- Figure 3

# lexical diversity difference and child language competence

label_custom <- function(x) {
  # Create a custom labeling function
  ifelse(x == 1, "1", ifelse(x == 0, "0", sprintf(".%s", substr(format(x), 3, 4))))
}

stroke = 1
shape = 21
size = 2

lexdiv_sumstats_long_types <- lexdiv_sumstats_long_types %>% 
  filter(!Language_name %in% c("Mandarin", "Polish", "Tseltal"))

p1 <- lexdiv_sumstats_long_types %>%
  drop_na(type_diff) %>%
  ggplot(., aes(color = Language_name)) +
  geom_point(aes(x = prop_multiword, y = Types, fill = Contingency),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color=FALSE) +
  new_scale_color() +
  stat_smooth(method=lm,size=1.2, se = TRUE,
              aes (x = prop_multiword, y = Types, color=Contingency)) +
  facet_wrap(. ~ Language_name,ncol = 6) +
  scale_color_manual(labels = c("Contingent", "Non-Contingent"),
                     values = alpha(c("black","white"), .95)) +
  scale_fill_manual(values = alpha(c("black", "white"), .3)) +
  guides(color=FALSE,
         fill=FALSE) +
  ylim(0, 400) +
  scale_x_continuous(limits = c(0, 1), labels = label_custom) +
  labs(tag = "A",
       y = "Number of unique words",
       x = "Child language proficiency \n (prop. multiword utterances)") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        aspect.ratio = .75,
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.90),
                                         size = 0, linetype = "dotted",
                                         colour = "white"),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey90"))

# wide to long
lexdiv_sumstats_long_tokens <- lexdiv_sumstats %>% 
  dplyr::select(transcript_id, Language_name, age, prop_multiword,
                tokens_contingent, `tokens_non-contingent`) %>% 
  mutate(token_diff = `tokens_non-contingent` - tokens_contingent) %>% 
  pivot_longer(cols = c(`tokens_non-contingent`, tokens_contingent)) %>% 
  rename(Contingency = name,
         Tokens = value) %>% 
  filter(!Language_name %in% c("Mandarin", "Polish", "Tseltal"))

p2 <- ggplot(lexdiv_sumstats_long_tokens, aes(color = Language_name)) +
  geom_point(aes(x = prop_multiword, y = Tokens, fill = Contingency),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color=FALSE) +
  new_scale_color() +
  stat_smooth(method=lm,size=1.2, se = TRUE,
              aes (x = prop_multiword, y = Tokens, color=Contingency)) +
  facet_wrap(. ~ Language_name,ncol = 6) +
  scale_color_manual(labels = c("Contingent", "Non-Contingent"),
                     values = alpha(c("black","white"), .95)) +
  scale_fill_manual(values = alpha(c("black", "white"), .3)) +
  guides(fill=FALSE) +
  ylim(0, 1500) +
  scale_x_continuous(limits = c(0, 1), labels = label_custom) +
  labs(tag = "B",
       y = "Total number of words",
       x = "Child language proficiency \n (prop. multiword utterances)") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        aspect.ratio = .75,
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.90),
                                         size = 0, linetype = "dotted",
                                         colour = "white"),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey90"))

legend <- cowplot::get_plot_component(p2, 'guide-box-bottom', return_all = TRUE)

pcol <- plot_grid(p1, p2 + theme(legend.position='none'), ncol = 1, labels = c("", ""))

g <- plot_grid(pcol, legend, ncol = 1, rel_heights = c(3, .4))

g

ggsave("figures/Figure_3.pdf", g, width = 12, height = 8.5)

# ---- difference score test by language proficiency

# model functions
tp_diff_reg_fun <- function(df) tidy(lm(df$`type_diff` ~ df$prop_multiword))

tk_diff_reg_fun <- function(df) tidy(lm(df$`token_diff` ~ df$prop_multiword))

# format function
adjust_pvalues_and_format <- function(df) {
  df %>%
    mutate(p.adj = p.adjust(p.value, "holm"),
           sig = ifelse(p.adj <0.05, "Sig.", "Non Sig."),
           p.value = format(round(p.value,3),nsmall=4),
           p.value= gsub("0.0000","<.0001",p.value),
           p.adj = format(round(p.adj,3),nsmall=4),
           p.adj = gsub("0.0000","<.0001",p.adj),
           estimate = format(round(estimate,2)))
}

# number of unique words (types)

# vector for rows to remove
to_remove <- c("Mandarin", "Polish", "Tseltal") # less than 3 observations

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
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Lexical diversity") %>%
  arrange(Language_name)

# number of words (tokens)

tk_diff_reg_nest <- lexdiv_sumstats_long_tokens %>%
  filter(!Language_name %in% to_remove) %>%
  drop_na(token_diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>% 
  nest() %>% 
  mutate(model = map(data, tk_diff_reg_fun))

tk_diff_reg_pr <- tk_diff_reg_nest %>% 
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  filter(!term %in% c("(Intercept)","df$year_collected")) %>%
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Total # of words") %>% 
  arrange(Language_name)

# publication-ready table
table_S11 <- bind_rows(tp_diff_reg_pr, tk_diff_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>% 
  kbl(.) %>% 
  kable_paper("striped", full_width = F) %>%
  pack_rows("Lexical diversity", 1, 12) %>%
  pack_rows("Total number of words", 13, 24)


# model functions
mlu_diff_reg_fun <- function(df) tidy(lm(df$diff ~ df$prop_multiword))

swu_diff_reg_fun <- function(df) tidy(lm(df$diff ~ df$prop_multiword))

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
  adjust_pvalues_and_format() %>%
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
  adjust_pvalues_and_format() %>%
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

# ---- contingent increase test

# model functions
tp_reg_fun <- function(df) tidy(lm(df$`Types` ~ df$prop_multiword))

tk_reg_fun <- function(df) tidy(lm(df$`Tokens` ~ df$prop_multiword))

# number of unique words (types)

# contingent

C_tp_reg_nest <- lexdiv_sumstats_long_types %>%
  filter(!Language_name %in% to_remove,
         Contingency == "types_contingent") %>%
  drop_na(Types) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, tp_reg_fun))

C_tp_reg_pr <- C_tp_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Contingent lexical diversity") %>%
  arrange(Language_name)

# non-contingent

NC_tp_reg_nest <- lexdiv_sumstats_long_types %>%
  filter(!Language_name %in% to_remove,
         Contingency == "types_non-contingent") %>%
  drop_na(Types) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, tp_reg_fun))

NC_tp_reg_pr <- NC_tp_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Non-contingent lexical diversity") %>%
  arrange(Language_name)

# publication-ready table
table_S14 <- bind_rows(C_tp_reg_pr, NC_tp_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>%
  kbl(.) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Number of contingent unique words", 1, 12) %>%
  pack_rows("Number of non-contingent unique words", 13, 24)

# number of words (tokens)

# contingent

C_tk_reg_nest <- lexdiv_sumstats_long_tokens %>%
  filter(!Language_name %in% to_remove,
         Contingency == "tokens_contingent") %>%
  drop_na(Tokens) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, tk_reg_fun))

C_tk_reg_pr <- C_tk_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Contingent total # of words") %>%
  arrange(Language_name)

# non-contingent

NC_tk_reg_nest <- lexdiv_sumstats_long_tokens %>%
  filter(!Language_name %in% to_remove,
         Contingency == "tokens_non-contingent") %>%
  drop_na(Tokens) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, tk_reg_fun))

NC_tk_reg_pr <- NC_tk_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Non-contingent total # of words") %>%
  arrange(Language_name)

# publication-ready table
table_S15 <- bind_rows(C_tk_reg_pr, NC_tk_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>%
  kbl(.) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Total number of contingent words", 1, 12) %>%
  pack_rows("Total number of non-contingent words", 13, 24)

# model functions
mlu_reg_fun <- function(df) tidy(lm(df$`Types` ~ df$prop_multiword))

swu_reg_fun <- function(df) tidy(lm(df$`Tokens` ~ df$prop_multiword))

mlu_sumstats <- mlu_sumstats %>%
  pivot_longer(cols = c(contingent, `non-contingent`),
               names_to = "Contingency",
               values_to = "MLUw")
  
swu_sumstats <- swu_sumstats %>%
  pivot_longer(cols = c(contingent, `non-contingent`),
               names_to = "Contingency",
               values_to = "SWU")

# Check from here forward:
# mean length of utterance in words

# contingent

C_mlu_reg_nest <- mlu_sumstats %>%
  filter(!Language_name %in% to_remove,
         Contingency == "contingent") %>%
  drop_na(MLUw) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, mlu_reg_fun))

C_mlu_reg_pr <- C_mlu_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Contingent mean length of utterance in words") %>%
  arrange(Language_name)

  # non-contingent

  NC_mlu_reg_nest <- mlu_sumstats %>%
  filter(!Language_name %in% to_remove,
         Contingency == "non-contingent") %>%
  drop_na(MLUw) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, mlu_reg_fun))

NC_mlu_reg_pr <- NC_mlu_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Non-contingent mean length of utterance in words") %>%
  arrange(Language_name)

# publication-ready table

table_S16 <- bind_rows(C_mlu_reg_pr, NC_mlu_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>%
  kbl(.) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Mean length of contingent utterances in words", 1, 12) %>%
  pack_rows("Mean length of non-contingent utterances in words", 13, 24)

# proportion of single word utterances

# contingent

C_swu_reg_nest <- swu_sumstats %>%
  filter(!Language_name %in% to_remove,
         Contingency == "contingent") %>%
  drop_na(SWU) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, swu_reg_fun))

C_swu_reg_pr <- C_swu_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Contingent proportion single word utterances") %>%
  arrange(Language_name)

  # non-contingent

  NC_swu_reg_nest <- swu_sumstats %>%
  filter(!Language_name %in% to_remove,
         Contingency == "non-contingent") %>%
  drop_na(SWU) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>%
  nest() %>%
  mutate(model = map(data, swu_reg_fun))

NC_swu_reg_pr <- NC_swu_reg_nest %>%
  dplyr::select(-data) %>%
  unnest(cols = c(model)) %>%
  ungroup() %>%
  filter(!term %in% c("(Intercept)", "df$year_collected", "df$age")) %>%
  adjust_pvalues_and_format() %>%
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Non-contingent proportion single word utterances") %>%
  arrange(Language_name)

# publication-ready table

table_S17 <- bind_rows(C_swu_reg_pr, NC_swu_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>%
  kbl(.) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Proportion of single word utterances in contingent utterances", 1, 12) %>%
  pack_rows("Proportion of single word utterances in non-contingent utterances", 13, 24)
