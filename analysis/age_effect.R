# Are there infant age effects in the extent to which caregiver contingent speech is simplified?

library("tidyverse")
library("gridExtra")
library("ggnewscale")
library("kableExtra")
library("ggpubr")
library("broom")

dropLeadingZero <- function(l){
  str_replace(l, '0(?=.)', '')
}

# ---- load data

cdat <- read_csv("../data/rand_dat_inc_master_cc_lexdiv.csv")
ncdat <- read_csv("../data/rand_dat_inc_master_nc_lexdiv.csv")

dat <- rbind(cdat, ncdat) %>% 
  mutate(single_word_utterance = ifelse(num_tokens==1,1,0)) %>% 
  rename(uniqueness = uniquenss)

# ---- linguistic complexity contingent and non-contingent 

lexdiv_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
         Language_name, contingent, uniqueness, num_tokens) %>% 
  group_by(transcript_id, contingent, Language_name) %>% 
  summarise(variable = 'result',
            types = sum(as.numeric(unlist(uniqueness))),
            tokens = sum(num_tokens),
            age = unique(target_child_age)) %>%
  pivot_wider(names_from = contingent,
              values_from = c(types, tokens))

mlu_sumstats <- dat %>%
  dplyr::select(target_child_id, transcript_id, target_child_age,
         Language_name, contingent, num_tokens) %>% 
  group_by(transcript_id, contingent, Language_name) %>% 
  summarise(mean = mean(num_tokens),
            age = unique(target_child_age)) %>%
  spread(contingent, mean) %>% 
  mutate(diff = `non-contingent` - contingent)

swu_sumstats <- dat %>%
  group_by(target_child_id, transcript_id, target_child_age,
           Language_name, contingent, single_word_utterance) %>% 
  group_by(transcript_id, contingent, Language_name) %>% 
  summarise(mean = mean(single_word_utterance),
            age = unique(target_child_age)) %>%
  spread(contingent, mean) %>% 
  mutate(diff = `non-contingent` - contingent)

# ---- dumbbell visualization

# wide to long
lexdiv_sumstats_long_types <- lexdiv_sumstats %>% 
  dplyr::select(transcript_id, Language_name, age, types_contingent,
         `types_non-contingent`) %>% 
  mutate(type_diff = `types_non-contingent` - types_contingent) %>% 
  pivot_longer(cols = c(`types_non-contingent`, types_contingent)) %>% 
  rename(Contingency = name,
         Types = value)

# separate contingent and non-contingent

non_contingent_types <- lexdiv_sumstats_long_types %>%
  filter(Contingency == "types_non-contingent")

contingent_types <- lexdiv_sumstats_long_types %>%
  filter(Contingency == "types_contingent")

# plot 1

stroke = 1
shape = 21
size = 2

p1 <- ggplot(lexdiv_sumstats_long_types, aes(color = Language_name)) +
  geom_point(aes(x = age, y = Types, fill = Contingency),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color=FALSE) +
  new_scale_color() +
  stat_smooth(method=lm,size=1.2, se = TRUE,
              aes (x = age, y = Types, color=Contingency)) +
  facet_wrap(. ~ Language_name,ncol = 7) +
  scale_color_manual(labels = c("Contingent", "Non-Contingent"),
                     values = alpha(c("black","white"), .95)) +
  scale_fill_manual(values = alpha(c("black", "white"), .3)) +
  guides(fill=FALSE) +
  ylim(0, 400) +
  labs(tag = "A",
       y = "Number of unique words",
       x = "Child age (months)") +
  theme_classic() +
  theme(text = element_text(size = 14),
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
  dplyr::select(transcript_id, Language_name, age, tokens_contingent,
                `tokens_non-contingent`) %>% 
  mutate(token_diff = `tokens_non-contingent` - tokens_contingent) %>% 
  pivot_longer(cols = c(`tokens_non-contingent`, tokens_contingent)) %>% 
  rename(Contingency = name,
         Tokens = value)

# separate contingent and non-contingent

non_contingent_tokens <- lexdiv_sumstats_long_tokens %>%
  filter(Contingency == "tokens_non-contingent")

contingent_tokens <- lexdiv_sumstats_long_tokens %>%
  filter(Contingency == "tokens_contingent")

# plot 2

p2 <- ggplot(lexdiv_sumstats_long_tokens, aes(color = Language_name)) +
  geom_point(aes(x = age, y = Tokens, fill = Contingency),
             size = size, shape = shape, stroke = stroke,
             show.legend = TRUE) +
  guides(color=FALSE) +
  new_scale_color() +
  stat_smooth(method=lm,size=1.2, se = TRUE,
              aes (x = age, y = Tokens, color=Contingency)) +
  facet_wrap(. ~ Language_name,ncol = 7) +
  scale_color_manual(values = alpha(c(tokens_contingent = "black",
                                      `tokens_non-contingent`="white"), .95)) +
  scale_fill_manual(values = alpha(c("black", "white"), .3)) +
  ylim(0, 1500) +
  labs(tag = "B",
       y = "Total number of words",
       x = "Child age (months)") +
  theme_classic() +
  theme(text = element_text(size = 14),
        aspect.ratio = .75,
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.90),
                                         size = 0, linetype = "dotted",
                                         colour = "white"),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey90"))

g <- ggarrange(p1, p2, ncol=1, align = "v", common.legend = TRUE, legend="bottom")
g

ggsave("../figures/age_effects_all_linear_4.pdf", g, width = 12, height = 8.5)

# ---- difference score test over development

# lexical diversity and total number of words

ggplot(lexdiv_sumstats_long_types,
       aes(x = age, y = type_diff)) + 
  geom_point() +
  facet_wrap(~ Language_name, ncol = 7) +
  stat_smooth(method = "lm", col = "red")

ggplot(lexdiv_sumstats_long_tokens,
       aes(x = age, y = token_diff)) + 
  geom_point() +
  facet_wrap(~ Language_name, ncol = 7) +
  stat_smooth(method = "lm", col = "red")

tp_diff_reg_fun <- function(df) tidy(lm(df$`type_diff` ~ df$age))

tk_diff_reg_fun <- function(df) tidy(lm(df$`token_diff` ~ df$age))

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
  unnest() %>%
  ungroup() %>% 
  mutate(p.adj = p.adjust(p.value,method="holm"),
         sig = ifelse(p.adj <0.05, "Sig.", "Non Sig."),
         p.value = format(round(p.value,3),nsmall=4),
         p.value= gsub("0.0000","<.0001",p.value),
         p.adj = format(round(p.adj,3),nsmall=4),
         p.adj = gsub("0.0000","<.0001",p.adj),
         estimate = format(round(estimate,2))) %>% 
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  filter((term != '(Intercept)')) %>%
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Lexical diversity") %>%
  arrange(Language_name)

tk_diff_reg_nest <- lexdiv_sumstats_long_tokens %>%
  filter(!Language_name %in% to_remove) %>%
  drop_na(token_diff) %>%
  distinct(transcript_id, .keep_all = TRUE) %>%
  group_by(Language_name) %>% 
  nest() %>% 
  mutate(model = map(data, tk_diff_reg_fun))

tk_diff_reg_pr <- tk_diff_reg_nest %>% 
  dplyr::select(-data) %>%
  unnest() %>%
  ungroup() %>% 
  mutate(p.adj = p.adjust(p.value,method="holm"),
         sig = ifelse(p.adj <0.05, "Sig.", "Non Sig."),
         p.value = format(round(p.value,3),nsmall=4),
         p.value= gsub("0.0000","<.0001",p.value),
         p.adj = format(round(p.adj,3),nsmall=4),
         p.adj = gsub("0.0000","<.0001",p.adj),
         estimate = format(round(estimate,2))) %>% 
  dplyr::select(c(Language_name, term, estimate, sig, p.value, p.adj)) %>% 
  filter((term != '(Intercept)')) %>%
  mutate(term = str_remove_all(term, "[df$]"),
         measure = "Total # of words") %>% 
  arrange(Language_name)

# publication-ready table
table_S4 <- bind_rows(tp_diff_reg_pr, tk_diff_reg_pr) %>%
  dplyr::select(Language_name, estimate, p.value, p.adj) %>%
  rename(Language = Language_name, Estimate = estimate,
         `p-value` = p.value, `Adjusted p-value`= p.adj) %>% 
  kbl(.) %>% 
  kable_paper("striped", full_width = F) %>%
  pack_rows("Lexical diversity", 1, 12) %>%
  pack_rows("Total number of words", 13, 24)