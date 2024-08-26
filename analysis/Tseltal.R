library(tidyverse)
# Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3.11")
Sys.setenv(RETICULATE_PYTHON = "/opt/homebrew/bin/python3.11")
library(reticulate)
# use_python("/usr/bin/python3.11")
use_python("/opt/homebrew/bin/python3.11")
source_python("analysis/data_proc/sample_extraction.py")

# ---- load and clean data

TSE_data <- read_csv("data/TSE_dat.csv") %>%
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
    arrange(transcript_id, media_start)

# ---- experimental look into child vocalizations

TSE_child_data <- TSE_data %>%
  filter(speaker_role == "Target_Child") %>%
  left_join(age_dat, by = "transcript_id") %>%
  mutate(gloss = factor(gloss, levels = c("C", "N", "L", "Y", "U")))

utt_props <- TSE_child_data %>%
    group_by(transcript_id, target_child_age, gloss) %>%
    summarize(n = n(), .groups = "drop") %>%
    complete(gloss, nesting(transcript_id, target_child_age), fill = list(n = 0)) %>%
    group_by(transcript_id) %>%
    mutate(proportion = n / sum(n)) %>%
    arrange(transcript_id) %>%
    ungroup()

sorted_filenames <- utt_props %>%
  filter(gloss == "C") %>%  
  arrange(proportion) %>%
  pull(transcript_id)

p1 <- ggplot(utt_props, aes(x = factor(transcript_id, levels = sorted_filenames),
                                  y = proportion, fill = gloss)) +
    geom_bar(stat = "identity") +
    labs(x = "",
         y = "Proportion of utt types",
         fill = "Utterance type") +
    scale_fill_viridis(discrete = TRUE,
                       labels = c("Y" = "Cry", "N" = "Non-canonical", "C" = "Canonical", "L"= "Laughter", "U" = "Unsure")) +
    theme_classic()

p2 <- ggplot(utt_props, aes(x = factor(transcript_id, levels = sorted_filenames), y = target_child_age)) +
  geom_point(shape=4) +
  labs(x = "Participant",
       y = "Age") +
  theme_classic()

legend <- get_legend(
  p1 + theme(legend.box.margin = margin(50, 0, 50, 0))
)

column_1 <- plot_grid(p1 + theme(legend.position="none"),
                      p2,
                      ncol=1,
                      align = "v",
                      rel_heights = c(1, .5))

plot_grid(column_1,
          legend,
          ncol=2,
          rel_widths = c(2, .5)) +
          theme(plot.margin = margin(0, 0, 0, 0, "cm"))

golden_ratio <- 1.61803398875
width <- 6.5
height <- width / golden_ratio

ggsave("figures/tseltal_child_utterance_descriptives.pdf", dpi = 1200, width = width, height = height)

# convert xds@FA1 to directedness column 

directedness_df <- TSE_data %>% filter(speaker_role == 'xds@FA1') %>%
    rename(directedness = gloss) %>%
    select(transcript_id, media_start, media_end, directedness) %>% 
    filter(directedness %in% c("T", "C"))

TSE_data <- TSE_data %>%
    filter(speaker_role != 'xds@FA1') %>%
    left_join(directedness_df, by = c('transcript_id', 'media_start', 'media_end')) %>% 
    filter(!gloss %in% c("L", "Y", "U")) # no laugh, cry or unsure

# include child directed utterances in general

TSE_data <- TSE_data %>%
  filter(!(speaker_role == "Mother" & is.na(directedness)))

# ---- test sample_extraction python functions

# at least 5 child utterances
TSE_data %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(sufficient_utterances = map_lgl(data, ~sufficient_child_utterances(.x, 5)))

# at least 5 caregiver utterances
TSE_data %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(sufficient_utterances = map_lgl(data, ~sufficient_caregiver_utterances(.x, 5)))

# has times
TSE_data %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(has_times = map_lgl(data, ~has_times(.x)))

# session durations are described here:
# Elmlinger, Goldstein & Casillas, 2023 Section 2.3
# each session 60 minutes in total
    
# ---- contingency detection

source_python("analysis/data_proc/contingent_extraction.py")

TSE_data <- TSE_data %>% 
    mutate(Language_name = "Tseltal",
           Language_Family = "Mayan",
           Language_Genus = "Mayan") %>% 
    filter(!is.na(gloss))

TSE_demo_data <- read_csv("data/ACLEW_list_of_corpora-recording_level-Casillas.csv")

age_dat <- TSE_demo_data %>% 
    select(aclew_id, age_mo_round) %>% 
    rename(transcript_id = aclew_id,
           target_child_age = age_mo_round) %>% 
    mutate(transcript_id = as.numeric(transcript_id))

TSE_demo_data %>% select(aclew_id)

TSE_data <- TSE_data %>%
    left_join(age_dat, by = "transcript_id") %>% 
    mutate(transcript_id = as.factor(transcript_id)) %>% 
    filter(speaker_role %in% c("Target_Child", "Mother")) %>% 
    mutate(caregiver = case_when(
                speaker_role == "Target_Child" ~ "target_child",
                speaker_role == "Mother" ~ "caregiver"),
                target_child_id = transcript_id) %>%
    arrange(transcript_id, media_start)

# transcription clense
TSE_data <- TSE_data %>%
    mutate(gloss = str_replace_all(gloss,
                                c("@s:spa" = "",
                                  "&=laughs" = "",
                                  "&=inhales" = "",
                                  "&=sucksteeth" = "",
                                  "&=sucksin" = "",
                                  "&=sings" = "",
                                  "@c" = "",
                                  "yyy" = "",
                                  "nt" = "",
                                  " x " = ""))) %>% 
    create_result(.)

# create targetchild speech only dataframe

child_dat <- TSE_data %>% 
    filter(speaker_role == "Target_Child") %>%
    write_csv("data/TSE_child_dat.csv")

# assign contingency
TSE_cont_dat <- TSE_data %>% 
    assign_contingency(.,3,.001)

# ---- save to file

TSE_cont_dat %>% 
    select(-directedness) %>%
    write_csv("data/TSE_cont_dat.csv")
