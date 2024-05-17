library(tidyverse)
Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3.11")
library(reticulate)
use_python("/usr/bin/python3.11")
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
    filter(!gloss %in% c("L", "Y", "U")) %>%
    select(-1) %>% 
    arrange(transcript_id, media_start)


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
# each session is 60 minutes in total
# adding arrange line here would probably help

# ---- contingency detection

source_python("analysis/data_proc/contingent_extraction.py")

TSE_data <- TSE_data %>% 
    mutate(Langauge_name = "Tseltal",
           Language_Family = "Mayan",
           Language_Genus = "Mayan") %>% 
    filter(!is.na(gloss))

TSE_demo_data <- read_csv("data/ACLEW_list_of_corpora-recording_level-Casillas.csv")

age_dat <- TSE_demo_data %>% 
    select(aclew_id, age_mo_round) %>% 
    rename(transcript_id = aclew_id,
           target_child_age = age_mo_round) %>% 
    mutate(transcript_id = as.numeric(transcript_id))

TSE_data <- TSE_data %>%
    left_join(age_dat, by = "transcript_id") %>% 
    mutate(transcript_id = as.factor(transcript_id)) %>% 
    filter(speaker_role %in% c("Target_Child", "Mother")) %>% 
    mutate(caregiver = case_when(
                speaker_role == "Target_Child" ~ "target_child",
                speaker_role == "Mother" ~ "caregiver"))

TSE_cont_dat <- TSE_data %>% 
    assign_contingency(.,3,.001)

# save to file

TSE_cont_dat %>% 
    write_csv("data/TSE_cont_dat.csv")
