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

# convert xds@FA1 to caregiver column 

# include child directed utterances in general

# see if column spill-over is happening to 60 some utterances... if so, fix...

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

# load Tseltal time chunks

TSE_time_chunks <- read_csv("data/TSE_time_chunks.csv") %>%
    mutate(duration = offset - onset,
           duration_min = duration/60)

# set seed for reproducibility
set.seed(74)

# randomly sample 2 5-minute chunks from each transcript_id

TSE_rand_time_chunks <- TSE_time_chunks %>%
    filter(duration_min >= 5) %>% 
    group_by(filename) %>%
    sample_n(2) %>%
    ungroup() %>%
    mutate(transcript_id = str_remove(filename, ".eaf"),
           media_start = onset,
           media_end = offset) %>%
    select(transcript_id, media_start, media_end)

# Give each chunk an index

TSE_rand_time_chunks <- TSE_rand_time_chunks %>%
    group_by(transcript_id) %>%
    mutate(chunk_index = row_number()) %>%
    ungroup()

TSE_rand_time_chunks %>%
    write_csv("data/TSE_rand_time_chunks.csv")
    
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

# assign contingency
TSE_cont_dat <- TSE_data %>% 
    assign_contingency(.,3,.001)

# ---- assign chunk_index to each row in TSE_cont_dat based on media_start and media_end

# Function to find chunk index based on start and end times
check_event_in_chunks <- function(start_time, end_time, chunks) {
  chunks %>%
    filter(media_start <= start_time, media_end >= end_time) %>%
    slice(1) %>%
    pull(chunk_index) %>%
    {if (length(.) == 0) NA else .}
}

# nest data by transcript_id
TSE_cont_dat_nest <- TSE_cont_dat %>%
  group_by(transcript_id) %>%
  nest()

# nest time chunks by transcript_id
TSE_rand_time_chunk_nest <- TSE_rand_time_chunks %>%
  group_by(transcript_id) %>%
  nest()

# merge nests
TSE_rand_dat <- TSE_cont_dat_nest %>%
  left_join(TSE_rand_time_chunk_nest, by = "transcript_id", suffix = c("", "_chunks"))

# denote chunk index for each row
TSE_rand_dat <- TSE_rand_dat %>%
  mutate(data = map2(data, data_chunks, ~ .x %>%
         rowwise() %>%
         mutate(chunk_index = check_event_in_chunks(media_start, media_end, .y)) %>%
         ungroup())) %>%
  select(-data_chunks) %>%
  unnest(data)

# filter to keep only rows with chunk index
TSE_rand_dat <- TSE_rand_dat %>%
  filter(!is.na(chunk_index))

# ---- check for sufficient utterances

# at least 5 child utterances
CH_U_req <- TSE_rand_dat %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(sufficient_utterances = map_lgl(data, ~sufficient_child_utterances(.x, 5)))

# at least 5 caregiver utterances
CG_U_req <- TSE_rand_dat %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(sufficient_utterances = map_lgl(data, ~sufficient_caregiver_utterances(.x, 5)))

# take new time chunks where sufficient_utterances == False

req_fails <- CH_U_req %>%
    select(transcript_id, sufficient_utterances) %>%
    left_join(CG_U_req, by = c("transcript_id")) %>%
    select(transcript_id, sufficient_utterances.x, sufficient_utterances.y) %>%
    filter(sufficient_utterances.x == FALSE | sufficient_utterances.y == FALSE)

needs_new_time_chunks <- TSE_rand_time_chunks %>%
    filter(transcript_id %in% req_fails$transcript_id)

set.seed(81)

TSE_new_time_chunks <- TSE_time_chunks %>%
    mutate(transcript_id = str_remove(filename, ".eaf"),
           media_start = onset,
           media_end = offset) %>%
    filter(transcript_id %in% needs_new_time_chunks$transcript_id,
           duration_min >= 5) %>% 
    group_by(transcript_id) %>%
    sample_n(2) %>%
    ungroup() %>%
    select(transcript_id, media_start, media_end) %>%
    group_by(transcript_id) %>%
    mutate(chunk_index = row_number()) %>%
    ungroup()

# extract new data for those two transcripts

TSE_new_rand_time_chunk_nest <- TSE_new_time_chunks %>%
  group_by(transcript_id) %>%
  nest()

TSE_new_rand_dat <- TSE_cont_dat_nest %>%
  left_join(TSE_new_rand_time_chunk_nest, by = "transcript_id", suffix = c("", "_chunks")) %>% 
  filter(!map_lgl(data_chunks, is.null))

# denote chunk index for each row
TSE_new_rand_dat <- TSE_new_rand_dat %>%
  mutate(data = map2(data, data_chunks, ~ .x %>%
         rowwise() %>%
         mutate(chunk_index = check_event_in_chunks(media_start, media_end, .y)) %>%
         ungroup())) %>%
  select(-data_chunks) %>%
  unnest(data)

# filter to keep only rows with chunk index
TSE_new_rand_dat <- TSE_new_rand_dat %>%
  filter(!is.na(chunk_index))

# add those newly extracted transcripts to the main dataset (after removing the old)

TSE_rand_dat <- TSE_rand_dat %>% 
    filter(!transcript_id %in% TSE_new_rand_dat$transcript_id)

TSE_rand_dat <- bind_rows(TSE_rand_dat, TSE_new_rand_dat)

# dplyr call to list unique transcript_id in TSE_rand_dat
TSE_rand_dat %>% 
    select(transcript_id) %>% 
    unique()

# at least 5 child utterances
TSE_rand_dat %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(sufficient_utterances = map_lgl(data, ~sufficient_child_utterances(.x, 5)))

# at least 5 caregiver utterances
TSE_rand_dat %>%
    group_by(transcript_id) %>% nest() %>%
    mutate(sufficient_utterances = map_lgl(data, ~sufficient_caregiver_utterances(.x, 5)))
    
# ---- save to file

TSE_rand_dat %>% 
    write_csv("data/TSE_cont_dat.csv")
