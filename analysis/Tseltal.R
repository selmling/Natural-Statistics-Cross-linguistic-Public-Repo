library(tidyverse)
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
    filter(!gloss %in% c("L", "Y", "U"))

# ---- randomly sample 10 minutes

TSE_rand_dat <- get_random_samples_TSE(TSE_data,600)
TSE_rand_dat 

