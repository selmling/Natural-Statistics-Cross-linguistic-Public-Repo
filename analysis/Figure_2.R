library('tidyverse')
library('patchwork')
library('ggplot2')
library('repr')

rand_lex_sumstats <- pd.read_csv("../data/rand_lex_sumstats.csv",index_col=0)
rand_mlu_sumstats <- pd.read_csv("../data/rand_mlu_sumstats.csv",index_col=0)
rand_swu_sumstats <- pd.read_csv("../data/rand_swu_sumstats.csv",index_col=0)