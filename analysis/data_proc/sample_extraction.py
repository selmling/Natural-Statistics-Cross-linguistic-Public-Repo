from childes_sampling import *
from sampling_viz import *
import childespy as cpy
import pandas as pd
import numpy as np
import math

np.random.seed(0)

def get_random_samples(corpora, interval):
    random_dat = pd.DataFrame()
    for i in corpora:
        c = cpy.get_utterances(corpus=i)
        print(i)
        ids = c['transcript_id'].unique()
        valid_ids = []
        for i in ids:
            tran = c[c['transcript_id']==i]
            if longer_than_ten(tran) and \
               sufficient_child_utterances(tran, 5) and \
               sufficient_caregiver_utterances(tran, 5) and \
               has_times(tran) and \
               "Target_Child" in list(tran["speaker_role"]):
                valid_ids.append(i)
        errs = []
        for tid in valid_ids:
            tran = c[c['transcript_id']==tid]
            try:
              rand = random_sample(tran, interval)
              random_dat = pd.concat([random_dat, rand], ignore_index=True)
            except:
              errs.append(tid)
    print("Errors: ", errs)
    return random_dat

def get_Persian_samples(corpora):
    pers_dat = pd.DataFrame()
    for i in corpora:
        c = cpy.get_utterances(corpus=i)
        print(i)
        ids = c['transcript_id'].unique()
        valid_ids = []
        for i in ids:
            tran = c[c['transcript_id']==i]
            if longer_than_ten(tran) and \
               sufficient_child_utterances(tran, 5) and \
               sufficient_caregiver_utterances(tran, 5) and \
               has_times(tran) and \
               "Target_Child" in list(tran["speaker_role"]):
                valid_ids.append(i)
        errs = []
        for tid in valid_ids:
            tran = c[c['transcript_id']==tid]
            try:
              pers_dat = pd.concat([pers_dat, tran], ignore_index=True)
            except:
              errs.append(tid)
    print("Errors: ", errs)
    return pers_dat

def get_random_samples_TSE(corpora, interval):
    c = corpora
    random_dat = pd.DataFrame()    
    ids = c['transcript_id'].unique()
    errs = []
    for tid in ids:
        tran = c[c['transcript_id']==tid]
        try:
          rand = random_sample(tran, interval)
          random_dat = pd.concat([random_dat, rand], ignore_index=True)
        except:
          errs.append(tid)
    print("Errors: ", errs)
    return random_dat

def get_maxvoc_samples(corpora, interval):
    maxvoc_dat = pd.DataFrame()
    for i in corpora:    
        c = cpy.get_utterances(corpus=i)
        ids = c['transcript_id'].unique()
        valid_ids = []
        for i in ids:
            tran = c[c['transcript_id']==i]
            if longer_than_ten(tran) and \
               sufficient_child_utterances(tran, 5) and \
               sufficient_caregiver_utterances(tran, 5) and \
               has_times(tran) and \
               "Target_Child" in list(tran["speaker_role"]):
                valid_ids.append(i)
        errs = []
        for tid in valid_ids:
            tran = c[c['transcript_id']==tid]
            tran = tran.dropna(subset = ["media_start"])
            try:
              maxvoc = max_vocal_sample(tran, interval)
              maxvoc_dat = maxvoc_dat.append(maxvoc)
            except:
              errs.append(tid)
    print("Errors: ", errs)
    return maxvoc_dat
    
def get_maxturn_samples(corpora, interval):
    maxturn_dat = pd.DataFrame()
    for i in corpora:    
        c = cpy.get_utterances(corpus=i)
        ids = c['transcript_id'].unique()
        valid_ids = []
        for i in ids:
            tran = c[c['transcript_id']==i]
            if has_times(tran) and \
               longer_than_ten(tran) and \
               sufficient_child_utterances(tran, 5) and \
               sufficient_caregiver_utterances(tran, 5) and \
               "Target_Child" in list(tran["speaker_role"]):
                valid_ids.append(i)
        errs = []
        for tid in valid_ids:
            tran = c[c['transcript_id']==tid]
            tran = tran.dropna(subset = ["media_start"])
            try:
              maxturn = max_turn_taking_sample(tran, 3, interval)
              maxturn_dat = maxturn_dat.append(maxturn)
            except:
              errs.append(tid)
    print("Errors: ", errs)
    return maxturn_dat
    
    
def has_times(tran):
  set_starts = set(list(tran['media_start']))
  set_ends = set(list(tran['media_end']))
  list_starts = list(set_starts)
  list_ends = list(set_ends)
  list_starts = list(filter(None, list_starts))
  list_ends = list(filter(None, list_ends))
  if None in list_starts or None in list_ends or math.nan in list_starts or math.nan in list_ends:
    print("no times")
    return False
  else:
    return True

def longer_than_ten(tran):
  length = tran['media_end'].iloc[-1]
  if length != None and float(length) > 600:
    return True
  else:
    print("not longer than 10")
    return False

def sufficient_child_utterances(tran, n):
  #n should be hard-coded to 5 in most cases
  child_df = tran[tran['speaker_role']=='Target_Child']
  if child_df.shape[0] > n:
    return True
  else:
    print("not enough child utterrances")
    return False
  
def sufficient_caregiver_utterances(tran, n):
  #n should be hard-coded to 5 in most cases
  caregiver_df = tran[(tran['speaker_role'] == 'Mother') |
                      (tran['speaker_role'] == 'Father')]
  #drop nan transcripts
  caregiver_df = caregiver_df.dropna(subset = ['gloss'])
  if caregiver_df.shape[0] > n:
    return True
  else:
    print("not enough caregiver utterances")
    return False

# unused for now, but maybe helpful later
def no_long_utterances(tran, limit):
  starts = list(tran['media_start'])
  ends = list(tran['media_end'])
  if len(starts) != len(ends):
    return False
  for i in range(len(starts)):
    if ends[i] - starts[i] >= limit:
      return False
    if i != len(starts)-1 and starts[i+1] - starts[i] >= limit:
      return False
  return True

