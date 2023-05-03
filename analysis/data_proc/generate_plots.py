from childes_sampling import *
from sampling_viz import *
import childespy as cpy
import math

def get_samples(corpus, n):
  """
  Gets n randomly chosen transcripts from the corpus, and generates random,
  max vocal, and max turn-taking plots
  """
  c = cpy.get_utterances(corpus=[corpus])
  ids = c['transcript_id'].unique()
  valid_ids = []
  #FILL IN: filter transcripts without timestamps, less than 10 min
  for i in ids:
    tran = c[c['transcript_id']==i]
    if has_times(tran) and longer_than_ten(tran) and sufficient_child_utterances(tran, 5) and "Target_Child" in list(tran["speaker_role"]):
      valid_ids.append(i)
  chosen = list(np.random.choice(valid_ids, n, replace=False))
  for tid in chosen:
    tran = c[c['transcript_id']==tid]
    #sample rand, vocal, and turn
    rand = random_sample(tran)
    vocal = max_vocal_sample(tran)
    #for turn taking, 3 represents 3 second maximum from onset to onset
    turn = max_turn_taking_sample(tran, 3)
    #make the three plots from these three samples
    dual_stream_viz(rand, corpus+'_'+str(tid)+"_random")
    dual_stream_viz(vocal, corpus+'_'+str(tid)+"_vocal")
    dual_stream_viz(turn, corpus+'_'+str(tid)+"_turn")

def has_times(tran):
  set_starts = set(list(tran['media_start']))
  set_ends = set(list(tran['media_end']))
  list_starts = list(set_starts)
  list_ends = list(set_ends)
  if None in list_starts or None in list_ends or math.nan in list_starts or math.nan in list_ends:
    print("no times")
    return False
  else:
    return True

def longer_than_ten(tran):
  length = tran['media_end'][-1]
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
    print("not enough utterrances")
    return False


def mult_corpora_plots(lst, n):
  """
  Plots all 3 samples for n different transcripts for each corpus in lst.
  """
  problems = []
  for i in lst:
    try:
      get_samples(i, n)
    except:
      problems.append(i)
      print("error with corpus "+i)
  print("corpora with problems: "+str(problems))
