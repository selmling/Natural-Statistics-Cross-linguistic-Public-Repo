import pandas as pd
import numpy as np

def mlu_proc(data):
    for a in range(len(data)):
        counter = 0
        utt = data.iloc[a,4]
        separated = utt.split()
        for y in separated:
            counter += 1
        data.loc[a,'length_of_utterance'] = counter
        
def swu_proc(data):
    data["swu"]=np.where(data["length_of_utterance"]==1,1,0)
    return data
        
def determine_uniqueness(utterance, word_set):
    split = utterance.split()
    unique_count = 0
    for word in split:
        if word not in word_set:
            unique_count += 1
        word_set.add(word)
    return unique_count, word_set

def update_word_set(word_set, utterance):
    split = utterance.split()
    for word in split:
        word_set.add(word)
    return word_set

def assign_uniqueness(df,sample):
    if sample=="rand":
        tar_col = 36
    elif sample=="maxvoc":
        tar_col = 38
    elif sample=="maxturn":
        tar_col = 39
    df['uniqueness'] = ""
    word_set = set()
    sub_set = set()
    trans = df["transcript_id"].unique()
    for i in trans:
        tran_frame = df[df["transcript_id"]==i]
        for index, row in tran_frame.iterrows():
            if row["caregiver"] == "caregiver":
                #reset word_set for each subject
                if row["transcript_id"] not in sub_set:
                    word_set = set()
                    sub_set.add(row["transcript_id"])
                utterance = row["gloss"]
                df.iat[index, tar_col],word_set = determine_uniqueness(utterance, word_set)
    return df

def create_c_result(df,sample): # sample either rand, maxvoc, or maxturn
    # read in data
    table = df
    table["gloss"] = table["gloss"].astype(str)
    result = assign_uniqueness(table,sample)
    result.to_csv("../data/{}_dat_inc_master_cc_lexdiv.csv".format(sample))

def create_nc_result(df,sample): # sample either rand, maxvoc, or maxturn
    # read in data
    table = df
    table["gloss"] = table["gloss"].astype(str)
    result = assign_uniqueness(table,sample)
    result.to_csv("../data/{}_dat_inc_master_nc_lexdiv.csv".format(sample))


def assign_tp_uniqueness(df):
    df['uniqueness'] = ""
    word_set = set()
    sub_set = set()
    subjects = df["sub"].unique()
    for i in subjects:
        subject_frame = df[df["sub"]==i]
        for index, row in subject_frame.iterrows():
            #reset word_set for each subject
            if row["sub"] not in sub_set:
                word_set = set()
                sub_set.add(row["sub"])
            utterance = row["cat"]
            df.iat[index, 7],word_set = determine_uniqueness(utterance, word_set)
    return df

def create_tp_result(df):
    # read in data
    table = df
    table["cat"] = table["cat"].astype(str)
    result = assign_tp_uniqueness(table)
    result.to_csv("../data/tp_cont_lexdiv_dat.csv")
#     result.to_csv("../data/tp_noncont_lexdiv_dat.csv")