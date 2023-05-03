import re
import pprint
import pandas as pd
import numpy as np
pp = pprint.PrettyPrinter()

# delete anything with pound sign

def clean_string(dirty_string):
    removed_pound = re.sub(r'#+\s|#+', "", dirty_string)
    removed_astrisk = re.sub(r'\*', "", removed_pound)
    removed_parens = re.sub(r'\(.*\)', "", removed_astrisk)
    removed_brackets = re.sub(r'\[.*\]', "", removed_parens)
    removed_qmarks = removed_brackets.replace("?", "")
    removed_periods = removed_qmarks.replace(".", "")
    removed_dash = removed_periods.replace("-", "")
    removed_fslash = removed_periods.replace("/", "")
    removed_bslash = removed_fslash.replace("\\", "")
    lower = removed_bslash.lower()
    return lower.strip()


def filter_table(df):
    copy = df.copy()
    copy["gloss"].replace('', np.nan, inplace=True)
    copy = copy.dropna(axis=0, subset=["gloss"])
    copy = copy.reset_index().drop("index", axis=1)
    return copy

def filter_tp_table(df):
    copy = df.copy()
    copy["cat"].replace('', np.nan, inplace=True)
    copy = copy.dropna(axis=0, subset=["cat"])
    copy = copy.reset_index().drop("index", axis=1)
    return copy

# def sort_dataframe(df):
#     # sort by subject number then begin time
#     result = df.sort_values(["sub", "onset"], ascending=[
#                             True, True]).reset_index()
#     result = result.drop('index', axis=1)
#     return result

def create_result(df):

    # convert to string type to clean strings
    df["gloss"] = df["gloss"].astype(str)
    df["gloss"] = df["gloss"].apply(clean_string)
    
    # filter out rows
    filtered = filter_table(df)

    filtered.to_csv("../data/rand_dat_inc_master.csv")
    
def create_tp_result(df):

    # convert to string type to clean strings
    df["cat"] = df["cat"].astype(str)
    df["cat"] = df["cat"].apply(clean_string)
    
    # filter out rows
    filtered = filter_tp_table(df)

    filtered.to_csv("../data/tp_dat_cont.csv")

def assign_contingency(df, window,buffer):
    transcripts = df["transcript_id"].unique()
    result = df.copy()
    result["contingent"] = 0
    for tran in transcripts:
        tran_frame = df[df["transcript_id"] == tran]
        vocs = tran_frame[tran_frame["speaker_role"]
                             == "Target_Child"].index.values
        for v_idx in vocs:
            voc_onset = result["media_start"][v_idx]
            within_window = tran_frame[(tran_frame["media_start"] >= (voc_onset+buffer)) & (
                                           tran_frame["media_start"] <= (voc_onset) + window )]
            # duplicate start times need removed from child vocalizations
            if within_window.duplicated(subset='media_start', keep=False).sum() > 0:
                within_window = within_window[within_window["caregiver"]=="caregiver"]
            cont_parent_row = within_window.index[within_window["caregiver"] == "caregiver"].tolist()
            if len(cont_parent_row) > 0:
                result.loc[cont_parent_row[0], "contingent"] = 1
    return result

def assign_contingency_point(df, window, buffer):
    transcripts = df["sub"].unique()
    result = df.copy()
    result["tp_contingent"] = 0
    result["voc_contingent"] = 0
    for tran in transcripts:
        tran_frame = df[df["sub"] == tran]
        tps = tran_frame[(tran_frame["tier"] == "inf_touch") |
                         (tran_frame["tier"] == "inf_point")].index.values
        vocs = tran_frame[tran_frame["tier"] == "Child_Voc"].index.values
        for tp_idx in tps:
            tp_onset = result["onset"][tp_idx]
            within_window = tran_frame[(tran_frame["onset"] >= (tp_onset+buffer)) & (
                                           tran_frame["onset"] <= (tp_onset) + window )]
            # duplicate start times need removed from child vocalizations
            if within_window.duplicated(subset='onset', keep=False).sum() > 0:
                within_window = within_window[within_window["tier"]=="Transcript"]
            cont_parent_row = within_window.index[within_window["tier"] == "Transcript"].tolist()
            if len(cont_parent_row) > 0:
                result.loc[cont_parent_row[0], "tp_contingent"] = 1
        for v_idx in vocs:
            voc_onset = result["onset"][v_idx]
            within_window = tran_frame[(tran_frame["onset"] >= (voc_onset+buffer)) & (
                                           tran_frame["onset"] <= (voc_onset) + window )]
            # duplicate start times need removed from child vocalizations
            if within_window.duplicated(subset='onset', keep=False).sum() > 0:
                within_window = within_window[within_window["tier"]=="Transcript"]
            cont_parent_row = within_window.index[within_window["tier"] == "Transcript"].tolist()
            if len(cont_parent_row) > 0:
                result.loc[cont_parent_row[0], "voc_contingent"] = 1
    return result

# buffer indicates how long to wait before calling a response contingent
# anything before buffer likely preplaned by caregiver, not contingent on babbling

'''
Road map:

[1] Distinction between (voc) and (other) in the script so we can decide whether to consider them in contingency calculation or not.
'''