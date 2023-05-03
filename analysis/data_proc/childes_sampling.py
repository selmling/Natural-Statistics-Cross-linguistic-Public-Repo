import childespy as cpy
import math
import pandas as pd
import random

"""
For these sampling methods, we are going to assume we have a dataframe
that represents a single session (transcript), as generated from the 
get_utterances method in the childespy library. This transcript will be longer than
10 minutes long, and have onsets and offsets in each row ('media_start' and 'media_end').
We will be sampling from such a dataframe (represented by parameter df in most functions).
"""

random.seed(42)

def session_durations(df, corp_count):
    markers = []
    uts = cpy.get_utterances(corpus=list(corp_count))
    for i in df["transcript_id"]:
        timestamps = uts.loc[uts["transcript_id"] == i]["media_end"].tolist()
        dummy = 0
        for time in reversed(timestamps):
            if not math.isnan(time):
                dummy = time
                markers.append(dummy)
                break
        if dummy == 0:
            markers.append(None)
    df["session_length"] = markers
    return df


def random_sample(df, interval):
    # find a time after the 10-minte mark randomly (600 seconds is 10 minutes)
    after_ten = df[df["media_end"] >= interval]
    # make it a list so random sampling is easier
    offsets = after_ten["media_end"].to_list()
    length = len(offsets)
    # take a random sample using the random library
    r = random.randint(0, length - 1)
    random_end = offsets[r]
    # now, go back 10 minutes, and find the next start time
    back_ten = random_end - interval
    # return a dataframe within this random 10 minute sample
    from_start = df[df["media_start"] >= back_ten]
    before_end = from_start[from_start["media_end"] <= random_end]
    rand_sample = before_end
    # indicate this was sampled randomly, and provide the final duration
    rand_sample["selection"] = "random"
    rand_sample["selection_duration"] = random_end - (rand_sample["media_start"][0])
    rand_sample = include_filename(rand_sample)
    return rand_sample


def ten_from_row(df, i, interval):
    """
    Given a dataframe df, returns the index of the final row included
    in a sample of interval seconds, starting from row i
    """
    # find the start time of this row
    start = df.iloc[i]["media_start"]
    # the upper bound on end time is 600 seconds later
    end = start + interval
    # cut off the end of the dataframe that is not within these 10 minutes
    within_ten = df[df["media_end"] < end]
    # return the the index we want
    return len(within_ten)


def max_vocal_sample(df, interval):
    # initialize variables that we will update to find the maximum
    sample = df[0 : ten_from_row(df, 0, interval)]
    max_child_rows = len(sample[sample["speaker_role"] == "Target_Child"])
    # find the index 10 minutes before the end to be the stopping index
    stop_time = df["media_end"][-1] - interval
    stop_index = len(df[df["media_start"] < stop_time]) - 1
    # iterate through, and update to find the 10 minutes with most child vocal activity
    for index in range(1, stop_index + 1):
        s = df.iloc[index : ten_from_row(df, index, interval)]
        # calculate the rows that are from a child
        child_rows = len(s[s["speaker_role"] == "Target_Child"])
        # update maximums, if needed
        if child_rows > max_child_rows:
            max_child_rows = child_rows
            sample = s
    sample["selection"] = "vocal_max"
    sample["selection_duration"] = sample.media_end[-1] - sample.media_start[0]
    sample = include_filename(sample)
    return sample


def max_turn_taking_sample(df, t, interval, speakers=["Target_Child", "Mother", "Father"]):
    # add a column in the dataframe for if that row is a turn with the row before it
    speaker_list = list(df["speaker_role"])
    onsets = list(df["media_start"])
    is_turn = [False]
    for i in range(1, len(speaker_list)):
        if (
            speaker_list[i] in speakers
            and speaker_list[i - 1] in speakers
            and speaker_list[i] != speaker_list[i - 1]
        ):
            if onsets[i] - onsets[i - 1] <= t:
                is_turn.append(True)
            else:
                is_turn.append(False)
        else:
            is_turn.append(False)
    df["turns"] = is_turn
    # initialize variables that we will update to find the maximum
    sample = df[0 : ten_from_row(df, 0, interval)]
    max_turns = len(sample[sample["turns"] == True])
    # find the index 10 minutes before the end to be the stopping index
    stop_time = df["media_end"][-1] - interval
    stop_index = len(df[df["media_start"] < stop_time]) - 1
    # iterate through, and update to find the 10 minutes with most turns
    for index in range(1, stop_index + 1):
        s = df.iloc[index : ten_from_row(df, index, interval)]
        # calculate the rows that are from a child
        num_turns = len(s[s["turns"] == True])
        # update maximums, if needed
        if num_turns > max_turns:
            max_turns = num_turns
            sample = s
    sample["selection"] = "turns_max"
    sample["selection_duration"] = sample.media_end[-1] - sample.media_start[0]
    sample = include_filename(sample)
    return sample

def include_filename(df):
    """
    Returns a new dataframe that includes the filename
    """
    c, id = list(df['corpus_name'])[0], list(df['transcript_id'])[0]
    t = cpy.get_transcripts(corpus=c)
    df['filename'] = list(t[t['transcript_id']==id].filename)[0]
    return df
