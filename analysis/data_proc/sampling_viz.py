import childespy as cpy
import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random
from childes_sampling import *


def dual_stream_viz(df, title):
    """
    Plots turn-taking for dataframe df
    """
    alpha = 1
    maxm = df.media_end[-1]
    par = list(df["speaker_role"].unique())
    variables = []
    for i in range(len(par)):
        variables.append(df.loc[df["speaker_role"] == par[i]])
    for j in range(min(len(variables), 4)):
        if variables[j]["speaker_role"][0] == "Target_Child":
            variables[j]["Color"] = "yellow"
        elif variables[j]["speaker_role"][0] == "Mother":
            variables[j]["Color"] = "blueviolet"
        elif variables[j]["speaker_role"][0] == "Father":
            variables[j]["Color"] = "darkslateblue"
        else:
            variables[j]["Color"] = "aliceblue"
    variable_1 = variables[0]
    try:
        variable_2 = variables[1]
    except:
        print("no 2")
    try:
        variable_3 = variables[2]
    except:
        print("no 3")
    try:
        variable_4 = variables[3]
    except:
        print("no 4")
    begin = 0
    qtr = (maxm - begin) / 4  #  divide into quarters
    columns = ["media_start", "media_end"]
    qtr1 = pd.DataFrame(
        np.array(
            [
                [begin, qtr + begin],
            ]
        ),
        columns=["media_start", "media_end"],
    )  #  1st quarter to visualize
    qtr2 = pd.DataFrame(
        np.array(
            [
                [qtr + 1 + begin, (qtr * 2) + begin],
            ]
        ),
        columns=columns,
    )  #  2nd quarter to visualize
    qtr3 = pd.DataFrame(
        np.array(
            [
                [(qtr * 2 + 1) + begin, (qtr * 3) + begin],
            ]
        ),
        columns=columns,
    )  #  3rd quarter to visualize
    qtr4 = pd.DataFrame(
        np.array(
            [
                [(qtr * 3 + 1) + begin, (qtr * 4) + begin],
            ]
        ),
        columns=columns,
    )  #  4th quarter to visualize
    fig, (ax1, ax2, ax3, ax4) = plt.subplots(4, figsize=(18.5, 12))
    for i in variables:
        if i["speaker_role"][0] != "Target_Child":
            ax1.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (2.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax1.set_xlim(qtr1.at[0, "media_start"], qtr1.at[0, "media_end"])
            ax2.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (2.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax2.set_xlim(qtr2.at[0, "media_start"], qtr2.at[0, "media_end"])
            ax3.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (2.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax3.set_xlim(qtr3.at[0, "media_start"], qtr3.at[0, "media_end"])
            ax4.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (2.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax4.set_xlim(qtr4.at[0, "media_start"], qtr4.at[0, "media_end"])
        else:
            ax1.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (1.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax1.set_xlim(qtr1.at[0, "media_start"], qtr1.at[0, "media_end"])
            ax2.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (1.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax2.set_xlim(qtr2.at[0, "media_start"], qtr2.at[0, "media_end"])
            ax3.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (1.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax3.set_xlim(qtr3.at[0, "media_start"], qtr3.at[0, "media_end"])
            ax4.broken_barh(
                list(
                    zip(
                        i["media_start"].values,
                        (i["media_end"] - i["media_start"]).values,
                    )
                ),
                (1.5, 0.99),
                color=i["Color"],
                edgecolor="black",
                alpha=alpha,
            )
            ax4.set_xlim(qtr4.at[0, "media_start"], qtr4.at[0, "media_end"])
    plt.savefig("{}.png".format(title))
