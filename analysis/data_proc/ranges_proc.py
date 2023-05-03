import pandas as pd
import numpy as np

def extract_nonov_ranges(df):
    nonov = pd.DataFrame(columns=["language","nonov_range",])
    test_samples_range_stats = (df.agg(["min","max"])
                            .reset_index())
    test_samples_range_stats.columns = (test_samples_range_stats.columns.get_level_values(0) +
                                    "_" +
                                    test_samples_range_stats.columns.get_level_values(1))
    test_samples_range_stats = test_samples_range_stats.rename(columns={"contingency_": "contingency"})
    test_samples_range_stats = test_samples_range_stats.rename(columns={"language_": "language"})
    langs = test_samples_range_stats["language"].unique()
    for i in langs:
        df = pd.DataFrame(test_samples_range_stats[test_samples_range_stats["language"]==i])
        for index, row in df.iterrows():
            range_window = df[df["language"]==row["language"]]
            cmax = range_window.filter(regex='max').iloc[0].head(-1)
            cmin = range_window.filter(regex='min').iloc[0].head(-1)
            ncmax = range_window.filter(regex='max').iloc[1].head(-1)
            ncmin = range_window.filter(regex='min').iloc[1].head(-1)
            upper = cmax-ncmin.values # contingent max, non-contingent min difference
            lower = cmin-ncmax.values # contingent min, non-contingent max difference
        overlap = pd.DataFrame(pd.concat([upper, lower], join="inner"))
        overlap.rename(columns={ overlap.columns[0]: "overlap" }, inplace = True)
        overlap["x"]=overlap.index.str.split("_").str.get(0)
        overlap["range"] = overlap.index.str.split("_").str.get(1)
        overlap["overlap"] = overlap["overlap"].astype(str).astype(float)
        overlap["x"] = overlap["x"].astype(str).astype(int)
        cdf = pd.DataFrame(pd.concat([cmax, cmin], join="inner"))
        cdf.rename(columns={ cdf.columns[0]: "value" }, inplace = True)
        cdf["contingency"] = "contingent"
        ncdf = pd.DataFrame(pd.concat([ncmax, ncmin], join="inner"))
        ncdf.rename(columns={ ncdf.columns[0]: "value" }, inplace = True)
        ncdf["contingency"] = "noncontingent"
        df = pd.DataFrame(pd.concat([cdf,ncdf], join="inner"))
        df["x"]=df.index.str.split("_").str.get(0)
        df["range"] = df.index.str.split("_").str.get(1)
        df["value"] = df["value"].astype(str).astype(float)
        overlap_wide = overlap.pivot_table(index=["x"],
                                           columns='range',
                                           values='overlap').reset_index().sort_values(by=['x'])
        overlap_cpy = overlap_wide
        overlap_wide = overlap_wide[(overlap_wide["max"]<0) & (overlap_wide["min"]<0)]
        overlap_wide["chunks"] = overlap_wide.index.to_series().diff().ne(1).cumsum().sub(1)
        maxchunk = overlap_wide["chunks"].max()
        overlap_wide = overlap_wide[overlap_wide["chunks"]==maxchunk]
        if len(overlap_wide) > 0:
            nonovlap_pnt = overlap_wide["x"].idxmin() - 1
            nonovlap_pnt = overlap_cpy.loc[overlap_cpy.index[nonovlap_pnt], 'x']
        else:
            nonovlap_pnt = np.nan
        nonov = nonov.append({'language' : i,
                              'nonov_range' : nonovlap_pnt},
                              ignore_index=True)
    return nonov
        