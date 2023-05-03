# Natural-Statistics-Cross-linguistic-Public-Repo

## Directions running in a virtual environment

1) Download the code from this repository, and ```cd``` into the directory
2) To create a virtual environment with name NAME, in terminal, write ```python3 -m venv NAME```
3) To activate the virutal environment, run ```source NAME/bin/activate```

Now you're in a virtual environment!
  
## Installing all requirements
There are required packages in both python and R for this project. You must install them all in order for the code to run error-free.  
  
  1) Install required python packages. Simply run `pip3 install -r requirements.txt`.
  2) Install R packages. Enter an R console window, and run: ```install.packages(c("ggplot2", "repr", "plotrix", "dplyr", "lme4", "emmeans", "lmerTest", "tidyverse", "patchwork", "viridis", "scales"))```
 
  
## Running Code in Jupyter Lab
  
  1) In the command line, type ```jupyter lab```
  2) For each of the listed files below (all in the `analysis` folder), do the following: In the upper left-hand corner, find click `Run` and then `Run All Cells`. This may take varying amounts of time, with estimates of the time to run each file in the brackets after their file name. Make sure the previous file is finished running before running the next one.

- `rand_sample.ipynb` [30 min]
- `rand_gen_contingency.ipynb` [5 min]
- `rand_LexDiv.ipynb` [<1 min]
- `rand_MLUw.ipynb`[<1 min]
- `rand_SWU.ipynb`. [<1 min]
- `Figure_2.ipynb`
- `effect_size_compare.ipynb`
- `Figre_3.ipynb`
