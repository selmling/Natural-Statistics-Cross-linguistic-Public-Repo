#### How to run these scripts to generate visualizations of sessions within various CHILDES corpora, given various sampling procedures.
Overview: in terminal, import the childespy library, and create your own dataframes using the get_transcript function. Then, sample using the functions writte in childes_sampling.py, and create plots of those samples using functions from sampling_viz.py.

#### Method 1:

0. Lauch python3 in a terminal:
```bash
python3
```

1. Import the necessary files/libraries in terminal (ensure you are in the same directory as these files)

```bash
import childespy as cpy
from childes_sampling import *
from sampling_viz import *
```

2. Get the CHILDES data you are interested in (for the demo, we will select one transcript from the Snow corpus)
```bash
kovacevic = cpy.get_utterances(corpus='Kovacevic')
#10879 is a randomly chosen transcript id from the Snow corpus (query the 'transcript_id' column of the get_utterances dataframe to display ids)
ids = kovacevic['transcript_id'].unique()
tid = np.random.choice(ids, 1).item()
tran = kovacevic[kovacevic['transcript_id']==tid]
tran = session_durations(tran, ['Kovacevic'])
tran['corpora'] = 'Kovacevic'
rand = random_sample(tran)
vocal = max_vocal_sample(tran)
#for turn taking, 3 represents 3 second maximum from onset to onset
turn = max_turn_taking_sample(tran, 3)
```

3. Make the plots from the samples.
    - The first argument is the dataframe and the second is the title of the file:
```bash
dual_stream_viz(rand, "random_sample")
dual_stream_viz(vocal, "vocal_sample")
dual_stream_viz(turn, "turn_sample")
```

#### Method 2
```bash
from generate_plots import *
get_samples(<corpus (str)>, <n (int)>)
```
