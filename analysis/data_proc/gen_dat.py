import pandas as pd
import numpy as np
import pympi
import glob
import os


def txt2csv(corpus_root,outpath):
    directory = os.fsencode('{}/'.format(corpus_root))
    for file_path in glob.glob('{}/*.txt'.format(corpus_root)):
        filename= os.path.basename(file_path)
        if  filename.startswith('.') or filename.endswith('.py'):
             continue
        else:
            file = os.fsdecode(filename)
            print(file)
        df=pd.read_table(file_path,sep='\t',lineterminator='\n',header=None)
        df.to_csv('{}/{}.csv'.format(outpath,file), header=False)


def speech_cat(data_file,outname):
    conditions = [
        (data_file['transcript'] == '(voc)'),
        (data_file['transcript'] == '(other)')]
    choices = ['final_infraphonology','Other']
    data_file['tier'] = np.select(conditions, choices, default='Parent')
    data_file.to_csv("{}".format(outname))
    

def eaf2csv(corpus_root,cols,subjectarray,tier_names,naming_constant,output_path,outname):
    directory = os.fsencode('{}/eaf/'.format(corpus_root))
    output = pd.DataFrame(columns=cols)
    for a in range(len(subjectarray)):
        sub = subjectarray[a]
        print(sub)
        flag = -1
        # Loop over all elan files the corpusroot subdirectory called eaf
        for file_path in glob.glob('{}/eaf/*.eaf'.format(corpus_root)):
            # Initialize the elan file
            flag = flag+1
            filename = os.listdir(directory)[flag]
            # correct for .DS_store files
            if filename.decode('utf-8') == '.DS_Store':
                os.remove(os.path.join(directory.decode('utf-8'), filename.decode('utf-8')))
                break
            file = os.fsdecode(filename)
            if file == "s{}{}.eaf".format(sub,naming_constant):
                # if file == "%d reliability subject.eaf" % sub:
                print(file)
                eafob = pympi.Elan.Eaf(file_path)
                # Loop over all the defined tiers that contain orthography
                for ort_tier in tier_names:
                    # If the tier is not present in the elan file spew an error and
                    # continue. This is done to avoid possible KeyErrors
                    # If the tier is present we can loop through the annotation data
                    counter = -1
                    all_utt = pd.DataFrame(columns=cols)
                    for annotation in eafob.get_annotation_data_for_tier(ort_tier):
                        # We are only interested in the utterance
                        counter = counter+1
                        all_utt.loc[counter, 'sub'] = sub
                        all_utt.loc[counter, 'onset'] = annotation[0]/1000
                        all_utt.loc[counter, 'offset'] = annotation[1]/1000
                        all_utt.loc[counter, 'cat'] = annotation[2]
                        all_utt.loc[counter, 'tier'] = ort_tier
                    output = output.append(all_utt)
    output.to_csv('{}/{}.csv'.format(output_path,outname))
