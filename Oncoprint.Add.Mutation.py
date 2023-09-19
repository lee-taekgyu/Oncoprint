#!/usr/bin/python3

import pandas as pd
import glob
import os
import numpy as np

Dir = glob.glob('bpdcn_*')
Dir.sort()

CNV_Dict = {}

Sample = ['BPDCN' + str(i) for i in range(1, 14)]

for i in range(1, 14):
    CNV_Dict[f'BPDCN{i}'] = {}

for i in range(1, len(Dir) + 1):
    file_names = os.listdir(Dir[i-1])
    Gene = [gene.split('.')[0] for gene in file_names]
    for gene in Gene:
        CNV_Dict[f'BPDCN{i}'][gene] = 'CNV'

Oncoprint = pd.read_excel('/labmed/05.BPDCN/1214.BPDCN.Funtional.Oncoprint.xlsx',
                          header='infer')
Oncoprint = Oncoprint.fillna('NaN')

Onco_Dict = {}
for i in range(1, 14):
    Onco_Dict[f'BPDCN{i}'] = {}

for i in range(1, 14):
    mutattion = Oncoprint.iloc[:,i].tolist()
    Gene = Oncoprint.iloc[:,0].tolist()
    Match = dict(zip(Gene, mutattion))
    for gene in Gene:
        Onco_Dict[f'BPDCN{i}'] = Match

Total_Dict = {}
for sample in Sample:
    Total_Dict[sample] = {}
    Onco_Gene= list(Onco_Dict[sample].keys())
    CNV_Gene = list(CNV_Dict[sample].keys())
    Total_Gene = list(set(Onco_Gene + CNV_Gene))
    

    for gene in Total_Gene:
        if gene in Onco_Gene and gene in CNV_Gene:
            mut_info = [Onco_Dict[sample][gene], CNV_Dict[sample][gene]]
            mut_info = '; '.join(mut_info)
            mut_info = mut_info.replace('NaN; ', '')
            Data = {gene : [mut_info]}
            Total_Dict[sample].update(Data)
        elif gene in Onco_Gene and gene not in CNV_Gene:
            Data = {gene : [Onco_Dict[sample][gene]]}
            Total_Dict[sample].update(Data)
        elif gene not in Onco_Gene and gene in CNV_Gene:
            Data = {gene : ['CNV']}
            Total_Dict[sample].update(Data)

df = Total_Dict['BPDCN1']
df = pd.DataFrame(df)
df = df.transpose()
df.columns = ['BPDCN1']
df['Gene'] = df.index
df.reset_index(drop=True, inplace=True)
Merge = df[['Gene', 'BPDCN1']]

for sample in list(Total_Dict.keys())[1:]:
    df = Total_Dict[sample]
    df = pd.DataFrame(df)
    df = df.transpose()
    df.columns = [sample]
    df['Gene'] = df.index
    df.reset_index(drop=True, inplace=True)
    df = df[['Gene', sample]]

    Merge = pd.merge(Merge, df, on='Gene', how='outer')

Merge = Merge.replace('NaN', '')
Merge = Merge.fillna('')

Merge['Count'] = Merge.apply(lambda row: sum(len(str(val)) for val in row), axis=1)
Merge_sorted = Merge.sort_values(by='Count', ascending=False)
print(Merge_sorted['Count'])
Merge_sorted.drop(columns=['Count'], inplace=True)

Merge_sorted.to_excel('230919.BPDCN.Oncoprint.xlsx',
               index=False,
               header='infer')
