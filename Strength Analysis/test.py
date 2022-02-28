import pandas as pd



number_rows = 1000
df1 = pd.read_csv("unigram_freq.csv", nrows=number_rows)
df2 = pd.read_csv("rockyousubset.csv", nrows=number_rows)

list_of_matches = []


# for i in file.index:
#     for j in common_words.index:
#         if(common_words['name'][j] in file['name'][i]):
#             list_of_matches.append(common_words['name'][j])


for ind1 in df1.index:
    df1.loc[ind1, 'df2_match'] = ', '.join(list(df2[df2['name'].str.contains(df1['name'][ind1])]['name']))