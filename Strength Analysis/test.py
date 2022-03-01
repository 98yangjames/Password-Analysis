import pandas as pd

import math


number_rows = 1000
country_dataset = pd.read_csv("../Datasets/top_200_password_2020_by_country_extended.csv")
common_words = pd.read_csv("unigram_freq.csv")
list_of_bad_words = []

for i in common_words.index:
    if(len(str(common_words['name'][i])) <= 1):
        list_of_bad_words.append(i)

common_words = common_words.drop(list_of_bad_words)

country_dataset['Data Length'] = country_dataset['Password'].str.len()

L = country_dataset['Data Length']
N = 127 #number of symbols that can be typed (ascii table)

country_dataset['strength'] = L*(math.log(N))/math.log(2)

country_dataset.sort_values(by=['Data Length'])
country_dataset.dropna()

# Calculate the list of matches with the most common words
list_of_matches = []

for i in country_dataset.index:
    match = ""
    for j in common_words.index:
        if (str(common_words['name'][j]) in str(country_dataset['Password'][i])):
            match += str(common_words['name'][j]) + ","

    print(match)
    list_of_matches.append(match[:-1])

print(list_of_matches)