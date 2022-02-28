#this function uses the user_count column to give accurate frequencies of each password
#instead of just assuming every password occurs at the same frequencies
user_count_summer <-function(df, col_name){
  column_values <- c(0:max(top_200_password_2020_by_country_extended[,c(col_name)]))
  sums <- c()
  for(i in column_values){
    df_temp <- df[df[,c(col_name)] == i,]
    freq_sum <- sum(df_temp$User_count)
    sums <- c(sums, freq_sum)
  }
  return(sums)
} 

#breaking down each country into its own DF
df_china <- top_200_password_2020_by_country_extended[top_200_password_2020_by_country_extended$country == "China", ]
df_us <- top_200_password_2020_by_country_extended[top_200_password_2020_by_country_extended$country == "United States", ]
df_russia <- top_200_password_2020_by_country_extended[top_200_password_2020_by_country_extended$country == "Russia", ]
df_vietnam <- top_200_password_2020_by_country_extended[top_200_password_2020_by_country_extended$country == "Vietnam", ]
df_spain <- top_200_password_2020_by_country_extended[top_200_password_2020_by_country_extended$country == "Spain", ]

#using our function to sum up lengths with the user_count variable
lengths_china <- user_count_summer(df_china, "length")
lengths_us <- user_count_summer(df_us, "length")
lengths_russia <- user_count_summer(df_russia, "length")
lengths_vietnam <- user_count_summer(df_vietnam, "length")
lengths_spain <- user_count_summer(df_spain, "length")

#plotting them by country
barplot(lengths_china, names.arg = lengths)
barplot(lengths_us, names.arg = lengths)
barplot(lengths_russia, names.arg = lengths)
barplot(lengths_vietnam, names.arg = lengths)
barplot(lengths_spain, names.arg = lengths)

#running anova on each country with user_count weighting the lengths
summary(aov(length ~ country, data=top_200_password_2020_by_country_extended, weights=User_count))
#running anova on unique_chars
summary(aov(unique_chars ~ country, data=top_200_password_2020_by_country_extended, weights=User_count))
#running anova on # of numbers
summary(aov(numbers ~ country, data=top_200_password_2020_by_country_extended, weights=User_count))
#running anova on sequentials
summary(aov(sequentials ~ country, data=top_200_password_2020_by_country_extended, weights=User_count))
