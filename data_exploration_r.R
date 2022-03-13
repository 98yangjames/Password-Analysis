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

nums_china <- user_count_summer(df_china, "unique_chars")
nums_us <- user_count_summer(df_us, "unique_chars")
nums_russia <- user_count_summer(df_russia, "unique_chars")
nums_spain <- user_count_summer(df_vietnam, "unique_chars")
nums_vietnam <- user_count_summer(df_spain, "unique_chars")

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
library(pwr)
pwr.anova.test(k =5 , n =200, f =3.932, sig.level = 0.05)



#checking assumptions, lets check normality and equal variance with the residuals vs fitted and QQ plots
model_len <- lm(length ~ factor(country),data=top_200_password_2020_by_country_extended, weights=User_count)
model_uniq <- lm(unique_chars ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
model_nums <- lm(numbers ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
model_seq <- lm(sequentials ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
plot(model_len)
plot(model_uniq)
plot(model_nums)
plot(model_seq)

#trying to fix assumptions via logarithms
model_len_log <- lm(log(length,10) ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
model_uniq_log <- lm(log(unique_chars,10) ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
model_nums_log <- lm(log(numbers,10) ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
model_seq_log <- lm(log(sequentials,10) ~ country,data=top_200_password_2020_by_country_extended, weights=User_count)
plot(model_len_log)
plot(model_uniq_log)
plot(model_nums_log)
plot(model_seq_log)

summary()

#checking sample size
library(Hmisc)
lengths <- c(0:20)

sqrt(wtd.var(lengths, nums_russia))
sqrt(wtd.var(lengths, nums_china))
sqrt(wtd.var(lengths, nums_us))
sqrt(wtd.var(lengths, nums_spain))
sqrt(wtd.var(lengths, nums_vietnam))

#pairwise t test across langauges
#length
wtd.t.test(lengths, lengths, weight=lengths_us, weighty=lengths_china, bootse=TRUE, bootn=20)
#linear regression with robust SE, using weighted linear regression
#compare the linear regression with non-robust SE's and robust SE's to see how much influence the non-constant variance has
library(lmtest)
summary(model_len)
coeftest(model_len, vcov = vcovHC(model_len))
coeftest(model_uniq, vcov = vcovHC(model_uniq))


a <- aov(length ~ country, data=top_200_password_2020_by_country_extended, weights=User_count)
pairwise.t.test(top_200_password_2020_by_country_extended$length, top_200_password_2020_by_country_extended$country, weights=User_count)
pairwise.t.test(top_200_password_2020_by_country_extended$unique_chars, top_200_password_2020_by_country_extended$country, weights=User_count)
pairwise.t.test(top_200_password_2020_by_country_extended$numbers, top_200_password_2020_by_country_extended$country, weights=User_count)
pairwise.t.test(top_200_password_2020_by_country_extended$sequentials, top_200_password_2020_by_country_extended$country, weights=User_count)

bartlett.test(top_200_password_2020_by_country_extended$length, top_200_password_2020_by_country_extended$country)
t.test()




