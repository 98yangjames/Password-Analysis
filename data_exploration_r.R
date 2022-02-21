lengths = c(0:20)
sums = c()
for(i in lengths){
  df_temp <- top_200_password_2020_by_country_extended[top_200_password_2020_by_country_extended$length == i,]
  freq_sum <- sum(df_temp$User_count)
  sums <- c(sums, freq_sum)
}

barplot(sums, names.arg = lengths)