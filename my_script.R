#summing all the frequencies across the Koch Corpora 

Freq_El_Omrani_Jaouhar$Koch=Freq_El_Omrani_Jaouhar$`Koch - Marieke`+ Freq_El_Omrani_Jaouhar$`Koch - Merit` + Freq_El_Omrani_Jaouhar$`Koch - Simon`
View(Freq_El_Omrani_Jaouhar)

#summing all frequencies across all Miller corpora
Freq_El_Omrani_Jaouhar$Miller=Freq_El_Omrani_Jaouhar$`Miller-Simone`+Freq_El_Omrani_Jaouhar$`Miller-Kerstin`
View(Freq_El_Omrani_Jaouhar)

#summing all child speech frequencies across all corpora 

Freq_El_Omrani_Jaouhar$Child_speech=Freq_El_Omrani_Jaouhar$Leo + Freq_El_Omrani_Jaouhar$Koch + Freq_El_Omrani_Jaouhar$Miller
View(Freq_El_Omrani_Jaouhar)

#summing all child directed speech frequencies across all corpora

Freq_El_Omrani_Jaouhar$CDS=Freq_El_Omrani_Jaouhar$`CDS Miller` + Freq_El_Omrani_Jaouhar$`CDS Koch` + Freq_El_Omrani_Jaouhar$`CDS Leo`
View(Freq_El_Omrani_Jaouhar)

#Summing all child speech frequencies and all child directed speech frequency across different corpora

Freq_El_Omrani_Jaouhar$total_CS_CDS=Freq_El_Omrani_Jaouhar$`Child_speech` + Freq_El_Omrani_Jaouhar$`CDS`
View(Freq_El_Omrani_Jaouhar)
# Histogram and Shapiro-Wilk test for Leo
hist(Freq_El_Omrani_Jaouhar$Leo, main = "Frequency of Leo's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$Leo)

# Histogram and Shapiro-Wilk test for Koch-marieke
hist(Freq_El_Omrani_Jaouhar$`Koch - Marieke`, main = "Frequency of Koch - Marieke's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$`Koch - Marieke`)

# Histogram and Shapiro-Wilk test for Koch-Merit
hist(Freq_El_Omrani_Jaouhar$`Koch - Merit`, main = "Frequency of Koch - Merit's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$`Koch - Merit`)

# Histogram and Shapiro-Wilk test for Koch-Simon
hist(Freq_El_Omrani_Jaouhar$`Koch - Simon`, main = "Frequency of Koch - Simon's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$`Koch - Simon`)

# Histogram and Shapiro-Wilk test for Miller-Simone
hist(Freq_El_Omrani_Jaouhar$`Miller-Simone`, main = "Frequency of Miller-Simone's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$`Miller-Simone`)

# Histogram and Shapiro-Wilk test for Miller-Kerstin
hist(Freq_El_Omrani_Jaouhar$`Miller-Kerstin`, main = "Frequency of Miller-Kerstin's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$`Miller-Kerstin`)

# Histogram and Shapiro-Wilk test for Miller 
hist(Freq_El_Omrani_Jaouhar$Miller, main = "Frequency of Miller's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$Miller)

# Histogram and Shapiro-Wilk test for Koch 
hist(Freq_El_Omrani_Jaouhar$Koch, main = "Frequency of Koch's speech across all corpora", xlab = "Frequency", ylab = "Count")
shapiro.test(Freq_El_Omrani_Jaouhar$Koch)

# Histogram and Shapiro-Wilk test for Child_speech
hist(Freq_El_Omrani_Jaouhar$Child_speech, main = "Frequency of child speech across all corpora", xlab = "Frequency", ylab = "Count")



# Correlation between CS of Leo and Leipzig rankings
cor.test(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$Leo)
plot(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$Leo, 
     main = "Figure 6: Correlation between Leipzig frequency rankings and Child directed speech frequencies", 
     xlab = "Leipzig frequency rankings", 
     ylab = "Child directed speech frequencies of Leo's speech")
abline(lm(Freq_El_Omrani_Jaouhar$`Leipzig - Rank` ~ Freq_El_Omrani_Jaouhar$Leo), col = "blue")


# Correlation between CS of Marieke and Leipzig rankings
cor.test(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Koch - Marieke`)
plot(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Koch - Marieke`, 
     main = "Figure 7: Correlation between Leipzig frequency rankings and Marieke's speech frequencies from Koch corpus", 
     xlab = "Leipzig frequency rankings", 
     ylab = "Koch - Marieke's speech frequencies")
abline(lm(Freq_El_Omrani_Jaouhar$`Leipzig - Rank` ~ Freq_El_Omrani_Jaouhar$`Koch - Marieke`), col = "blue")

# Correlation between CS of Merit and Leipzig rankings
cor.test(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Koch - Merit`)
plot(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Koch - Merit`, 
     main = "Figure 8: Correlation between Leipzig frequency rankings and Merit's speech frequencies from Koch corpus", 
     xlab = "Leipzig frequency rankings", 
     ylab = "Koch - Merit's speech frequencies")
abline(lm(Freq_El_Omrani_Jaouhar$`Leipzig - Rank` ~ Freq_El_Omrani_Jaouhar$`Koch - Merit`), col = "blue")

# Correlation between CS of Simon and Leipzig rankings
cor.test(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Koch - Simon`)
plot(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Koch - Simon`, 
     main="Figure 9: Correlation between Leipzig frequency rankings and Simon’s speech frequencies from Koch corpus",
     xlab = "Leipzig frequency rankings", ylab = "Koch-Simon’s speech frequencies")
abline(lm(Freq_El_Omrani_Jaouhar$`Leipzig - Rank` ~ Freq_El_Omrani_Jaouhar$`Koch - Simon`), col = "blue")


# Correlation between CS of Simone and Leipzig rankings
cor.test(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Miller-Simone`)
plot(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Miller-Simone`, 
     main="Figure 10: Correlation between Leipzig frequency rankings and Simones’ speech frequencies from Miller corpus", 
     xlab = "Leipzig frequency rankings", ylab = "Miller-Simones’ speech frequencies from Miller corpus")
abline(lm(Freq_El_Omrani_Jaouhar$`Leipzig - Rank` ~ Freq_El_Omrani_Jaouhar$`Miller-Simone`), col = "blue")

# Correlation between CS of Kersten and Leipzig rankings
cor.test(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Miller-Kersten`)
plot(Freq_El_Omrani_Jaouhar$`Leipzig - Rank`, Freq_El_Omrani_Jaouhar$`Miller-Kersten`, 
     main="Figure 11: Correlation between Leipzig frequency rankings and Miller-Kersten’s speech frequencies from Miller corpus", 
     xlab = "Leipzig frequency rankings", ylab = "Miller-Kersten’s speech frequencies from Miller corpus")
abline(lm(Freq_El_Omrani_Jaouhar$`Leipzig - Rank` ~ Freq_El_Omrani_Jaouhar$`Miller-Kersten`), col = "blue")





