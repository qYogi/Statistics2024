minimbarbati = min(male)
maximbarbati = max(female)
minimfemei = min(female)
maximfemei = max(female)

linesman = seq(from = minimbarbati, to = maximbarbati, length.out = 8 )
linesfemale = seq(from = minimfemei, to = maximfemei, length.out = 8 )

hist(male, breaks = linesman, right = F, freq = F, col = "blue", main = "Barbati")
hist(female, breaks = linesfemale, right = F, freq = F, col = "red", main = "Femei")