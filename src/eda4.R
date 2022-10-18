# Summary statistics for Adelie
A <- my.penguins %>% 
  filter(species == "Adelie")

describe(A)

# Summary statistics for Gentoo
G <- my.penguins %>% 
  filter(species == "Gentoo")

describe(G)

# Summary statistics for Chinstrap
C <- my.penguins %>% 
  filter(species == "Chinstrap")

describe(C)

# From the Q-Q plots in Appendix, it appears bill length and body mass may be the most discrepant between males and females.
# To test this, a 2-sample t-test hypothesis

#Test equal variance b/w males and females for Adelie Bill Length
bartlett.test(bill_length_mm ~ sex, data = A) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Adelie Bill Length
t.test(bill_length_mm ~ sex, data = A, var.equal = T) # p-value is definitely much less than 0.05 and so we have strong evidence to suggest there is a difference in mean bill length between males and females for Adelie penguins.


#Test equal variance b/w males and females for Adelie Body Mass
bartlett.test(body_mass_g ~ sex, data = A) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Adelie Body Mass
t.test(body_mass_g ~ sex, data = A, var.equal = T) # p-value is definitely much less than 0.05 and so we have strong evidence to suggest there is a difference in mean bill length between males and females for Adelie penguins.

#Test equal variance b/w males and females for Gentoo Bill Length
bartlett.test(bill_length_mm ~ sex, data = G) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Gentoo Bill Length
t.test(bill_length_mm ~ sex, data = G, var.equal = T) # p-value is definitely much less than 0.05 and so we have strong evidence to suggest there is a difference in mean bill length between males and females for Gentoo penguins.

#Test equal variance b/w males and females for Gentoo Body Mass
bartlett.test(body_mass_g ~ sex, data = G) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Gentoo Body Mass
t.test(body_mass_g ~ sex, data = G, var.equal = T) # p-value is definitely much less than 0.05 and so we have strong evidence to suggest there is a difference in mean bill length between males and females for Gentoo penguins.

#Test equal variance b/w males and females for Chinstrap Bill Length
bartlett.test(bill_length_mm ~ sex, data = C) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Chinstrap Bill Length
t.test(bill_length_mm ~ sex, data = C, var.equal = T) # p-value is definitely much less than 0.05 and so we have strong evidence to suggest there is a difference in mean bill length between males and females for Chinstrap penguins.

#Test equal variance b/w males and females for Chinstrap Body Mass
bartlett.test(body_mass_g ~ sex, data = C) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Gentoo Body Mass
t.test(body_mass_g ~ sex, data = C, var.equal = T) # p-value is less than 0.05, but higher than 0.01 and so we have moderate evidence to suggest there is a difference in mean bill length between males and females for Chinstrap penguins.


# Contradictory test for flipper length of Adelie penguins

#Test equal variance b/w males and females for Adelie Flipper Length
bartlett.test(flipper_length_mm ~ sex, data = A) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Adelie Flipper Length
t.test(flipper_length_mm ~ sex, data = A, var.equal = T) # p-value is less than 0.05, but higher than 0.01, and so we have moderate evidence to suggest there is a difference in mean bill length between males and females for Adelie penguins.

# Contradictory test for bill depth of Adelie penguins

#Test equal variance b/w males and females for Adelie depth Length
bartlett.test(bill_depth_mm ~ sex, data = A) # p-value is greater than 0.05, so the assumption of equal variance is valid.

#t-test for Adelie bill depth
t.test(bill_depth_mm ~ sex, data = A, var.equal = T) # p-value is definitely much less than 0.05 and so we have strong evidence to suggest there is a difference in mean bill length between males and females for Chinstrap penguins.

#If you have time test hypothesis for all. 
