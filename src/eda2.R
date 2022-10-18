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

# Create a table of summary statistics

Species_summary <- tribble(~Variable, ~"A Mean", ~"A SD", ~"G Mean", ~"G SD", ~"C Mean", ~"C SD", "Bill Length (mm)", 38.70, 2.58, 46.88, 3.08, 48.99,2.68, "Bill Depth (mm)", 18.15, 1.21, 14.88, 0.91, 18.44, 1.19, "Flippper Length (mm)", 189.78, 6.80, 189.78, 6.80, 216.42, 5.68, "Body Mass (g)", 3691.84, 452.38, 3691.84, 452.38, 5075.76, 481.76)

# Calculate confidence interval for Adelie Bill length
ABL.mean <- 38.70
ABL.sd <- 2.58
A.n <- 49
ABL.error <- qnorm(0.975)*ABL.sd/sqrt(ABL.n)
ABL.left <- ABL.mean - ABL.error
ABL.right <- ABL.mean + ABL.error
ABL.left
ABL.right

# Calculate confidence interval for Adelie Bill Depth
ABD.mean <- 18.15
ABD.sd <- 1.21
A.n <- 49
ABD.error <- qnorm(0.975)*ABD.sd/sqrt(A.n)
ABD.left <- ABD.mean - ABD.error
ABD.right <- ABD.mean + ABD.error
ABD.left
ABD.right

# Calculate confidence interval for Adelie Flipper Length
AFL.mean <- 189.78
AFL.sd <- 6.80
A.n <- 49
AFL.error <- qnorm(0.975)*AFL.sd/sqrt(A.n)
AFL.left <- AFL.mean - AFL.error
AFL.right <- AFL.mean + AFL.error
AFL.left
AFL.right

# Calculate confidence interval for Adelie Body Mass
ABM.mean <- 3691.84
ABM.sd <- 452.38
A.n <- 49
ABM.error <- qnorm(0.975)*ABM.sd/sqrt(A.n)
ABM.left <- ABM.mean - ABM.error
ABM.right <- ABM.mean + ABM.error
ABM.left
ABM.right

# Calculate confidence interval for Gentoo Bill length
GBL.mean <- 46.88
GBL.sd <- 3.08
G.n <- 33
GBL.error <- qnorm(0.975)*GBL.sd/sqrt(G.n)
GBL.left <- GBL.mean - GBL.error
GBL.right <- GBL.mean + GBL.error
GBL.left
GBL.right

# Calculate confidence interval for Gentoo Bill Depth
GBD.mean <- 14.88
GBD.sd <- 0.91
G.n <- 33
GBD.error <- qnorm(0.975)*GBD.sd/sqrt(G.n)
GBD.left <- GBD.mean - GBD.error
GBD.right <- GBD.mean + GBD.error
GBD.left
GBD.right

# Calculate confidence interval for Gentoo Flipper Length
GFL.mean <- 216.42
GFL.sd <- 5.68
G.n <- 33
GFL.error <- qnorm(0.975)*GFL.sd/sqrt(G.n)
GFL.left <- GFL.mean - GFL.error
GFL.right <- GFL.mean + GFL.error
GFL.left
GFL.right

# Calculate confidence interval for Gentoo Body Mass
GBM.mean <- 5075.76
GBM.sd <- 481.76
G.n <- 33
GBM.error <- qnorm(0.975)*GBM.sd/sqrt(G.n)
GBM.left <- GBM.mean - GBM.error
GBM.right <- GBM.mean + GBM.error
GBM.left
GBM.right

# Calculate confidence interval for Chinstrap Bill length
CBL.mean <- 48.99
CBL.sd <- 2.68
C.n <- 33
CBL.error <- qnorm(0.975)*CBL.sd/sqrt(C.n)
CBL.left <- CBL.mean - CBL.error
CBL.right <- CBL.mean + CBL.error
CBL.left
CBL.right

# Calculate confidence interval for Chinstrap Bill Depth
CBD.mean <- 18.44
CBD.sd <- 1.19
C.n <- 33
CBD.error <- qnorm(0.975)*CBD.sd/sqrt(C.n)
CBD.left <- CBD.mean - CBD.error
CBD.right <- CBD.mean + CBD.error
CBD.left
CBD.right

# Calculate confidence interval for Chinstrap Flipper Length
CFL.mean <- 194.33
CFL.sd <- 6.65
C.n <- 33
CFL.error <- qnorm(0.975)*CFL.sd/sqrt(C.n)
CFL.left <- CFL.mean - CFL.error
CFL.right <- CFL.mean + CFL.error
CFL.left
CFL.right

# Calculate confidence interval for Chinstrap Body Mass
CBM.mean <- 3690.28
CBM.sd <- 432.17
C.n <- 33
CBM.error <- qnorm(0.975)*CBM.sd/sqrt(C.n)
CBM.left <- CBM.mean - CBM.error
CBM.right <- CBM.mean + CBM.error
CBM.left
CBM.right

# Bring confidence intervals together in a table. 

Bill_Length_summary <- tribble(~Species, ~"Mean (mm)", ~"95% CI (mm)", ~"SD", 
                                "Adelie", 38.70, "37.98-39.42", 2.58,
                                "Gentoo", 46.88, "45.83-47.93", 3.09,
                                "Chinstrap", 48.99, "48.08-49.90", 2.68)
kable(Bill_Length_summary, caption = "Bill Length Summary Statistics") %>%
  kable_classic_2(full_width = F, latex_options = "HOLD_position")

Bill_Depth_summary <- tribble(~Species, ~"Mean (mm)", ~"95% CI (mm)", ~"SD", 
                               "Adelie", 18.15, "17.81-18.49", 1.21, 
                               "Gentoo", 14.88, "14.57-15.19", 0.91, 
                               "Chinstrap", 18.44, "18.03-18.85", 1.19)
kable(Bill_Depth_summary, caption = "Bill Depth Summary Statistics") %>%
  kable_classic_2(full_width = F, latex_options = "HOLD_position")

Flipper_Length_summary <- tribble(~Species, ~"Mean (mm)", ~"95% CI (mm)", ~"SD",
                               "Adelie", 189.78, "187.88-191.68", 6.80,
                               "Gentoo", 216.42, "214.48-218.36", 5.68,
                               "Chinstrap", 194.33, "192.06-196.60", 6.65)
kable(Flipper_Length_summary, caption = "Flipper Length Summary Statistics") %>%
  kable_classic_2(full_width = F, latex_options = "HOLD_position")

Body_Mass_summary <- tribble(~Species, ~"Mean (g)", ~"95% CI (g)", ~"SD", 
                               "Adelie", 3691.84, "3565.18-3818.50", 452.38,
                               "Gentoo", 5075.76, "4911.39-5240.13", 481.76,
                               "Chinstrap", 3690.28, "3542.83-3837.73", 432.17)
kable(Body_Mass_summary, caption = "Body Mass Summary Statistics") %>%
  kable_classic_2(full_width = F, latex_options = "HOLD_position")
