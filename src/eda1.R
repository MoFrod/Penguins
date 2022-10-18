# Histogram of bill length for penguins
H1 <- my.penguins %>%
  ggplot(aes(bill_length_mm)) + geom_histogram(binwidth = 2, fill = "#a6cee3", color = "#1f78b4") + labs(x = "Bill Length (mm)", y = "Count") + scale_x_continuous(limits = c(30, 60))

# Histogram of bill depth for penguins
H2 <- my.penguins %>%
  ggplot(aes(bill_depth_mm)) + geom_histogram(binwidth = 0.7, fill = "#1f78b4", color = "#a6cee3") + labs(x = "Bill Depth (mm)", y = "Count")  + scale_x_continuous(limits = c(10, 25))

# Histogram of flipper length
H3 <- my.penguins %>%
  ggplot(aes(flipper_length_mm)) + geom_histogram(binwidth = 4.8, fill = "#b2df8a", color = "#33a02c") + labs(x = "Flipper Length (mm)", y = "Count")  + scale_x_continuous(limits = c(150, 250))

# Histogram of flipper length
H4 <- my.penguins %>%
  ggplot(aes(body_mass_g)) + geom_histogram(binwidth = 300, fill = "#33a02c", color = "#b2df8a") + labs(x = "Body Mass (g)", y = "Count") + scale_x_continuous(limits = c(2000, 7000))

# Visualise the graphs
grid.arrange(H1, H2, H3, H4, ncol=2, nrow=2)

# Summary statistics for penguin variables
describe(my.penguins)

# Calculate coefficient of variance of variables
BL <- (5.31/43.25) * 100
BD <- (1.93/17.13) * 100
FL <- (13.69/199.39) * 100
BM <- (796.44/4148.25) * 100


# Create a table of summaries
my.summary <- tribble(~Variable, ~Median, ~Mean, ~SD, ~CoV, "Bill Length (mm)", 43.15, 43.25, 5.31, 12.28, "Bill Depth (mm)", 17.20, 17.13, 1.93, 11.27, "Flippper Length (mm)", 196.00, 199.39, 13.69, 6.87, "Body Mass (g)", 3962.50, 4148.25, 796.44, 19.20)

# Boxplot of bill length for penguin species
B1 <- my.penguins %>%
  ggplot(aes(x = species, y = bill_length_mm)) + geom_boxplot(alpha = 0.5, color = "#a6cee3") + labs(y = "Bill Length (mm)") + theme(axis.title.x=element_blank()) + expand_limits(y = 30)

# Boxplot of bill depth for penguin species
B2 <- my.penguins %>%
  ggplot(aes(x = species, y = bill_depth_mm)) + geom_boxplot(alpha = 0.5, color = "#1f78b4") + labs(y = "Bill Depth (mm)") + theme(axis.title.x=element_blank()) + expand_limits(y = 12)

# Boxplot of flipper length for penguin species
B3 <- my.penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm)) + geom_boxplot(alpha = 0.5, color = "#b2df8a") + labs(y = "Flipper Length (mm)") + theme(axis.title.x=element_blank()) + expand_limits(y = 160)

# Boxplot of body mass for penguin species
B4 <- my.penguins %>%
  ggplot(aes(x = species, y = body_mass_g)) + geom_boxplot(alpha = 0.5, color = "#33a02c") + labs(y = "Body Mass (g)") + theme(axis.title.x=element_blank()) + expand_limits(y = 2000)

# Visualise the plots
grid.arrange(B1, B2, B3, B4, ncol=2, nrow=2)

# Density Distribution of bill length for penguins by species
D1 <- my.penguins %>%
  ggplot(aes(x = bill_length_mm, colour = species)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bill Length (mm)", y = "Density", color = "Species") +   theme(legend.position="none") + scale_x_continuous(limits = c(30, 60)) + scale_color_brewer(palette = "Paired")

# Density Distribution of bill depth for penguins by species
D2 <- my.penguins %>%
  ggplot(aes(x = bill_depth_mm, colour = species)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bill Depth (mm)", y = "Density") +   theme(legend.position="none") + scale_x_continuous(limits = c(10, 25)) + scale_color_brewer(palette = "Paired")

# Density Distribution of flipper length for penguins by species
D3 <- my.penguins %>%
  ggplot(aes(x = flipper_length_mm, colour = species)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Flipper Length (mm)", y = "Density") +   theme(legend.position="none") + scale_x_continuous(limits = c(150, 250)) + scale_color_brewer(palette = "Paired")

# Density Distribution of flipper length for penguins by species
D4 <- my.penguins %>%
  ggplot(aes(x = body_mass_g, colour = species)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Body Mass (g)", y = "Density") +   theme(legend.position="none") + scale_x_continuous(limits = c(2000, 7000)) + scale_color_brewer(palette = "Paired")

# Visualise the denisty distributions
ggarrange(D1, D2, D3, D4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

# QQ-plot of bill length 
my.penguins %>%
  filter(species != "Chinstrap") %>%
  ggplot(aes(sample = bill_length_mm, colour = species)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Bill Length", color = "Species") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)

# QQ-plot of bill depth 
my.penguins %>%
  filter(species != "Chinstrap") %>%
  ggplot(aes(sample = bill_depth_mm, colour = species)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Bill Depth", color = "Species") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)

# QQ-plot of flipper length 
my.penguins %>%
  filter(species != "Chinstrap") %>%
  ggplot(aes(sample = flipper_length_mm, colour = species)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Flipper Length", color = "Species") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)

# QQ-plot of body mass
my.penguins %>%
  filter(species != "Chinstrap") %>%
  ggplot(aes(sample = body_mass_g, colour = species)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Body Mass", color = "Species") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)


# QQ-plot of bill length for Adelie 
Q1 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(sample = bill_length_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Adelie", color = "Sex") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of bill length for Gentoo 
Q2 <- my.penguins %>%
  filter(species == "Gentoo") %>%
  ggplot(aes(sample = bill_length_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Gentoo") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of bill length for Chinstrap 
Q3 <- my.penguins %>%
  filter(species == "Chinstrap") %>%
  ggplot(aes(sample = bill_length_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Chinstrap") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# Visualise the QQ-plots for bill length
ggarrange(Q1, Q2, Q3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

# QQ-plot of bill depth for Adelie 
Q4 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(sample = bill_depth_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Adelie", color = "Sex") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of bill depth for Gentoo 
Q5 <- my.penguins %>%
  filter(species == "Gentoo") %>%
  ggplot(aes(sample = bill_depth_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Gentoo") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of bill depth for Chinstrap 
Q6 <- my.penguins %>%
  filter(species == "Chinstrap") %>%
  ggplot(aes(sample = bill_depth_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Chinstrap") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# Visualise bill depth QQ plots
ggarrange(Q4, Q5, Q6, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

# QQ-plot of flipper length for Adelie 
Q7 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(sample = flipper_length_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Adelie", color = "Sex") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of flipper length for Gentoo 
Q8 <- my.penguins %>%
  filter(species == "Gentoo") %>%
  ggplot(aes(sample = flipper_length_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Gentoo") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of flipper length for Chinstrap 
Q9 <- my.penguins %>%
  filter(species == "Chinstrap") %>%
  ggplot(aes(sample = flipper_length_mm, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Chinstrap") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# Visualise flipper length QQ plots
ggarrange(Q7, Q8, Q9, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

# QQ-plot of body mass for Adelie 
Q10 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(sample = body_mass_g, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Adelie", color = "Sex") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of body mass for Gentoo 
Q11 <- my.penguins %>%
  filter(species == "Gentoo") %>%
  ggplot(aes(sample = body_mass_g, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Gentoo") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# QQ-plot of body mass for Chinstrap 
Q12 <- my.penguins %>%
  filter(species == "Chinstrap") %>%
  ggplot(aes(sample = body_mass_g, colour = sex)) + geom_qq() + labs(y = "Sample Quantiles", x = "Theoretical Quantiles", title = "Chinstrap") + scale_color_brewer(palette = "Paired") + stat_qq_line(alpha = 0.7)  +   theme(legend.position="none")

# Visualise bill depth QQ plots
ggarrange(Q10, Q11, Q12, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

