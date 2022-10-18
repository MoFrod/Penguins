# How many penguins of each species are on each island?
locations_species <- my.penguins %>%
  group_by(island, species) %>%
  summarise(species) %>%
  count(species) %>%
  as_tibble() # B= 17, D = 17, T = 15

# Quick plot count of each species per island
locations_species %>%
  ggplot(aes(x = island, y = n, fill = species)) + geom_bar(stat="identity", position="dodge") + labs(title = "Penguins Species per Island", x = "Location", y = "Count", fill = "Species") + scale_fill_brewer(palette = "Paired")

# Adelie penguins is the only species at appears on all three islands, so the investigation will focus on them. 

# Boxplot of bill length for Adelie species by Island
B5 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = island, y = bill_length_mm)) + geom_boxplot(alpha = 0.5, color = "#a6cee3") + labs(y = "Bill Length (mm)") + theme(axis.title.x=element_blank()) + expand_limits(y = 30)

# Boxplot of bill depth for Adelie species by Island
B6 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = island, y = bill_depth_mm)) + geom_boxplot(alpha = 0.5, color = "#1f78b4") + labs(y = "Bill Depth (mm)") + theme(axis.title.x=element_blank()) + expand_limits(y = 12)

# Boxplot of flipper length for Adelie species by Island
B7 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = island, y = flipper_length_mm)) + geom_boxplot(alpha = 0.5, color = "#b2df8a") + labs(y = "Flipper Length (mm)") + theme(axis.title.x=element_blank()) + expand_limits(y = 160)

# Boxplot of body mass for Adelie species by Island
B8 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = island, y = body_mass_g)) + geom_boxplot(alpha = 0.5, color = "#33a02c") + labs(y = "Body Mass (g)") + theme(axis.title.x=element_blank()) + expand_limits(y = 2000)

# Arrange boxplots on grid (USE IN REPORT)
grid.arrange(B5, B6, B7, B8, ncol=2, nrow=2)

#Hypothesis tests:

A <- my.penguins %>% 
  filter(species == "Adelie")

# Dream has larger body mass than Biscoe
Abd <- A %>%
  filter (island != "Torgersen")

##Test equal variance b/w Dream and Biscoe
bartlett.test(body_mass_g ~ island, data = Abd) # p-value = 0.1539, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(body_mass_g ~ island, data = Abd, var.equal = T)  # p-value = 0.2545, p-value is greater than 0.1, no evidence against the body mass being the same across both islands

# Dream has larger body mass than Torgersen
Adt <- A %>%
  filter (island != "Biscoe")

##Test equal variance b/w Dream and Torgersen
bartlett.test(body_mass_g ~ island, data = Adt) # p-value= 0.558, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(body_mass_g ~ island, data = Adt, var.equal = T)  #p-value = 0.3328, p-value is greater than 0.1, no evidence against the body mass being the same across both islands

# Dream has larger bill length than Biscoe

##Test equal variance b/w Dream and Biscoe
bartlett.test(bill_length_mm ~ island, data = Abd) # p-value = 0.2076, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(bill_length_mm ~ island, data = Abd, var.equal = T)  # p-value = 0.191, its greater than 0.1, no evidence against the body mass being the same across both islands

# Dream has larger bill length Torgersen

##Test equal variance
bartlett.test(bill_length_mm ~ island, data = Adt) # p-value = 0.7645, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(bill_length_mm ~ island, data = Adt, var.equal = T)  # p-value = 0.5087, its greater than 0.1, no evidence against the body mass being the same across both islands

# Torgersen has larger bill depth than Biscoe
Atb <- A %>%
  filter (island != "Dream")

##Test equal variance
bartlett.test(bill_depth_mm ~ island, data = Atb) # p-value= 0.3285, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(bill_depth_mm ~ island, data = Atb, var.equal = T) # p-value = 0.6344, its greater than 0.1, no evidence against the body mass being the same across both islands

# Torgerson has larger bill depth than Dream

##Test equal variance
bartlett.test(bill_depth_mm ~ island, data = Adt) #p-value = 0.3247, its greater 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(bill_depth_mm ~ island, data = Adt, var.equal = T) # p-value = 0.815, its greater than 0.1, no evidence against the body mass being the same across both islands

# Torgersen has larger flipper length than Biscoe

##Test equal variance
bartlett.test(flipper_length_mm ~ island, data = Atb) # p-value = 0.7714, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(flipper_length_mm ~ island, data = Atb, var.equal = T) # p-value = 0.483, its greater than 0.05, so the assumption of equal variance is valid.

# Torgersen has larger flipper length than dream

##Test equal variance
bartlett.test(flipper_length_mm ~ island, data = Adt) # p-value = 0.411, its greater than 0.05, so the assumption of equal variance is valid.

##t-test for hypothesis
t.test(flipper_length_mm ~ island, data = Adt, var.equal = T) # p-value = 0.9351,  its greater than 0.05, so the assumption of equal variance is valid.

# Density Distribution of bill length for Adelie species by Island
D5 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = bill_length_mm, colour = island)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bill Length (mm)", y = "Density", color = "Island") +   theme(legend.position="none") + scale_x_continuous(limits = c(30, 60)) + scale_color_brewer(palette = "Paired")

# Density Distribution of bill depth for Adelie species by Island
D6 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = bill_depth_mm, colour = island)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bill Depth (mm)", y = "Density") +   theme(legend.position="none") + scale_x_continuous(limits = c(10, 25)) + scale_color_brewer(palette = "Paired")

# Density Distribution of flipper length for Adelie species by Island
D7 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = flipper_length_mm, colour = island)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Flipper Length (mm)", y = "Density") +   theme(legend.position="none") + scale_x_continuous(limits = c(150, 250)) + scale_color_brewer(palette = "Paired")

# Density Distribution of flipper length for Adelie species by Island
D8 <- my.penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = body_mass_g, colour = island)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Body Mass (g)", y = "Density") +   theme(legend.position="none") + scale_x_continuous(limits = c(2000, 7000)) + scale_color_brewer(palette = "Paired")

# Arrange box on grid
ggarrange(D5, D6, D7, D8, ncol=2, nrow=2, common.legend = TRUE, legend="top")
