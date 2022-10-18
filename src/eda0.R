# Summarise my.penguin
summary(my.penguins)

# Count how many years there are
years <- my.penguins %>%
  group_by(year) %>% 
  count() # There are 3, 2007 = 32, 2008 = 36, 2009 - 32

# Quick plot count of each species
species_sum %>%
  ggplot(aes(x = species, y = n, fill = species)) + geom_bar(stat="identity", position="dodge") + labs(title = "Number of Penguins for Each Species", x = "Species of Penguin", y = "Number of Penguins", fill = "Species") + scale_fill_brewer(palette = "Paired")

# Quick plot count per island
locations_sum %>%
  ggplot(aes(x = island, y = n, fill = island)) + geom_bar(stat="identity", position="dodge") + labs(title = "Number of Penguins on Each Island", x = "Location of Penguins", y = "Number of Penguins", fill = "Island") + scale_fill_brewer(palette = "Paired")

# How many penguins of each species are on each island?
locations_species <- my.penguins %>%
  group_by(island, species) %>%
  summarise(species) %>%
  count(species) %>%
  as_tibble()

# Quick plot count of each species per island
locations_species %>%
  ggplot(aes(x = island, y = n, fill = species)) + geom_bar(stat="identity", position="dodge") + labs(title = "Number of Penguins of Each Species per Island", x = "Location of Penguins", y = "Number of Penguins", fill = "Species") + scale_fill_brewer(palette = "Paired")

# How many male and female penguins are there per Species?
species_sex <- my.penguins %>%
  group_by(species, sex) %>%
  summarise(sex) %>%
  count(sex) %>%
  as_tibble() # There are always fewer men

# How many male and female penguins are there per Island?
location_sex <- my.penguins %>%
  group_by(island, sex) %>%
  summarise(sex) %>%
  count(sex) %>%
  as_tibble() # There's less of a difference between count of male and female penguins on Dream

# Quick plot body mass by male and females per year
my.penguins %>%
  ggplot(aes(x = year, y = body_mass_g, fill = sex)) + geom_bar(stat="identity", position="dodge") + labs(title = "Body Mass of Male and Female Penguins by Year", x = "Year", y = "Penguin Body Mass (grams)", fill = "Sex") + scale_fill_brewer(palette = "Paired")

# Quick plot body mass by male and females per island
my.penguins %>%
  ggplot(aes(x = island, y = body_mass_g, fill = sex)) + geom_bar(stat="identity", position="dodge") + labs(title = "Body Mass of Male and Female Penguins by Location", x = "Location", y = "Penguin Body Mass (grams)", fill = "Sex") + scale_fill_brewer(palette = "Paired")

# Quick plot body mass by males and females per species
my.penguins %>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) + geom_bar(stat="identity", position="dodge") + labs(title = "Body Mass of Male and Female Penguins by Species", x = "Species", y = "Penguin Body Mass (grams)", fill = "Sex") + scale_fill_brewer(palette = "Paired")

