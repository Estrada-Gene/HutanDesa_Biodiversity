######### vegetation data for Hutan Desa (HD) sites ###########
library(tidyverse)
library(vegan)

#reading in vegetation data from 2016 HD survey 
v <- read.csv(file = "Veg_2016.csv", header = TRUE, na.strings = c("", " "))
head(v)

#it looks like all stems except 34 were ID'd to the species level
table(is.na(v$Species))

#only one stem did not have an ID to the genus level
table(is.na(v$Genus))

#creating a column for Latin Binomial for rows that have both Genus and Species ID's
v$scientific.name <- ifelse(!is.na(v$Genus) & !is.na(v$Species), 
                            paste(v$Genus, v$Species, sep = " "), 
                            NA)

########################################################################################################
###### VEGETATION DIVERSITY ######

#table showing observations of species by HD site
species_table <- v %>%
  group_by(Location, scientific.name) %>%
  filter(!is.na(scientific.name)) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Location, values_from = count, values_fill = 0) %>%
  arrange(desc(`Nipah Kuning`))

print(species_table, n = nrow(species_table))  

#similar table showing observations by genera
genus_table <- v %>%
  group_by(Location, Genus) %>%
  filter(!is.na(Genus)) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Location, values_from = count, values_fill = 0) %>%
  arrange(desc(`Nipah Kuning`))

print(genus_table, n = nrow(genus_table))  


#comparing vegetation composition and diversity by site
# Calculate species richness by site
species_richness <- v %>%
  group_by(Location) %>%
  summarise(SpeciesRichness = length(unique(scientific.name))) %>%
  arrange(desc(SpeciesRichness))

# Calculate Shannon diversity index by site
shannon_index <- diversity(table(v$Location, v$scientific.name), index = "shannon")
shannon_index <- rownames_to_column(as.data.frame(shannon_index), var = "Location")

# Calculate Simpson diversity index by site
simpson_index <- diversity(table(v$Location, v$scientific.name), index = "simpson")
simpson_index <- rownames_to_column(as.data.frame(simpson_index), var = "Location")

#create summary table
veg_summary <- merge(species_richness, shannon_index, by = "Location")
veg_summary <- merge(veg_summary, simpson_index, by = "Location")
veg_summary


#ordination analysis
t1 <- v %>%
  group_by(Location, scientific.name) %>%
  filter(!is.na(scientific.name)) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = scientific.name, values_from = count, values_fill = 0)

MDS1 <- metaMDS(t1[,-1])
ordiplot(MDS1, type = "points")

########################################################################################################
###### STRUCTURAL DIFFERENCES ######

#I'm not sure if I'll use this data much, so just visually assessing things for now

#comparing DBH across sites - no obvious difference here, minus the one outlier in Pemangkat
ggplot(v, aes(x = DBH, y = Location)) +
  geom_violin() + 
  geom_boxplot(width=0.3, alpha=0.2) +
  geom_jitter(height = .1, alpha = 0.2) + 
  labs(title = "Tree Diameters at Hutan Desa Sites (2016)",
       x = "DBH (cm)", y = "") +
  theme_minimal()

#comparing tree height across sites
ggplot(v, aes(x = Tree.Height, y = Location)) +
  geom_violin() + 
  geom_boxplot(width=0.3, alpha=0.2) +
  geom_jitter(height = .1, alpha = 0.2) + 
  labs(title = "Tree Height at Hutan Desa Sites (2016)",
       x = "height (m)", y = "") +
  theme_minimal()
