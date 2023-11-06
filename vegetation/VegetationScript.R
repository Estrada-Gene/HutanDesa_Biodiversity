######### vegetation data for Hutan Desa (HD) sites ###########
library(tidyverse)
library(vegan)

#reading in vegetation data from 2016 HD survey 
v <- read.csv(file = "vegetation/Veg_2016.csv", header = TRUE, na.strings = c("", " "))
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

#similar table showing observations by genus
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
  summarise(SpeciesRichness = n_distinct(scientific.name)) %>%
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

### again by genus
genus_richness <- v %>%
  group_by(Location) %>%
  summarise(GenusRichness = n_distinct(Genus)) %>%
  arrange(desc(GenusRichness))

# Calculate Shannon diversity index by site
shannon_index_gen <- diversity(table(v$Location, v$Genus), index = "shannon")
shannon_index_gen <- rownames_to_column(as.data.frame(shannon_index_gen), var = "Location")

# Calculate Simpson diversity index by site
simpson_index_gen <- diversity(table(v$Location, v$Genus), index = "simpson")
simpson_index_gen <- rownames_to_column(as.data.frame(simpson_index_gen), var = "Location")

#create summary table
veg_summary_gen <- merge(genus_richness, shannon_index_gen, by = "Location")
veg_summary_gen <- merge(veg_summary_gen, simpson_index_gen, by = "Location")
veg_summary_gen


###### ordination analysis
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


#########################################################################################################
##### Andy's pheno data from camp plots 2015-2020
pheno <- read.csv(file = "vegetation/Jul2015-Sep2020_pheno_AJM.csv", header = TRUE)
head(pheno)

pheno$hab_unbiased <- factor(pheno$hab_unbiased, levels = c("PS", "FS", "AB", "LS", "LG", "UG"))

#replacing "NULL" and other odd values in the Species and Genus column with NA
#table not in the best shape, many species spelled slightly different, will inflate species counts
pheno$species[pheno$species == "NULL"] <- NA
pheno$species[pheno$species == ""] <- NA
pheno$species[pheno$species == "`"] <- NA
pheno$genus[pheno$genus == ""] <- NA
pheno$genus[pheno$genus == "?"] <- NA
pheno$genus[pheno$genus == "???"] <- NA
pheno$genus[pheno$genus == "????"] <- NA
pheno$genus[pheno$genus == "NA"] <- NA

table(pheno$species)
table(is.na(pheno$species))

table(pheno$genus)

#fixing some of the names manually - some I'm not sure what to do with
pheno$species <- gsub("\\?", "", as.character(pheno$species))
pheno$species[pheno$species == "zwageri"] <- "zwagerii"
pheno$species[pheno$species == "zwageriii"] <- "zwagerii"
pheno$species[pheno$species == "stelllatus"] <- "stellatus"
pheno$species[pheno$species == "sp nov"] <- "spnov"
pheno$genus[pheno$genus == "uvaria"] <- "Uvaria"


#creating a column for Latin Binomial for rows that have both Genus and Species ID's
pheno$scientific.name <- ifelse(!is.na(pheno$genus) & !is.na(pheno$species), 
                            paste(pheno$genus, pheno$species, sep = " "), 
                            NA)


##### extracting data for 2016 and for Peat Swamp only, for initial comparison

#species table - not many species, this might be due to a lack of stems ID'd to the species level
species_table_CPRS <- pheno %>%
  filter(hab_unbiased == "PS" & pheno_year == "2016") %>%
  group_by(scientific.name) %>%
  filter(!is.na(scientific.name)) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(species_table_CPRS, n = nrow(species_table_CPRS))  

#genus table - many more genera listed here
genus_table_CPRS <- pheno %>%
  filter(hab_unbiased == "PS" & pheno_year == "2016") %>%
  group_by(genus) %>%
  filter(!is.na(genus)) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(genus_table_CPRS, n = nrow(genus_table_CPRS))  

##### comparing species richness for the peat swamp across years, just to see if there's any annual difference

#species counts across years - 2016-2019 counts are stable, but counts are lower for 2015 & 2020
PS_sp_all_years <- pheno %>%
  filter(hab_unbiased == "PS") %>%
  group_by(pheno_year, scientific.name) %>%
  filter(!is.na(scientific.name)) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pheno_year, values_from = count, values_fill = 0) %>%
  arrange(desc(2016))

print(PS_sp_all_years, n = nrow(PS_sp_all_years))  

#genus counts across years - similar trend as seen in the species counts above
PS_gen_all_years <- pheno %>%
  filter(hab_unbiased == "PS") %>%
  group_by(pheno_year, genus) %>%
  filter(!is.na(genus)) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pheno_year, values_from = count, values_fill = 0) %>%
  arrange(desc(2016))

print(PS_gen_all_years, n = nrow(PS_gen_all_years))  

##### comparing species and genus counts across forest types at CPRS

#by species first, just to contextualize the counts in the Peat Swamp to other forest types at CPRS
pheno %>%
  group_by(hab_unbiased) %>%
  filter(!is.na(scientific.name)) %>%
  summarise(n.species = n_distinct(scientific.name))

#by genus
pheno %>%
  group_by(hab_unbiased) %>%
  filter(!is.na(genus)) %>%
  summarise(n.species = n_distinct(genus))

##### calculating diversity scores by forest type at CPRS using both species and genera

##first by species
# Calculate species richness by site
species_richness_CPRS <- pheno %>%
  group_by(hab_unbiased) %>%
  summarise(SpeciesRichness = n_distinct(scientific.name)) %>%
  arrange(desc(SpeciesRichness))

# Calculate Shannon diversity index by site
shannon_index_CPRS <- diversity(table(pheno$hab_unbiased, pheno$scientific.name), index = "shannon")
shannon_index_CPRS <- rownames_to_column(as.data.frame(shannon_index_CPRS), var = "hab_unbiased")

# Calculate Simpson diversity index by site
simpson_index_CPRS <- diversity(table(pheno$hab_unbiased, pheno$scientific.name), index = "simpson")
simpson_index_CPRS <- rownames_to_column(as.data.frame(simpson_index_CPRS), var = "hab_unbiased")

#create summary table
veg_summary_CPRS <- merge(species_richness_CPRS, shannon_index_CPRS, by = "hab_unbiased")
veg_summary_CPRS <- merge(veg_summary_CPRS, simpson_index_CPRS, by = "hab_unbiased")
veg_summary_CPRS

##again by genus
# Calculate species richness by site
genus_richness_CPRS <- pheno %>%
  group_by(hab_unbiased) %>%
  summarise(GenusRichness = n_distinct(genus))

# Calculate Shannon diversity index by site
shannon_index_CPRS_gen <- diversity(table(pheno$hab_unbiased, pheno$genus), index = "shannon")
shannon_index_CPRS_gen <- rownames_to_column(as.data.frame(shannon_index_CPRS_gen), var = "hab_unbiased")

# Calculate Simpson diversity index by site
simpson_index_CPRS_gen <- diversity(table(pheno$hab_unbiased, pheno$genus), index = "simpson")
simpson_index_CPRS_gen <- rownames_to_column(as.data.frame(simpson_index_CPRS_gen), var = "hab_unbiased")

#create summary table
veg_summary_CPRS_gen <- merge(genus_richness_CPRS, shannon_index_CPRS_gen, by = "hab_unbiased")
veg_summary_CPRS_gen <- merge(veg_summary_CPRS_gen, simpson_index_CPRS_gen, by = "hab_unbiased")
veg_summary_CPRS_gen

###### comparing species and genus richness and diversity between HD and CPRS forests
#by species - this may not be the best way to compare using Andy's pheno data given the lack of stems ID'd to species
colnames(veg_summary_CPRS) <- c("Location", "SpeciesRichness", "shannon_index", "simpson_index")
veg_summary_CPRS <- veg_summary_CPRS %>% arrange(Location)
rbind(veg_summary, veg_summary_CPRS)

#by genus - this might be the best way to compare, using Andy's data at least
colnames(veg_summary_CPRS_gen) <- c("Location", "GenusRichness", "shannon_index_gen", "simpson_index_gen")
veg_summary_CPRS_gen <- veg_summary_CPRS_gen %>% arrange(Location)
rbind(veg_summary_gen, veg_summary_CPRS_gen)
