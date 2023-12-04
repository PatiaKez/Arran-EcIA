### Arran Tree Data analysis ###

#load the data "ArranTrees.csv"

trees <- read.csv(file.choose(), header = TRUE)

#correct column names
colnames(trees)[1] <- "eventID"


##subset the data into north and south

SouthTrees <- trees[c(1:9),]

NorthTrees <- trees[c(10:13),]


##Make a barchart of species: 

#load ggplot and dplyr

library(ggplot2)
library(dplyr)


#making a plot now for species in the Northern Plot:

ggplot(NorthTrees, aes(x = species, y = individualCount, fill = transect))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Tree Species", y = "Individual Counts") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)+
  facet_wrap(~ transect, nrow = 2) 



##and now repeat the above for the southern plot


ggplot(SouthTrees, aes(x = species, y = individualCount, fill = transect))+
  geom_bar(stat = "identity", color="black", position = position_dodge2(width = 0.8))+
  labs(x = "Tree Species", y = "Individual Counts") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)+
  facet_wrap(~ transect, nrow = 3)



###################### Shannon indeces

#shannon indices:

###For the NOrth Plot

#load vegan package
library(vegan)

#group by species
Ntrees <- NorthTrees %>%
  group_by(species) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonN <- diversity(Ntrees$total_counts, index = "shannon")

print(ShannonN)
#0.953271


###And for the South Plot

#group by species
Strees <- SouthTrees %>%
  group_by(species) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonS <- diversity(Strees$total_counts, index = "shannon")

print(ShannonS)
#1.157338


##Plotting the Shannon indices 

#Make a shannon diversity dataset
ShannondivT <- data.frame(
  Plot = c("North", "South"),
  Shannon_Diversity = c(ShannonN, ShannonS)
)

#and now plot it:
ggplot(ShannondivT, aes(x = Plot, y = Shannon_Diversity, fill = Plot))+
  geom_bar(stat="identity", color = "black", alpha = 0.8, width = 0.5)+
  labs(x="Region", y = "Shannon Diversity") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)



### And finally I can plot tree numbers only separating by plot:


#group by species and plot 

treesGrouped <- trees %>%
  group_by(plot, species) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()


#and making the plot:

ggplot(treesGrouped, aes(x = species, y = total_counts, fill = plot)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.7)) +
  labs(x = "Species", y = "Count") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)+
  facet_wrap(~ plot, nrow = 2)



#and some basic stats:

sum(treesGrouped$total_counts)
#136

#
sum(Ntrees$total_counts)
#15

sum(Strees$total_counts)
#121


nspecies <- length(unique(treesGrouped$species))
print(nspecies)
#7

nspeciesN <- length(unique(Ntrees$species))
print(nspeciesN)
#4


nspeciesS <- length(unique(Strees$species))
print(nspeciesS)
#5
