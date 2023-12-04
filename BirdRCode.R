### Arran Bird Data analysis ###

#load the data "ArranBirds.csv"

birds <- read.csv(file.choose(), header = TRUE)

#correct column names
colnames(birds)[1] <- "eventID"

##subset the data into north and south

NorthBirds <- birds[c(1:13,33:37),]

SouthBirds <- birds[c(14:32,38:41),]

##Make a barchart of species: 

#load ggplot

library(ggplot2)
library(dplyr)


#making a plot of abundance for species in the Northern Plot:

ggplot(NorthBirds, aes(x = species, y = individualCount, fill = transect))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Species", y = "Individual Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 85, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  facet_wrap(~ transect, nrow = 2) +
  guides(fill = FALSE)

#including formatting

################################


total_birdsN <- sum(NorthBirds$total_counts)

#shannon indicex north:

#load in necessary vegan package
library(vegan)

#create a new dataframe in the correct format to calculate shannon
#diversity using the vegan package
Nbirds <- NorthBirds %>%
  group_by(species) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonN <- diversity(Nbirds$total_counts, index = "shannon")

print(ShannonN)
#1.816095




##################################

##and now repeat the above for the southern plot


ggplot(SouthBirds, aes(x = species, y = individualCount, fill = transect))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Species", y = "Individual Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 85, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  facet_wrap(~ transect, nrow = 2) +
  guides(fill = FALSE)

##including formatting



#shannon index south:

#calculate total counts of individuals:

total_birdsS <- sum(SouthBirds$individualCount)

#Calculate Shannon diversity index

#create a new dataframe in the correct format to calculate shannon
#diversity using the vegan package
Sbirds <- SouthBirds %>%
  group_by(species) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonS <- diversity(Sbirds$total_counts, index = "shannon")

print(ShannonS)
#2.092463

#seems to be a slightly higher diversity on the Southern Plot


##Now, plot those Shannon diversities

#make it into a dataframe
Shannondiv <- data.frame(
  Plot = c("North", "South"),
  Shannon_Diversity = c(ShannonN, ShannonS)
)

#and now plot them:
ggplot(Shannondiv, aes(x = Plot, y = Shannon_Diversity, fill = Plot))+
  geom_bar(stat="identity", color = "black", alpha = 0.8, width = 0.5)+
  labs(x="Plot", y = "Species Shannon Diversity") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)


####################################

###make a plot of bird species individual counts in total and
#separated by plot only:


#First make a new dataframe with counts grouped by species and plot

birdsGrouped <- birds %>%
  group_by(plot, species) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#and making the plot:

ggplot(birdsGrouped, aes(x = species, y = total_counts, fill = plot)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.7)) +
  labs(x = "Species", y = "Individual Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 85, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE) +
  facet_wrap(~ plot, nrow = 2) 


######### Finally, I will just get some very simple data to use in my write-up

sum(birdsGrouped$total_counts)
#141 bird individuals

#
sum(Nbirds$total_counts)
#81

sum(Sbirds$total_counts)
#60


nspecies <- length(unique(birdsGrouped$species))
print(nspecies)
#17

nspeiesN <- length(unique(NorthBirds$species))
#11

nspeciesN <- length(unique(Nbirds$species))
#11


nspeciesS <- length(unique(SouthBirds$species))
#11

nspecS <- length(unique(Sbirds$species))
#11
