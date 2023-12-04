### Arran Aquatic Inverts Data analysis ###

#load the data "ArranAqInverts.csv"

aq_inverts <- read.csv(file.choose(), header = TRUE)

#correct column names
colnames(aq_inverts)[1] <- "eventID"


##subset the data into north and south

NAInverts <- aq_inverts[c(1:24),]

SAInverts <- aq_inverts[c(25:50),]



#load ggplot and dplyr

library(ggplot2)
library(dplyr)

#making a plot now for species in the Northern Plot:

#first group by order and waterbody

NAI_bywater <- NAInverts %>%
  group_by(waterbody, order) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#plot order abundances
ggplot(NAI_bywater, aes(x = order, y = total_counts, fill = waterbody))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Aquatic Invertebrate Orders", y = "Individual Counts") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)+
  facet_wrap(~ waterbody, nrow = 3)


#### Do the same for the Southern slope:

#group by order and waterbody
SAI_bywater <- SAInverts %>%
  group_by(waterbody, order) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#plot order abundances
ggplot(SAI_bywater, aes(x = order, y = total_counts, fill = waterbody))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Aquatic Invertebrate Orders", y = "Individual Counts") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)+
  facet_wrap(~ waterbody, nrow = 2)


####################

#shannon indices:

###For the NOrth Plot

#load in necessary library
library(vegan)

#group by order
NAinvs <- NAInverts %>%
  group_by(order) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonN <- diversity(NAinvs$total_counts, index = "shannon")

print(ShannonN)
#1.40483


#and for the South slope:

#group by order
SAinvs <- SAInverts %>%
  group_by(order) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonS <- diversity(SAinvs$total_counts, index = "shannon")

print(ShannonS)
#1.826863

##Slightly higher aquatic invertebrate diversity on the Southern plot


#### And now plot the shannon indices...


#Make a dataframe with the shannon indices
ShannondivAI <- data.frame(
  Plot = c("North", "South"),
  Shannon_Diversity = c(ShannonN, ShannonS)
)

#and plotting them
ggplot(ShannondivAI, aes(x = Plot, y = Shannon_Diversity, fill = Plot))+
  geom_bar(stat="identity", color = "black", alpha = 0.8, width = 0.5)+
  labs(x="Plot", y = "Order Shannon Diversity") + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)



###And now finally, I can create a plot with terrestrial invertebrate diversity 
#by plot only, for better comparison ability:


#First group by order and plot

AIGrouped <- aq_inverts %>%
  group_by(plot, order) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#and making a plot of order abundance:

ggplot(AIGrouped, aes(x = order, y = total_counts, fill = plot)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.7)) +
  labs(x = "Order", y = "Count") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE) +
  facet_wrap(~ plot, nrow = 2) 


###Finally, generating summary stats:

sum(AIGrouped$total_counts)
#90

#
sum(NAinvs$total_counts)
#51

sum(SAinvs$total_counts)
#39


norders <- length(unique(AIGrouped$order))
print(norders)
#8

nordersN <- length(unique(NAinvs$order))
print(nordersN)
#6


nordersS <- length(unique(SAinvs$order))
print(nordersS)
#7
