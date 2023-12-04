### Arran Terrestrial Inverts Data analysis ###

#load the data "ArranTerrestrialInverts.csv"

terr_inverts1 <- read.csv(file.choose(), header = TRUE)

#correct column names
colnames(terr_inverts1)[1] <- "eventID"

### most are ID'd to Order at least, and many to family
#so first will remove the rows with nas in the family column
#and work with families

terr_inverts1$family[terr_inverts1$family ==""] <- NA

na_fam <- which(is.na(terr_inverts1$family))

terr_inverts <- terr_inverts1[-na_fam, ] 


##now I could try subset the data into north and south

NorthTInverts <- terr_inverts[c(1:104),]

SouthTInverts <- terr_inverts[c(105:174),]

 

#load ggplot and dplyr

library(ggplot2)
library(dplyr)

#making a plot now for family abundance in the Northern Plot:

##First create a new dataframe grouping the counts by trasect and family

Ninverts_by_transect <- NorthTInverts %>%
  group_by(transect, family) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#now make the plot

ggplot(Ninverts_by_transect, aes(x = family, y = total_counts, fill = transect))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Terrestrial Invertebrate Families", y = "Individual Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE) + 
  facet_wrap(~ transect, nrow = 3)



#### Now repeat the same for the Southern slope:

#group by family and transect

Sinverts_by_transect <- SouthTInverts %>%
  group_by(transect, family) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#plot family abundance

ggplot(Sinverts_by_transect, aes(x = family, y = total_counts, fill = transect))+
  geom_bar(stat = "identity", color="black", position = position_dodge(width = 0.8))+
  labs(x = "Terrestrial Invertebrate Families", y = "Individual Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE) + 
  facet_wrap(~ transect, nrow = 3)


####################

#Shannon indices

###For the NOrth Plot

#calculate total counts of individuals:

total_tinvertsN <- sum(NorthTInverts$individualCount)

#load necessary library

library(vegan)

#group by family
NTinverts <- NorthTInverts %>%
  group_by(family) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package
ShannonN <- diversity(NTinverts$total_counts, index = "shannon")

print(ShannonN)
#2.861686


#and for the South slope:

#calculate total counts of individuals:

total_tinvertsS <- sum(SouthTInverts$individualCount)

#group by family

STinverts <- SouthTInverts %>%
  group_by(family) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#calculate shannon diversity using vegan package

ShannonS <- diversity(STinverts$total_counts, index = "shannon")

print(ShannonS)
#2.564504


#### Now plot the shannon indices


#Create a dataframe of the shannon indices
ShannondivTI <- data.frame(
  Plot = c("North", "South"),
  Shannon_Diversity = c(ShannonN, ShannonS)
)

#and now plot them
ggplot(ShannondivTI, aes(x = Plot, y = Shannon_Diversity, fill = Plot))+
  geom_bar(stat="identity", color = "black", alpha = 0.8, width = 0.5)+
  labs(x="Plot", y = "Terrestrial Invertebrate family Shannon Diversity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)



###Finally, create a plot with terrestrial invertebrate diversity 
#by plot only, for better comparison ability:


#Group by plot and family

TIGrouped <- terr_inverts %>%
  group_by(plot, family) %>%
  summarise(total_counts=sum(individualCount)) %>%
  ungroup()

#and make the plot:

ggplot(TIGrouped, aes(x = family, y = total_counts, fill = plot)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.7)) +
  labs(x = "Terrestrial Invertebrate Families", y = "Count") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        panel.grid = element_line(color = "gray", size = 0.2),  # Thin grey gridlines
        panel.background = element_rect(fill = "lightgray"))+  # Pale grey background
  guides(fill = FALSE)+
  facet_wrap(~ plot, nrow = 2) 


###Finally, obtain summary stats

sum(birdsGrouped$total_counts)
#141 bird individuals

#
sum(NTinverts$total_counts)
#231

sum(STinverts$total_counts)
#146


nspecies <- length(unique(terr_inverts$family))
print(nspecies)
#52

nspeiesN <- length(unique(NTinverts$family))
#37


nspeciesS <- length(unique(STinverts$family))
#29
