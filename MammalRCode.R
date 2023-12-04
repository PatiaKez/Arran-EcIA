### Arran Mammal Data analysis ###

#load in the "ArranMammals2.csv" data
mammals <- read.csv(file.choose(), header = TRUE)

#correct column names
colnames(mammals)[1] <- "eventID"


##subset the data into north and south

NMammals <- mammals[c(1:7),]

SMammals <- mammals[c(8:12),]


#load ggplot and dplyr

library(ggplot2)
library(dplyr)


#creating a presance absence matrix:

#load necessary library
library(reshape2)

#create presence-absence matrix
pres_abs <- dcast(mammals, species~plot, fun.aggregate = function(x) as.numeric(length(unique(x))>0))


#Making a presence-absence plot

#load necessary library
library(tidyr)

#reshape the data for ggplot2:
pres_abs_long <- tidyr::gather(pres_abs, key = "plot", value = "presence", -species)


#Code for the plot

ggplot(pres_abs_long, aes(x=plot, y=species, fill=factor(presence)))+
  geom_tile(color="white")+
  scale_fill_manual(values = c("gray90", "skyblue"), labels = c("Absent", "Present"))+
  labs(x = "Plot",
       y = "Species") +
  theme_minimal()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=12))


#####################################