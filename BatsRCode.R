##### Arran Bats data analysis ####

#load the data "ArranBats.csv"

bats <- read.csv(file.choose(), header = TRUE)

#correct column names
colnames(bats)[1] <- "eventID"


###ok, since this is only presence data, I want to create a presence/absence 
#matrix, which will make ti easier to visualise and plot:

#first I will separate it by plot, so that I can visualise presence/absence
#by habitat type

Sbats <- bats[c(1:25),]

Nbats <- bats[c(26:43),]


##converting the data into a pres/absence matrix fromat:

pres_abs <- dcast(bats, genus ~ plot, fun.aggregate = function(x) as.numeric(length(unique(x))>0))



### Making the plot

#load necessary library
library(tidyr)

#reshape the data for ggplot2:
pres_abs_long <- tidyr::gather(pres_abs, key = "plot", value = "presence", -genus)


#plot code

ggplot(pres_abs_long, aes(x=plot, y=genus, fill=factor(presence)))+
  geom_tile(color="white")+
  scale_fill_manual(values = c("gray90", "skyblue"), labels = c("Absent", "Present"))+
  labs(title = "Bat Genus Presence-Absence Plot",
       x = "Plot",
       y = "Species") +
  theme_minimal()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=12))

