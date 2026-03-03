#Heatmap for virulence index

library(readxl)
library(tidyverse)
library(scales)
library(cowplot)


setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI")
VI<-read_xlsx("AUC_VI_DATA.xlsx", sheet=2)

VI<-VI%>%
  filter(Phage !="Bacteria Controls AUC")
VI<- VI%>%
  mutate(mean=rowMeans(across(c(virulence_index_1, virulence_index_2, virulence_index_3)), na.rm=TRUE))
VI<-VI%>%
  arrange(desc(mean))
palette <- colorRampPalette(c("#DAF1EA", "#009E73"))
vir_palette<-palette(100)

Infection <- VI %>% mutate(Infection = ifelse(mean >= 0.5, "Yes", "No"))

phage_order <- Infection %>%
  group_by(Phage) %>%
  summarise(Infections = sum(Infection == "Yes")) %>%
  arrange(desc(Infections))

phage_order <- phage_order[c(which(phage_order$Phage == "CPL1301 Control"), which(phage_order$Phage != "CPL1301 Control")), ]

#reordering the data
Infection$Phage <- factor(Infection$Phage, levels = phage_order$Phage)

strain_order <- Infection%>%
  group_by(Strain) %>%
  summarise(Infections = sum(Infection == "Yes")) %>%
  arrange(desc(Infections))
Infection$Strain <- factor(Infection$Strain, levels = strain_order$Strain)

#heatmap showing VI
heatmap_virulence <- ggplot(Infection, aes(x=Strain, y=Phage, fill=mean)) +
  geom_tile(colour = "white", lwd = 1, linetype = 1) +
  scale_fill_gradientn(
    colors = c(rep("#DAF1EA", 10), vir_palette),
    values = rescale(c(seq(-0.5, 0, length.out = 10), seq(0, 1, length.out = length(vir_palette)))),
    limits = c(-0.5, 1),
    oob = squish,
    name = "Virulence Index") +
  geom_text(aes(label = round(mean, 2)), color = "white", size = 4) + 
  scale_y_discrete('Phage', expand = c(0,0)) +
  scale_x_discrete("ST131 Strain", expand = c(0,0)) +
  theme_cowplot(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black"))

heatmap_virulence