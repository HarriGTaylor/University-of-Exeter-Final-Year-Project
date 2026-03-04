#what does branch distance show - substitutions per site

library(ggtree)
library(ape)
library(treeio)
library(ggplot2)
library(tidyverse)

setwd("~/uni/Year 4/Dissertation/Lab Work/ST131 Strains/phylogenetic tree")

#importing phylogenetic tree data
treedata<-read.iqtree("core_gene_alignment_named.CONTREE")
tree_data<-as.phylo(treedata)

#assigning groups to the strains which are susceptible and not susceptible to CPL1301 and creating a data frame containing the data
strains<-c("HVM2044", "HVR83", "S79EC","S125EC", "S101EC", "S113EC", "S24EC", "S39EC", "S65EC", "S97EC", "S112EC", "S116EC", "S129EC", "S115EC", "EC958", "S19EC", "S96EC", "S37EC", "S34EC", "S77EC","P112EC","HVM1181", "UTI-89","CFT-073","S26EC", "S2EC", "S5EC","S32EC", "S114EC", "S6EC","S21EC", "HVM1147", "S22EC", "HVM2289", "HVM52", "HVM277", "HVM3017", "HVM1299", "IR18E", "MS2481", "P56EC", "HVM5", "P50EC", "HVR2496", "HVM3189", "P146EC",  "MS2493", "HVM826", "P53EC", "P189EC", "S1EC", "S11EC", "IR68", "IR65", "IR49", "S15EC", "S12EC", "S10EC", "HVM834", "HVM1619", "B36EC", "DFI-4","UTI-I6", "DFI-13", "CFI-016-VIM-4", "CFI-140-OXA-181", "BW25113", "HVM2049", "HVM2757" )
strains_S<- c("HVM2044", "HVR83", "S79EC","S125EC", "S101EC", "S113EC", "S24EC", "S39EC", "S65EC", "S97EC", "S112EC", "S116EC", "S129EC", "S115EC", "EC958")
strains_R<-c("S19EC", "S96EC", "S37EC", "S34EC", "S77EC")
strains_O_131<-c("P112EC","HVM1181", "UTI-89","CFT-073","S26EC", "S2EC", "S5EC","S32EC", "S114EC", "S6EC","S21EC", "HVM1147", "S22EC", "HVM2289", "HVM52", "HVM277", "HVM3017", "HVM1299", "IR18E", "MS2481", "P56EC", "HVM5", "P50EC", "HVR2496", "HVM3189", "P146EC",  "MS2493", "HVM826", "P53EC", "P189EC", "S1EC", "S11EC", "IR68", "IR65", "IR49", "S15EC", "S12EC", "S10EC", "HVM834", "HVM1619", "B36EC", "HVM2049", "HVM2757")
strains_O<-c("DFI-4","UTI-I6", "DFI-13", "CFI-016-VIM-4", "CFI-140-OXA-181")
strains_n<-"BW25113"
legend_order<-c("Susceptible to CPL1301", "Not Susceptible to CPL1301", "ST131 Not Tested", "Not ST131", "Unknown")

in_data<-data.frame(Strain=unlist(strains))
in_data<-in_data%>%
  mutate(group= case_when(
    Strain %in% strains_S ~ "Susceptible to CPL1301",
    Strain %in% strains_R ~"Not Susceptible to CPL1301",
    Strain %in% strains_O ~ "Unknown",
    Strain %in% strains_n ~"Not ST131",
    Strain %in% strains_O_131 ~ "ST131 Not Tested"))

tree_rooted<-root(tree_data, outgroup=c("DFI-13", "CFI-016-VIM-4", "CFI-140-OXA-181", "BW25113"), resolve.root=TRUE)

  
#creating a tree using branch distance
dis_tree<-ggtree(tree_rooted) %<+% in_data +
  geom_treescale(x=0.0032, y=24, width=0.003, fontsize=6, linesize=1)+
    annotate("text", x=0.005, y=25, label="Tree Scale:\n \nSubsitutions per site", size=6)+
  geom_tiplab(aes(label = label, fill=group),
              size=2.7,
              geom = "label",
              label.padding = unit(0.2, "lines"),
              colour=NA)+
  scale_fill_manual(name="Susceptibility to CPL1301", values = c("Susceptible to CPL1301" = "#009E73","Not Susceptible to CPL1301" = "#D55E00","Unknown" = "white", "Not ST131" ="grey", "ST131 Not Tested" = "#97D0F2"), breaks=legend_order) +
  geom_tiplab(aes(label = label,subset = label %in% strains_O),
              size=2.7,
              color = "black") +
  geom_tiplab(aes(label = label,subset = label %in% strains_R),
              size=2.7,
              color = "white")+
  geom_tiplab(aes(label = label,subset = label %in% strains_O_131),
              size=2.7,
              color = "black") +
  geom_tiplab(aes(label = label,subset = label %in% strains_n),
              size=2.7,
              color = "black") +
  geom_tiplab(aes(label = label,subset = label %in% strains_S),
              size=2.7,
              color = "white")+
  scale_x_continuous(expand = expansion(mult = c(0.0, 0.1)))+
  theme(legend.position = c(0.32, 0.75),
        legend.text=element_text(size=18,),
        legend.title = element_text(size=18))

dis_tree
#saving the trees to a file

ggsave("Phylogenetic_tree_distance_n.png",dis_tree, width =6.2 , height =9.6 , dpi= 300 )

