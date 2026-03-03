library(openxlsx)
library(ggplot2)
library(tidyverse)
library(readxl)

setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing")

#these are the strains which show no susceptibility to the phage so can be merged together
c1301<-c("S34EC", "S37EC", "S96EC", "S77EC", "S19EC")
c1573<-c("S34EC", "S37EC", "S77EC", "S19EC")
c1575<-c("S34EC", "S37EC", "S96EC", "S77EC", "S19EC")
c1578<-c("S34EC", "S37EC", "S19EC")
c1579<-c("S34EC", "S37EC", "S19EC")
c1581<-c("S34EC", "S37EC")
c1592<-c("S34EC", "S37EC", "S96EC", "S77EC", "S19EC")
c1606<-c("S34EC", "S37EC")
c1613<-c("S34EC", "S37EC")
c1618<-c("S34EC", "S37EC","S77EC")

#first 130 lines ish of code are to create the CPL1301 control graph

CPL1301<-read.xlsx("CPL1301.xlsx", sheet=1, rows=2:98, cols=2:27)

colnames(CPL1301)[colnames(CPL1301)=="Time"]<-"Strain"
#this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
od_reading<-CPL1301%>%
  select(where(is.numeric))%>%
  names()

blk<-CPL1301 %>%
  filter(Strain=="Blank")%>%
  select(all_of(od_reading)) %>%
  unlist()

clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
mean_blk<-mean(clean_blk, na.rm=TRUE)

import_data_blk<-CPL1301 %>%
  mutate(across(all_of(od_reading), ~ pmax(.x - mean_blk, 0)))
import_data_blk<-import_data_blk %>%
  filter(Strain !="Blank")

#this adds the replicate column and reformats the data into long format
import_data_blk<- import_data_blk%>%
  group_by(Strain)%>%
  mutate(Replicate=row_number()) %>%
  ungroup()

repeats_long<- pivot_longer(import_data_blk,
                            cols= -c(Replicate, Strain),
                            names_to = "Hours",
                            values_to = "OD")

repeats_long$Hours <- as.numeric(gsub("\\.h", "", repeats_long$Hours))
repeats_long$OD <- as.numeric(unlist(repeats_long$OD))
repeats_long$Strain <- as.character(repeats_long$Strain)
repeats_long$OD <- pmax(repeats_long$OD, 0)


#this bit calculates all the mean OD values
mean_data_total <- repeats_long %>%
  group_by(Strain, Hours) %>%
  summarise(
    Mean_OD = mean(OD, na.rm = TRUE),
    .groups = "drop")

#separates out and calculates the mean of strains determined to be not susceptible to the phage
no_activity_avg<-mean_data_total%>%
  filter(Strain %in% c1301)%>%
  group_by(Hours)%>%
  summarise(Mean_OD= mean(Mean_OD,na.rm=TRUE), .groups ="drop")

#creates the mean of all phage with a final OD of less than or equal to 0.1 so the phage is highly active
high_activity_strains<-mean_data_total%>%
  filter(Hours==24, Mean_OD <=0.1)%>%
  pull(Strain)

high_activity_avg<-mean_data_total%>%
  filter(Strain %in% high_activity_strains)%>%
  group_by(Hours)%>%
  summarise(Mean_OD=mean(Mean_OD, na.rm=TRUE), .groups="drop")

#combines the two lists of phage and creates a dataframe of all the other phage
high_low_phage<-c(c1301, high_activity_strains)

mid_activity_phage<-mean_data_total%>%
  filter(!Strain %in% high_low_phage)

#adds a strain name
high_activity_avg<- high_activity_avg%>%
  mutate(Strain="Highly Active")

no_activity_avg<-no_activity_avg%>%
  mutate(Strain="Not Susceptible")

#merges all the mean values back into one data frame
mean_activity<-data.frame(
  Strain=character(), 
  Hours=numeric(), 
  Mean_OD=numeric())

mean_activity<-rbind(mean_activity, high_activity_avg, no_activity_avg, mid_activity_phage)
mean_activity<-mean_activity%>%
  filter(mean_activity$Strain !="Highly Active")

strain_colours<-c("Not Susceptible" = "black", 
           "HVM2044" = "#6AD6B9", "EC958" = "#56B4E9",
           "HVR83" = "#0072B2", "S101EC" = "#3E22F4",
           "S112EC" = "#4B00B9", "S113EC" = "#CC79A7",
           "S115EC" = "#C7217D", "S116EC" = "#D55E00", 
           "S129EC" = "#E69F00", "S24EC" = "#F0E442", 
           "S39EC" = "#407717", "S65EC" = "#A1E071",
           "S77EC" = "#821350", "S79EC" = "#79948D", 
           "S96EC" = "#AD87E6", "S19EC" = "#8D4DEB")

caption<-paste0(
  "Phage not susceptible to: ", paste(c1301, collapse = ", "), "\n",
  "Phage highly active against: ", paste(high_activity_strains, collapse = ", "))

CP1301<-ggplot(mean_activity, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
  geom_line(size = 1) +  
  labs(
    title = "CPL1301",
    x = "Time (hours)",
    y = "OD600",
    caption=caption) +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
  scale_color_manual(values = strain_colours)+
  theme_classic()+
  theme(
    plot.caption = element_text(hjust = 0.5,size = 10,face = "italic",color = "black",margin = margin(t = 10)),
    plot.title = element_text(size=14, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size=12, face="bold"),
    axis.line = element_line(linewidth=0.8),
    axis.ticks = element_line(linewidth=0.8))


setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs/summary")
ggsave("CPL1301 Control.png",CP1301, width = 10, height = 6, dpi = 300)

#bacteria only controls summary growth curve
controls<-read.xlsx("Controls.xlsx", sheet=1, rows=2:98, cols=2:27)

colnames(controls)[colnames(controls)=="Time"]<-"Strain"
#this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
od_reading_cont<-controls%>%
  select(where(is.numeric))%>%
  names()

blk_c<-controls %>%
  filter(Strain=="Blank")%>%
  select(all_of(od_reading_cont)) %>%
  unlist()

clean_blk_c<-blk_c[abs(blk_c-median(blk_c, na.rm=TRUE))<=5*mad(blk_c, na.rm=TRUE)]
mean_blk_c<-mean(clean_blk_c, na.rm=TRUE)

import_data_blk_c<-controls %>%
  mutate(across(all_of(od_reading_cont), ~ pmax(.x - mean_blk_c, 0)))
import_data_blk_c<-import_data_blk_c %>%
  filter(Strain !="Blank")

#this adds the replicate column and reformats the data into long format
import_data_blk_c<- import_data_blk_c%>%
  group_by(Strain)%>%
  mutate(Replicate=row_number()) %>%
  ungroup()

controls_long<- pivot_longer(import_data_blk_c,
                            cols= -c(Replicate, Strain),
                            names_to = "Hours",
                            values_to = "OD")

controls_long$Hours <- as.numeric(gsub("\\.h", "", controls_long$Hours))
controls_long$OD <- as.numeric(unlist(controls_long$OD))
controls_long$Strain <- as.character(controls_long$Strain)
controls_long$OD <- pmax(controls_long$OD, 0)

mean_data_total_c <- controls_long %>%
  group_by(Strain, Hours) %>%
  summarise(
    Mean_OD = mean(OD, na.rm = TRUE),
    .groups = "drop")

strain_colours_cont<-c(
                  "HVM2044" = "#6AD6B9", "EC958" = "#56B4E9",
                  "HVR83" = "#0072B2", "S101EC" = "#3E22F4",
                  "S112EC" = "#4B00B9", "S113EC" = "#CC79A7",
                  "S115EC" = "#C7217D", "S116EC" = "#D55E00", 
                  "S129EC" = "#E69F00", "S24EC" = "#F0E442", 
                  "S39EC" = "#407717", "S65EC" = "#A1E071",
                  "S77EC" = "#821350", "S79EC" = "#79948D", 
                  "S96EC" = "#AD87E6", "S19EC" = "#8D4DEB",
                  "S125EC" = "red", "S34EC" = "sienna4", 
                  "S37EC" = "sandybrown","S97EC" = "grey37")

control<-ggplot(mean_data_total_c, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
  geom_line(size = 1) +  
  labs(
    title = "Bacteria Only Control",
    x = "Time (hours)",
    y = "OD600") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
  scale_color_manual(values = strain_colours_cont)+
  theme_classic()+
  theme(
    plot.title = element_text(size=14, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size=12, face="bold"),
    axis.line = element_line(linewidth=0.8),
    axis.ticks = element_line(linewidth=0.8))

setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs/summary")
ggsave("Bacteria Control.png",control, width = 10, height = 6, dpi = 300)

#this is a function to create the summary graphs for all the phage cocktails

summary_plot<-function(file_path, no_active_phage){
  
  
  CPL<-read.xlsx(file_path, sheet=1, rows=2:98, cols=2:27)
  file_name<-tools::file_path_sans_ext(basename(file_path))
  output_file_name_image<-paste0(file_name,".png")
  
  colnames(CPL)[colnames(CPL)=="Time"]<-"Strain"
  
  
  #this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
  od_reading<-CPL%>%
    select(where(is.numeric))%>%
    names()
  
  blk<-CPL %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
    unlist()
  
  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)
  
  import_data_blk<-CPL %>%
    mutate(across(all_of(od_reading), ~ pmax(.x - mean_blk, 0)))
  import_data_blk<-import_data_blk %>%
    filter(Strain !="Blank")
  
  #this adds the replicate column and reformats the data into long format
  import_data_blk<- import_data_blk%>%
    group_by(Strain)%>%
    mutate(Replicate=row_number()) %>%
    ungroup()
  
  repeats_long<- pivot_longer(import_data_blk,
                              cols= -c(Replicate, Strain),
                              names_to = "Hours",
                              values_to = "OD")
  
  repeats_long$Hours <- as.numeric(gsub("\\.h", "", repeats_long$Hours))
  repeats_long$OD <- as.numeric(unlist(repeats_long$OD))
  repeats_long$Strain <- as.character(repeats_long$Strain)
  repeats_long$OD <- pmax(repeats_long$OD, 0)
  
  
  #this bit calculates all the mean OD values
  mean_data_total <- repeats_long %>%
    group_by(Strain, Hours) %>%
    summarise(
      Mean_OD = mean(OD, na.rm = TRUE),
      .groups = "drop")
  
  #separates out and calculates the mean of strains determined to be not susceptible to the phage
  no_activity_avg<-mean_data_total%>%
    filter(Strain %in% no_active_phage)%>%
    group_by(Hours)%>%
    summarise(Mean_OD= mean(Mean_OD,na.rm=TRUE), .groups ="drop")
  
  #creates the mean of all phage with a final OD of less than or equal to 0.1 so the phage is highly active
  high_activity_strains<-mean_data_total%>%
    filter(Hours==24, Mean_OD <=0.1)%>%
    pull(Strain)
  
  high_activity_avg<-mean_data_total%>%
    filter(Strain %in% high_activity_strains)%>%
    group_by(Hours)%>%
    summarise(Mean_OD=mean(Mean_OD, na.rm=TRUE), .groups="drop")
  
  #combines the two lists of phage and creates a dataframe of all the other phage
  high_low_phage<-c(no_active_phage, high_activity_strains)
  
  mid_activity_phage<-mean_data_total%>%
    filter(!Strain %in% high_low_phage)
  
  #adds a strain name
  high_activity_avg<- high_activity_avg%>%
    mutate(Strain="Highly Active")
  
  no_activity_avg<-no_activity_avg%>%
    mutate(Strain="Not Susceptible")
  
  #merges all the mean values back into one data frame
  mean_activity<-data.frame(
    Strain=character(), 
    Hours=numeric(), 
    Mean_OD=numeric())
  
  mean_activity<-rbind(mean_activity, high_activity_avg, no_activity_avg, mid_activity_phage)
  mean_activity<-mean_activity%>%
    filter(mean_activity$Strain !="Highly Active")
  
  strain_colours<-c("Not Susceptible" = "black", 
                    "HVM2044" = "#6AD6B9", "EC958" = "#56B4E9",
                    "HVR83" = "#0072B2", "S101EC" = "#3E22F4",
                    "S112EC" = "#4B00B9", "S113EC" = "#CC79A7",
                    "S115EC" = "#C7217D", "S116EC" = "#D55E00", 
                    "S129EC" = "#E69F00", "S24EC" = "#F0E442", 
                    "S39EC" = "#407717", "S65EC" = "#A1E071",
                    "S77EC" = "#821350", "S79EC" = "#79948D", 
                    "S96EC" = "#AD87E6", "S19EC" = "#8D4DEB")
  
  caption<-paste0(
    "Phage not susceptible to: ", paste(no_active_phage, collapse = ", "), "\n",
    "Phage highly active against: ", paste(high_activity_strains, collapse = ", "))
  
  plot<-ggplot(mean_activity, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
    geom_line(size = 1) +  
    labs(
      title = paste0(file_name, " and CPL1301"),
      x = "Time (hours)",
      y = "OD600",
      caption=caption) +
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
    scale_color_manual(values = strain_colours)+
    theme_classic()+
    theme(
      plot.caption = element_text(hjust = 0.5,size = 10,face = "italic",color = "black",margin = margin(t = 10)),
      plot.title = element_text(size=14, face="bold", hjust = 0.5),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.title = element_text(size=12, face="bold"),
      axis.line = element_line(linewidth=0.8),
      axis.ticks = element_line(linewidth=0.8))
  
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs/summary")
  ggsave(output_file_name_image,plot, width = 10, height = 6, dpi = 300)
  
  return("saved")}

summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1573.xlsx", c1573)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1575.xlsx",c1575)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1578.xlsx",c1578)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1579.xlsx",c1579)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1581.xlsx",c1581)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1592.xlsx",c1592)

#these three have no data at 1hr due to clariostar error
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1606.xlsx",c1606)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1613.xlsx",c1613)
summary_plot("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1618.xlsx",c1618)

