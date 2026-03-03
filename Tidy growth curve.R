#producing growthcuvres and AUC_VI data using functions


library(openxlsx)
library(growthcurver)
library(ggplot2)
library(tidyverse)
library(readxl)

#this code automates producing growth curve graphs

plot_gc<-function(file_path){
  
  data<-read.xlsx(file_path, sheet=1, rows=2:98, cols=2:27)
  file_name<-tools::file_path_sans_ext(basename(file_path))
  output_file_name_image<-paste0(file_name,".png")

#this changes the Time column to be called Strain
   colnames(data)[colnames(data)=="Time"]<-"Strain"
#colnames(bacteria_controls)[colnames(bacteria_controls)=="Time"]<-"Strain"

#this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
  od_reading<-data%>%
   select(where(is.numeric))%>%
   names()

  blk<-data %>%
   filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
  unlist()

  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)

  import_data_blk<-data %>%
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

  strain_colours<- c("HVM2044" ="red3","HVR83" = "orangered1","S77EC" ="orange","S79EC" = "darkorange3","S125EC" = "yellow","S101EC" ="gold","S113EC" ="darkviolet","S24EC" ="purple4","S34EC" ="hotpink","S37EC" ="maroon","S39EC" = "sienna4","S65EC" ="cyan","S96EC" = "green","S97EC" ="darkgreen","S112EC" ="limegreen","S116EC" ="forestgreen","S129EC" ="blue","S115EC" ="skyblue","S19EC" ="darkblue","EC958" ="slateblue")
  
  repeats_mean <- repeats_long %>%
    group_by(Strain, Hours) %>%
    summarise(
      Mean_OD = mean(OD),
      SD_OD = sd(OD),
      .groups = "drop")
  
  plot<-ggplot(repeats_mean, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
    geom_line(aes(y = Mean_OD, color = Strain), size = 1) +  
    geom_ribbon(aes(ymin = pmax(Mean_OD - SD_OD,0), ymax = Mean_OD + SD_OD), alpha = 0.2, color = NA, size=0.1)+
    
    labs(
      title = paste0(file_name, " and CPL1301"),
      x = "Time (hours)",
      y = "OD600") +
    
    scale_fill_manual (values=strain_colours)+
    scale_color_manual(values = strain_colours)+
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
    
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
  
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs")
  ggsave(output_file_name_image,plot, width = 10, height = 6, dpi = 300)
return("saved")}#this is the main function

plot_pha_control<-function(file_path){
  data<-read.xlsx(file_path, sheet=4, rows=2:98, cols=2:27)
  output_file_name_image<-("CPL1301 Only COntrol")
  
  #this changes the Time column to be called Strain
  colnames(data)[colnames(data)=="Time"]<-"Strain"
  #colnames(bacteria_controls)[colnames(bacteria_controls)=="Time"]<-"Strain"
  
  #this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
  od_reading<-data%>%
    select(where(is.numeric))%>%
    names()
  
  blk<-data %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
    unlist()
  
  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)
  
  import_data_blk<-data %>%
    mutate(across(all_of(od_reading), ~ .x - mean_blk))
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
  
  strain_colours<- c("HVM2044" ="red3","HVR83" = "orangered1","S77EC" ="orange","S79EC" = "darkorange3","S125EC" = "yellow","S101EC" ="gold","S113EC" ="darkviolet","S24EC" ="purple4","S34EC" ="hotpink","S37EC" ="maroon","S39EC" = "sienna4","S65EC" ="cyan","S96EC" = "green","S97EC" ="darkgreen","S112EC" ="limegreen","S116EC" ="forestgreen","S129EC" ="blue","S115EC" ="skyblue","S19EC" ="darkblue","EC958" ="slateblue")
  
  repeats_mean <- repeats_long %>%
    group_by(Strain, Hours) %>%
    summarise(
      Mean_OD = mean(OD),
      SD_OD = sd(OD),
      .groups = "drop")
  
  plot<-ggplot(repeats_mean, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
    geom_line(aes(y = Mean_OD, color = Strain), size = 1) +  
    geom_ribbon(aes(ymin = pmax(Mean_OD - SD_OD,0), ymax = Mean_OD + SD_OD), alpha = 0.2, color = NA, size=0.1)+
    
    labs(
      title = "CPL1301 Control",
      x = "Time (hours)",
      y = "OD600") +
    
    scale_fill_manual (values=strain_colours)+
    scale_color_manual(values = strain_colours)+
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
    
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
  
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs")
  ggsave("CPL1301 Control.png", plot, width = 10, height = 6, dpi = 300)

return("saved")}#this creates graphs for the CPL1301 control

plot_control<-function(file_path){
  data<-read.xlsx(file_path, sheet=1, rows=2:98, cols=2:27)
  
  output_file_name_image<-("Control")
  
  #this changes the Time column to be called Strain
  colnames(data)[colnames(data)=="Time"]<-"Strain"
  #colnames(bacteria_controls)[colnames(bacteria_controls)=="Time"]<-"Strain"
  
  #this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
  od_reading<-data%>%
    select(where(is.numeric))%>%
    names()
  
  blk<-data %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
    unlist()
  
  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)
  
  import_data_blk<-data %>%
    mutate(across(all_of(od_reading), ~ .x - mean_blk))
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
  
  strain_colours<- c("HVM2044" ="red3","HVR83" = "orangered1","S77EC" ="orange","S79EC" = "darkorange3","S125EC" = "yellow","S101EC" ="gold","S113EC" ="darkviolet","S24EC" ="purple4","S34EC" ="hotpink","S37EC" ="maroon","S39EC" = "sienna4","S65EC" ="cyan","S96EC" = "green","S97EC" ="darkgreen","S112EC" ="limegreen","S116EC" ="forestgreen","S129EC" ="blue","S115EC" ="skyblue","S19EC" ="darkblue","EC958" ="slateblue")
  
  repeats_mean <- repeats_long %>%
    group_by(Strain, Hours) %>%
    summarise(
      Mean_OD = mean(OD),
      SD_OD = sd(OD),
      .groups = "drop")
  
  plot<-ggplot(repeats_mean, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
    geom_line(aes(y = Mean_OD, color = Strain), size = 1) +  
    geom_ribbon(aes(ymin = pmax(Mean_OD - SD_OD,0), ymax = Mean_OD + SD_OD), alpha = 0.2, color = NA, size=0.1)+
    
    labs(
      title = "Bacteria Only Control",
      x = "Time (hours)",
      y = "OD600") +
    
    scale_fill_manual (values=strain_colours)+
    scale_color_manual(values = strain_colours)+
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
    
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
  
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs")
  ggsave("Controls.png", plot, width = 10, height = 6, dpi = 300)
  
  return("saved")} #this creates growthcurves for the bacteria only control

plot_gc_h1h<-function(file_path){
  
  data<-read.xlsx(file_path, sheet=1, rows=2:98, cols=2:27)
  file_name<-tools::file_path_sans_ext(basename(file_path))
  output_file_name_image<-paste0(file_name," h.png")
  
  #this changes the Time column to be called Strain
  colnames(data)[colnames(data)=="Time"]<-"Strain"
  #colnames(bacteria_controls)[colnames(bacteria_controls)=="Time"]<-"Strain"
  
  #this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
  od_reading<-data%>%
    select(where(is.numeric))%>%
    names()
  
  blk<-data %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
    unlist()
  
  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)
  
  import_data_blk<-data %>%
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
  
  strain_colours<- c("HVM2044" ="red3","HVR83" = "orangered1","S77EC" ="orange","S79EC" = "darkorange3","S125EC" = "yellow","S101EC" ="gold","S113EC" ="darkviolet","S24EC" ="purple4","S34EC" ="hotpink","S37EC" ="maroon","S39EC" = "sienna4","S65EC" ="cyan","S96EC" = "green","S97EC" ="darkgreen","S112EC" ="limegreen","S116EC" ="forestgreen","S129EC" ="blue","S115EC" ="skyblue","S19EC" ="darkblue","EC958" ="slateblue")
  
  repeats_mean <- repeats_long %>%
    group_by(Strain, Hours) %>%
    summarise(
      Mean_OD = mean(OD),
      SD_OD = sd(OD),
      .groups = "drop")
  
  plot<-ggplot(repeats_mean, aes(x = Hours, y = Mean_OD, color = Strain, fill = Strain, group = Strain)) +
    geom_line(aes(y = Mean_OD, color = Strain), size = 1) +  
    geom_ribbon(aes(ymin = pmax(Mean_OD - SD_OD,0), ymax = Mean_OD + SD_OD), alpha = 0.2, color = NA, size=0.1)+
    
    labs(
      title = paste0(file_name, " and CPL1301"),
      x = "Time (hours)",
      y = "OD600") +
    
    scale_fill_manual (values=strain_colours)+
    scale_color_manual(values = strain_colours)+
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.00,3.4), breaks = seq(0.00, 3.4, 0.2), expand = c(0, 0)) +
    
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
  
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs")
  ggsave(output_file_name_image,plot, width = 10, height = 6, dpi = 300)
  return("saved")} #this creates a growth curve with a larger y axis

plot_control("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/Controls.xlsx")
plot_pha_control("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/Controls.xlsx")

plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1573.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1575.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1578.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1579.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1581.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1592.xlsx")

#these three have no data at 1hr due to clariostar error
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1606.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1613.xlsx")
plot_gc("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1618.xlsx")

plot_gc_h1h("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1592.xlsx")


#calculating Area Under the Curve and Virulence Index
AUC_Control<- function(file_path){
  data<-read.xlsx(file_path, sheet=2, rows=2:98, cols=2:24)
  file_name<-tools::file_path_sans_ext(basename(file_path))
  output_file_name_data<-paste0(file_name,"_AUC.xlsx")
  colnames(data)[colnames(data)=="Time"]<-"Strain"
  
  od_reading<-data%>%
    select(where(is.numeric))%>%
    names()
  
  blk<-data %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
    unlist()
  
  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)
  
  import_data_blk<-data %>%
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
  
  repeats_long <- repeats_long %>%
    mutate(
      Hours  = as.numeric(Hours),
      OD     = as.numeric(OD),
      OD     = pmax(OD, 0)          
    ) %>%
    filter(!is.na(Hours), !is.na(OD))
  
  #this creates all the data
  growth_fits <- repeats_long %>%
    group_by(Strain, Replicate) %>%
    arrange(Hours) %>%
    nest() %>%
    mutate(
      gc = map(data, ~ SummarizeGrowth(
        .x$Hours,
        .x$OD)))
  
  growth_fits <- growth_fits %>%
    filter(!map_lgl(gc, is.null))
  
  #this section creates and reports AUC, avg AUC, VI, and avg VI data
  auc_results <- growth_fits %>%
    mutate(AUC_e = map_dbl(gc, ~ .x$vals$auc_e)) %>%  # extract auc_e from nested vals
    select(Strain, Replicate, AUC_e)
  
  Export<-auc_results%>%
    pivot_wider(
      id_cols=Strain,
      names_from=Replicate,
      values_from=c(AUC_e),
      names_glue="{.value}_{Replicate}")
  Export<-dplyr::rename(Export,
      auc_results_1=AUC_e_1,
      auc_results_2=AUC_e_2,
      auc_results_3=AUC_e_3)
  
  wb<- createWorkbook()
  addWorksheet(wb, "AUC")
  writeData(wb,sheet="AUC", x=Export )
 
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI") 
  saveWorkbook(wb, output_file_name_data, overwrite=TRUE)
  return("saved")

}

VI_AUC<-function(file_path){

  #this this repeats what was in the growth curve script to format all the data
  data<-read.xlsx(file_path, sheet=2, rows=2:98, cols=2:24)
  file_name<-tools::file_path_sans_ext(basename(file_path))
  output_file_name_data<-paste0(file_name,"_AUC_VI.xlsx")
  
  bacteria_controls<-read.xlsx("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/Controls.xlsx", sheet=2, rows=2:98, cols=2:24)
  
  #this changes the Time column to be called Strain
  colnames(data)[colnames(data)=="Time"]<-"Strain"
  colnames(bacteria_controls)[colnames(bacteria_controls)=="Time"]<-"Strain"
  
  #this creates a mean blank, gets rid of any outliers, and then blank adjusts all the data
  od_reading<-data%>%
    select(where(is.numeric))%>%
    names()
  
  blk<-data %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading)) %>%
    unlist()
  
  clean_blk<-blk[abs(blk-median(blk, na.rm=TRUE))<=5*mad(blk, na.rm=TRUE)]
  mean_blk<-mean(clean_blk, na.rm=TRUE)
  od_reading_c<-bacteria_controls%>%
    select(where(is.numeric))%>%
    names()
  
  blk_c<-bacteria_controls %>%
    filter(Strain=="Blank")%>%
    select(all_of(od_reading_c)) %>%
    unlist()
  
  clean_blk_c<-blk_c[abs(blk_c-median(blk_c, na.rm=TRUE))<=5*mad(blk_c, na.rm=TRUE)]
  mean_blk_c<-mean(clean_blk_c, na.rm=TRUE)
  
bacteria_controls_blk<-bacteria_controls %>%
    mutate(across(all_of(od_reading_c), ~ .x - mean_blk_c))
  bacteria_controls_blk<-bacteria_controls_blk %>%
    filter(Strain !="Blank") 
  
  bacteria_controls_blk<- bacteria_controls_blk%>%
    group_by(Strain)%>%
    mutate(Replicate=row_number()) %>%
    ungroup()
  control_long<-pivot_longer(bacteria_controls_blk,
                          cols= -c(Replicate, Strain),
                          names_to = "Hours",
                          values_to = "OD")
  
  import_data_blk<-data %>%
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
  
  control_long<-pivot_longer(bacteria_controls_blk,
                             cols= -c(Replicate, Strain),
                             names_to = "Hours",
                             values_to = "OD")
  
  repeats_long$Hours <- as.numeric(gsub("\\.h", "", repeats_long$Hours))
  control_long$Hours <-as.numeric(gsub("\\.h","",control_long$Hours))
  repeats_long$OD <- as.numeric(unlist(repeats_long$OD))
  repeats_long$Strain <- as.character(repeats_long$Strain)
  repeats_long$OD <- pmax(repeats_long$OD, 0)
  
  control_long <- control_long %>%
    mutate(
      Hours  = as.numeric(Hours),
      OD     = as.numeric(OD),
      OD     = pmax(OD, 0)) %>%
    filter(!is.na(Hours), !is.na(OD))
  
  control_fits <- control_long %>%
    group_by(Strain, Replicate) %>%
    arrange(Hours) %>%
    nest() %>%
    mutate(
      gc = map(data, ~ SummarizeGrowth(
        .x$Hours,
        .x$OD )))
  
  control_fits <- control_fits %>%
    filter(!map_lgl(gc, is.null))
  
  auc_results_controls <- control_fits %>%
    mutate(AUC_e = map_dbl(gc, ~ .x$vals$auc_e)) %>%
    select(Strain, Replicate, AUC_e)
  
  
  #this is the confusing growthcurver script for imported phage
  repeats_long <- repeats_long %>%
    mutate(
      Hours  = as.numeric(Hours),
      OD     = as.numeric(OD),
      OD     = pmax(OD, 0)          
    ) %>%
    filter(!is.na(Hours), !is.na(OD))
  
  #this creates all the data
  growth_fits <- repeats_long %>%
    group_by(Strain, Replicate) %>%
    arrange(Hours) %>%
    nest() %>%
    mutate(
      gc = map(data, ~ SummarizeGrowth(
        .x$Hours,
        .x$OD)))
  
  growth_fits <- growth_fits %>%
    filter(!map_lgl(gc, is.null))
  
  #this section creates and reports AUC, avg AUC, VI, and avg VI data
  auc_results <- growth_fits %>%
    mutate(AUC_e = map_dbl(gc, ~ .x$vals$auc_e)) %>%  # extract auc_e from nested vals
    select(Strain, Replicate, AUC_e)
  
  
  avg_auc_phage <- auc_results %>%
    group_by(Strain) %>%
    summarise(
      avg_auc_e = mean(AUC_e, na.rm = TRUE))
  
  avg_auc_control <- auc_results_controls %>%
    group_by(Strain) %>%
    summarise(
      avg_auc_e = mean(AUC_e, na.rm = TRUE))
  
  
  virulence_index_results <- dplyr::rename(avg_auc_control,avg_auc_control = avg_auc_e) %>%
    inner_join(dplyr::rename(avg_auc_phage,avg_auc_phage = avg_auc_e),
               by = c("Strain")) %>%
    mutate(virulence_index = 1 - (avg_auc_phage / avg_auc_control))
  
  
  Replicate_VI<-dplyr::rename(auc_results_controls,auc_results_controls = AUC_e)%>%
    inner_join(dplyr::rename(auc_results,auc_results = AUC_e),
               by = c("Strain", "Replicate")) %>%
    mutate(virulence_index = 1 - (auc_results /auc_results_controls))
  
  setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI")
  
  Export<-Replicate_VI%>%
    pivot_wider(
      id_cols=Strain,
      names_from=Replicate,
      values_from=c(auc_results, virulence_index),
      names_glue="{.value}_{Replicate}")
  
  wb<- createWorkbook()
  addWorksheet(wb, "AUC_VI")
  writeData(wb,sheet="AUC_VI", x=Export )
  
  saveWorkbook(wb, output_file_name_data, overwrite=TRUE)
  
  return("saved")}

AUC_Control("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/Controls.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1301.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1573.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1575.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1578.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1579.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1581.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1592.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1606.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1613.xlsx")
VI_AUC("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/CPL1618.xlsx")


#this bit of code combines all the AUC and VI data into one file

data_location <- "C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI"
files <- list.files(data_location, pattern = "\\.xlsx$", full.names = TRUE)
files <- list.files(data_location, pattern = "^[^~].*\\.xlsx$",full.names = TRUE)

wb <- createWorkbook()
addWorksheet(wb, sheetName = "AUC")
addWorksheet(wb, sheetName="VI")

AUC_cols <- c("Strain", "auc_results_1", "auc_results_2", "auc_results_3")
VI_cols<-c("Strain", "virulence_index_1", "virulence_index_2", "virulence_index_3")

AUC_Data <- lapply(files, function(file) {
  auc_data <- read_xlsx(file, sheet = 1) %>%    
    select(any_of(AUC_cols))            
  auc_data$Phage <- str_replace(tools::file_path_sans_ext(basename(file)), "_AUC_VI", "")
  return(auc_data)}) %>% bind_rows()

VI_Data <- lapply(files, function(file) {
  vi_data <- read_xlsx(file, sheet = 1) %>%    
    select(any_of(VI_cols))           
  vi_data$Phage <- str_replace(tools::file_path_sans_ext(basename(file)), "_AUC_VI", "")
  return(vi_data)}) %>% bind_rows()


writeData(wb, sheet = "AUC", AUC_Data)
writeData(wb, sheet="VI", VI_Data)
saveWorkbook(wb, "AUC_VI_DATA.xlsx")

