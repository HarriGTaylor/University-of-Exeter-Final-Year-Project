#T TEST AND P-VALUE ADJUSTMENT CODE

library(writexl)
library(tidyverse)
library(readxl)


#THIS CODE IS FOR THE COMPLETE GROWTH CURVE VI
#importing the data and creating a long table removing unnecessary columns
setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI")


VIdata<-read_xlsx("AUC_VI_DATA.xlsx", sheet=2)
VI<-pivot_longer(VIdata, cols=c(virulence_index_1, virulence_index_2, virulence_index_3), names_to="replicate", values_to="VI")
VI_data<-VI %>% filter(Phage !="Bacteria Controls AUC")
VI_data$replicate<-NULL


#determining the strain order for the graph to be based in number of infections - same as the heatmap
VI_for_order<-VIdata%>%
  filter(Phage !="Bacteria Controls AUC")
VI_for_order<- VI_for_order%>%
  mutate(mean=rowMeans(across(c(virulence_index_1, virulence_index_2, virulence_index_3)), na.rm=TRUE))
VI_for_order<-VI_for_order%>%
  arrange(desc(mean))

Infection <- VI_for_order %>% mutate(Infection = ifelse(mean >= 0.5, "Yes", "No"))
strain_order <- Infection%>%
  group_by(Strain) %>%
  summarise(Infections = sum(Infection == "Yes")) %>%
  arrange(desc(Infections))
Infection$Strain <- factor(Infection$Strain, levels = strain_order$Strain)

#creating a new data frame for results
p_value_df <- data.frame(
  Strain = character(),
  Phage = character(),
  p_value = numeric(),
  delta_vi = numeric())

#creating a vector list of all the unique phage names
phage_list <- VI_data %>%
  select(Phage) %>%
  unique() %>%
  as.vector() %>%
  unlist() %>%
  unname()

#testing phage + bacteria combos
#dataframe will be just everything
#bacteria is a character thing of the strain of interest
#phage will be a vector of interested phages


#creating a function to perform the T Test
run_t_test <- function(input_df, bacteria) {
  
  results <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric())
  
  # Control values
  control <- input_df %>%
    filter(Strain == bacteria,
           Phage == "CPL1301 Control") %>%
    pull(VI)
  
  for (phage in phage_list) {
    
    if (phage == "CPL1301 Control") next
    
    cocktail <- input_df %>%
      filter(Strain == bacteria,
             Phage == phage) %>%
      pull(VI)
    
    test <- t.test(cocktail, control)
    
    temp_df <- data.frame(
      Strain = bacteria,
      Phage = phage,
      delta_vi = mean(cocktail) - mean(control),
      p_value = test$p.value
    )
    
    results <- rbind(results, temp_df)
  }
  
  return(results)
}

#running the t.tests for each bacterial strain and adding to the p_value_df
p_value_df <- rbind(p_value_df, 
                    HVM2044_data<-run_t_test(VI_data, "HVM2044"),
                    HVR83_data<-run_t_test(VI_data, "HVR83"),
                    S77EC_data<-run_t_test(VI_data, "S77EC"),
                    S79EC_data<-run_t_test(VI_data, "S79EC"),
                    S125EC_data<-run_t_test(VI_data, "S125EC"),
                    S101EC_data<-run_t_test(VI_data, "S101EC"),
                    S113EC_data<-run_t_test(VI_data, "S113EC"),
                    S24EC_data<-run_t_test(VI_data, "S24EC"),
                    S34EC_data<-run_t_test(VI_data, "S34EC"),
                    S37EC_data<-run_t_test(VI_data, "S37EC"),
                    S39EC_data<-run_t_test(VI_data, "S39EC"),
                    S65EC_data<-run_t_test(VI_data, "S65EC"),
                    S96EC_data<-run_t_test(VI_data, "S96EC"),
                    S97EC_data<-run_t_test(VI_data, "S97EC"),
                    S112EC_data<-run_t_test(VI_data, "S112EC"),
                    S116EC_data<-run_t_test(VI_data, "S116EC"),
                    S129EC_data<-run_t_test(VI_data, "S129EC"),
                    S115EC_data<-run_t_test(VI_data, "S115EC"),
                    S19EC_data<-run_t_test(VI_data, "S19EC"),
                    EC958_data<-run_t_test(VI_data, "EC958"))


#Benjamini-Hochberg P-value adjustment
p_value_df<- p_value_df %>%
  mutate(p_value_adj=NA)
p_value_df$p_value_adj<-p.adjust(p_value_df$p_value, method="BH")


#filters the p values to find those equal to or lower than 0.05 in the adjusted and non adjusted sets
p_value_df_sig<-data.frame(p_value_df%>%
                             filter(p_value<=0.05))

p_value_df_adj_sig<-data.frame(p_value_df%>%
                                 filter(p_value_adj<=0.05))

#creates an excel document with the p-values
write_xlsx(
  list(
    All_Data=p_value_df,
    Sig_non_adj=p_value_df_sig,
    Sig_adj=p_value_df_adj_sig),
  path="T.Test Output.xlsx")


#plotting the virulence index data with different colours to show significant difference from the CPL1301 control

stars<-p_value_df%>%
  mutate(stars= ifelse(p_value_adj <= 0.001, "p\u2264 0.001",
                       ifelse(p_value_adj <= 0.01, "p\u2264 0.01",
                              ifelse(p_value_adj <= 0.05, "p\u2264 0.05", "not significant"))))
stars$delta_vi=NULL
stars$p_value=NULL
stars$p_value_adj=NULL

VI_data<-VI_data%>%
  left_join(stars, by=c("Strain", "Phage"))

VI_plot<-function(phage){
  
  Phage_<-VI_data%>%filter(Phage==phage)
  Phage_$Strain<-factor(Phage_$Strain, levels=strain_order$Strain)
  
  plot<-ggplot()+
    geom_point(data=Phage_, aes(x=Strain, y=VI, colour=stars),position = position_jitter(width = 0.1))+
    scale_colour_manual(values=c("p\u2264 0.05"= "#009E73",
                                 "p\u2264 0.01"="#E69F00",
                                 "p\u2264 0.001"="#0072B2", 
                                 "not significant"="black"))+
    geom_hline(yintercept=0.5, linetype="dashed", colour="red", linewidth=1)+
    labs(title= paste0(phage, " and CPL1301 Virulence Index"), colour="Significance")+
    scale_y_continuous(limits = c(-0.7,1.02), breaks = c(-0.6, -0.4, -0.2, -0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
    theme_classic()+
    theme(
      plot.title = element_text(size=14, face="bold", hjust = 0.5),
      axis.text.x = element_text(size = 10, angle=45, hjust=0.9),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 9, face = "bold"),
      legend.title = element_text(size=12, face="bold"),
      axis.line = element_line(linewidth=0.8),
      axis.ticks = element_line(linewidth=0.8))
  setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI/graphs")
  ggsave(paste0(phage, ".png"), plot, width = 6,height = 4,dpi = 300)
  
  return("saved"
  )}

Control_p<-VI_data%>%filter(Phage=="CPL1301 Control")
Control_p$Strain<-factor(Control_p$Strain, levels=strain_order$Strain)

CPL1301<-ggplot()+
  geom_point(data=Control_p, aes(Strain, VI), position = position_jitter(width = 0.1))+
  labs(title= paste0("CPL1301 Only Control Virulence Index"))+
  geom_hline(yintercept=0.5, linetype="dashed", colour="red", linewidth=1)+
  scale_y_continuous(limits = c(-0.7,1.02), breaks = c(-0.6, -0.4, -0.2, -0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme_classic()+
  theme(
    plot.title = element_text(size=14, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 10, angle=45, hjust=0.9),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 9, face = "bold"),
    legend.title = element_text(size=12, face="bold"),
    axis.line = element_line(linewidth=0.8),
    axis.ticks = element_line(linewidth=0.8))
setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Complete VI/graphs")
ggsave("CPL1301 Control.png", CPL1301, width = 6,height = 4,dpi = 300)


#phage treatment plotting

VI_plot("CPL1573")
VI_plot("CPL1578")
VI_plot("CPL1579")
VI_plot("CPL1581")
VI_plot("CPL1592")
VI_plot("CPL1606")
VI_plot("CPL1613")
VI_plot("CPL1618")
VI_plot("CPL1575")




#THIS IS FOR BACTEIRAL SUPRESSION HOURS 3 TO 8
setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Bacterial Supression")


VIdata_bs<-read_xlsx("AUC_VI_DATA_BS.xlsx", sheet=2)
VI_bs<-pivot_longer(VIdata_bs, cols=c(virulence_index_1, virulence_index_2, virulence_index_3), names_to="replicate", values_to="VI")
VI_data_bs<-VI_bs %>% filter(Phage !="Controls_AUC_BS")
VI_data_bs$replicate<-NULL


#determining the strain order for the graph to be based in number of infections - same as the heatmap
VI_for_order_bs<-VIdata_bs%>%
  filter(Phage !="Bacteria Controls AUC")
VI_for_order_bs<- VI_for_order_bs%>%
  mutate(mean=rowMeans(across(c(virulence_index_1, virulence_index_2, virulence_index_3)), na.rm=TRUE))
VI_for_order_bs<-VI_for_order_bs%>%
  arrange(desc(mean))

Infection_bs <- VI_for_order_bs %>% mutate(Infection = ifelse(mean >= 0.5, "Yes", "No"))
strain_order_bs <- Infection_bs%>%
  group_by(Strain) %>%
  summarise(Infections = sum(Infection == "Yes")) %>%
  arrange(desc(Infections))
Infection_bs$Strain <- factor(Infection_bs$Strain, levels = strain_order_bs$Strain)

#creating a new data frame for results
p_value_df_bs <- data.frame(
  Strain = character(),
  Phage = character(),
  p_value = numeric(),
  delta_vi = numeric(),
  test=character())

#this is a normal two sample t test
t.test_2 <- function(input_df, bacteria) {
  
  results <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric())
  
  # Control values
  control <- input_df %>%
    filter(Strain == bacteria,
           Phage == "CPL1301 Control") %>%
    pull(VI)
  
  for (phage in phage_list) {
    
    if (phage == "CPL1301 Control") next
    
    cocktail <- input_df %>%
      filter(Strain == bacteria,
             Phage == phage) %>%
      pull(VI)
    
    test <- t.test(cocktail, control)
    
    temp_df <- data.frame(
      Strain = bacteria,
      Phage = phage,
      delta_vi = mean(cocktail) - mean(control),
      p_value = test$p.value,
      test="two-sample"
    )
    
    results <- rbind(results, temp_df)
  }
  
  return(results)
}

#this is a two sample t test unless the phage is in the list so it is a one sample t test
t.test_dif1301<-function(input_data, bacteria, cc){
  
  input_data<-input_data%>%
    filter(Strain==bacteria)
  
  control <- input_data%>%
    filter(Strain == bacteria,
           Phage == "CPL1301 Control") %>%
    pull(VI)
  
  phage_2<-setdiff(phage_list, cc)
  
  results_1 <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric(),
    test=character())
  
  results_2 <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric(),
    test=character())
  
  results <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric(),
    test=character())
  
  
  for (phage in phage_list) {
    
    if (phage == "CPL1301 Control") next
    if(phage %in% cc){
      
      cocktail_1 <- input_data %>%
        filter(Strain == bacteria,
               Phage == phage) %>%
        pull(VI)
      
      test_1 <- t.test(control, mu=1)
      
      temp_df_1 <- data.frame(
        Strain = bacteria,
        Phage = phage,
        delta_vi = mean(cocktail_1) - mean(control),
        p_value = test_1$p.value,
        test="one-sample"
      )
      
      results_1<- rbind(results_1, temp_df_1)
    }
    
    if(phage %in% phage_2){
      cocktail<- input_data %>%
        filter(Strain == bacteria,
               Phage == phage) %>%
        pull(VI)
      
      test<- t.test(cocktail, control)
      
      temp_df<- data.frame(
        Strain = bacteria,
        Phage = phage,
        delta_vi = mean(cocktail) - mean(control),
        p_value = test$p.value,
        test="two-sample"
      )
      
      results_2 <- rbind(results_2, temp_df)
    }}
    results<-rbind(results_1, results_2)
  
  
  return(results)}

#this is a one sample t test unless the phage is in the list then it will not perform a t test
t.test_nodiff_1301<-function(input_data, bacteria, cc){
  
  input_data<-input_data%>%
    filter(Strain==bacteria)
  
  control <- input_data%>%
    filter(Strain == bacteria,
           Phage == "CPL1301 Control") %>%
    pull(VI)
  
  phage_2<-setdiff(phage_list, cc)
  
  results_1 <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric(),
    test=character())
  
  results_2 <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric(),
    test=character())
  
  results <- data.frame(
    Strain = character(),
    Phage = character(),
    delta_vi = numeric(),
    p_value = numeric(),
    test=character())
  
  
  for (phage in phage_list) {
    
    if (phage == "CPL1301 Control") next
    if(phage %in% cc){
      
      cocktail_1 <- input_data %>%
        filter(Strain == bacteria,
               Phage == phage) %>%
        pull(VI)
      
      temp_df_1 <- data.frame(
        Strain = bacteria,
        Phage = phage,
        delta_vi = mean(cocktail_1) - mean(control),
        p_value = "na",
        test="no test"
      )
      
      results_1<- rbind(results_1, temp_df_1)
    }
    
    if(phage %in% phage_2){
      cocktail<- input_data %>%
        filter(Strain == bacteria,
               Phage == phage) %>%
        pull(VI)
      
      test<- t.test(cocktail, mu=1)
      
      temp_df<- data.frame(
        Strain = bacteria,
        Phage = phage,
        delta_vi = mean(cocktail) - mean(control),
        p_value = test$p.value,
        test="one-sample"
      )
      
      results_2 <- rbind(results_2, temp_df)
    }
    results<-rbind(results_1, results_2)
  }
  
  return(results)}

#for these 1301 =1 as 2 or 3 repeats were 1
c101<-c("CPL1592", "CPL1613")
C116<-c("CPL1613", "CPL1618")
C24<-c("CPL1575", "CPL1581", "CPL1606", "CPL1618")
C958<-c("CPL1573", "CPL1575", "CPL1579", "CPL1581", "CPL1592", "CPL1618")

#for these CPL1301 has variation but 2 or 3 of the cocktail VI is 1
C112<-c("CPL1578", "CPL1581", "CPL1613", "CPL1618")
C113<-c("CPL1578", "CPL1579", "CPL1581", "CPL1592", "CPL1613", "CPL1618")
C115<-c("CPL1581", "CPL1618")
C129<-c("CPL1592", "CPL1613")
C39<-c("CPL1581", "CPL1592", "CPL1618")
C65<-"CPL1592"



p_value_df_bs<-rbind(p_value_df_bs, 
                     HVM2044_data_bs<-t.test_2(VI_data_bs, "HVM2044"),
                     S77EC<-t.test_2(VI_data_bs, "S77EC"),
                     HVR83_data_bs<-t.test_2(VI_data_bs, "HVR83"),
                     S79EC_data_bs<-t.test_2(VI_data_bs, "S79EC"),
                     S125EC_data_bs<-t.test_2(VI_data_bs, "S125EC"),
                     S19EC_data_bs<-t.test_2(VI_data_bs, "S19EC"),
                     S34EC_data_bs<-t.test_2(VI_data_bs, "S34EC"),
                     S37EC_data_bs<-t.test_2(VI_data_bs, "S37EC"),
                     S97EC_data_bs<-t.test_2(VI_data_bs, "S97EC"),
                     S96EC_data_bs<-t.test_2(VI_data_bs, "S96EC"),
                     S129EC<-t.test_dif1301(VI_data_bs, "S129EC", C129),
                     S65EC<-t.test_dif1301(VI_data_bs, "S65EC", C65),
                     S112EC<-t.test_dif1301(VI_data_bs, "S112EC", C112),
                     S113EC<-t.test_dif1301(VI_data_bs, "S113EC", C113),
                     S115EC<-t.test_dif1301(VI_data_bs, "S115EC", C115),
                     S39EC<-t.test_dif1301(VI_data_bs, "S39EC", C39),
                     S101EC<-t.test_nodiff_1301(VI_data_bs, "S101EC", c101),
                     S116EC<-t.test_nodiff_1301(VI_data_bs, "S116EC", C116),
                     S24EC<-t.test_nodiff_1301(VI_data_bs, "S24EC", C24),
                     EC958<-t.test_nodiff_1301(VI_data_bs, "EC958", C958))


#Benjamini-Hochberg P-value adjustment
p_value_df_bs<- p_value_df_bs %>%
  mutate(p_value_adj=NA)
p_value_df_bs$p_value_adj<-p.adjust(p_value_df_bs$p_value, method="BH")


#filters the p values to find those equal to or lower than 0.05 in the adjusted and non adjusted sets
p_value_df_sig_bs<-data.frame(p_value_df_bs%>%
                                filter(p_value<=0.05))

p_value_df_adj_sig_bs<-data.frame(p_value_df_bs%>%
                                    filter(p_value_adj<=0.05))

#creates an excel document with the p-values
write_xlsx(
  list(
    All_Data=p_value_df_bs,
    Sig_non_adj=p_value_df_sig_bs,
    Sig_adj=p_value_df_adj_sig_bs),
  path="T.Test Output_bs.xlsx")


#just need to plot the graphs of the 3hr to 8 hr VI

stars_bs<-p_value_df_bs%>%
  mutate(stars= ifelse(p_value_adj <= 0.001, "p\u2264 0.001",
                       ifelse(p_value_adj <= 0.01, "p\u2264 0.01",
                              ifelse(p_value_adj <= 0.05, "p\u2264 0.05", "not significant"))))
stars_bs$delta_vi=NULL
stars_bs$p_value=NULL
stars_bs$p_value_adj=NULL

VI_data_bs<-VI_data_bs%>%
  left_join(stars_bs, by=c("Strain", "Phage"))

VI_plot_bs<-function(phage){
  
  Phage_<-VI_data_bs%>%filter(Phage==phage)
  Phage_$Strain<-factor(Phage_$Strain, levels=strain_order$Strain)
  
  plot<-ggplot()+
    geom_point(data=Phage_, aes(x=Strain, y=VI, colour=test),position = position_jitter(width = 0.1))+
    geom_hline(yintercept=0.5, linetype="dashed", colour="red", linewidth=1)+
    scale_colour_manual(values=c("two-sample"= "#009E73",
                                 "one-sample"="#E69F00",
                                 "no test"="black"))+
    labs(title= paste0(phage, " and CPL1301 Bacterial Supression VI"), colour="T Test")+
    scale_y_continuous(limits = c(-0.7,1.02), breaks = c(-0.6, -0.4, -0.2, -0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
    theme_classic()+
    theme(
      plot.title = element_text(size=14, face="bold", hjust = 0.5),
      axis.text.x = element_text(size = 10, angle=45, hjust=0.9),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 9, face = "bold"),
      legend.title = element_text(size=12, face="bold"),
      axis.line = element_line(linewidth=0.8),
      axis.ticks = element_line(linewidth=0.8),
      legend.position="none")
  setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Bacterial Supression/graphs")
  ggsave(paste0(phage, " bacterial_supression_no_legend.png"), plot, width = 6,height = 4,dpi = 300)
  
  return("saved")}


Control_p<-VI_data_bs%>%filter(Phage=="CPL1301 Control")
Control_p$Strain<-factor(Control_p$Strain, levels=strain_order$Strain)

CPL1301<-ggplot()+
  geom_point(data=Control_p, aes(x=Strain, y=VI), position = position_jitter(width = 0.1))+
  labs(title= paste0("CPL1301 Only Control Bacterial Supression VI"))+
  geom_hline(yintercept=0.5, linetype="dashed", colour="red", linewidth=1)+
  scale_y_continuous(limits = c(-0.7,1.02), breaks = c(-0.6, -0.4, -0.2, -0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme_classic()+
  theme(
    plot.title = element_text(size=14, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 10, angle=45, hjust=0.9),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 9, face = "bold"),
    legend.title = element_text(size=12, face="bold"),
    axis.line = element_line(linewidth=0.8),
    axis.ticks = element_line(linewidth=0.8),
    legend.position="none")
setwd("~/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/AUC and VI Data/Bacterial Supression/graphs")
ggsave("CPL1301 Control_bacterial supression_no_legend.png", CPL1301, width = 6,height = 4,dpi = 300)

VI_plot_bs("CPL1573")
VI_plot_bs("CPL1575")
VI_plot_bs("CPL1578")
VI_plot_bs("CPL1579")
VI_plot_bs("CPL1581")
VI_plot_bs("CPL1592")
VI_plot_bs("CPL1606")
VI_plot_bs("CPL1613")
VI_plot_bs("CPL1618")

