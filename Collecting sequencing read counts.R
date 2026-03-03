#obtaining read counts, average read depth, and number of contigs for sequencing files

library(Rsamtools)
library(writexl)
library(GenomicAlignments)

#setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/ST131 Strains/Harri_genomics/mappings")
setwd("D:/diss/ST131 Strain Data/mapped reads and assemblies/mappings")

seq_data<-function(bam){
  #number of reads
  reads<-countBam(bam)
  reads_no<-reads$records
  #average read depth
  bf<-BamFile(bam)
  cov<-coverage(bf)
  depth_df<-as.data.frame(cov)
  avg_depth<-mean(unlist(cov))
  #number of contigs
  header<-scanBamHeader(bam)
  contigs<- names(header[[1]]$targets)
  no_contigs<-length(contigs)
  #add together
  df<-data.frame(
    Strain= sub("\\.sorted\\.bam$", "", bam),
    Read_Counts=reads_no,
    Avg_read_depth=avg_depth,
    No_Contigs=no_contigs)
  return(df)
}

sequencing_info<-rbind(
  seq_data("HVM2044RphiP1301_1.sorted.bam"),
  seq_data("HVM2044RphiP1301_2.sorted.bam"),
  seq_data("EC958RphiP1301.sorted.bam"),
  seq_data("HVR83RphiP1301_1.sorted.bam"),
  seq_data("HVR83RphiP1301_2.sorted.bam"),
  seq_data("S115ECRphiP1301_1.sorted.bam"),
  seq_data("S115ECRphiP1301_2.sorted.bam"), 
  seq_data("S116ECRphiP1301.sorted.bam"),
  seq_data("S125ECRphiP1301_2.sorted.bam"),
  seq_data("S24ECRphiP1301_2.sorted.bam"),
  seq_data("S79ECRphiP1301.sorted.bam"),
  seq_data("S129ECRphiP1301.sorted.bam"))

setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/ST131 Strains/Harri_genomics")
write_xlsx(sequencing_info, "Sequencing Data.xlsx")
