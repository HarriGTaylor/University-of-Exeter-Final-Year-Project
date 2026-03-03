#fasta extraction from bam files

library(Biostrings)
library(Rsamtools)


setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/ST131 Strains/Harri_genomics/mappings")

# Read Excel mapping
names <- read_xlsx("bam to fasta names.xlsx")

bam_folder <- "C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/ST131 Strains/Harri_genomics/mappings"
output_folder <- "C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/ST131 Strains/fasta files"
dir.create(output_folder, showWarnings = FALSE)

# Check column names
if(!all(c("BAM_name", "FASTA_name") %in% colnames(names))){
  stop("Excel file must contain columns BAM_name and FASTA_name")
}

# Loop over all BAM files
for(i in seq_len(nrow(names))){
  
  bam_file_name <- names$BAM_name[i]
  fasta_name <- names$FASTA_name[i]
  
  bam_path <- file.path(bam_folder, bam_file_name)
  
  if(!file.exists(bam_path)){
    warning(paste("BAM file not found:", bam_file_name))
    next
  }
  
  # Open BAM file
  bam <- BamFile(bam_path)
  
  # Scan BAM for mapped sequences
  param <- ScanBamParam(what = c("qname", "seq"), flag = scanBamFlag(isUnmappedQuery = FALSE))
  bam_data <- scanBam(bam, param = param)
  
  # Extract sequences and names
  sequences <- bam_data[[1]]$seq
  names(sequences) <- bam_data[[1]]$qname
  
  # Convert to DNAStringSet
  dna <- DNAStringSet(sequences)
  
  # Output FASTA path
  fasta_path <- file.path(output_folder, paste0(fasta_name, ".fasta"))
  
  # Write FASTA
  writeXStringSet(dna, filepath = fasta_path, format = "fasta")
  
  message(paste("Processed BAM:", bam_file_name, "→ FASTA:", fasta_path))
}
