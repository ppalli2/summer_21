# set working directory and seed
setwd("/Users/pranavpalli/Desktop/summer_21")
set.seed(2003)

# importing libraries
library(DESeq2)
library(dplyr)

# meta data setup
diff_gene_expression <- function(plant_type) {
  
  #data imports
  file = paste("./data/data_organization/DESEQ_files/", paste(plant_type, ".csv", sep = ""), sep = "")
  countData = read.csv(file, header = TRUE, row.names = 1)
  
  # meta data construction
  time = c("R5", "R5", "R5", "R5", "R5", "R5", "R5/6", "R5/6", "R5/6", # represents time for
           "R5/6", "R5/6", "R5/6", "R6", "R6", "R6", "R6", "R6", "R6") # each of the samples
  
  # genotypes--- sets up genotype for each set of comparisons
  genotype = switch(plant_type,
                    "wr1" = c("control", "control", "control", "wr1", "wr1", "wr1",
                              "control", "control", "control", "wr1", "wr1", "wr1",
                              "control", "control", "control", "wr1", "wr1", "wr1"),

                    "dgat" = c("control", "control", "control", "dgat", "dgat", "dgat",
                               "control", "control", "control", "dgat", "dgat", "dgat",
                               "control", "control", "control", "dgat", "dgat", "dgat"),

                    "kasii" = c("control", "control", "control", "kasii", "kasii", "kasii",
                                "control", "control", "control", "kasii", "kasii", "kasii",
                                "control", "control", "control", "kasii", "kasii", "kasii"),
                    )
  
  colData = as.data.frame(cbind(colnames(countData), genotype, time)) # writes colData as a dataframe
  
  # DESEQ2 object and results
  dds = DESeqDataSetFromMatrix(countData = countData,
                               colData = colData,
                               design = ~time + genotype)
  
  dds = DESeq(dds) # processes all the data including normalization
  res = results(dds) # obtains results
  res_lfc = results(dds, lfcThreshold = 0.01) # same as line 27 but excludes log2foldchange of 0
  
  # mutating dataframe
  res = as.data.frame(res) #rewriting res as dataframe
  res = mutate(res, sig = ifelse(res$padj < 0.005, "FDR < 0.005", "Not Sig")) # if adjusted p-value < 0.05, count as valuable
  res[which(abs(res$log2FoldChange) < 0.7), "sig"] = "Not Sig" # if log2FoldChange is less than 0.1, count as not-sig
  res = res[order(abs(res$log2FoldChange), decreasing = TRUE), ] # arranges log2FoldChange from greatest to lowest
  
  # eliminating useless output
  res_sig = res[res$sig == "FDR < 0.005", ] # only gets significant data
  res_sig = res_sig[complete.cases(res_sig), ] # eliminates NA rows from res
  
  # combining final names
  folder_name = "./data/deseq_results/analysis_results/"
  isolated_folder = paste(plant_type, "_isolated/", sep = "")
  file_name_reg = paste(isolated_folder, paste(plant_type, ".csv", sep = ""), sep = "")
  file_name_sig = paste(isolated_folder, paste(plant_type, "_sig.csv", sep = ""), sep = "")
  
  # writing output
  write.csv(res, paste(folder_name, file_name_reg, sep = ""))
  write.csv(res_sig, paste(folder_name, file_name_sig, sep = ""))
}

# function calls
diff_gene_expression("wr1")
diff_gene_expression("dgat")
diff_gene_expression("kasii")
