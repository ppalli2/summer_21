#set wd
setwd("/Users/pranavpalli/Desktop/summer_21/")

#files
data <- read.csv(file = "./data/time_function/reg_files/wr1_kasii_reg.csv") #file of the avg data to read-- last was wr1_kasii

#vectors
times <- c(1, 2, 3) #1 = R5, 2 = R5/6, 3 = R6
mat <- matrix(nrow = 0, ncol = 1) #matrix of the size of the df to append
df <- data.frame(mat) #new dataframe to store avg linear regression data

for(x in 1:nrow(data)){
  #variables
  normalized_gene_counts <- data[x, c("A4_avg", "B4_avg", "C4_avg")] #rows in each gene type-1, 2, 3, 4 for all of them
  gene_count_list <- c()
  
  for(value in normalized_gene_counts){
    gene_count_list <- append(gene_count_list, value)
  }
  
  coef <- coefficients(lm(gene_count_list ~ times))
  
  slope_half <- paste(as.numeric(coef[2]), "(R)", sep = "")
  intercept_half <- paste("+", as.numeric(coef[1]), sep = " ")
  equation <- paste(slope_half, intercept_half, sep = " ")
  
  df[x, ] <- equation
}

write.table(df, "./data/lm_results/test.csv", quote = FALSE, row.names = FALSE, col.names = FALSE) #appends rows
print("done")