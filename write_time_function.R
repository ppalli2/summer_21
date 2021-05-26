#setting working directory
setwd("/Users/pranavpalli/Desktop/summer_21")

#files
data <- read.csv(file = "./data/data_soybean.csv") #data file

#vectors
genes = data$gene

#write function based on time
write_time <- function(plant_type){
  
  #switch cases based on input
  if(plant_type == 'wild_type'){
    # wild_type plants
    specific_data <- data[, c('A1_1', 'A1_2', 'A1_3', 'B1_1', 'B1_2', 'B1_3', 'C1_1', 'C1_2', 'C1_3')]
    
  } else if(plant_type == 'wr1'){
    # wr1
    specific_data <- data[, c('A2_1', 'A2_2', 'A2_3', 'B2_1', 'B2_2', 'B2_3', 'C2_1', 'C2_2', 'C2_3')]

  } else if(plant_type == 'wr1_dgat'){
    # wr1 + dgat
    specific_data <- data[, c('A3_1', 'A3_2', 'A3_3', 'B3_1', 'B3_2', 'B3_3', 'C3_1', 'C3_2', 'C3_3')]

  } else if(plant_type == 'wr1_kasII'){
    # wr1 + kasII
    specific_data <- data[, c('A4_1', 'A4_2', 'A4_3', 'B4_1', 'B4_2', 'B4_3', 'C4_1', 'C4_2', 'C4_3')]
    
  }
  
  # rownames are genes
  rownames(specific_data) <- genes
  
  # write to files
  folder <- './data/time_function/'
  final_file <- paste(plant_type, '.csv', sep = '')
  write.csv(specific_data, file = paste(folder, final_file, sep = ''))
}

# executing function
write_time('wild_type')
write_time('wr1')
write_time('wr1_dgat')
write_time('wr1_kasII')