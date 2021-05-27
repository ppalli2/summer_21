#setting working directory
setwd("/Users/pranavpalli/Desktop/summer_21")

#files
data <- read.csv(file = "./data/data_soybean.csv") #data file

#vectors
plant_times <- c("A", "B", "C") #vector of plants at R5, 5/6, 6 times
cols <- c() #vector of all column names
genes <- data$genes #genes column from data

#write function based on time
write_time <- function(plant_type){
  
  # plant_number based on plant_type for cols
  plant_number = switch (plant_type,
    "wild_type" = 1,
    "wr1" = 2,
    "wr1_dgat" = 3,
    "wr1_kasii" = 4
  )
  
  #function to decide cols
  for(plant_time in plant_times){ #iterates through all values in plant_times
    for(col_number in 1:3){ #from 1-3 in col_end
      col_beg <- paste(plant_time, toString(plant_number), sep = '') #start of each col name i.e. "A1/2/3/4"
      col_end <- paste('_', toString(col_number), sep = '') #end of each coll name i.e. "_1/2/3"
      col <- paste(col_beg, col_end, sep = '') #combines both
      
      #adds each column to cols
      cols <- append(cols, col)
    }
  }
  
  # final data that is written in each file
  final_data <- data[, cols]
  
  # rownames are genes
  rownames(final_data) <- genes
  
  # write to files
  folder <- './data/time_function/'
  final_file <- paste(plant_type, '.csv', sep = '')
  write.csv(final_data, file = paste(folder, final_file, sep = ''))
  
}

# calling functions
write_time("wild_type")
write_time("wr1")
write_time("wr1_dgat")
write_time("wr1_kasii")