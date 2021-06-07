#setting working directory
setwd("/Users/pranavpalli/Desktop/summer_21")

#files
data <- read.csv(file = "./data/normalized_soybean_data.csv") #data file

#vectors
plant_times <- c("A", "B", "C") #vector of plants at R5, 5/6, 6 times
cols <- c() #vector of all column names
genes <- data$genes #genes column from data
