# T2: What is the gender disparity in areas?
## How many men and women are in a given age group?
### ((females_total / inhabitants_total) * population in a given age group)
### ((males_total / inhabitants_total) * population in a given age group)
## What is the female-to-male ratio for each area?
### (females_total / males_total)
## How many single women are in each area?
### (inhabitants_total - (inhabitants_total * (females_total / males_total)))
## Sort by the amount of single women decreasing
# H1: Will show the optimal areas to find a partner for single men
# Data structure: | area | inhabitants_total | females_total | males_total | female-to-male_ratio | (females_age_group_x | males_age_group_x |) ...

run_task_2 <- function() {
  library("reshape")
  
  population_structure_df <- read.csv("Datasets/paavo_1_he_population_structure.csv", header = TRUE, sep = ";", as.is = TRUE)
  
  removeDotDotsFromDatasets <- function(dataset) {
    for (row in 1:nrow(dataset)) {
      dataset[,1] = as.character(dataset[,1])
      for (column in 2:ncol(dataset)) {
        if (dataset[row, column] == "..") {
          dataset[row, column] = 0
        }
      }
    }
    # Set all the other columns as integers
    for (column in 2:ncol(dataset)) {
      dataset[,column] = as.numeric(as.character(dataset[,column]))
    }
    return(dataset)
  }
  
  # The datasets have some ".." instead of a null or a zero in it's place which makes data manipulation hard
  # So remove them
  population_structure_df = removeDotDotsFromDatasets(population_structure_df)
  
  gender_disparity_df <- data.frame()
  gender_disparity_col_names <- colnames(population_structure_df[1:4])
  gender_disparity_col_names <- c(gender_disparity_col_names, "Female-To-Male ratio")
  for (column in 6:ncol(population_structure_df)) {
    column_name <- colnames(population_structure_df)[column]
    gender_disparity_col_names <- c(gender_disparity_col_names, paste("Males", column_name))
    gender_disparity_col_names <- c(gender_disparity_col_names, paste("Females", column_name))
  }
  
  for (row in 1:nrow(population_structure_df)) {
    area <- population_structure_df[row, 1]
    inhabitants_total <- population_structure_df[row, 2]
    females_total <- population_structure_df[row, 3]
    males_total <- population_structure_df[row, 4]
    if (is.nan(females_total / males_total)) {
      female_to_male_ratio = 0.0
    } else {
      female_to_male_ratio <- (females_total / males_total) + 0.0
    }
    
    temp_vector <- vector(length = length(gender_disparity_col_names))
    temp_vector[1] = area
    temp_vector[2] = inhabitants_total
    temp_vector[3] = females_total
    temp_vector[4] = males_total
    temp_vector[5] = female_to_male_ratio
    
    temp_vector_index <- 6
    for (col in 6:ncol(population_structure_df)) {
      temp_vector[temp_vector_index] = ((males_total / inhabitants_total) * population_structure_df[row, (col)])
      temp_vector[temp_vector_index + 1] = ((females_total / inhabitants_total) * population_structure_df[row, (col)])
      temp_vector_index = temp_vector_index + 2
    }
    
    temp_list <- as.list(temp_vector)
    names(temp_list) <- gender_disparity_col_names
    
    temp_df <- as.data.frame(temp_list)
    gender_disparity_df = rbind(gender_disparity_df, temp_df)
  }
  
  gender_disparity_df = removeDotDotsFromDatasets(gender_disparity_df)
  gender_disparity_order <- order(gender_disparity_df$Female.To.Male.ratio)
  gender_disparity_order = rev(gender_disparity_order)
  gender_disparity_df = gender_disparity_df[gender_disparity_order, ]
  
  return (gender_disparity_df)
}

visualize_task_2 <- function(data_to_visualize) {
  library("reshape2")
  library("ggplot2")
  
  View(data_to_visualize)
  
  melted_data <- melt(data_to_visualize[, c(1,5)], "Postal.code.area")
  ggplot(melted_data, aes(x = Postal.code.area, y = value)) + geom_bar(stat = "identity") + coord_flip()
}
