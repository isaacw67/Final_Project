rm(list = ls())
setwd("~/R/Final_Project")
library(tidyverse)

# Preliminary regression work

# Let's import block tree

to_numeric_height <- function(str) {
  split <- str_split(str, "-")
  val1 <- parse_number(split[[1]][1])
  val2 <- parse_number(split[[1]][2])
  mean_val <- ((val1 + val2) / 2)
  return(mean_val)

}

durham_tree <- read_csv("data/durham_trees.csv")
block_tree <- read_csv("data/block_tree.csv")
block_tree <- left_join(durham_tree, block_tree, by = c("Planting Site Id" = "Planting Site Id", "Point X" = "Point X", "Point Y" = "Point Y"))
block_tree <- mutate(block_tree,
                     block_id = as.numeric(block_fips),
                     short_grp = floor(block_id/1000), 
                     )

block_tree <- mutate(rowwise(block_tree),
                     numeric_height = to_numeric_height(`Tree: Height.x`)
)
# Read in 2019, 2018, and 2017

data_2017 <- read_csv("data/2017_import.csv")
estimates_2017 <- read_csv("data/estimates_2017.csv")
estimates_2018 <- read_csv("data/estimates_2018.csv")
estimates_2019 <- read_csv("data/estimates_2019.csv")

data_2017 <- mutate(rowwise(data_2017),
                    block_fips = str_remove(GEO_ID, "1500000US"),
)

data_2017$best_range <- estimates_2017$best_range

data_2018 <- read_csv("data/2018_import.csv")

data_2018 <- mutate(rowwise(data_2018),
                    block_fips = str_remove(GEO_ID, "1500000US"),
)

data_2018$best_range <- estimates_2018$best_range


data_2019 <- read_csv("data/2019_import.csv")

data_2019 <- mutate(rowwise(data_2019),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                  )

data_2019$best_range <- estimates_2019$best_range


to_numeric_val <- function(str) {
  # Split the str into parts by spaces
  split <- str_split(str, " ")
  
  # Now take the first and last number:
  if (split[[1]][1] == "Less") {
    return(parse_number(split[[1]][3]))
  }
  
  val1 <- parse_number(split[[1]][1])
  val2 <- parse_number(split[[1]][3])
  mean_val <- ((val1 + val2) / 2)
  return(mean_val)
  }


# Merge 2017, 2018, 2019

data_2017$year <- 2017
data_2019$year <- 2019
data_2018$year <- 2018

# Drop first row from 2018 and 2019

data_2018 <- data_2018[-c(1),]
data_2019 <- data_2019[-c(1),]

grouped_data <- rbind(data_2017, data_2018, data_2019)

grouped_data <- mutate(rowwise(grouped_data),
                       numeric_val = to_numeric_val(best_range)
                       )

counts <- count(block_tree, short_grp)

counts_2017 <- subset(block_tree, (`Updated At.x` < as.Date("2018-01-01") | `Tree: Planting Date` == "2017to2018"))
counts_2018 <- subset(block_tree, ((`Updated At.x` < as.Date("2019-01-01") & `Updated At.x` >= as.Date("2018-01-01")) | `Tree: Planting Date` == "2018to2019"))
counts_2019 <- subset(block_tree, ((`Updated At.x` < as.Date("2020-01-01") & `Updated At.x` >= as.Date("2019-01-01")) | `Tree: Planting Date` == "2019to2020"))

# Now, we can map HV in certain years to street tree counts:
# We need some estimate 

block_tree <- mutate(block_tree,
                       empty = case_when(`Tree Present` == FALSE ~ 1, TRUE ~ 0),
                       planted = case_when(`Tree Present` == FALSE ~ 0, TRUE ~ 1)
                       )


counts_2017 <- count(counts_2017, short_grp)
counts_2018 <- count(counts_2018, short_grp)
counts_2019 <- count(counts_2019, short_grp)
counts_2020 <- subset(block_tree, `Updated At` < as.Date("2021-01-01"))
counts_2020 <- count(counts_2020, short_grp)
# So let's assign tree nums over, we'll use 2019 as the litmus test

lookup_count <- function(code, year) {
  if (year == 2017) {
    dat <- counts_2017
  }
  
  if (year == 2018) {
    dat <- counts_2018
  }
  
  if (year == 2019) {
    dat <- counts_2019
  }
  
  sub <- subset(dat, short_grp == code)
  
  if (length(sub$n) < 1) {
    return(0)
  }
  
  sub$n[[1]]
}

avg_tree_width <- function(code, year) {
  matching <- subset(block_tree, short_grp == code & `Updated At` < as.Date(paste((year+1), "-01-01", sep = "")))
  return(mean(matching$Diameter, na.rm = TRUE))
}

avg_tree_height <- function(code, year) {
  matching <- subset(block_tree, short_grp == code & `Updated At` < as.Date(paste((year+1), "-01-01", sep = "")))
  return(mean(matching$numeric_height, na.rm = TRUE))
}

grouped_data$num_trees = 0

grouped_data <- mutate(rowwise(grouped_data),
                       num_trees = lookup_count(block_fips, year),
                       tree_width = case_when(num_trees == 0 ~ 0,
                                              TRUE ~ avg_tree_width(block_fips, year)),
                       tree_height = case_when(num_trees == 0 ~ 0,
                                               TRUE ~ avg_tree_height(block_fips, year)),
                       year = year - 2017
)
model_no_tree <- glm(numeric_val~year + factor(block_fips) , data = grouped_data[-c(1),])
model_nofe <- glm(numeric_val~year + num_trees, data = grouped_data[-c(1),])
model_cont <- glm(numeric_val~year + num_trees + tree_height + tree_width, data = grouped_data[-c(1),])
model_fe <- glm(numeric_val~year + num_trees + factor(block_fips), data = grouped_data[-c(1),])
model_feplus <- glm(numeric_val~year + num_trees + tree_height + tree_width + factor(block_fips), data = grouped_data[-c(1),])
model_all_fe <- glm(numeric_val~year + num_trees + tree_height + tree_width + factor(block_fips)*year, data = grouped_data[-c(1),])
model_feplus_nw <- glm(numeric_val~year + num_trees + tree_height + factor(block_fips), data = grouped_data[-c(1),])
model_all_fe_nw <- glm(numeric_val~year + num_trees + tree_height + factor(block_fips) + factor(block_fips)*year, data = grouped_data[-c(1),])
model_logged <- glm(log(numeric_val)~year + (num_trees) + (tree_height) + factor(block_fips), data = grouped_data[-c(1),])
 




