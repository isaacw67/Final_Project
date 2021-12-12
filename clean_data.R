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

harris_cities <- c("Harrisburg")
harris_tree <- read_csv("data/harris_tree.csv")
harris_tree <- subset(harris_tree, City %in% harris_cities)
write_csv(harris_tree, "data/harris_trees.csv")
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

# Drop trees that don't exist yet:

block_tree <- filter(block_tree, `Tree Present.x`)
# Read in 2019, 2018, and 2017

data_2017 <- read_csv("data/2017_import.csv")
estimates_2017 <- read_csv("data/estimates_2017.csv")
estimates_2018 <- read_csv("data/estimates_2018.csv")
estimates_2019 <- read_csv("data/estimates_2019.csv")

value_guide <- estimates_2017[c(1),]

for (i in seq(1, dim(value_guide)[2])) {
  value_guide[i] = to_numeric_val(value_guide[i])
}

value_guide <- value_guide[c(3:28)]

# So, we have numbers in rows, 
# We need to do some math to get the values:
# 3 to 56, add 2
# 5, 7, 9, 11, .... 55
# 1, 2, 3, 4, 5, ..., 25
# times 2 plus 3?
mean_home_val <- function(row) {
  num_homes = 0
  val_homes = 0
  bands = dim(value_guide)[2]
  for (i in seq(1:(bands))) {
    num_homes = num_homes + as.numeric(row[2*i+1])
    val_homes = val_homes + as.numeric(row[2*i+1]) * value_guide[[i]][1]
  }
  val_homes / num_homes
}


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

# Estimate!!Total!!$750,000 to $999,999
# Estimate!!Total!!$2,000,000 or more

to_numeric_val <- function(str) {
  # Split the str into parts by spaces
  split_up <- str_split(str, " ")
  
  # Find the ones with numbers
  has_nums <- grepl("\\d", split_up[[1]])
  
  # Get the ones with numbers, and return them:
  ret = 0
  num_true = 0
  for (i in seq(1, length(split_up[[1]]))) {
    if (has_nums[i] == TRUE) {
      num_true = num_true +  1
      ret = ret + parse_number(split_up[[1]][i])
    }
  }
  
  ret / num_true
  
}

# Merge 2017, 2018, 2019

data_2017$year <- 2017
data_2019$year <- 2019
data_2018$year <- 2018

# Drop first row from 2018 and 2019

data_2018 <- data_2018[-c(1),]
data_2019 <- data_2019[-c(1),]

grouped_data <- rbind(data_2017, data_2018, data_2019)

# 3:56
grouped_data <- mutate(rowwise(grouped_data),
                       numeric_val = to_numeric_val(best_range),
                       mean_value = mean_home_val(c_across(3:56))
                       )




counts <- count(block_tree, short_grp)

counts_2017 <- subset(block_tree, (`Updated At.x` < as.Date("2018-01-01") | `Tree: Planting Date` == "2017to2018"))
counts_2018 <- subset(block_tree, ((`Updated At.x` < as.Date("2019-01-01") | `Tree: Planting Date` == "2017to2018" | `Tree: Planting Date` == "2018to2019")))
counts_2019 <- subset(block_tree, ((`Updated At.x` < as.Date("2020-01-01")  | `Tree: Planting Date` == "2018to2019" | `Tree: Planting Date` == "2019to2020")))

# Now, we can map HV in certain years to street tree counts:
# We need some estimate 

block_tree <- mutate(block_tree,
                       empty = case_when(`Tree Present.x` == FALSE ~ 1, TRUE ~ 0),
                       planted = case_when(`Tree Present.x` == FALSE ~ 0, TRUE ~ 1)
                       )


counts_2017 <- count(counts_2017, short_grp)
counts_2018 <- count(counts_2018, short_grp)
counts_2019 <- count(counts_2019, short_grp)
#counts_2020 <- subset(block_tree, `Updated At` < as.Date("2021-01-01"))
#counts_2020 <- count(counts_2020, short_grp)
# So let's assign tree nums over, we'll use 2019 as the litmus test

lookup_count <- function(code, year) {
  
  if (year <= 2016) {
    return(0)
  }
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
  matching <- subset(block_tree, short_grp == code & ((`Updated At.x` < as.Date(paste((2017+1), "-01-01", sep = ""))) |`Tree: Planting Date` == paste(year, year+1, sep = "to")))
  return(mean(matching$Diameter.x, na.rm = TRUE))
}

# Testing: 370630001011
wd_test <- subset(block_tree, short_grp == 370630001011 & (`Updated At.x` < as.Date(paste((2017+1), "-01-01", sep = ""))) |`Tree: Planting Date` == "2017to2018")
mean(wd_test$numeric_height, na.rm = TRUE)

avg_tree_height <- function(code, year) {
  matching <- subset(block_tree, short_grp == code & `Updated At.x` < as.Date(paste((year+1), "-01-01", sep = "")))
  return(mean(matching$numeric_height, na.rm = TRUE))
}

percent_full <- function(code, year) {
  matching <- subset(block_tree, short_grp == code & `Updated At.x` < as.Date(paste((year+1), "-01-01", sep = "")))
  return(mean(matching$numeric_height, na.rm = TRUE))
}

grouped_data$num_trees = 0

grouped_data <- mutate(rowwise(grouped_data),
                       num_trees = lookup_count(block_fips, year),
                       tree_width = case_when(num_trees == 0 ~ 0,
                                              TRUE ~ avg_tree_width(block_fips, year)),
                       tree_height = case_when(num_trees == 0 ~ 0,
                                               TRUE ~ avg_tree_height(block_fips, year)),
)

at_least_one <- filter(grouped_data[-c(1),], num_trees > 0)

plt <- ggplot(data = at_least_one, aes(log(mean_value))) +
  geom_histogram(binwidth = 0.28) +
  theme_bw()
plot(plt)



# Transformations: 
# Logarithm on Trees (specially, log(num_trees))
# Sometimes log home value, gets it slightly more normal

# Make better models:

model_all_fe_nw <- glm(mean_value~factor(year) + log(num_trees) + tree_width + factor(block_fips) + factor(block_fips)*factor(year), data = at_least_one)
model_logged <- glm(log(mean_value)~factor(year) + log(num_trees) + tree_width + factor(block_fips)+ factor(block_fips)*factor(year), data = at_least_one)

model_just_trees <- glm(mean_value~factor(year) + log(num_trees), data = at_least_one)

# No fe
model_tree_no_fe <- glm(mean_value~factor(year) + log(num_trees) , data = at_least_one)
lmodel_tree_no_fe <- glm(log(mean_value)~factor(year) + log(num_trees) , data = at_least_one)

# BG fe

model_tree_bg <- glm(mean_value~factor(year) + log(num_trees)  + factor(block_fips), data = at_least_one)
lmodel_tree_bg <- glm(log(mean_value)~factor(year) + log(num_trees)  + factor(block_fips), data = at_least_one)

# BG-factor(year) fe

model_tree_bgy <- glm(mean_value~factor(year) + log(num_trees)  + factor(year)*factor(block_fips), data = at_least_one)
lmodel_tree_bgy <- glm(log(mean_value)~factor(year) + log(num_trees)  + factor(year)*factor(block_fips), data = at_least_one)

# All fe

model_tree_all <- glm(mean_value~factor(year) + log(num_trees) + factor(block_fips), data = at_least_one)
lmodel_tree_all <- glm(log(mean_value)~factor(year) + log(num_trees) + factor(block_fips), data = at_least_one)

log_list <- c(lmodel_tree_no_fe, lmodel_tree_bg, lmodel_tree_bgy, lmodel_tree_all)

val_list <- c(model_tree_no_fe, model_tree_bg, model_tree_bgy, model_tree_all)

stargazer(lmodel_tree_no_fe, lmodel_tree_bg, lmodel_tree_bgy, lmodel_tree_all)
stargazer(model_tree_no_fe, model_tree_bg, model_tree_bgy, model_tree_all)

