# Event study
# 

rm(list = ls())
setwd("~/R/Final_Project")
library(tidyverse)




#
#

ddata_2013 <- read_csv("event_study_folder/ddata_2013.csv")
ddata_2014 <- read_csv("event_study_folder/ddata_2014.csv")
ddata_2015 <- read_csv("event_study_folder/ddata_2015.csv")
ddata_2016 <- read_csv("event_study_folder/ddata_2016.csv")
ddata_2017 <- read_csv("data/2017_import.csv")
ddata_2018 <- read_csv("data/2018_import.csv")
ddata_2019 <- read_csv("data/2019_import.csv")

ddata_2013 <- select(ddata_2013, GEO_ID, NAME, everything())
ddata_2014 <- select(ddata_2014, GEO_ID, NAME, everything())
ddata_2015 <- select(ddata_2015, GEO_ID, NAME, everything())
ddata_2016 <- select(ddata_2016, GEO_ID, NAME, everything())
ddata_2017 <- select(ddata_2017, GEO_ID, NAME, everything())

data_vec <- c(ddata_2013, ddata_2014,ddata_2015,ddata_2016,ddata_201,ddata_201,ddata_201,ddata_201,ddata_201, )

ddata_2013 <- mutate(rowwise(ddata_2013),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                    B25075_026E = 0,
                    B25075_026M = 0,
                    B25075_027E = 0,
                    B25075_027M = 0,
                    year = 2013
)
ddata_2014 <- mutate(rowwise(ddata_2014),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                    B25075_026E = 0,
                    B25075_026M = 0,
                    B25075_027E = 0,
                    B25075_027M = 0,
                    year = 2014
)
ddata_2015 <- mutate(rowwise(ddata_2015),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                    year = 2015
)
ddata_2016 <- mutate(rowwise(ddata_2016),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                    year = 2016
)
ddata_2017 <- mutate(rowwise(ddata_2017),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                    year = 2017
)

ddata_2018 <- mutate(rowwise(ddata_2018),
                    block_fips = str_remove(GEO_ID, "1500000US"),
                    year = 2018
)

ddata_2019 <- mutate(rowwise(ddata_2019),
                     block_fips = str_remove(GEO_ID, "1500000US"),
                     year = 2019
)

# Drop first row
ddata_2013 <- data_2013[-c(1),]
ddata_2014 <- data_2014[-c(1),]
# data_2015 <- data_2015[-c(1),]
ddata_2016 <- ddata_2016[-c(1),]
ddata_2017 <- ddata_2017[-c(1),]
ddata_2018 <- ddata_2018[-c(1),]
ddata_2019 <- ddata_2019[-c(1),]

# Merge now:

estimates_2017 <- read_csv("data/estimates_2017.csv")
value_guide <- estimates_2017[c(1),]
all_grouped <- rbind(ddata_2015, ddata_2016, ddata_2017, ddata_2018, ddata_2019)
for (i in seq(1, dim(value_guide)[2])) {
  value_guide[i] = to_numeric_val(value_guide[i])
}

value_guide <- value_guide[c(3:28)]


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

all_grouped <- mutate(rowwise(all_grouped),
                       mean_value = mean_home_val(c_across(3:56))
)


all_grouped <- mutate(all_grouped,
                      had_planted = case_when(block_fips %in% counts_2019$short_grp ~ 1, TRUE ~ 0))

grouped_data <- mutate(rowwise(all_grouped),
                       num_trees = lookup_count(block_fips, year),
                       tree_width = case_when(num_trees == 0 ~ 0,
                                              TRUE ~ avg_tree_width(block_fips, year)),
                       tree_height = case_when(num_trees == 0 ~ 0,
                                               TRUE ~ avg_tree_height(block_fips, year)),
                       has_trees = case_when(num_trees == 0 ~ "Not Treated", TRUE ~ "Treated"),
                       log_trees = log(num_trees + 1)
)


sum <- summarize(group_by(all_grouped, had_planted, year), mean = mean(mean_value, na.rm = TRUE))
plt <- ggplot(data = sum, aes(x = year, y = mean, color = factor(had_planted))) +
  geom_line() +
  labs(title = "Mean Mean Home Values in Block Groups, Treated vs. Untreated",
       color = "Treatment Status") +
  ylab("Mean Mean Home Value") +
  xlab("Year") +
  geom_vline(xintercept = 2018) +
  theme_bw()
  
plot(plt)


 model_tree_bucket <- glm(mean_value~factor(year) + factor(block_fips) + factor(has_trees), data = grouped_data)
model_tree_all <- glm(mean_value~factor(year) + log_trees + factor(block_fips) + factor(has_trees), data = grouped_data)
lmodel_tree_all <- glm(log(mean_value)~factor(year) + log_trees + factor(block_fips) + factor(has_trees), data = grouped_data)

model_tree_all
lmodel_tree_all

stargazer(model_tree_bucket)
