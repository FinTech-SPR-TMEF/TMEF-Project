# load Libraries
library(tidyverse)

# load functions
source("helper.R")

# set seed
set.seed(02138)

# load data
merged_review_film_pre <- read_csv("merged_review_pre.csv")
# merged_review_film_after <- read_csv("merged_review_after.csv")

merged_review_film_pre <- merged_review_film_pre %>% 
                          mutate(profit = revenue - budget)

train_df <- merged_review_film_pre %>% 
              filter(release_date < "2016-01-01")
test_df <- merged_review_film_pre %>% 
             filter(release_date > "2016-01-01")

# ##################################### #

text_column = "Review"
predict_column = "profit"

accuracy <- calculate_accuracy(train_df, test_df, text_column, predict_column)
accuracy
