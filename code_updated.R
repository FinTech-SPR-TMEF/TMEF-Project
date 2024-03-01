# packages loading
install.packages("readxl")
install.packages("readr")
install.packages("openxlsx")
library(readr)
library(readxl)
library(data.table)
library(tidyverse)
library(openxlsx)
library(dplyr)

# file reading
movie_info1 <- read_excel("all_movie.xlsx")%>%
                mutate(Title = toupper(Title))
movie_info1 <- distinct(movie_info1, Title, .keep_all = TRUE)

movie_info2 <- read_excel("movies_metadata.xlsx")%>%
                mutate(original_title = toupper(original_title))
movie_info2 <- distinct(movie_info2, original_title, .keep_all = TRUE)

review <- read_csv("review2.csv", show_col_types = FALSE) %>%
          mutate(Movie = toupper(Movie)) %>% 
          mutate(Reviewer = toupper(Reviewer))

critics <- read_excel("rotten_tomatoes_movie_reviews.xlsx") %>% 
          mutate(criticName = toupper(criticName))
critics <- distinct(critics, criticName, .keep_all = TRUE) #Delete Duplicate

#data merging

colnames(critics)[colnames(critics)=="criticName"] <- "Reviewer"
merged_review <- left_join(review, critics, by = "Reviewer")

# merged_review <- merge(critics,review, by = "Reviewer")


colnames(movie_info1)[colnames(movie_info1)=="Title"] <- "Movie"
merged_review_film1 <- left_join(merged_review, movie_info1, by = "Movie")

# merged_review_film1 <- merge(merged_review, movie_info1, by = "Movie")

colnames(movie_info2)[colnames(movie_info2)=="original_title"] <- "Movie"
merged_review_film12 <- left_join(merged_review_film1, movie_info2, by = "Movie")

# merged_review_film12 <-merge(movie_info2,merged_review_film1, by = "Movie")

#delete one of the two release date and "belongs_to_collection" column
merged_review_film_12 <- merged_review_film12%>%
  dplyr::select(-`Release Date`)%>%
  dplyr::select(-belongs_to_collection) %>% 
  filter(Year > 1999)


#standardize datadate and filter Date < release_date, budget and revenue != 0
merged_review_film_pre <- merged_review_film_12 %>%
  mutate(
    release_date = as.Date(release_date, format = "%Y-%m-%d", na.rm = TRUE),
    Date = as.Date(Date, format = "%d/%m/%Y", na.rm = TRUE)
  )%>%
  filter(Date < release_date)%>%
  filter(budget != 0)%>%
  filter(revenue != 0)%>%
  distinct(Review, .keep_all = TRUE)

merged_review_film_after <- merged_review_film_12 %>%
  mutate(
    release_date = as.Date(release_date, format = "%Y-%m-%d", na.rm = TRUE),
    Date = as.Date(Date, format = "%d/%m/%Y", na.rm = TRUE)
  )%>%
  filter(Date > release_date)%>%
  filter(budget != 0)%>%
  filter(revenue != 0)%>%
  distinct(Review, .keep_all = TRUE)

write.csv(merged_review_film_pre, "merged_review_pre.csv", row.names = TRUE)
write.csv(merged_review_film_after, "merged_review_after.csv", row.names = TRUE)
