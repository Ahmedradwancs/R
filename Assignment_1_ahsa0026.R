#####-----Title------####
# Introduction to R - Assignment 1



#####-----Q1------####
mtcars <- get(data("mtcars"))

car_countries <- c("Japan", "Japan", "Japan", "USA", "USA",
                   "USA", "USA", "Germany", "Germany", "Germany", "Germany",
                   "Germany", "Germany", "Germany", "USA",
                   "USA", "USA", "Italy", "Japan",
                   "Japan", "Japan", "USA", "USA",
                   "USA", "USA", "Italy", "Germany", "UK",
                   "USA", "Italy", "Italy", "Sweden")


mtcars$car_countries <- car_countries


average_mpg <- mean(mtcars$mpg)
below_average <- rownames(mtcars[mtcars$mpg < average_mpg, ])
above_average <- rownames(mtcars[mtcars$mpg > average_mpg, ])


#####-----Q2------####


USA_cars <- mtcars[mtcars$car_countries == "USA", ]
Japan_cars <- mtcars[mtcars$car_countries == "Japan", ]
  

#####-----Q3------####


mpg_per_cyl_USA <- mean(USA_cars$mpg / USA_cars$cyl)
mpg_per_cyl_Japan <- mean(Japan_cars$mpg / Japan_cars$cyl)




#####-----Q4------####


#install.packages('billboard') 

#You need to run the install only once.
# Comment it out by removing the #, then comment it out again 

library(billboard)
spotify_track_data <-  get(data("spotify_track_data"))
print(head(spotify_track_data))
dim(spotify_track_data)
my_playlist <- spotify_track_data[spotify_track_data$artist_name %in% c("Rihanna", "Michael Jackson", "Elvis Presley", "Eminem"), ]

  

#####-----Q5------####

median_danceability <- median(my_playlist$danceability)
dance_tracks <- my_playlist[my_playlist$danceability > median_danceability, ]


#####-----Q6------####


Rihanna_dance_tracks <- nrow(dance_tracks[dance_tracks$artist_name == "Rihanna", ]) / nrow(my_playlist)


#####-----Q7------####

# Note: Do not alter the original spotify_track_data dataset!
# You should alter only the corrected_playlist dataset.

corrected_playlist <- spotify_track_data
corrected_playlist[corrected_playlist$artist_name == "Michael Jackson", "danceability"] <- 
  corrected_playlist[corrected_playlist$artist_name == "Michael Jackson", "danceability"] - 0.05


