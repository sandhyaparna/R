install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages(c("SnowballC","Snowball", "RWeka", "rJava", "RWekajars"))
#RESTART R session!
library("devtools", lib.loc="~/R/win-library/3.3")
library("rjson", lib.loc="~/R/win-library/3.3")
library("bit64", lib.loc="~/R/win-library/3.3")
library("RWekajars", lib.loc="~/R/win-library/3.3")
library("rJava", lib.loc="~/R/win-library/3.3")
library("tm", lib.loc="~/R/win-library/3.3")
library("SnowballC", lib.loc="~/R/win-library/3.3")
library(devtools)
library(ROAuth)
install_github("twitteR", username="geoffjentry")
library(twitteR)

api_key <- ""

api_secret <- ""

access_token <- ""

access_token_secret <- ""

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Access Twitter stream - live streaming data
library(ROAuth)
library(streamR)
my_oauth <- OAuthFactory$new(consumerKey = "QvlLldoTKNGQ5NNparmcU8QmL",
                             consumerSecret = "ep0UnI0xN1kaU0g6X5TZiNvaCm92WihzVVWX69p8wYvcGxJSJG",
                             requestURL = "https://api.twitter.com/oauth/request_token",
                             accessURL = "https://api.twitter.com/oauth/access_token",
                             authURL = "https://api.twitter.com/oauth/authorize")

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file = "my_oauth.Rdata")


load("my_oauth.Rdata")

filterStream(file.name = "tweets.json", # Save tweets in a json file
             track = c("Dear 2017"), # Collect tweets mentioned
             language = "en",
             timeout = 60, # Keep connection alive for 60 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

# Locations Based Tweets - Cali location  
filterStream(file.name = "tweets_cali.json", # Save tweets in a json file
             track = c("Dear 2017"),
             locations = c(-125, 30, -114, 42),# Collect tweets mentioned
             language = "en",
             timeout = 60, # Keep connection alive for 60 seconds
             oauth = my_oauth,
             verbose = TRUE)

tweets_cali.df <- parseTweets("tweets_cali.json", simplify = FALSE)

View(tweets_cali.df)
save(file="tweetsDF.RDATA", tweets_cali.df)

library(stringr)
tweets_cali.df$hashtags <- str_extract(tweets_cali.df$text, "#[:alnum:]+")

tweets_cali.df$hashtags <- as.factor(tweets_cali.df$hashtags)
summary(tweets_cali.df$hashtags)

# Visualization - California map and tweets associated with the latitude and longitude of the State

library(ggplot2)
library(grid)
map.data <- map_data("state", region=c("california"))
# We only need the long and lat values from the data. 
# These are put in a new object.
points <- data.frame(x = as.numeric(tweets_cali.df$place_lon), 
                     y = as.numeric(tweets_cali.df$place_lat))
# This line is needed for the second plot, when hashtags are added.
points$hashtags <- tweets_cali.df$hashtags
# The next lines are just used to remove points that are not specified or 
# are incidental too far a way from California.
points[!is.na(tweets_cali.df$lon), "x"] <- as.numeric(tweets_cali.df$lon)[!is.na(tweets_cali.df$lon)]
points[!is.na(tweets_cali.df$lat), "y"] <- as.numeric(tweets_cali.df$lat)[!is.na(tweets_cali.df$lat)]

points <- points[(points$y > 25 & points$y < 42), ]
points <- points[points$x < -114,]

# The following code creates the graphic.
mapPlot <- ggplot(map.data) + # ggplot is the basic plotting function used.
  # The following lines define the map-areas.
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white", 
           color = "grey20", 
           size = 0.25) +  
  expand_limits(x = map.data$long, 
                y = map.data$lat) + 
  # The following parameters could be altered to insert axes, title, etc.
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  # The next line plots points for each tweet. Size, transparency (alpha) 
  # and color could be altered.
  geom_point(data = points, 
             aes(x = x, y = y), 
             size = 2, 
             alpha = 1/20, 
             color = "steelblue")

mapPlot # This command plots the object.
