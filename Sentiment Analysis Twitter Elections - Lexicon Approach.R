### Sentiment_Analysis_Elections2016

Sentiment analysis is performed on Election 2016 Twitter data for hastags Trump and Hillary for 50 US States.

Twitter data was collected for different states using latitude and longitude. Data pre-processing tasks such as removing punctuation,URLs,
  numbers, stop words, etc is performed before calculating sentiment score using positive and negative words based on Lexicon approach.

  
install.packages("RCurl")
install.packages("ROAuth")
install.packages("twitteR")
install.packages("plyr")

library(RCurl)
library(ROAuth)
library(twitteR)
library(stringr)
library(plyr)
library(tm)

api_key <- ""

api_secret <- ""

access_token <- ""

access_token_secret <- ""

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Tweets on Trump

# List of latitudes and longitudes for all US states
# https://inkplant.com/code/state-latitudes-longitudes

# Size of US States
# http://www.statesymbolsusa.org/symbol-official-item/national-us/uncategorized/states-size

Alabama <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="32.806671,-86.791130,50mi", since="2016-01-01")
Alabama.df <- twListToDF(Alabama)

Alaska <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                        geocode="61.370716,-152.404419,100mi", since="2016-01-01")
Alaska.df <- twListToDF(Alaska)

Arizona <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="33.729759,-111.431221,100mi", since="2016-01-01")
Arizona.df <- twListToDF(Alabama)

Arkansas <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="34.969704,-92.373123,100mi", since="2016-01-01")
Arkansas.df <- twListToDF(Arkansas)

California <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                            geocode="36.116203,-119.681564,100mi", since="2016-01-01")
California.df <- twListToDF(California)

Colorado <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="39.059811,-105.311104,100mi", since="2016-01-01")
Colorado.df <- twListToDF(Colorado)

Connecticut <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                             geocode="41.597782,-72.755371,100mi", since="2016-01-01")
Connecticut.df <- twListToDF(Connecticut)

Delaware <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="39.318523,-75.507141,50mi", since="2016-01-01")
Delaware.df <- twListToDF(Delaware)

District_of_Columbia <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                                      geocode="38.897438,-77.026817,50mi", since="2016-01-01")
District_of_Columbia.df <- twListToDF(District_of_Columbia)

Florida <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="27.766279,-81.686783,50mi", since="2016-01-01")
Florida.df <- twListToDF(Florida)

Georgia <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="33.040619,-83.643074,50mi", since="2016-01-01")
Georgia.df <- twListToDF(Georgia)

Hawaii <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                        geocode="21.094318,-157.498337,50mi", since="2016-01-01")
Hawaii.df <- twListToDF(Hawaii)

Idaho <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                       geocode="44.240459,-114.478828,50mi", since="2016-01-01")
Idaho.df <- twListToDF(Idaho)

Illinois <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="40.349457,-88.986137,50mi", since="2016-01-01")
Illinois.df <- twListToDF(Illinois)

Indiana <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="39.849426,-86.258278,50mi", since="2016-01-01")
Indiana.df <- twListToDF(Indiana)

Iowa <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                      geocode="42.011539,-93.210526,50mi", since="2016-01-01")
Iowa.df <- twListToDF(Iowa)

Kansas <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                        geocode="38.526600,-96.726486,50mi", since="2016-01-01")
Kansas.df <- twListToDF(Kansas)

Kentucky <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="37.668140,-84.670067,50mi", since="2016-01-01")
Kentucky.df <- twListToDF(Kentucky)

Louisiana <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                           geocode="31.169546,-91.867805,50mi", since="2016-01-01")
Louisiana.df <- twListToDF(Louisiana)

Maine <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                       geocode="44.693947,-69.381927,50mi", since="2016-01-01")
Maine.df <- twListToDF(Maine)

Maryland <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="39.063946,-76.802101,50mi", since="2016-01-01")
Maryland.df <- twListToDF(Maryland)

Massachusetts <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                               geocode="42.230171,-71.530106,50mi", since="2016-01-01")
Massachusetts.df <- twListToDF(Massachusetts)

Michigan <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="43.326618,-84.536095,50mi", since="2016-01-01")
Michigan.df <- twListToDF(Michigan)

Minnesota <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                           geocode="45.694454,-93.900192,50mi", since="2016-01-01")
Minnesota.df <- twListToDF(Minnesota)

Mississippi <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                             geocode="32.741646,-89.678696,50mi", since="2016-01-01")
Mississippi.df <- twListToDF(Mississippi)

Missouri <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="38.456085,-92.288368,50mi", since="2016-01-01")
Missouri.df <- twListToDF(Missouri)

Montana <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="46.921925,-110.454353,50mi", since="2016-01-01")
Montana.df <- twListToDF(Montana)

Nebraska <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="41.125370,-98.268082,50mi", since="2016-01-01")
Nebraska.df <- twListToDF(Nebraska)

Nevada <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                        geocode="38.313515,-117.055374,50mi", since="2016-01-01")
Nevada.df <- twListToDF(Nevada)

New_Hampshire <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                               geocode="43.452492,-71.563896,50mi", since="2016-01-01")
New_Hampshire.df <- twListToDF(New_Hampshire)

New_Jersey <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                            geocode="40.298904,-74.521011,50mi", since="2016-01-01")
New_Jersey.df <- twListToDF(New_Jersey)

New_Mexico <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                            geocode="34.840515,-106.248482,50mi", since="2016-01-01")
New_Mexico.df <- twListToDF(New_Mexico)

New_York <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="32.806671,-86.791130,50mi", since="2016-01-01")
New_York.df <- twListToDF(New_York)

North_Carolina <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                                geocode="32.806671,-86.791130,50mi", since="2016-01-01")
North_Carolina.df <- twListToDF(North_Carolina)

North_Dakota <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                              geocode="47.528912,-99.784012,50mi", since="2016-01-01")
North_Dakota.df <- twListToDF(North_Dakota)

Ohio <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                      geocode="40.388783,-82.764915,50mi", since="2016-01-01")
Ohio.df <- twListToDF(Ohio)

Oklahoma <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="35.565342,-96.928917,50mi", since="2016-01-01")
Oklahoma.df <- twListToDF(Oklahoma)

Oregon <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                        geocode="44.572021,-122.070938,50mi", since="2016-01-01")
Oregon.df <- twListToDF(Oregon)

Pennsylvania <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                              geocode="40.590752,-77.209755,50mi", since="2016-01-01")
Pennsylvania.df <- twListToDF(Pennsylvania)

Rhode_Island <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                              geocode="41.680893,-71.511780,50mi", since="2016-01-01")
Rhode_Island.df <- twListToDF(Rhode_Island)

South_Carolina <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                                geocode="33.856892,-80.945007,50mi", since="2016-01-01")
South_Carolina.df <- twListToDF(South_Carolina)

South_Dakota <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                              geocode="44.299782,-99.438828,50mi", since="2016-01-01")
South_Dakota.df <- twListToDF(South_Dakota)

Tennessee <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                           geocode="35.747845,-86.692345,50mi", since="2016-01-01")
Tennessee.df <- twListToDF(Tennessee)

Texas <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                       geocode="31.054487,-97.563461,50mi", since="2016-01-01")
Texas.df <- twListToDF(Texas)

Utah <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                      geocode="40.150032,-111.862434,50mi", since="2016-01-01")
Utah.df <- twListToDF(Utah)

Vermont <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="44.045876,-72.710686,50mi", since="2016-01-01")
Vermont.df <- twListToDF(Vermont)

Virginia <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                          geocode="37.769337,-78.169968,50mi", since="2016-01-01")
Virginia.df <- twListToDF(Virginia)

Washington <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                            geocode="47.400902,-121.490494,50mi", since="2016-01-01")
Washington.df <- twListToDF(Washington)

West_Virginia <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                               geocode="38.491226,-80.954453,50mi", since="2016-01-01")
West_Virginia.df <- twListToDF(West_Virginia)

Wisconsin <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                           geocode="44.268543,-89.616508,50mi", since="2016-01-01")
Wisconsin.df <- twListToDF(Wisconsin)

Wyoming <- searchTwitter("Trump OR trump OR #trump", n=500, lang="en", 
                         geocode="42.755966,-107.302490,50mi", since="2016-01-01")
Wyoming.df <- twListToDF(Wyoming)


#State <- California
#State.text = laply(State, function(t)t$getText())

myCorpus <- Corpus(VectorSource(California.df$text))

# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, c(stopwords('english')))

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
dictCorpus <- myCorpus

# stem words - return the word to its basic form
myCorpus <- tm_map(myCorpus, stemDocument)

a <- myCorpus
myCorpus <- a
# myCorpus <- tm_map(myCorpus, stemCompletion, dictionary = myCorpusCopy, lazy=TRUE)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

# Import Positive and Negative words
pos = scan("C:/Users/Sandhya/OneDrive/Data Science Books/Notes/Text Mining/Sentiment Analysis/positive-words.txt", what="character",comment.char=";")
neg= scan("C:/Users/Sandhya/OneDrive/Data Science Books/Notes/Text Mining/Sentiment Analysis/negative-words.txt", what="character",comment.char=";")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    neg.matches = match(words, neg.words)
    pos.matches = match(words, pos.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


myCorpus.text <- strwrap(myCorpus)

analysis = score.sentiment(myCorpus.text, pos, neg, .progress="none")
table(analysis$score)
mean(analysis$score)

hist(analysis$score, main ="#Trump Sentiment Score", ylim=c(0,8000), col ="red", xlab="score" )
