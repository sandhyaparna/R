# https://journal.r-project.org/archive/2013-1/collingwood-jurka-boydstun-etal.pdf
library("RTextTools", lib.loc="~/R/win-library/3.3")

data(USCongress)

# Major variable - Topic code(Dependent Variable)

# Convert dataframe variable to corpus for text data pre-processing
myCorpus <- Corpus(VectorSource(USCongress$text))

for (i in c(1:2, 91)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}

# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))

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

#####
# Convert Corpus back to data frame to create Document Term Matrix
dataframe <-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
                      stringsAsFactors=F)

require(RWeka)
library("parallel", lib.loc="C:/Program Files/R/R-3.3.1/library")
source("GenerateTDM.R") # generatetdm function in appendix
tdm.generate <- function(string, ng){
  
  # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
  
  corpus <- Corpus(VectorSource(string)) # create corpus for TM processing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english")) 
  options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
  BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, Weka_control(min = ng, max = ng))} # create n-grams
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)) # create tdm from n-grams
  tdm
}
options(mc.cores=1)
source("GenerateTDM.R") # generatetdm function in appendix
tdm <- tdm.generate(dataframe$text, 2)

library("RWeka")
library("tm")

data("crude")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))

# Create Document Term Matrix
doc_matrix <- create_matrix(dataframe$text, language="english", removeNumbers=TRUE,
                            removeSparseTerms=.998)

# Create a container to use in the ML model
container <- create_container(doc_matrix, USCongress$major, trainSize=1:3750,
                              testSize=3751:4449, virgin=FALSE)

# Train ML Model - Uses train data from the Container
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

# Classify Test Data (Output is Dependent variable predicted value and probability)
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)

# Evaluation of summary stats
analytics <- create_analytics(container,cbind(SVM_CLASSIFY
                                              , GLMNET_CLASSIFY, SLDA_CLASSIFY,MAXENT_CLASSIFY))

summary(analytics)

# Evaluation of summary stats
analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
                                    BOOSTING_CLASSIFY, BAGGING_CLASSIFY,
                                    RF_CLASSIFY, GLMNET_CLASSIFY,
                                    NNET_CLASSIFY, TREE_CLASSIFY,
                                    MAXENT_CLASSIFY))
summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary

create_ensembleSummary(analytics@document_summary)

# K-fold cross validation
SVM <- cross_validate(container, 4, "SVM")
GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
TREE <- cross_validate(container, 4, "TREE")
