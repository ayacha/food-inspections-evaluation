##
## Because this step takes so long, it's pre-calculated here.
##

##==============================================================================
## INITIALIZE
##==============================================================================
## Remove all objects; perform garbage collection
rm(list=ls())
gc(reset=TRUE)
## Detach libraries that are not used
geneorama::detach_nonstandard_packages()
## Load libraries that are used
geneorama::loadinstall_libraries(c("data.table", "MASS"))
## Load custom functions
geneorama::sourceDir("CODE/functions/")

##==============================================================================
## LOAD CACHED RDS FILES
##==============================================================================
foodInspect <- readRDS("DATA/food_inspections.Rds")
foodInspect <- filter_foodInspect(foodInspect)

##==============================================================================
## CALCULATE FEATURES BASED ON FOOD INSPECTION DATA
##==============================================================================

## Calculate violation matrix and put into data.table with inspection id as key
## calculate_violation_types calculates violations by categories:
##       Critical, serious, and minor violations
vio <- strsplit(foodInspect$Violations,"| ",fixed=T)



#vio_text <- strsplit( unlist(strsplit(foodInspect$Violations,"Comments:",fixed=T) ), "| ", fixed=T)

vio_text <- strsplit(foodInspect$Violations,"| ",fixed=T)
vio_text <- lapply(vio_text, function(item) strsplit(item, "Comments: ",fixed=T) )

posText <- c()
negText <- c()

temp1 = ""
temp2 = ""

for(i in 1:length(vio_text))
{
    temp1 = ""
    temp2 = ""
    if(length(vio_text[[i]]) != 0)
    {
    for (j in 1:length(vio_text[[i]]))
    {
        if(foodInspect[i]$Results == "Pass"){
            if(length(vio_text[[i]][[j]]) >1)
                temp1 <- paste(temp1, vio_text[[i]][[j]][[2]])
        }
        else {
            if(length(vio_text[[i]][[j]]) >1)                
                temp2 <- paste(temp2, vio_text[[i]][[j]][[2]])
        }
    }
    

    }
    if(foodInspect[i]$Results == "Pass")
        posText = c(posText, temp1)
    else 
        negText = c(negText, temp2)
}

sentimentScore <- function(sentences, negTerms, posTerms){
    final_scores <- matrix('', 0, 3)
    scores <- laply(sentences, function(sentence, negTerms, posTerms){
        initial_sentence <- sentence
        #remove unnecessary characters and split up by word 
        
        sentence <- gsub('[[:punct:]]', '', sentence)
        sentence <- gsub('[[:cntrl:]]', '', sentence)
        sentence <- gsub('[0-9]', '', sentence)
        sentence <- gsub('\\d+', '', sentence)
        sentence <- tolower(sentence)
        wordList <- str_split(sentence, '\\s+')
        words <- unlist(wordList)
        
        #build vector with matches between sentence and each category
        posMatches <- match(words, posTerms)
        negMatches <- match(words, negTerms)
        
        #sum up number of words in each category
        posMatches <- sum(!is.na(posMatches))
        negMatches <- sum(!is.na(negMatches))
        score <- c( negMatches, posMatches)
        
        #add row to scores table
        newrow <- c(initial_sentence, score)
        final_scores <- rbind(final_scores, newrow)
        return(final_scores)
    }, negTerms, posTerms)
    return(scores)
}


        
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)

names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary", "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")


###########################################################
library(plyr)
library(stringr)
library(e1071)

library(tm)
library(sentiment)
library(randomForest)

#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(posText, negTerms, posTerms))
negResult <- as.data.frame(sentimentScore(negText, negTerms, posTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'neg', 'pos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'neg', 'pos', 'sentiment')

#combine the positive and negative tables
results <- rbind(posResult, negResult)

#run the naive bayes algorithm using all four categories
classifier <- naiveBayes(results[,2:3], results[,4])
class_pol = classify_polarity(results[,1], algorithm="bayes")
#polarity = class_pol[,4]

for (i in 1:dim(class_pol)[1]){
    if(class_pol[i,4]=='neutral'){
        class_pol[i,4] = 'positive'
    }
}


sentiment_col = class_pol[,4]

ret <- data.table(Inspection_ID ,sentiment_col)
data.table::set(x = ret, 
                j = names(foodInspect$Inspection_ID), 
                value = foodInspect$Inspection_ID[[1]])
setkeyv(ret, names(Inspection_ID))



Inspection_ID = foodInspect$Inspection_ID


ret <- data.table(criticalCount,
                  seriousCount,
                  minorCount)
data.table::set(x = ret, 
                j = names(key_vec), 
                value = key_vec[[1]])
setkeyv(ret, names(key_vec))
return(ret)



saveRDS(ret, "DATA/sent_violation_dat.Rds")
