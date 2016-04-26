# Trip Advisor

taData <- read.csv(file= "C:/Users/Jake/OneDrive/School Work/Senior Spring/Data Analytics/Data Sets/taAllCities.csv", head=TRUE, sep=",")

taData <- read.csv(file= "C:\\Users\\Elias\\Documents\\BC Junior Year\\Analytics & Business Intelligence\\taAllCities.csv",head =TRUE, sep=",")
head(taData)

# correlogram
library(corrgram)
corrgram(taData, order=FALSE, lower.panel=panel.pie, upper.panel=NULL, text.panel=panel.txt, main="", cex.labels=1)


# poisson to see significance of independent variables
poisson <- glm(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, family="poisson", data = taData)
summary(poisson)


# marginal effects of independent variables
library(mfx)

# Poisson Marginal Effects
poissonmfx(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, data = taData)


# service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + avgHelpfulnessOfReviewer,

# Ordinary Least Squares - all vars
olsReg <- lm(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, data = taData)
summary(olsReg)

# Ordinary Least Squares - cost
olsCost <- lm(rating ~ cost, data = taData)
summary(olsCost)

# Ordinary Least Squares - service
olsService <- lm(rating ~ service, data = taData)
summary(olsService)

# Ordinary Least Squares - service
olsCostAndService <- lm(rating ~ cost + service, data = taData)
summary(olsCostAndService)

# Ordinary Least Squares - some vars
olsSome <- lm(rating ~ service + vibe +  desert + bathroom + cost + music + location + lunch + breakfast + totalReviewsOfReviewer + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, data = taData)
summary(olsSome)


# dollar stuff
taData$dollarChar <- as.character(taData$dollars)
taData$dollarAmt <- ifelse(tolower(substr(taData$dollarChar, 1, 1)) == 'n', 0, nchar(as.vector(taData$dollars)))

head(taData)



# logit stuff - NOTE: ratingBinary no longer exists - see binary bucketing below

logit <- glm(ratingBinary ~ cost + service, data = taData, family = "binomial")
summary(logit)

logitCost <- glm(ratingBinary ~ cost, data = taData, family = "binomial")
summary(logitCost)

logitService <- glm(ratingBinary ~ service, data = taData, family = "binomial")
summary(logitService)

cat("logitBoth: 182757, logitService: 184643, logitCost: 182895")

# run runLogit with dataset, string targetVar, and string vector for inptVars
# for example: runLogit(taData, "ratingBinary", c("breakfast", "lunch", "dinner")
# prints summary and variables string with AIC, returns AIC
runLogit <- function(dataset, targetVar, inptVars) {
  inptToString <- ""
  for (thisone in inptVars) {
    if (inptToString == "") {
      inptToString <- thisone
    }
    else {
      inptToString <- paste (inptToString, thisone, sep=" + ")
    }
  }
  frm<-paste(targetVar,inptToString, sep=" ~ ")
  
  myModel <- glm(formula(frm),data=dataset,family="binomial")
  print(summary(myModel))
  cat(frm, " has AIC: ", AIC(myModel), "\n", sep="")
  return (AIC(myModel))
}

cat(runLogit(taData, "ratingBinary", c("breakfast", "lunch", "dinner")), "has AIC of 184724")
cat(runLogit(taData, "ratingBinary", c("breakfast")), "has AIC of 184774")
cat(runLogit(taData, "ratingBinary", c("lunch")), "has AIC of 184833")
cat(runLogit(taData, "ratingBinary", c("dinner")), "has AIC of 184889")
cat(runLogit(taData, "ratingBinary", c("breakfast", "lunch")), "has AIC of 184724")
cat(runLogit(taData, "ratingBinary", c("breakfast", "dinner")), "has AIC of 184772")
cat(runLogit(taData, "ratingBinary", c("lunch", "dinner")), "has AIC of 184833")


runLogit(taData, "ratingBinary", c("avgHelpful"))

# getACC needs a matrix in table form
getACC <- function(matrix) {
  correct = 0
  for (i in 1:length(matrix)) {
    correct = correct + matrix[i,i]
  }
  return (correct/sum(matrix))
}

m <- table(c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(1,1,1,1))
head(taData)

taData[taData$breakfast==1 && taData$lunch==1,]


# Binary buketing - below are different ways of converting ratings to binary values. 

# printInfo prints out how many good/bad values exist in a given column
printInfo <- function(split, colname) {
  ngood <- nrow(taData[colname == 1,])
  goodperc <- ngood/nrow(taData)*100
  nbad <- nrow(taData[colname == 0,])
  badperc <- nbad/nrow(taData)*100
  cat("When bucketing rating ", split+1, " or higher as good and rating ", split, " or lower as bad, there are ", ngood, " good ratings (", goodperc, "%) and ", nbad, " bad ratings (", badperc, "%).", sep="")
  return (c(split, ngood, goodperc, nbad, badperc))
}


# the following code buckets ratings into good/bad with various split values
# creates a table binDF that shows how many are labeled good and bad
taData$ratingBinaryOne <- ifelse(taData$rating > 1, 1, 0)
one <- printInfo(1, taData$ratingBinaryOne)

taData$ratingBinaryTwo <- ifelse(taData$rating > 2, 1, 0)
two <- printInfo(2, taData$ratingBinaryTwo)

taData$ratingBinaryThree <- ifelse(taData$rating > 3, 1, 0)
three <- printInfo(3, taData$ratingBinaryThree)

taData$ratingBinaryFour <- ifelse(taData$rating > 4, 1, 0)
four <- printInfo(4, taData$ratingBinaryFour)

binDF <- rbind(one, two, three, four)
colnames(binDF) <- c('maxBadVal', 'Good', 'Good %', 'Bad', 'Bad %')

binDF



# the code below checks each meal with each split value and puts the result into the dataframe tout
num <- c()
aicval <- c()
meal <- c()
i <- 1
for (numcheck in c('ratingBinaryOne', 'ratingBinaryTwo', 'ratingBinaryThree', 'ratingBinaryFour')) {
  for (thisone in c('breakfast','lunch','dinner')) {
    num <- c(num, i)
    meal <- c(meal, thisone)
    aicval <- c(aicval, runLogit(taData, numcheck, c(thisone)))
  }
  i <- i+1
}
tout <- rbind(num, meal, aicval)
colnames(tout) <- c(1:12)
tout


# bag of words stuff
library(tm)
library(SnowballC) 
library(wordcloud)

corpus <- Corpus(VectorSource(taData$reviewText))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeNumbers)
dtm <- DocumentTermMatrix(corpus)
inspect(dtm[1:5, 1:20])
dtm <- removeSparseTerms(dtm, 0.99)

freq <- colSums(as.matrix(dtm))
head(freq)

wordcloud(names(freq), freq, min.freq=300,colors=brewer.pal(9,"Spectral"))


binary <- weightBin(dtm)
binarys <- removeSparseTerms(binary, 0.99)

# regex stuff
head(taData)

updateTerm <- function(df, regexp) {
  df$containsTerm <- ifelse(grepl(regexp,df$reviewText), 1, 0)
  return (df)
}

# OLS of standardized logs
taData$ratingLog <- log(taData$rating)
taData$scaledRating <- scale(taData$ratingLog)
olsReg <- lm(scaledRating ~ containsTerm, data = taData)
summary(olsReg)

# graph to see if normal distribution
library(ggplot2)
library(grid)
ggplot(taData, aes(x=scaledRating, fill = dollars))+geom_histogram()+
  theme_bw(base_size = 18)+
  xlab("Log of Rating")+
  ylab("Count")+
  theme(legend.position = "right",axis.title.y = element_text(vjust=-0.05),axis.text.x = element_text( vjust=0.6, hjust = 0),plot.margin = unit(c(1, 1, 1, 1), "cm"))


# rating -> ratingBinary: 5->1, 1->0, 2-4->-1
taData$ratingBinary <- ifelse(taData$rating == 5, 1, ifelse(taData$rating == 1, 0, -1))
head(taData)$ratingBinary
middleGone <- taData[taData$ratingBinary > -1,]
head(middleGone)
nrow(middleGone)
nrow(taData)

middleGone <- updateTerm(middleGone, '([Cc]hild(ren)?)|[Kk]ids?')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))
head(middleGone)
cat("Reviews that mention kids tend to give lower ratings")

middleGone <- updateTerm(middleGone, 'free')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))

middleGone <- updateTerm(middleGone, 'customers?')
cat("Reviews that mention customer(s) tend to give lower ratings", "Mentioning customer(s) lowers the binary rating by 0.25", "AIC:", runLogit(middleGone, 'ratingBinary', c('containsTerm')), sep="\n")

middleGone <- updateTerm(middleGone, '[Ww]ines?')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))


middleGone <- updateTerm(middleGone, '[Gg]ood|[Gg]reat|[Ww]onder(ful)?|[Ee]xcellent|[Ff]antastic')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))

middleGone <- updateTerm(middleGone, '[Bb]ad|[Tt]errible|[Hh]orrible|[Aa]wful|[Oo]k(ay)?')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))

middleGone <- updateTerm(middleGone, 'steak')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))

middleGone <- updateTerm(middleGone, 'chicken')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))


middleGone <- updateTerm(middleGone, 'vibe')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))


middleGone <- updateTerm(middleGone, '([Gg]ood|[Gg]reat|[Ww]onder(ful)?|[Ee]xcellent|[Ff]antastic)')

middleGone <- updateTerm(middleGone, '([Bb]ad|[Tt]errible|[Hh]orrible|[Aa]wful|[Oo]k(ay)?|[Ss]low).service')
runLogit(middleGone, 'ratingBinary', c('containsTerm'))


library(mfx)
logitmfx(ratingBinary ~ containsTerm, data = middleGone)


nrow(taData[taData$rating == 1,])/nrow(taData)
nrow(taData[taData$rating == 2,])/nrow(taData)
nrow(taData[taData$rating == 3,])/nrow(taData)
nrow(taData[taData$rating == 4,])/nrow(taData)
nrow(taData[taData$rating == 5,])/nrow(taData)
