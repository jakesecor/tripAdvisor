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
