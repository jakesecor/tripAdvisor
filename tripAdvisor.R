# Trip Advisor

taData <- read.csv(file= "C:/Users/Jake/OneDrive/School Work/Senior Spring/Data Analytics/Data Sets/taAllCities.csv", head=TRUE, sep=",")

taData <- read.csv(file= "C:\\Users\\Elias\\Documents\\BC Junior Year\\Analytics & Business Intelligence\\taAllCities.csv",head =TRUE, sep=",")

head(taData)

# poisson to see significance of independent variables
poisson <- glm(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, family="poisson", data =taData)
summary(poisson)


# marginal effects of independent variables
library(mfx)

poissonmfx(rating ~ service + vibe +  desert + bathroom + drink + cost + music + location + parking + lunch + breakfast + dinner + totalReviewsOfReviewer + avgHelpfulnessOfReviewer, data =taData)
