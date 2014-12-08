getPopulationV2 <- function () {
  
  # Get the raw data from the census bureau.  This fill will change over time.
  messyData <- read.csv("http://www.census.gov/popest/data/national/asrh/2013/files/NC-EST2013-ALLDATA-R-File09.csv")
  
  # Get the values as of January
  messyData <- messyData[messyData$MONTH==1,]
  
  # Get rid of the total in the first line (AGE=999)
  messyData <- messyData[messyData$AGE != 999,]
  
  # Assume everyone of the same age is neatly in a year
  birthYear <- 2013- messyData$AGE
  males <- messyData$TOT_MALE
  females <- messyData$TOT_FEMAL
  
  data.frame(birthYear, males, females)
}

getMaleDistribution <- function(year, populationFrame) {
  total <- sum(populationFrame$males)
  populationFrame$males[populationFrame$birthYear == year] / total
}