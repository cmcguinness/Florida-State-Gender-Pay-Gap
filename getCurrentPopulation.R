getCurrentPopulation <- function () {
  messyData <- read.xls("http://www.census.gov/population/age/data/files/2012/2012gender_table1.xlsx")
  
  # We assume this data is not going to change ever.  But just for safety's sake, let's look at a couple of
  # data items in it to see if it is what we expect...
  if (trim(as.character(messyData[6,1])) != ".Under 5 years") {
    print(messyData[6,1])
    stop("Data is not in right format.")
    
    return
  }
  
  if (as.numeric(as.character(messyData[10,5]))!=7.3) {
    print(messyData[10,5])
    stop("Data is not in right format.")
    return
  }
  
  # OK, hopefully still the same data ...
  yearBornLower <- 2012-seq(4,89,5)
  malePopulation <- as.numeric(as.character(messyData[6:23,4]))*1000
  femalePopulation <- as.numeric(as.character(messyData[6:23,6]))*1000
  
  data.frame(yearBornLower,malePopulation,femalePopulation)
}