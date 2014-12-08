

getNamesSimple <- function () {
  if (file.exists("firstnames.RData")) {
    load("firstnames.RData", .GlobalEnv)
    return(invisible(firstNames))
  }
  
  names <- character(0)
  males <- integer(0)
  females <- integer(0)
  
  for (year in 1945:2013) {
    print(year)
    fname <- paste0("SSA/yob", year, ".txt")
    yeardata <- read.csv(fname, stringsAsFactors = FALSE, header=FALSE, col.names=c("name", "sex", "count"))
    for (i in seq_along(yeardata$name)) {
      name <- tolower(yeardata$name[i])
      namepos <- match(name, names)
      if (is.na(namepos)) {
        names <- c(names,name)
        males <- c(males, 0)
        females <- c(females,0)
        namepos <- length(names)
      }
      
      if (tolower(yeardata$sex[i]) == "f") {
        females[namepos] = females[namepos] + yeardata$count[i]
      } else {
        males[namepos] = males[namepos] + yeardata$count[i]
      }
    } # end for i
  } # end for year
  
  firstNames <<- data.frame(names,males,females)
  invisible(firstNames)
}

getGender <- function (firstname) {
  firstname <- tolower(firstname)
  pos <- match(firstname, firstNames$names)
  if (is.na(pos)) {
    # is there a space in the name? Try the part before the space; it may be a middle name afterwards
    firstnames <- strsplit(firstname,split=" ",fixed=TRUE)[[1]]
    if (length(firstnames) > 1)
    {
      for (n in firstnames) {
        namematch <- getGender(n)
        if ((namematch == "M") | (namematch=="F")) {
          return(namematch)
        }
      }
      
    }
    # is there a dash in the name? Try the part before the space; it may be a middle name afterwards
    firstnames <- strsplit(firstname,split="-",fixed=TRUE)[[1]]
    if (length(firstnames) > 1)
    {
      for (n in firstnames) {
        namematch <- getGender(n)
        if ((namematch == "M") | (namematch=="F")) {
          return(namematch)
        }
      }
    }
    
    # OK, still no idea ....
    return("U")
  }
  
  mcount <- firstNames[pos,"males"]
  fcount <- firstNames[pos,"females"]
  ratio <- mcount/(mcount+fcount)
  
  if (ratio >= .90) {
    return("M")
  }
  
  if (ratio <= .10) {
    return("F")
  }
  
  "A"   # Androgenous
}