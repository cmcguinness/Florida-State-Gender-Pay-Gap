# #############################################################################
# getNamesSimple: 
#
# Get the list of first names from the Social Security Admin's
# website to build up the database of male/femail names
#
# More details about the baby names can be found on this page:
#   http://www.ssa.gov/oact/babynames/limits.html
# #############################################################################
getNamesSimple <- function () {
  # We cache the names in a file so that subsequent runs of this go faster
  if (file.exists("firstnames.RData")) {
    load("firstnames.RData", .GlobalEnv)
    return(invisible(firstNames))
  }
  
  # Do we have the downloaded data?
  if (! file.exists("SSA")) {
    # No, so let's download it ...
    download.file("http://www.ssa.gov/oact/babynames/names.zip", "names.zip")
    # Create the directory
    dir.create("SSA")
    # Expand the files into it ...
    unzip("names.zip", exdir = "SSA")
    # Remove the downloaded file
    unlink("names.zip")
  }
  
  # Hold the data as we build it in vectors...
  names <- character(0)
  males <- integer(0)
  females <- integer(0)
  
  # We look at names from 1945 to 2013.  The data goes back a lot further
  # than that, though, so 
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