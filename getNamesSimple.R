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
    # print(year)
    
    # Read in the data for the year
    fname <- paste0("SSA/yob", year, ".txt")
    yeardata <- read.csv(fname, stringsAsFactors = FALSE, header=FALSE, col.names=c("name", "sex", "count"))
    for (i in seq_along(yeardata$name)) {
      name <- tolower(yeardata$name[i])
      
      # See if we already have the name in our list
      namepos <- match(name, names)
      
      # If not, add it with 0,0 counts 
      if (is.na(namepos)) {
        names <- c(names,name)
        males <- c(males, 0)
        females <- c(females,0)
        namepos <- length(names)
      }
      
      # If the name is for females, add the count to the female count
      if (tolower(yeardata$sex[i]) == "f") {
        females[namepos] = females[namepos] + yeardata$count[i]
      } else {
        # Otherwise, add it to the male count...
        males[namepos] = males[namepos] + yeardata$count[i]
      }
    } # end for i
  } # end for year
  
  # Save the list of names in the global environment (I know, tacky)
  firstNames <<- data.frame(names,males,females)
  invisible(firstNames)
}

# #############################################################################
# getGender:
#
# Given a name, tell us whether it's male, female, both, or I don't know ...
# #############################################################################
getGender <- function (firstname) {
  firstname <- tolower(firstname)
  
  # Find it in our list of names...
  pos <- match(firstname, firstNames$names)
  
  # Did we not find it?
  if (is.na(pos)) {
    # Let's see what we can do to transform the name into something we can find
    
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
    
    # OK, still cannot find the name
    return("U")
  }
  
  # We found the name, now lets see how many male vs. female babies were given
  # this name
  
  mcount <- firstNames[pos,"males"]
  fcount <- firstNames[pos,"females"]
  
  # What fraction of the names are male (note that 1-ratio is the fraction that are female)
  ratio <- mcount/(mcount+fcount)
  
  # We use 90% as the "certainty" limit
  if (ratio >= .90) {
    return("M")
  }
  
  # This is logically the same as >= 90% female
  if (ratio <= .10) {
    return("F")
  }
  
  # Well, it could be either.  Oh well...
  return("A")   # Androgenous
}