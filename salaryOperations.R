# #############################################################################
# salaryByClass:
#
# Returns a list of lists. For each job classification, return a list of
# salaries paid to males with that title, a list of salaries paid to females
# with that title, and a list of salaries paid to people we cannot determine
# but with that title.
#
# This gets used by subsequent analyses, and is basically just a restructuring
# of the data to prepare it for downstream work.
# #############################################################################

salaryByClass <- function (in_salaries) {
  num <- 0
  
  titles <- character()
  males <- list()
  females <- list()
  others <- list()
  
  # We want to get the list of unique job titles, and the unique() function
  # will do that for us...
  uniqueTitles <- unique(in_salaries$Class.Title)
  
  # cat("A total of ", length(uniqueTitles), " titles\n")
  
  # For each different title in the payroll data...
  for (title in uniqueTitles) {
    # Not sure why, but some titles are blank, so skip them...
    if (title != "") {
      
      num <- num + 1
      
      # Add the title to the list ...
      titles[num] <- title
      
      # Create three vectors to hold the array of salaries
      temp_males <- numeric()
      temp_females <- numeric()
      temp_other <- numeric()
      
      # Find all employees which have this title
      for (emp in which(in_salaries$Class.Title == title)) {
        
        # And add their salary data to the appropriate vector
        if (in_salaries$gender[emp] == "M") {
          temp_males <- c(temp_males, in_salaries$nSalary[emp])
        } else if (in_salaries$gender[emp] == "F") {
          temp_females <- c(temp_females, in_salaries$nSalary[emp])
        } else {
          temp_other <- c(temp_other, in_salaries$nSalary[emp])
        }
      }
      
      # Save the salary vectors into our lists
      males[[num]] <- temp_males
      females[[num]] <- temp_females
      others[[num]] <- temp_other
    }
  }
  
  # Build the final list of lists...
  salByClass <- list(titles,males,females,others)
  names(salByClass) <- c("titles","males", "females", "others")
  return(salByClass)
}

# #############################################################################
# computeBias
#
# Using the data as preprocessed by salaryByClass, compute the "normalized"
# salary for each man and woman so long as the job class has at least one
# man and one woman in it.
#
# The normalization logic consists of dividing the persons salary by the
# average salary in the class.  Thus, a score of 1 means the person is
# earning exactly the average paid to all workers.  A score of .9 means
# the person is earning 90 cents on the dollar, so to speak.  A score of
# 1.1 means the person is earning $1.10 when the average person is earning
# $1.00.
#
# This is useful because we typically express bias in terms of how much
# an average woman makes when an average man makes $1.  So, in this approach
# we normalize everyone's salaries as if the average pay in each class is $1
# #############################################################################

computeBias <- function (salaryByClass) {
  
  # These will hold the list of normalized salaries for men and women
  mens <- numeric()
  womens <- numeric()
  
  orphanMen <<- numeric()
  orphanWomen <<- numeric()
  
  # How many job classifications have either all men or all women?
  noMatches <<- 0
  
  # Look at each title ....
  for (i in seq_along(salaryByClass$titles)) {
    # If there is at least one man and one woman with that title
    if ((length(salaryByClass$males[[i]]) > 0 )  & (length(salaryByClass$females[[i]]) > 0)) {
      # compute the average salary of all workers
      average <- mean(c(salaryByClass$males[[i]], salaryByClass$females[[i]], salaryByClass$others[[i]]))
      # for all the women ...
      for (salary in salaryByClass$females[[i]]) {
        # add their "normalized" salary to the female list
        womens <- c(womens, salary/average)
      }
      # for all the men
      for (salary in salaryByClass$males[[i]]) {
        # add their normalized salary to the male list
        mens <- c(mens, salary/average)
      }
      
      # while we use the unknowns' salaries to compute the average, we do not do anything
      # with their salary specifically ...
    } else {
      # We'll just lump all "orphan" men and women into a group for further analysis
      noMatches <<- noMatches + 1
      for (salary in salaryByClass$females[[i]]) {
        orphanWomen <<- c(orphanWomen, salary)
      }
      for (salary in salaryByClass$males[[i]]) {
        orphanMen <<- c(orphanMen, salary)
      }
    }
  }
  
  # Make the list ... (remember, mens and womens are of unequal length, so this cannot be a data frame)
  biasData <- list(mens,womens)
  names(biasData) <- c("Mens", "Womens")
  # and give it back!
  return(biasData)
}


# #############################################################################
# computeLopsided
#
# Other analysis shows that most job classifications have fairly even pay within
# job classifications.  What about a bias where male dominated classifications
# get paid more than women? So even if men in the "female" jobs are paid the
# same as women, they're getting paid less to do "women's work"?
#
# Let's run the numbers ...
# #############################################################################
computeLopsided <- function (sbc) {
  
  threshold <- 0.666    # Look for classes that have more than two thirds men or women
  
  # These will hold the list of normalized salaries for men and women
  mens <- numeric()
  womens <- numeric()
  
  # Look at each title ....
  for (i in seq_along(sbc$titles)) {
    # If there is at least one man and one woman with that title
    numMen <- length(sbc$males[[i]])
    numWomen <- length(sbc$females[[i]])
    
    if (is.na(numMen)) numMen = 0
    if (is.na(numWomen)) numWomen = 0
    
    total <- numMen + numWomen
    
    pcMen <- numMen / total
    pcWomen <- numWomen / total
    
    if (is.nan(pcMen)) pcMen = 0
    if (is.nan(pcWomen)) pcWomen =0

    
    if (pcWomen > threshold) {
      for (salary in sbc$females[[i]]) {
        womens <- c(womens, salary)
      }
      for (salary in sbc$males[[i]]) {
        womens <- c(womens, salary)
      }
      for (salary in sbc$others[[i]]) {
        womens <- c(womens, salary)
      }
    }
    
    if (pcMen > threshold) {
      for (salary in sbc$females[[i]]) {
        # add their "normalized" salary to the female list
        mens <- c(mens, salary)
      }
      for (salary in sbc$males[[i]]) {
        # add their normalized salary to the male list
        mens <- c(mens, salary)
      }
      for (salary in sbc$others[[i]]) {
        # add their normalized salary to the male list
        mens <- c(mens, salary)
      }
    }
    
  }
  
  # Make the list ... (remember, mens and womens are of unequal length, so this cannot be a data frame)
  biasData <- list(mens,womens)
  names(biasData) <- c("Mens", "Womens")
  # and give it back!
  return(biasData)
}