library(reshape)

salaryByClass <- function (in_salaries) {
  num <- 0
  
  titles <- character()
  males <- list()
  females <- list()
  others <- list()
  
  uniqueTitles <- unique(in_salaries$Class.Title)
  
#cat("A total of ", length(uniqueTitles), " titles\n")
  for (title in uniqueTitles) {
    if (title != "") {

      num <- num + 1
#       if ((num %% 100) == 0) {
#         cat("Loop #",num,"\n")
#       }
      titles[num] <- title
      temp_males <- numeric()
      temp_females <- numeric()
      temp_other <- numeric()
      
      for (emp in which(in_salaries$Class.Title == title)) {
        if (in_salaries$gender[emp] == "M") {
          temp_males <- c(temp_males, in_salaries$nSalary[emp])
        } else if (in_salaries$gender[emp] == "F") {
          temp_females <- c(temp_females, in_salaries$nSalary[emp])
        } else {
          temp_other <- c(temp_other, in_salaries$nSalary[emp])
        }
      }
      males[[num]] <- temp_males
      females[[num]] <- temp_females
      others[[num]] <- temp_other
    }
  }
  
  salByClass <- list(titles,males,females,others)
  names(salByClass) <- c("titles","males", "females", "others")
  return(salByClass)
}

computeBias <- function (salaryByClass) {
#  if (file.exists("biasData.RData")) {
#    load("biasData.RData", .GlobalEnv)
#    return(biasData)
#  }
  
  mens <- numeric()
  womens <- numeric()
  
  for (i in seq_along(salaryByClass$titles)) {
    if ((length(salaryByClass$males[[i]]) > 0 )  & (length(salaryByClass$females[[i]]) > 0)) {
      average <- mean(c(salaryByClass$males[[i]], salaryByClass$females[[i]], salaryByClass$others[[i]]))
      for (salary in salaryByClass$females[[i]]) {
        womens <- c(womens, salary/average)
      }
      for (salary in salaryByClass$males[[i]]) {
        mens <- c(mens, salary/average)
      }
    }
  }
  
  biasData <- list(mens,womens)
  names(biasData) <- c("Mens", "Womens")
#  save(biasData,file="biasData.RData")
  return(biasData)
}