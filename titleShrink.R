titleShrink <- function (salaries) {
  salaries$Class.Title <- gsub("- ...(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub("- ....(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub("-...(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub("-....(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" III(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" II(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" I(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" IV(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" V(\\s*)$","", salaries$Class.Title)
  
  return(salaries)
}