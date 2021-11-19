### Introduction to Biocomputing — Exercise 9 — Matthew Doktorczyk

dir <- list.files("../data", full.names = TRUE)
dir
fileCOV <- c()
usable <- c()
coeffOfVar <- function(dir){
  usable <- dir[grep(".csv", dir)]
  for(i in 1:length(usable)){
    fil <- read.csv(file = usable[i], header = TRUE, stringsAsFactors = FALSE)
    y <- sapply(fil,class)
    z <- colnames(fil)
    print(paste("For which column would you like to calculate the coefficient of variation?"))
    print("Oly select a column of integers or numerics.")
    x <- as.name(readline())
    if(length(fil[[x]]) < 50){
      print("In order to be most accurate, a coefficient of variation should have at least 50 entries, and this one does not.")
      question <- (readline("Would you still like to continue? Yes or no: "))
      if(question == "Yes"){
        mean <- mean(fil[[x]], na.rm = TRUE)
        std <- sd(fil[[x]], na.rm = TRUE)
        cov <- std/mean
        fileCOV <- c(fileCOV, cov)
        i = i+1
      }else {
        i = i+1
      }
    }
    else {
    mean <- mean(fil[[x]], na.rm = TRUE)
    std <- sd(fil[[x]], na.rm = TRUE)
    cov <- std/mean
    fileCOV <- c(fileCOV, cov)
    i=i+1
    }
  }
  return(fileCOV)
}
coeffOfVar(dir)