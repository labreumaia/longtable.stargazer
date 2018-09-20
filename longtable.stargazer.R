##### Function to make stargazer compatible with longtable #####
#### Lucas de Abreu Maia ####
#### Department of Political Science ####
#### UCSD ####
#### lucasamaia.com ####
#### labreumaia@gmail.com ####

## Description
# This function simply makes stargazer compatible with the longtable 
# LaTeX environment
## Arguments
# ... - any argument to be passed to stargazer.
# float - logical. Whether or not the output of stargazer should be in 
# a float environment. This is useful if you want your table to have a 
# title and label associated with it.
# longtable.float - logical. Whether or not you want the longtable 
# itself to be  within a float environment.
# longtable.head - logical. If you want column headers to appear at 
# the top of every page.
# filename - character. An optional file path for the printed output. 
# If abcent, the output is printed to the console.

longtable.stargazer = function(..., float = T, longtable.float = F, 
  longtable.head = T, filename = NULL){
  # Capturing stargazer to hack it
  require(stargazer)
  res = capture.output(
    stargazer(..., float = float)
  )
  # Changing tabulare environment for longtable
    res = gsub("tabular", "longtable", res)
  # removing floating environment
  if(float == T & longtable.float == F){
    res[grep("table", res)[1]] = res[grep("longtable", res)[1]]
    # Removing extra longtable commands
    res = res[-grep("longtable", res)[2]]
    res = res[-length(res)]
  }
  # Adding page headings
  if(longtable.head == T){
    res = c(res[1:which(res == "\\hline \\\\[-1.8ex] ")[1] - 1], "\\endhead", res[which(res == "\\hline \\\\[-1.8ex] ")[1]:length(res)])
  }
  # Exporting
  cat(res, sep = "\n")
  # Exporting
  if(!is.null(filename)){
    cat(res, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
      collapse = ""))
  }else{
    cat(res, sep = "\n")
  }
}


## Example
# Generating data
set.seed(62442)
library(MASS)
k = 25
A = matrix(runif(k^2) * 2 - 1, nr = k)
Sigma = t(A) %*% A
K = as.data.frame(mvrnorm(28, rnorm(k), Sigma))

# Running model
m = lm(K)

# Exporting
longtable.stargazer(m, title = "Output example", align = T, 
  filename = "output/longtable.stargazer.example.table.tex")