#installing and loading required pacakges for the project 

if (!require(tidyverse)) { 
  install.packages("tidyverse") 
  library(tidyverse) 
}

if (!require(rmarkdown)) { 
  install.packages("rmarkdown") 
  library(rmarkdown) 
}

if (!require(knitr)) { 
  install.packages("knitr") 
  library(knitr) 
}
