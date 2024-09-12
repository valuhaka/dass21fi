# This script creates a table displaying the normative data for
# DASS-21 in the Finnish working population.

library(tidyverse)
library(rempsyc)

#------------------------------------------------------------------------------#


## Get the data ##

setwd(".../data")              #  YOUR DIRECTORY HERE"


# Choose only those that answered in Finnish. #

load("dass.Rdata")
for (k in 1:2) dass[[k]] <- dass[[k]][suomi[[k]],]


#------------------------------------------------------------------------------#


## Define a function to generate percentile ranks. ##

generatePRs <- function(x, len){

  name  <- names(x)[1]
    
  x     <- x %>% unlist() %>% c()
  reps  <- factor(x) %>% levels() %>% as.numeric()
  reps  <- reps[reps %% 1 == 0]                    # integers only
  
  df    <- data.frame(rep("", len),
                      rep("", len))
  
  PRs   <- {(rank(x) / length(x)) * 100} %>% round(digits = 1)
  
  for (i in 1:len) {
    ind  <- match(reps[i], x)
    df[i, ] <- c(x[ind], PRs[ind])
  }
  
  names(df) <- c(name, paste("PR", sep = "_", name))
  return(df)
}


#------------------------------------------------------------------------------#

# Apply the function. ##

norms <- list()

for (k in 1:2) {
  norms[[k]]        <- lapply(scales[[k]], len = 93, FUN = generatePRs)
  norms[[k]]        <- cbind(norms[[k]][[1]], norms[[k]][[2]],
                        norms[[k]][[3]], norms[[k]][[4]])
  names(norms[[k]]) <- c("D", "PR_D", "A", "PR_A", "S", "PR_S", "DAS", "PR_DAS")
}


## Format the table. ##

niceNormsYoung <- rempsyc::nice_table(norms[[1]], title = c("Table 1",
                                                  "Normative values for the DAS subscales and the General Distress scale among working-aged Finns."))
niceNormsOld   <- rempsyc::nice_table(norms[[2]], title = c("Table 2",
                                                  "Normative values for the DAS subscales and the General Distress scale among older Finns."))

#------------------------------------------------------------------------------#


## Display ##

print(niceNormsYoung)
print(niceNormsOld)



##  Save.  ##

flextable::save_as_docx(niceNormsYoung, path = paste0(getwd(), "/normsYoung.docx"))
flextable::save_as_docx(niceNormsOld, path = paste0(getwd(), "/normsOld.docx"))
