library(tidyverse)
library(rempsyc)
library(officer)

## This script is used to compare the prevalence of DASS-21 symptoms between groups.

backupOptions <- options()

#------------------------------------------------------------------------------#

## Get the data. ##

setwd(".../data")              #  YOUR DIRECTORY HERE"
load("dass.Rdata")

# Get gendered data.

load("chosenData.Rdata")
dass[[1]]$gender     <- data[[1]]$sukupuoli
dass[[2]]$gender     <- data[[2]]$i1

#------------------------------------------------------------------------------#

## Create the table. ##

# ("Scales" contains the sumscores from each scale in each cohort.) 

df <- data.frame(matrix(ncol = 6, 
                        nrow = 4))

# First the younger cohort.

df[, 1] <- scales[[1]][dass[[1]]$gender == 1, ] %>% 
  lapply(FUN=mean) %>% 
  unlist %>% 
  round(digits = 1)
df[, 2] <- scales[[1]][dass[[1]]$gender == 2, ] %>% 
  lapply(FUN=mean) %>% 
  unlist %>% 
  round(digits = 1)
df[, 3] <- scales[[1]] %>% 
  lapply(FUN=mean) %>% 
  unlist %>% 
  round(digits = 1)

# Then the older one.

df[, 4] <- scales[[2]][dass[[1]]$gender == 1, ] %>% 
  lapply(FUN=mean) %>% 
  unlist %>% 
  round(digits = 1)
df[, 5] <- scales[[2]][dass[[1]]$gender == 2, ] %>% 
  lapply(FUN=mean) %>% 
  unlist %>% 
  round(digits = 1)
df[, 6] <- scales[[2]] %>% 
  lapply(FUN=mean) %>% 
  unlist %>% 
  round(digits = 1)

## T-tests (nonequal variances).

pTable <- data.frame(matrix(ncol = 2, 
                            nrow = 4))

for (i in 1:4) {
  
  # Younger cohort
  
  chosenMen     <- scales[[1]][dass[[1]]$gender == 1, i]
  chosenWomen   <- scales[[1]][dass[[1]]$gender == 2, i]
  p             <- t.test(chosenMen, chosenWomen,
                  var.equal = FALSE)$p.value
  pTable[i, 1]  <- p.adjust(p, method = "bonferroni", 8)
  
  # Older cohort
  
  chosenMen     <- scales[[2]][dass[[2]]$gender == 1, i]
  chosenWomen   <- scales[[2]][dass[[2]]$gender == 2, i]
  p             <- t.test(chosenMen, chosenWomen,
                          var.equal = FALSE)$p.value
  pTable[i, 2]  <- p.adjust(p, method = "bonferroni", 8)
  
}

## Format.

# Merge tables.

df <- add_column(pTable[, 1], .data = df, 
                 .after = 3)
df <- add_column(pTable[, 2], .data = df, 
                 .after = 7)

# Labels.

colnames(df) <- c("Working-age cohort.Total (n = 3206)",
                  "Working-age cohort.Men (n = 640)",
                  "Working-age cohort.Women (n = 2566)",
                  "Working-age cohort.p",
                  "Older cohort.Total (n = 5736)",
                  "Older cohort.Men (n = 1046)",
                  "Older cohort.Women (n = 4690)",
                  "Older cohort.p")

df           <- cbind(Scale = c("Depression", "Anxiety", "Stress", "Total (GD)"),
                      df)


## Wrap up.

nDigits <- function(x) formatC(x, digits = 3)

niceDf       <- rempsyc::nice_table(df,
                           title = c("Table 6.",
                                     "Mean levels of depression, anxiety, stress and general distress among the men and women of the two cohorts."),
                           separate.header = TRUE,
                           note = "p-values from Welch's unequal variances T-test, with Bonferroni correction (k = 8).",
                           col.format.custom = c(2, 3, 4, 6, 7, 8), format.custom = "nDigits")

print(niceDf)

# flextable::save_as_docx(niceDf, path = paste0(getwd(), "/prevalenceTable.docx"))

options(backupOptions)