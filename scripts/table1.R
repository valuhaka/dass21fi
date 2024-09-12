library(tidyverse)
library(rempsyc)
library(officer)
library(table1)

finnishOnly     <- 1  #  Only include Finnish respondents.

#------------------------------------------------------------------------------#

## Load the data ##

setwd(".../data")              #  YOUR DIRECTORY HERE"
load("chosenData.Rdata")

# If chosen, include Finnish responses only.

if (finnishOnly) for (k in 1:2) data[[k]] <- data[[k]][data[[k]]$suomi, ]


#------------------------------------------------------------------------------#

## Immigrant background

nImmigrants <- (data[[1]]$isanmaa == 2 & data[[1]]$aidinmaa == 2) %>% sum(na.rm = TRUE)

#------------------------------------------------------------------------------#

## Working for the municipality ##

municipality                       <- data$tyonantaja[data$tyonantaja == 1] %>% 
                                                   sum(na.rm = TRUE)

## Choose variables ##

chosenVars      <- list()

chosenVars[[1]] <- data[[1]] %>% dplyr::select(    ## All in 2022.
  sukupuoli,                                       # Gender.
  b_ika,                                           # Age.
  koulutus,                                        # Education.
  siviilisaaty,                                    # Marital status.
)
chosenVars[[2]] <- data[[2]] %>% dplyr::select(
  i1,                                              # Gender in 2022.
  h_ika,                                           # Age in 2017.
  k4,                                              # Education 2000-2002
  i3,                                              # Marital status in 2022.
)

chosenVars[[2]]$h_ika <- chosenVars[[2]]$h_ika + 5 # Age in 2022.

for (i in 1:2) colnames(chosenVars[[i]]) <- c("Gender", "Age", "Education", 
                                           "Marital_status")



#------------------------------------------------------------------------------#

## Making tables. ##


# Means. Naming. Refactoring.

chosenVars[[1]]$Education <- chosenVars[[1]]$Education %>% 
  cut(c(0, 1.5, 3.5, 7),
      c("Middle school or below  (1–2)", 
        "High school or equivalent (3–4)", 
        "University or equivalent (5–8)"))

chosenVars[[2]]$Education <- chosenVars[[2]]$Education %>% 
  cut(c(0, 2.5, 4.5, 6),
      c("Middle school or below  (1–2)", 
        "High school or equivalent (3–4)", 
        "University or equivalent (5–8)"))

for (k in 1:2) {
  chosenVars[[k]]$Gender <- factor(chosenVars[[k]]$Gender)
  levels(chosenVars[[k]]$Gender) <- c("Men", "Women")
  
  chosenVars[[k]]$Marital_status   <- chosenVars[[k]]$Marital_status %>% 
    cut(c(0, 1.5, 3.5, 6),
        c("Single", "Married or cohabiting", "Divorced or widowed"))
}

## Format the table. 

tableOne      <- list()

for (k in 1:2) tableOne[[k]] <- table1::table1(~ Age + Education + Marital_status 
                                  | Gender, data = chosenVars[[k]]) 




# Format into one table.

niceTableOne  <- cbind(tableOne[[1]] %>% data.frame %>% 
                       
                       add_row(X. = "Missing", Men = "0 (0%)",  # Report the missing
                                    Women = "0 (0%)",           # despite no one
                                    Overall = "0 (0%)",         # missing.
                                    .after = 4),                
                       
                       tableOne[[2]] %>% data.frame)

niceTableOne  <- niceTableOne[,-5]        # Remove the duplicate column "X.1".

# Fix the column names.

cnames                 <- colnames(niceTableOne)[2:4]
cnames                 <- c(paste0("Working-age cohort (ages 24–45).", cnames), 
                            paste0("Older cohort (ages 60–82).", cnames))
colnames(niceTableOne) <- c("X", cnames)


# Make the table

niceTableOne <- nice_table(niceTableOne, separate.header = TRUE,
              title = c("Table 1", 
              "Demographic variables in the two cohorts."))

#------------------------------------------------------------------------------#

## Save. ##

# Note: the follwing puts the table in your data folder in a .docx file.

#flextable::save_as_docx(niceTableOne, path = paste0(getwd(), '/table1.docx'))