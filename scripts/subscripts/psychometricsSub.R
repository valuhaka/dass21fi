# Creates a table of statistics on the DASS questions and subscales (D, A, S, GD),
# including range, n, mean, median, SD, skew, kurtosis, alpha, omegas and SEm.

#------------------------------------------------------------------------------#

psychometricsTable <- function(dass, scaleLocs, finnish) {
  
  # If chosen, remove non-Finnish answers.
  
  if (finnish == 1) { 
    for (k in 1:2) dass[[k]]   <- dass[[k]][suomi[[k]], ]
    for (k in 1:2) scales[[k]] <- scales[[k]][suomi[[k]], ]
  } else {
    for (k in 1:2) dass[[k]]   <- dass[[k]][!suomi[[k]], ]
    for (k in 1:2) scales[[k]] <- scales[[k]][!suomi[[k]], ]
  }
  
  # Calculate the Ns.
  
  N      <- lapply(dass, FUN = dim)
  N[[1]] <- N[[1]][1]
  N[[2]] <- N[[2]][1]
  
  
  ## Calculate the reliabilities. ##
  
  rels <- list(data.frame(rep(NA, 25),
                          rep(NA, 25),
                          rep(NA, 25)),
               data.frame(rep(NA, 25),
                          rep(NA, 25),
                          rep(NA, 25)))
  
  for (k in 1:2) {
    for (i in 1:3) {
      rel            <- dass[[k]]            %>% 
        select(scaleLocs[, i])   %>% 
        psych::reliability()
      
      rels[[k]][i, ] <- rel$result.df[1:3]  %>% 
        round(digits = 2)
    }
    rel            <- dass[[k]]             %>% 
      psych::reliability()
    
    rels[[k]][4, ] <- rel$result.df[1:3]    %>% 
      round(digits = 2)
    
    colnames(rels[[k]]) <- c("omegah", "alpha", "omegatot")
    
    rels[[k]]      <- rels[[k]] %>% relocate("alpha", 1)
    
    rm(rel)
  }
  
  ## SEm = std(sum(X)) * sqrt(1 - R)
  
  SEm <- data.frame(rep(NA, 25), rep(NA, 25))
  
  for (k in 1:2) {
    for (i in 1:3) {
      SEm[i, k] <- dass[[k]]            %>% 
        select(scaleLocs[, i])          %>%
        rowSums()                       %>%
        sd(., na.rm = TRUE) * sqrt(1 - rels[[k]][i, 3]) # omega_tot
    }
    SEm[4, k] <- dass[[k]]              %>%
      rowSums()                         %>%
      sd(., na.rm = TRUE) * sqrt(1 - rels[[k]][4, 2])   # omega_h
  }
  
  ## Into a data frame ##
  
  df            <- list()
  
  for (k in 1:2) {
    df[[k]]     <- dass[[k]]            %>% 
      cbind(scales[[k]], .)                             %>%
      psych::describe(type = 2)                         %>% 
      data.frame()                                      %>% 
      select(mean, median, sd, skew, kurtosis)       %>% 
      round(digits = 3)                                 %>% 
      add_column(rels[[k]], SEm[, k], .after = "kurtosis")
    df[[k]]$median <- df[[k]]$median %>%
      round(digits = 0)
  }
  
  ## Format ##
  
  ranges                        <- c(rep("0—21", 3), "0—63", rep("0—7", 21))
  
  df            <- data.frame(rownames(df[[1]]),
                              ranges,
                              df[[1]], 
                              df[[2]])
  
  names(df)     <- c("Qs", "Range", 
                     
                     "Working-aged.m",     "Working-aged.md", 
                     "Working-aged.SD",
                     "Working-aged.skew",  "Working-aged.kurtosis", 
                     "Working-aged.α",     "Working-aged.ω_h",
                     "Working-aged.ω_tot", "Working-aged.SEm", 
                     
                     "Older.m",    "Older.md", 
                     "Older.SD", 
                     "Older.skew",  "Older.kurtosis", 
                     "Older.α",     "Older.ω_h", 
                     "Older.ω_tot", "Older.SEm")
  
  niceDf        <- nice_table(df, title = c("Table 2.", 
                                            "Sample distribution and reliability statistics in the two cohorts in 2022"),
                              separate.header = TRUE,
                              note = "SEm = SD × √(1 - reliability). ωtot was used for the subscales D, A and S. ωh was used for GD (Zinbarg et al., 2005). Skew estimated as G1, kurtosis as G2. For details, see Joanes & Gill (1998).",
                              width = 1)
  
  psychometricsRes <- list(raw = df, formattedDf <- niceDf, ns <- N)
  return(psychometricsRes)
}