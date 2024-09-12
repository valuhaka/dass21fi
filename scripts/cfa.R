# Investigate the structure of the DASS-21.

#------------------------------------------------------------------------------#

library(foreign)
library(lavaan)
library(lavaanPlot)
library(semTools)
library(tidyverse)
library(psych)
library(GPArotation)
library(corrplot)
library(rempsyc)
library(ggrepel)
library(cowplot)

backupOptions <- options()

#------------------------------------------------------------------------------#

dataFolder <- ".../data"    #  YOUR DIRECTORY HERE

# Choices.
# Note: running multiple subsections may cause bugs.

finnishOnly        <- TRUE     # Should only Finnish answers be included?

nFactors           <- FALSE     # Should the amount of factors be analysed?
figures1           <- FALSE    # Should the structure be plotted?
fitsTable          <- FALSE    # Should model fit indices be tabled?

loadings           <- FALSE    # Should the loadings be tabled?
figures2           <- FALSE    # Should the solutions be plotted?
residuals          <- FALSE    # Should the residual matrix of the bifactor  
                               # solution be plotted?

#------------------------------------------------------------------------------#

## Fetch the data and other necessary material.   ##

setwd(dataFolder)

load("dass.Rdata")           # Load the data.
load("cfaModels.Rdata")      # Load the models.
load("scaleLocs.Rdata")      # Load the scale locations among the questions.

qNames              <- paste0("Q", scaleLocs %>% unlist())

## If chosen, remove those that didn't answer in Finnish. ##

if (finnishOnly) for (k in 1:2) dass[[k]] <- dass[[k]][suomi[[k]], ]

#------------------------------------------------------------------------------#

## Number of Factors ##

if (nFactors == 1) {
  
  parallel      <-list()
  scree    <- list()
  
  for (k in 1:2) {
    parallel[[k]]  <- psych::fa.parallel(dass[[k]], 
                                  fm     = "wls",
                                  fa     = "fa",
                                  n.iter = 100,
                                  cor    = "poly")
  }
  
  # Visualise. Inspired by a post by user "jksakalauk" 
  # on Wordpress (2016).
  
  faObs     <- list()
  faSim     <- list()
  eigenData <- list()
  faPlot    <- list()
  
  for (k in 1:2) {
    
    # Observed values.
    
    faObs[[k]] <- data.frame(parallel[[k]]$fa.values,
                              c("Observed data"),
                              1:21)
    colnames(faObs[[k]]) <- c("Eigenvalues", "Type", "n")
    
    # Simulated values. Percentiles.
    
    percentile <- apply(parallel[[k]]$values, 2, function(x) quantile(x, .95))
    max        <- 4 * nrow(faObs[[k]])
    min        <- max - (nrow(faObs[[k]]) - 1)
    
    percentile <- percentile[min:max]
    
    # Simulated values. Choose.
    
    faSim[[k]] <- data.frame(percentile,
                     c("Simulated Data (95th percentile)"),
                     1:21)
    colnames(faSim[[k]]) <- c('Eigenvalues', 'Type', 'n')
    
    # Define a theme
    
    apatheme=theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border     = element_blank(),
            text=element_text(),
            legend.title     = element_blank(),
            legend.position  = "inside",
            legend.position.inside = c(.8, .9),
            axis.line.x = element_line(color='black'),
            axis.line.y = element_line(color='black'))
    
    # Merge, make plot.
    
    eigenData[[k]] <- rbind(faObs[[k]][1:8, ], faSim[[k]][1:8, ])
    
    faPlot[[k]] <- ggplot(eigenData[[k]],
                     aes(x     = n,
                         y     = Eigenvalues,
                         shape = Type,
                         label = Eigenvalues %>% round(digits = 1))) +
    scale_y_continuous(name   = "Eigenvalue") +
    scale_x_continuous(name   = "Factor Number", 
                       breaks = 1:8) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel() +
    geom_line() + 
    scale_shape_manual(values = c(16, 1)) +
    geom_vline(xintercept = parallel[[k]]$nfact, linetype = "dashed") +
    apatheme
  }
  
  # Plot.
  
  faPlotGrid <- cowplot::plot_grid(faPlot[[1]],
                                   faPlot[[2]],
                                   labels = c("a", "b"),
                                   ncol = 1,
                                   nrow = 2)
  
}

#------------------------------------------------------------------------------#

## Structural images of the models. ##

if (figures1 == 1) {
  
  par(mfrow = c(3,2))
  
  for (i in 1:5) {
  models[i] %>%
    lavaanify() %>%
    semPlot::semPaths(residuals = TRUE, bifactor = c("D", "A", "S"), 
                      layout = "tree3", label.cex = 2)
    title(main = c("a", "b", "c", "d", "e")[i], adj = 0)
  }
}

#------------------------------------------------------------------------------#

##  CFA  ##
  
# The parts below run the main confirmatory factor analyses.
  
#------------------------------------------------------------------------------#


# Create necessary variables.

quantities <- c("chisq", "df", "cfi",   # What to measure?
                   "tli", "rmsea", "srmr") 
result     <- list(list(), list())
modelFits  <- list(matrix(NA, 5, 6), 
                   matrix(NA, 5, 6))

#------------------------------------------------------------------------------#

for (k in 1:2) {
  for (i in 1:5 ) {
    if (i == 2) {                          # Which models are orthogonal?
      orth = FALSE
    } else {
      orth = TRUE
    }
    
    # Fit models and solve.
    
    result[[k]][[i]]  <- models[i] %>% paste() %>%
              lavaan::cfa(data = dass[[k]],
                    estimator = "WLSMV",
                    ordered = TRUE,
                    std.lv = TRUE,
                    orthogonal = orth)
    
    # Get fit indices.
    
    modelFits[[k]][i, ]   <- fitMeasures(result[[k]][[i]], quantities)
    
  }
    
  # Format the fit table.
  
  modelFits[[k]]           <- modelFits[[k]] %>% data.frame()
  colnames(modelFits[[k]]) <- quantities
  rownames(modelFits[[k]]) <- c(names(models))
  
  
  # Also format chisquare and chisquare/df.
  
  chisq <- modelFits[[k]][, 1] %>%           # Save for later.
        unlist() %>% as.vector()
  
  df    <- modelFits[[k]][, 2] %>%
        unlist() %>% as.vector()
  
  modelFits[[k]] <- round(modelFits[[k]], digits = 2)
  
  for (i in 1:5) {
    modelFits[[k]][i, 1]        <- paste0(
                                  round(modelFits[[k]][i, 1] %>% as.numeric, digits = 0), " (",
                                  modelFits[[k]][i, 2], ")")
  }
  
  colnames(modelFits[[k]])[2] <- "chisq/df"
  modelFits[[k]][, 2]         <- chisq / df 
  modelFits[[k]][i, 2]        <- modelFits[[k]][i, 2] %>% unlist() %>% 
                                  as.numeric() %>% round(digits = 2)

}

# Find the interfactor correlations from the three-factor solution.

interFactorCorrs      <- list()
interFactorCorrs[[1]] <- lavInspect(result[[1]][[2]], what = "est")$psi %>% cov2cor()
interFactorCorrs[[2]] <- lavInspect(result[[2]][[2]], what = "est")$psi %>% cov2cor()
  
#------------------------------------------------------------------------------#

## Tables.

if (fitsTable == 1) {
  
  modelFitsTable        <- data.frame(modelFits[[1]], modelFits[[2]])
  
  modelFitsTable[, 2]   <- modelFitsTable[, 2] %>% as.numeric() %>% round(digits = 2)
  modelFitsTable[, 8]   <- modelFitsTable[, 8] %>% as.numeric() %>% round(digits = 2)
  
  quantities            <- c("χ²(df)", "χ²/df", "CFI", "TLI", "RMSEA", "SRMR")
                              
  modelFitsTable        <- 1:5 %>% 
                              add_column(.data = modelFitsTable, .before = 1)
  
  colnames(modelFitsTable) <- c("model", "working-aged.χ²(df)",  "working-aged.χ²/df", 
                               "working-aged.CF I", "working-aged.TL I", 
                               "working-aged.RMSE A", "working-aged.SRM R", 
                               "older.χ²(df)", "older.χ²/df", "older.CF I", 
                               "older.TL I", "older.RMSE A", "older.SRM R")   
  
  cutOffs               <- rep(c(NA, "< 2", "≥ 0.95", "≥ 0.95", 
                                         "≤ 0.06", "≤ 0.08"), 2)
  modelFitsTable        <- rbind(modelFitsTable, c("cutoffs", cutOffs))
  
  niceModelFits         <- nice_table(modelFitsTable,
                             separate.header = TRUE,
                             title = c("Table 3.", 
        "Model fit indices from a confirmatory factor analysis (CFA) on the DASS-21 data from the working-aged and older cohorts"),
                             note  = "Fits esimated using the robust WLSMV method. ")
  
  print(niceModelFits)
  
  # flextable::save_as_docx(niceModelFits, path = paste0(dataFolder, "/cfaTable.docx"))
}

#------------------------------------------------------------------------------#

## Extract the correlation matrices

# First, the items in the data.

corrsQs       <- list()

corrsQs[[1]]  <- lavInspect(result[[1]][[2]], what = "cor.ov")
corrsQs[[2]]  <- lavInspect(result[[2]][[2]], what = "cor.ov")  

# Then, between the three factors in model 2.

corrsDAS      <- list()

corrsDAS[[1]] <- lavInspect(result[[1]][[2]], what = "cor.lv")
corrsDAS[[2]] <- lavInspect(result[[2]][[2]], what = "cor.lv")

## Format and save.

niceCorrsQs     <- corrsQs

for (k in 1:2) {
  
  # Format.
  
  niceCorrsQs[[k]][upper.tri(niceCorrsQs[[k]], diag = FALSE)] <- NA
  
  Qs            <- paste0("Q", 1:21)
  niceCorrsQs[[k]] <- data.frame(Qs, niceCorrsQs[[k]])
  
  niceCorrsQs[[k]] <- nice_table(niceCorrsQs[[k]], title = c("Table.",
                               "The correlations between the DASS-21 items in the ______ cohort."),
                               note = "Autocorrelations on the diagonal. The correlations were polychoric (Olsson et al., 1970).")
}

# flextable::save_as_docx(niceCorrsQs[[1]], path = paste0(dataFolder, "/corrMatYoung.docx"))
# flextable::save_as_docx(niceCorrsQs[[2]], path = paste0(dataFolder, "/corrMatOld.docx"))

#------------------------------------------------------------------------------#
  
## Create and table the residuals.

if (residuals == 1) {
  
  residualTable <- list(list(), list())
  
  for (i in 1:5) { 
    for (k in 1:2) {
    
      # Create the residual matrix.
    
      residual                    <- resid(result[[k]][[i]]) %>%
        data.frame()
      
      # Format.
      
      residual[upper.tri(residual)] <- NA
      colnames(residual)          <- c("Qs", qNames)
      residual$Qs                 <- qNames
      
      # Save.
      
      residualTable[[k]][[i]]         <- residual
      rm(residual)
    } 
  } 
  
  # Format the tables for the residuals after fitting the bifactor solution.
  
  niceResidualTableYoung <- nice_table(residualTable[[1]][[3]], title = c("Table 1",
          "The residual covariance matrix of DASS-21 items after fitting the bifactor solution."))
  # flextable::save_as_docx(niceResidualTableYoung, paste0(dataFolder, "/residYoung.docx"))
  niceResidualTableOld <- nice_table(residualTable[[2]][[3]], title = c("Table 2",
          "The residual covariance matrix of DASS-21 items after fitting the bifactor solution."))
  # flextable::save_as_docx(niceResidualTableOld, paste0(dataFolder, "/residOld.docx"))
}


#------------------------------------------------------------------------------#

## Table the solutions.

if (loadings == 1) {
  
  summaries <- list()
  estimates <- list(matrix(NA, nrow=21, ncol=8) %>% data.frame(), # Create empty
                    matrix(NA, nrow=21, ncol=8) %>% data.frame()) # data frames.
  
  
  
  # Now inspect the 3+1 factor solution.
  
  for (k in 1:2) {
    
    summaries[[k]]             <- lavaan::summary(result[[k]][[3]])
    
    estimates[[k]][, 1:2]      <- summaries[[k]]$pe[22:42, 5:6]
    estimates[[k]][1:7, 3:4]   <- summaries[[k]]$pe[1:7, 5:6]
    estimates[[k]][8:14, 5:6]  <- summaries[[k]]$pe[8:14, 5:6]
    estimates[[k]][15:21, 7:8] <- summaries[[k]]$pe[15:21, 5:6]
    
    estimates[[k]]             <- estimates[[k]] %>% round(digits = 2)
    colnames(estimates[[k]])   <- c("GD.slope", "GD.SE",
                                     "D.slope", "D.SE",
                                     "A.slope", "A.SE",
                                     "S.slope", "S.SE")
    
    estimates[[k]]             <- data.frame(qNames, estimates[[k]]) %>%
                                   nice_table(estimates[[k]], 
                                              title = c("Table 4.",
                                                        "Estimates for the standardized factor loadings from the bifactorial solution to CFA in the two cohorts"),
                                              separate.header = TRUE)
    
    
  }
  
  # save_as_docx(estimates[[1]], paste0(dataFolder, "/estYoung.docx"))
  # save_as_docx(estimates[[2]], paste0(dataFolder, "/estOld.docx"))
}


#------------------------------------------------------------------------------#
  
## Plot the solutions.

if (figures2 == 1) {
  par(mfrow = c(1,1))
  semPlot::semPaths(result[[1]][[3]], "std", bifactor = c("D", "A", "S"), layout = "tree2", 
                    sizeLat = 5, edge.label.cex = .6, sizeMan = 6, residuals = 0, esize = 1)
  
  semPlot::semPaths(result[[2]][[3]], "std", bifactor = c("D", "A", "S"), layout = "tree2", 
                    sizeLat = 5, edge.label.cex = .6, sizeMan = 6, residuals = 0, esize = 1)
}

#------------------------------------------------------------------------------#

# Reload options.
  
options(backupOptions)
