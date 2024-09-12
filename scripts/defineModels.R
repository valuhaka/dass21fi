## Here we define the factor solutions used for the analyses. ##

setwd(".../data")              #  YOUR DIRECTORY HERE"

models <- list()


models$oneFactorModel <- "
  GD =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21 +
  Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20 +
  Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
"

models$threeFactorModel <- "
  D =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21
  A =~ Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20
  S =~ Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
  D ~~ A 
  A ~~ S
  D ~~ S
"

models$threePlusOneFactorModel <- "
  D =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21
  A =~ Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20
  S =~ Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
  
  GD =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21 +
  Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20 +
  Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
"

models$twoPlusOneFactorModel1 <- "
  D =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21
  A =~ Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20
  
  GD =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21 +
  Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20 +
  Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
"

models$twoPlusOneFactorModel2 <- "
  D =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21
  A =~ Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20
  
  D ~~ A
  
  GD =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21 +
  Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20 +
  Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
"

save(models, file = "cfaModels.Rdata")
