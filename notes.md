Cases in which the sign of a posterior expectation might be important:

- Analysis 2.3 in ExtendedMethods.html
  + Info:
    + Figure 3 in the paper (Figure 3.12 in ExtendedMethods.html): 
    + the A histogram compares log_10 viral load for a subpopulation with B.1.1.7 
  and a subpopulation with non-B.1.1.7. Unclear if these are predictions on the training data
  or draws from the posterior 
    + Table 3.4 in ExtendedMethods.html prints the number of observations in each data subset
  + Pros:
    + the final analysis should take a little amount of time to run (~ 2.5k observations)
  + Cons: 
    + the code to manipulate the data frames is actually data.table(), and I am not 
    super familiar with the package
    
    
- Analysis 2.1 in ExtendedMethods.html
  + Cons:
    + plots are computed using newdata quantities