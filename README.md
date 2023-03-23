# MUVR
**Multivariate methods with Unbiased Variable selection in R**  
Associate Professor Carl Brunius  <carl.brunius@chalmers.se>  
PhD candidate Yingxiao Yan  <yingxiao@chalmers.se>  
Department of LIFE Sciences
Chalmers University of Technology www.chalmer.se

## General description
The MUVR package allows for predictive multivariate modelling with minimally biased variable selection incorporated into a repeated double cross-validation framework. The MUVR procedure simultaneously produces both minimal-optimal and all-relevant variable selections.

An easy-to-follow tutorial on how to use the MUVR package for classification, regression and multilevel analysis can be found at this repository at [Tutorial/MUVR_Tutorial.docx] (https://gitlab.com/YingxiaoYan/MUVR/blob/master/Tutorial/MUVR2_Tutorial.docx)

In brief, MUVR proved the following functionality:
- Types: Classification, regression and multilevel
- Model cores: Partial Least Square(PLS), Random Forest(RF) and Elastic net(EN)
- Validation: Repeated double cross-validaiton (rdCV; Westerhuis et al 2008, Filzmoser et al 2009)
- Variable selection: Recursive feature elimination embedded in the rdCV loop (PLS and RF). Non-zero beta-coefficients across rdCV loops (EN).   

## Installation
- You will need to have installed R (https://www.r-project.org/)
- Normally, you will want to work in RStudio (https://rstudio.com/) or some other IDE

You also need to have the `remotes` R package installed. Just run the following from an R script or type it directly at the R console (normally the lower left window in RStudio):
```
install.packages('remotes')
```
When `remotes` is installed, you can install the `MUVR` package by running (In this scenario you donot need devtools since remotes is part of devtools package):
```
library(remotes)
install_gitlab('YingxiaoYan/MUVR')
```

In addition to functions relevant for cross validated, predictive multivariate modelling, the MUVR package also provides data to reproduce figures from the original *Shi et al* paper (below).

## References
- *Shi, L., Westerhuis, J. A., Rosén, J., Landberg, R., & Brunius, C. (2019). Variable selection and validation in multivariate modelling. Bioinformatics, 35(6), 972-980.*
- *Hanhineva, K., Brunius, C., Andersson, A., Marklund, M., Juvonen, R., Keski‐Rahkonen, P., ... & Landberg, R. (2015). Discovery of urinary biomarkers of whole grain rye intake in free‐living subjects using nontargeted LC‐MS metabolite profiling. Molecular nutrition & food research, 59(11), 2315-2325.* 
- *Buck, M., Nilsson, L. K., Brunius, C., Dabiré, R. K., Hopkins, R., & Terenius, O. (2016). Bacterial associations reveal spatial population dynamics in Anopheles gambiae mosquitoes. Scientific Reports, 6(1), 1-9. *
- *Westerhuis, J. A., van Velzen, E. J., Hoefsloot, H. C., & Smilde, A. K. (2010). Multivariate paired data analysis: multilevel PLSDA versus OPLSDA. Metabolomics, 6(1), 119-128.*
- *Lindgren, F., Hansen, B., Karcher, W., Sjöström, M., & Eriksson, L. (1996). Model validation by permutation tests: applications to variable selection. Journal of Chemometrics, 10(5‐6), 521-532. *


## Version history
version | date | comment
:------ | :--- | :------
