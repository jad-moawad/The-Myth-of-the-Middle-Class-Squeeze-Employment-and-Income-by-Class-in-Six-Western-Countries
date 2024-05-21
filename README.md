# Replication Files for "The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020"

This folder contains files to help you replicate the analyses from the study "The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020". The study uses data from the Luxembourg Income Study (LIS, 2024).

## Accessing the Data

To access the data, you must request access to the LIS data by visiting the [LIS data access page](https://www.lisdatacenter.org/data-access/lissy/eligibility/). Once you have access, you can use the codes in this folder to analyze the data.

## Conducting the Analysis

The analysis was conducted in R, and all the packages used are included in the R scripts.

### Note on LIS Data Access

The LIS data are not directly accessible. Instead, researchers can access an online platform to submit their analysis codes. The platform then returns the results. You cannot download or physically access the data, but you can still use the codes provided here to get the same results as the original study.

## Updates and Versions

The LIS regularly updates new modules and revises old ones. This project started in 2022 and, therefore, uses the selection of countries released in LIS in that year. The final execution of this set of modules was done in 2024. To ensure you get the same results, please select the modules in the code and run them on the 2024 release.

# Software

Analyses were conducted in R, version 4.3.1. 

I used a number of packages in R, these include:  ggplot2 (Wikcham, 2016), ggpubr (Kassambara, 2020), ggeffects (Lüdecke, 2018), and tidyverse (Wickham et al., 2019),  dplyr (Wickham et al., 2015).

# References:

Kassambara, A., 2020. ggpubr:“ggplot2” based publication ready plots. R package version 0.4.0. 

Lüdecke D (2018). ggeffects: Tidy Data Frames of Marginal Effects from Regression Models. Journal of Open Source Software, 3(26), 772. doi: 10.21105/joss.00772

Wickham, H., 2016. ggplot2: Elegant Graphics for Data Analysis, 2nd ed. 2016. ed, Use R! Springer International Publishing : Imprint: Springer, Cham. https://doi.org/10.1007/978-3-319-24277-4

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T., Miller, E., Bache, S., Müller, K., Ooms, J., Robinson, D., Seidel, D., Spinu, V., Takahashi, K., Vaughan, D., Wilke, C., Woo, K., Yutani, H., 2019b. Welcome to the Tidyverse. JOSS 4, 1686. https://doi.org/10.21105/joss.01686

Hadley Wickham, Romain François, Lionel Henry, Kirill Müller (2023).
  dplyr: A Grammar of Data Manipulation. R package version 1.0.10.
  https://CRAN.R-project.org/package=dplyr

*Date of last update: 2024-05-21*
