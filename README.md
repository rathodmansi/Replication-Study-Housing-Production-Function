
## A replication study to estimate the production function for housing

### Contributors: 

1. [Aniruddha Dutta](https://github.com/aniruddha29) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-3905-946X)
2. [Mansi Rathod](https://github.com/rathodmansi) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)]( https://orcid.org/0000-0001-7089-4300 )
3. [Mayur Gupta](https://github.com/mayurgpt07) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-9139-5519) 
4. [Pradeep Prabhakar](https://github.com/Pradeepprabhakar92) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-6202-5607) 
5. [Sreeja Vishaly Manohar](https://github.com/Sreejavm) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-7566-5336 )

### Contents:

The purpose of this repository is for the team to collaborate on a replication study for the DATA 598 A course and will contain all the work required for the project check-ins and the final submission. The aim of the project is to replicate a key scientific claim of one of the papers published in the American Economic Review that estimates the production function for housing. Based on two factors - the observed variation in land prices and housing values per unit of land, the paper has provided an algorithm to identify the housing supply function per unit of land. This in turn is used to derive an estimate of the underlying production function. The approach yielded plausible estimates for the price elasticity of housing supply per unit of land, based on data from Allegheny County in Pennsylvania. For the project, the team wishes to replicate the figures that plots the estimates of the supply function and the production function. \
Estimating housing production functions is challenging, as the quantity and price per unit of the housing services are not observed by the econometrician. Replicating the study helps us to understand how the underlying production function is estimated by treating prices and quantities of housing services as latent variables, without relying on strong functional form assumptions.
#### Citation:
Epple, Dennis, Brett Gordon, and Holger Sieg. 2010. *"A New Approach to Estimating the Production Function for Housing."* American Economic Review, 100 (3): 905-24.DOI: 10.1257/aer.100.3.905

### Data:
The paper for the replication study was found at the *[American Economic Association](https://www.aeaweb.org/articles?id=10.1257/aer.100.3.905)* and the data was obtained from the *[Open ICPSR](https://www.openicpsr.org/openicpsr/project/112360/version/V1/view)* in the form of text files. The data describes the residential and commercial real estate in Allegheny County in Pennsylvania post 1995 <br/>

### Dependencies:
For our project, we had to install two package ‘MASS’ and 'Bolstad' . Below is the complete list of our system requirements for the project and also the packages critical for our replication purpose.

Packages ‘odesolve’ and 'R.basic' which were used in the base paper have been removed from the CRAN repository. The more recent package 'deSolve' completely supersedes odesolve and we have installed the same. R.basic can be installed from [this](http://www.braju.com/R/repos/) repository as shown below. \
install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")

<pre>
─ Session info ───────────────────────────────────────────────────
 setting  value                       
 version  R version 3.6.2 (2019-12-12)
 os       macOS Mojave 10.14.6        
 system   x86_64, darwin15.6.0        
 ui       RStudio                     
 language (EN)                        
 collate  en_US.UTF-8                 
 ctype    en_US.UTF-8                 
 tz       America/Los_Angeles         
 date     2020-02-21                  
</pre>
<pre>
─ Packages ───────────────────────────────────────────────────────
 package     * version  date       lib source        
 assertthat    0.2.1    2019-03-21 [1] CRAN (R 3.6.0)
 backports     1.1.5    2019-10-02 [1] CRAN (R 3.6.0)
 Bolstad       0.2.40   2018-10-09 [1] CRAN (R 3.6.0)
 bookdown      0.17     2020-01-11 [1] CRAN (R 3.6.0)
 callr         3.4.0    2019-12-09 [1] CRAN (R 3.6.0)
 cli           2.0.1    2020-01-08 [1] CRAN (R 3.6.0)
 crayon        1.3.4    2017-09-16 [1] CRAN (R 3.6.0)
 desc          1.2.0    2018-05-01 [1] CRAN (R 3.6.0)
 deSolve     * 1.27.1   2020-01-02 [1] CRAN (R 3.6.0)
 devtools      2.2.1    2019-09-24 [1] CRAN (R 3.6.0)
 digest        0.6.23   2019-11-23 [1] CRAN (R 3.6.0)
 ellipsis      0.3.0    2019-09-20 [1] CRAN (R 3.6.0)
 evaluate      0.14     2019-05-28 [1] CRAN (R 3.6.0)
 fansi         0.4.1    2020-01-08 [1] CRAN (R 3.6.0)
 fs            1.3.1    2019-05-06 [1] CRAN (R 3.6.0)
 glue          1.3.1    2019-03-12 [1] CRAN (R 3.6.0)
 htmltools     0.4.0    2019-10-04 [1] CRAN (R 3.6.0)
 knitr         1.28     2020-02-06 [1] CRAN (R 3.6.0)
 lokern      * 1.1-8    2016-10-11 [1] CRAN (R 3.6.0)
 magrittr      1.5      2014-11-22 [1] CRAN (R 3.6.0)
 MASS        * 7.3-51.5 2019-12-20 [1] CRAN (R 3.6.0)
 memoise       1.1.0    2017-04-21 [1] CRAN (R 3.6.0)
 packrat       0.5.0    2018-11-14 [1] CRAN (R 3.6.0)
 pkgbuild      1.0.6    2019-10-09 [1] CRAN (R 3.6.0)
 pkgload       1.0.2    2018-10-29 [1] CRAN (R 3.6.0)
 prettyunits   1.1.0    2020-01-09 [1] CRAN (R 3.6.0)
 processx      3.4.1    2019-07-18 [1] CRAN (R 3.6.0)
 ps            1.3.0    2018-12-21 [1] CRAN (R 3.6.0)
 R6            2.4.1    2019-11-12 [1] CRAN (R 3.6.0)
 Rcpp          1.0.3    2019-11-08 [1] CRAN (R 3.6.0)
 remotes       2.1.0    2019-06-24 [1] CRAN (R 3.6.0)
 rlang         0.4.4    2020-01-28 [1] CRAN (R 3.6.0)
 rprojroot     1.3-2    2018-01-03 [1] CRAN (R 3.6.0)
 rstudioapi    0.10     2019-03-19 [1] CRAN (R 3.6.0)
 rmarkdown     2.1      2020-01-20 [1] CRAN (R 3.6.0)
 sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 3.6.0)
 sfsmisc       1.1-5    2020-02-09 [1] CRAN (R 3.6.2)
 stargazer     5.2.2    2018-05-30 [1] CRAN (R 3.6.2)
 testthat      2.3.1    2019-12-01 [1] CRAN (R 3.6.0)
 usethis       1.5.1    2019-07-04 [1] CRAN (R 3.6.0)
 withr         2.1.2    2018-03-15 [1] CRAN (R 3.6.0)
 xfun          0.12     2020-01-13 [1] CRAN (R 3.6.0)
 yaml          2.2.1    2020-02-01 [1] CRAN (R 3.6.0)
</pre>
<pre>
[1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
</pre>

### Contributing:
We welcome contributions from everyone. Before you get started, please see our contributor guidelines. Please note that this project is released with a [Contributor Code of Conduct](https://github.com/ReplicationStudy/ReplicationStudyToEstimatePFforHousing/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its [terms](https://github.com/ReplicationStudy/ReplicationStudyToEstimatePFforHousing/blob/master/CONTRIBUTING.md).

