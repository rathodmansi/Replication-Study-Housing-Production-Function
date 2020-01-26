
## Title: A replication study to estimate the production function for housing

### Contributors: 

1. [Anirudha Dutta](https://github.com/aniruddha29) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-3905-946X)
2. [Mansi Rathod](https://github.com/rathodmansi) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)]( https://orcid.org/0000-0001-7089-4300 )
3. [Mayur Gupta](https://github.com/mayurgpt07) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-9139-5519) 
4. [Pradeep Prabhakar](https://github.com/Pradeepprabhakar92) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-6202-5607) 
5. [Sreeja Vishaly Manohar](https://github.com/Sreejavm) [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-7566-5336 )

### Contents:

The purpose of this repository is for the team to collaborate on a replication study for the DATA 598 A course and will contain all the work required for the project check-ins and the final submission. The aim of the project is to replicate a key scientific claim of one of the papers published in the American Economic Review. Based on two factors - the observed variation in land prices and housing values per unit of land, the paper has provided an algorithm to identify the housing supply function per unit of land. This in turn is used to derive an estimate of the underlying production function. The approach yielded plausible estimates for the price elasticity of housing supply per unit of land, based on data from Allegheny County in Pennsylvania. For the project, the team wishes to replicate the figures that plots the estimates of the supply function and the production function. \
Estimating housing production functions is challenging, as the quantity and price per unit of the housing services are not observed by the econometrician. Replicating the study helps us to understand how the underlying production function is estimated by treating prices and quantities of housing services as latent variables, without relying on strong functional form assumptions. \
*Citation:*
Epple, Dennis, Brett Gordon, and Holger Sieg. 2010. *"A New Approach to Estimating the Production Function for Housing."* American Economic Review, 100 (3): 905-24.DOI: 10.1257/aer.100.3.905

### Data:
The paper for the replication study was found at the *[American Economic Association](https://www.aeaweb.org/articles?id=10.1257/aer.100.3.905)* and the data was obtained from the *[Open ICPSR](https://www.openicpsr.org/openicpsr/project/112360/version/V1/view)* in the form of text files. The data describes the residential and commercial real estate in Allegheny County in Pennsylvania post 1995 <br/>

### Dependencies:
For our projects, we had to install one package ‘MASS’ . Below is the complete list of our system requirements for the project and also the packages critical for our replication purpose.

<pre>
─ Session info ─────────────────────────────────────────────
 setting  value                       
 version  R version 3.6.2 (2019-12-12)
 os       macOS Mojave 10.14.6        
 system   x86_64, darwin15.6.0        
 ui       RStudio                     
 language (EN)                        
 collate  en_US.UTF-8                 
 ctype    en_US.UTF-8                 
 tz       America/Los_Angeles         
 date     2020-01-26                  
 </pre>
 <pre>
─ Packages ─────────────────────────────────────────────────
 package     * version  date       lib source        
 assertthat    0.2.1    2019-03-21 [1] CRAN (R 3.6.0)
 backports     1.1.5    2019-10-02 [1] CRAN (R 3.6.0)
 callr         3.4.0    2019-12-09 [1] CRAN (R 3.6.0)
 cli           2.0.1    2020-01-08 [1] CRAN (R 3.6.0)
 crayon        1.3.4    2017-09-16 [1] CRAN (R 3.6.0)
 desc          1.2.0    2018-05-01 [1] CRAN (R 3.6.0)
 devtools      2.2.1    2019-09-24 [1] CRAN (R 3.6.0)
 digest        0.6.23   2019-11-23 [1] CRAN (R 3.6.0)
 ellipsis      0.3.0    2019-09-20 [1] CRAN (R 3.6.0)
 fansi         0.4.1    2020-01-08 [1] CRAN (R 3.6.0)
 fs            1.3.1    2019-05-06 [1] CRAN (R 3.6.0)
 glue          1.3.1    2019-03-12 [1] CRAN (R 3.6.0)
 magrittr      1.5      2014-11-22 [1] CRAN (R 3.6.0)
 MASS        * 7.3-51.5 2019-12-20 [1] CRAN (R 3.6.0)
 memoise       1.1.0    2017-04-21 [1] CRAN (R 3.6.0)
 pkgbuild      1.0.6    2019-10-09 [1] CRAN (R 3.6.0)
 pkgload       1.0.2    2018-10-29 [1] CRAN (R 3.6.0)
 prettyunits   1.1.0    2020-01-09 [1] CRAN (R 3.6.0)
 processx      3.4.1    2019-07-18 [1] CRAN (R 3.6.0)
 ps            1.3.0    2018-12-21 [1] CRAN (R 3.6.0)
 R6            2.4.1    2019-11-12 [1] CRAN (R 3.6.0)
 Rcpp          1.0.3    2019-11-08 [1] CRAN (R 3.6.0)
 remotes       2.1.0    2019-06-24 [1] CRAN (R 3.6.0)
 rlang         0.4.2    2019-11-23 [1] CRAN (R 3.6.0)
 rprojroot     1.3-2    2018-01-03 [1] CRAN (R 3.6.0)
 rstudioapi    0.10     2019-03-19 [1] CRAN (R 3.6.0)
 sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 3.6.0)
 testthat      2.3.1    2019-12-01 [1] CRAN (R 3.6.0)
 usethis       1.5.1    2019-07-04 [1] CRAN (R 3.6.0)
 withr         2.1.2    2018-03-15 [1] CRAN (R 3.6.0)
</pre>
<pre>
[1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
</pre>
</pre>
