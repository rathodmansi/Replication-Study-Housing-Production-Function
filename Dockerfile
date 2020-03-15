# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.6.2

# required
MAINTAINER Pradeep Prabhakar <prdp1992@uw.edu>

COPY . /compendium

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package and exotic dependencies
  && R -e "install.packages(c('R.utils', 'R.methodsS3', 'R.oo'))" \
  && R -e "install.packages('http://www.braju.com/R/repos/R.basic_0.53.0.tar.gz', repos = NULL, type='source')" \
  && R -e "devtools::install('/compendium', dep=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/compendium/Analysis/paper.Rmd')"
