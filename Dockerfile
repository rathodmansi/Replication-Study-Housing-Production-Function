# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.6.1

# required
MAINTAINER Pradeep Prabhakar <prdp1992@uw.edu>

COPY . /Re.Estimating.Production.function.for.housing

RUN R -e "install.packages('Bolstad',dependencies = T)"

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "devtools::install('/Re.Estimating.Production.function.for.housing', dep=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/Re.Estimating.Production.function.for.housing/Analysis/paper.Rmd')"
