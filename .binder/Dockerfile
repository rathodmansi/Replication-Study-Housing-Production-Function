FROM Pradeep Prabhakar
LABEL maintainer='Unknown'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/ReplicationStudy/ReplicationStudyToEstimatePFforHousing/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-03-15/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
