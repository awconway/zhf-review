FROM rocker/binder:3.6.0
LABEL maintainer='Aaron Conway'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

RUN chown -R ${NB_USER} ${HOME}

RUN Rscript -e "install.packages('here')"
RUN Rscript -e "install.packages('distill')"
RUN Rscript -e "install.packages('kableExtra')"

RUN installGithub.r awconway/ggprisma


USER ${NB_USER}


