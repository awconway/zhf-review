FROM rocker/binder:latest
LABEL maintainer='Aaron Conway'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

RUN chown -R ${NB_USER} ${HOME}

RUN install2.r --error \
    here \
    flextable \
    bookdown

RUN installGithub.r \
    awconway/ggprisma \
    awconway/gofer \
    davidgohel/officedown



USER ${NB_USER}


