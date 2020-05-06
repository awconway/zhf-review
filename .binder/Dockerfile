FROM rocker/binder:3.6.0
LABEL maintainer='Aaron Conway'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

RUN chown -R ${NB_USER} ${HOME}

RUN install2.r --error \
    here \
    flextable

RUN installGithub.r \
    awconway/ggprisma \
    awconway/gofer


USER ${NB_USER}


