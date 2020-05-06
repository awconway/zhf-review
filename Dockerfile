FROM rocker/verse:latest
LABEL maintainer='Aaron Conway'

RUN install2.r --error \
    here \
    flextable 

RUN installGithub.r \
    awconway/ggprisma \
    awconway/gofer \
    davidgohel/officedown \
    mcguinlu/robvis