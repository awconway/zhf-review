FROM rocker/binder:3.6.0
LABEL maintainer='Aaron Conway'

RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('shinyLP')"
RUN Rscript -e "install.packages('shinydashboard')"
RUN Rscript -e "install.packages('shinydashboardPlus')"
RUN Rscript -e "install.packages('DT')"
RUN Rscript -e "install.packages('robvis')"
RUN Rscript -e "install.packages('waiter')"
RUN Rscript -e "install.packages('shinydashboardPlus')"

RUN installGithub.r awconway/ggprisma
RUN installGithub.r awconway/gofer
