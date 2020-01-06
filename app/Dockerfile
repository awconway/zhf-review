FROM rocker/r-ver:3.6.1
LABEL maintainer='Aaron Conway'

RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('shinyLP')"
RUN Rscript -e "install.packages('shinydashboard')"
RUN Rscript -e "install.packages('shinydashboardPlus')"
RUN Rscript -e "install.packages('DT')"
RUN Rscript -e "install.packages('robvis')"
RUN Rscript -e "install.packages('waiter')"
RUN Rscript -e "install.packages('magrittr')"
RUN Rscript -e "install.packages('dplyr')"

RUN R -e "remotes::install_github('awconway/ggprisma')"
RUN R -e "remotes::install_github('awconway/gofer')"
