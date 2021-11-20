FROM rocker/tidyverse:4.1.2

# Extra R packages
RUN install2.r --error --skipinstalled \
  targets tarchetypes quantmod googlesheets4 blastula

# Rstudio interface preferences
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
