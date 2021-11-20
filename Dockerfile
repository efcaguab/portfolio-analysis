FROM rocker/tidyverse:4.1.2

# Extra R packages
RUN install2.r --error --skipinstalled \
  blastula \
  config \
  googlesheets4 \
  tarchetypes \
  targets \
  quantmod

# Rstudio interface preferences
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
