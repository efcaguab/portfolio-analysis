FROM rocker/tidyverse:4.2.0

# Run required system installs
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libglpk-dev \
  libgmp3-dev \
  libxml2-dev

# Extra R packages
RUN install2.r --error --skipinstalled \
  blastula \
  config \
  googlesheets4 \
  gt \
  logger \
  tarchetypes \
  targets \
  quantmod

# Rstudio interface preferences
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
