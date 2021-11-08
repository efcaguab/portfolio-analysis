FROM rocker/geospatial:4.1.2

# Extra R packages
RUN install2.r --error --skipinstalled \
  targets tarchetypes here janitor skimr brms ggdist inspectdf

# Rstudio interface preferences
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
