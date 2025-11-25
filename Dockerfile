# Use official Shiny base image
FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Create working directory
WORKDIR /home/shiny-app

# Copy renv files first
COPY ./aqbat/renv.lock ./renv.lock
COPY ./aqbat/renv ./renv
COPY ./aqbat/.Rprofile ./.Rprofile

# Initialize renv and restore packages
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore()"

# Copy app files
COPY ./aqbat/app.R ./app.R
COPY ./aqbat/translations.json ./translations.json
COPY ./aqbat/www ./www
COPY ./aqbat/pages ./pages
COPY ./aqbat/data ./data
COPY ./aqbat/templates ./templates
COPY ./aqbat/cdmap ./cdmap

# Copy required CSV files
COPY ./aqbat/pollnames.csv ./pollnames.csv
COPY ./aqbat/pollnames_fr.csv ./pollnames_fr.csv
COPY ./aqbat/prov.csv ./prov.csv
COPY ./aqbat/prov_fr.csv ./prov_fr.csv
COPY ./aqbat/xprov.csv ./xprov.csv
COPY ./aqbat/xprov_fr.csv ./xprov_fr.csv
COPY ./aqbat/Baseline_population_data.csv ./Baseline_population_data.csv
COPY ./aqbat/Baseline_population_data_fr.csv ./Baseline_population_data_fr.csv
COPY ./aqbat/cpi.csv ./cpi.csv
COPY ./aqbat/cpi_fr.csv ./cpi_fr.csv
COPY ./aqbat/pollutants_2016wtox.csv ./pollutants_2016wtox.csv
COPY ./aqbat/pollutants_2016wtox_fr.csv ./pollutants_2016wtox_fr.csv

# Add a command to verify the working directory and file structure
RUN ls -la /home/shiny-app && \
    ls -la /home/shiny-app/www

# Expose Shiny default port
EXPOSE 3838

# Run the app with www directory explicitly set
CMD ["R", "-e", "shiny::addResourcePath('www', '/home/shiny-app/www'); shiny::runApp('/home/shiny-app', port = 3838, host = '0.0.0.0')"]
