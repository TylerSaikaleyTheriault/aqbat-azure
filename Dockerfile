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

# Verify the www directory exists and set proper permissions
# This will fail the build if www directory is missing
RUN ls -la /home/shiny-app && \
    test -d /home/shiny-app/www || (echo "ERROR: www directory is missing!" && exit 1) && \
    ls -la /home/shiny-app/www && \
    chmod -R 755 /home/shiny-app/www && \
    echo "www directory verified and permissions set"

# Expose Shiny default port
EXPOSE 3838

# Run the app
# Shiny automatically serves the www directory when it's in the same directory as app.R
# Port 3838 matches WEBSITES_PORT setting in Azure App Service
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', port = 3838, host = '0.0.0.0')"]
