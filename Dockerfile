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

# Copy renv files first (for package installation cache)
COPY ./aqbat/renv.lock ./renv.lock
COPY ./aqbat/renv ./renv
COPY ./aqbat/.Rprofile ./.Rprofile

# Install renv (cached separately from package restore for better cache reuse)
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Restore packages (this layer will be cached if renv.lock hasn't changed)
RUN R -e "renv::restore()"

# Copy all app files (after renv restore to preserve package cache)
# .dockerignore excludes renv files since they're already copied above
COPY ./aqbat/ ./

# Set proper permissions for www directory (only if needed)
# Remove this line if files already have correct permissions from source
RUN chmod -R 755 /home/shiny-app/www

# Expose Shiny default port
EXPOSE 3838

# Run the app
# Shiny automatically serves the www directory when it's in the same directory as app.R
# Port 3838 matches WEBSITES_PORT setting in Azure App Service
# WORKDIR is already set to /home/shiny-app, so we can use '.' for current directory
CMD ["R", "-e", "shiny::runApp('.', port = 3838, host = '0.0.0.0')"]
