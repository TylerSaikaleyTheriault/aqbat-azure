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
# Using /app instead of /home/shiny-app to avoid Azure App Service volume mounts
WORKDIR /app

# Copy renv files first (for package installation cache)
COPY ./aqbat/renv.lock /app/renv.lock
COPY ./aqbat/renv /app/renv
COPY ./aqbat/.Rprofile /app/.Rprofile

# Install renv (cached separately from package restore for better cache reuse)
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Restore packages (this layer will be cached if renv.lock hasn't changed)
RUN R -e "renv::restore()"

# Copy all app files (after renv restore to preserve package cache)
# .dockerignore excludes renv files since they're already copied above
COPY ./aqbat/ /app/

# Expose Shiny default port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', port=3838, host='0.0.0.0')"]
