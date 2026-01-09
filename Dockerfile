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

# Sanity check: Verify files are in the right place
RUN echo "=== Sanity Check: Working Directory ===" && \
    pwd && \
    echo "=== Sanity Check: Files in /home/shiny-app ===" && \
    ls -la /home/shiny-app && \
    echo "=== Sanity Check: Looking for app.R ===" && \
    (ls -la /home/shiny-app/app.R 2>&1 || echo "app.R NOT FOUND") && \
    echo "=== Sanity Check: Directory structure ===" && \
    find /home/shiny-app -maxdepth 2 -type f -name "*.R" && \
    echo "=== Sanity Check: www directory ===" && \
    (ls -la /home/shiny-app/www 2>&1 || echo "www directory NOT FOUND")

# Expose Shiny default port
EXPOSE 3838

# Keep container running for debugging/SSH access
# This allows you to SSH into the container in Azure to inspect files and troubleshoot
# To run the app, uncomment the CMD below and comment out this one
# CMD ["tail", "-f", "/dev/null"]

# Set proper permissions for www directory (if it exists)
RUN if [ -d /home/shiny-app/www ]; then chmod -R 755 /home/shiny-app/www; else echo "www directory does not exist, skipping chmod"; fi

# Run the app
# Shiny automatically serves the www directory when it's in the same directory as app.R
# Port 3838 matches WEBSITES_PORT setting in Azure App Service
# Files are copied directly to /home/shiny-app/ (not /home/shiny-app/aqbat/)
CMD ["R", "-e", "shiny::runApp('/home/shiny-app/aqbat', port = 3838, host = '0.0.0.0')"]
