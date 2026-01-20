# Docker Container Debugging Commands

This document provides commands to troubleshoot the Docker container file structure issue.

## Dockerfile Options

This repository has two Dockerfiles:
- **`Dockerfile`**: Simple nginx test container (serves `index-test.html`) - used for initial ACR testing
- **`Dockerfile.shiny`**: Full Shiny app container with debugging capabilities

To build the Shiny app version:
```bash
docker build -f Dockerfile.shiny -t aqbat-shiny .
```

To build the test version (default):
```bash
docker build -t aqbat-test .
```

## Quick Debug Commands

### 1. Build the image locally to test
```bash
docker build -t aqbat-debug .
```

### 2. Run container interactively to inspect file structure
```bash
# Run container and keep it running
docker run -it --rm --name aqbat-debug aqbat-debug /bin/bash

# Once inside the container, run:
bash /debug-container.sh
# OR manually run the commands from debug-container.sh
```

### 3. Run the debug script directly
```bash
# Copy debug script into container and run it
docker run -it --rm aqbat-debug bash -c "$(cat debug-container.sh)"
```

### 4. Check file structure without entering container
```bash
docker run --rm aqbat-debug find /home/shiny-app -type f -name "*.R"
docker run --rm aqbat-debug ls -la /home/shiny-app
docker run --rm aqbat-debug ls -la /home/shiny-app/app.R
```

### 5. Test if the app can start
```bash
# Run container and check if app.R exists
docker run --rm aqbat-debug R -e "if(file.exists('/home/shiny-app/app.R')) { cat('app.R exists\n') } else { cat('app.R NOT FOUND\n') }"
```

## For Azure Container Instances / App Service

### 1. Connect to running container in Azure
```bash
# If using Azure Container Instances
az container exec --resource-group <resource-group> --name <container-name> --exec-command "/bin/bash"

# If using Azure App Service
az webapp ssh --resource-group <resource-group> --name <app-name>
```

### 2. Once connected, run these commands:
```bash
# Check current directory
pwd

# List files in /home/shiny-app
ls -la /home/shiny-app

# Check if app.R exists
ls -la /home/shiny-app/app.R

# Search for app.R anywhere
find / -name "app.R" 2>/dev/null

# Check directory structure
find /home/shiny-app -maxdepth 3 -type d

# Check environment variables
env | grep -E "(HOME|PWD|PATH|SHINY)"
```

## Common Issues and Solutions

### Issue: Path `/home/shiny-app/aqbat` doesn't exist
**Solution**: The app should run from `/home/shiny-app`, not `/home/shiny-app/aqbat`. Check:
1. Azure App Service configuration - ensure no custom startup command is overriding the Dockerfile CMD
2. The Dockerfile CMD should be: `shiny::runApp('/home/shiny-app', ...)`

### Issue: app.R not found
**Possible causes**:
1. Files not copied correctly during build
2. Wrong working directory
3. Files in wrong location

**Debug steps**:
```bash
# Check what was copied during build
docker build --progress=plain -t aqbat-debug . 2>&1 | grep -A 5 "COPY"

# Check files in image
docker run --rm aqbat-debug ls -la /home/shiny-app
```

### Issue: Files exist but app won't start
**Debug steps**:
```bash
# Check file permissions
docker run --rm aqbat-debug ls -la /home/shiny-app/app.R

# Check if R can read the file
docker run --rm aqbat-debug R -e "file.exists('/home/shiny-app/app.R')"
```

## Testing the Fix

After updating the Dockerfile:

1. **Build locally**:
   ```bash
   docker build -t aqbat-test .
   ```

2. **Check build output** for the sanity check messages

3. **Run container**:
   ```bash
   docker run -p 3838:3838 aqbat-test
   ```

4. **Check logs** for the runtime file structure check

5. **Test in browser**: http://localhost:3838
