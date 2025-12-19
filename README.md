<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->

<a name="readme-top"></a>

<div align="center">
  <h3 align="center">Air Quality Assessment Tool</h3>
  <p align="center">
    <br />
    <a href="https://github.com/adamsimonini/AQBAT/blob/master/README.md"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://aqbat.netlify.app/">Netlify demo</a>
  </p>
</div>

## About The Project

### Description

The Air Quality Benefits Assessment Tool (AQBAT) is a computer application designed to estimate the human health impacts of changes in Canada's ambient air quality. It is used to estimate the benefits (positive impacts) or damages (negative impacts) of proposed regulatory initiatives related to outdoor air quality as mandated by the Treasury Board Cabinet Directive on Regulatory Management.

### Built With

- RShinyApps
- [iframeResizer](https://github.com/davidjbradshaw/iframe-resizer)

## Getting Started

### Onboarding

When appropriate, new developers should be onboarded as maintainers on GitHub. This is done by sending a request to [devops@hc-sc.gc.ca](mailto:devops@hc-sc.gc.ca).

### Prerequisites

Install R & RStudio for windows [here](https://posit.co/download/rstudio-desktop/)

Guoliang, the lead developer of the app, is using R version 4.2.2

### Installation

1. With RStudio opened, start a new project using an existing directory with `file -> new project -> existing directory`
2. Choose the `aqbat` folder from this repo. It's the one that contains `app.R`
3. Install packages using renv:
   ```R
   # Install renv if not already installed
   install.packages("renv")
   
   # Initialize renv in the project
   renv::init()
   
   # Restore all packages from the lockfile
   renv::restore()
   ```
   This will install all required packages with their exact versions as specified in `renv.lock`
4. To run the app, select `app.R` and then click "▶️ Run App"

### Development

Asset files like images and css files should be placed here `./www`

With RStudio installed, bring up the `app.R` in the RStudio IDE, and then click the "▶️ Run" button. The app will run. Select "Show in browser" to see it in your default browser.

For most local testing, you'll be working on the `app.R` file. This means you can run the app using RStudio, and see your changes by refreshing the browser.

Some issues will require testing the "full" app, meaning you'll run the index.html file, and shinyapp will load. This will require you to have the app.R file running on RStudio. You'll also need to adjust the localUrl file in the index.html file to match the local URL of your RStudio instance. This port changes often, so you'll need to confirm this each time your start RStudio.

## Architecture & Patterns

### Data Management

- **JSON Reference Data**: The application uses JSON files to store and load reference data:
  - `pmReferences.json` - Contains PM2.5 health impact references
  - `valuationReferences.json` - Contains economic valuation references
  - Translations are managed through separate language-specific JSON files

### Template System

- **HTML Templates**: The app uses a modular template system for reusable UI components:
  - Templates are stored in the `templates/` directory
  - Components are loaded using `htmlTemplate()` function
  - Example: `clear-data-button.html` template is reused across different pages

### Iframe Integration

- **iframeResizer**: The app uses iframeResizer for responsive iframe management
- **Cross-frame Communication**:
  - Scripts monitor changes within iframes
  - Communication between parent and child frames is handled through postMessage
  - Example: Timeout warnings and session management
- **Dynamic URL Configuration**:
  - Automatically detects environment (local/netlify/production)
  - Sets appropriate Shiny app URL based on context:
    ```javascript
    // Local development: http://127.0.0.1:4202
    // Netlify: https://hcscpsd.shinyapps.io/aqbat-netlify-en/
    // Production: https://hcscpsd.shinyapps.io/aqbat-netlify-en/
    ```
  - Handles query parameter passing to iframe source
  - Manages loading spinner visibility

### Session Management

- **Timeout Handling**:
  - Implemented in `utilities.js`
  - Uses Bootstrap modals for warning messages
  - Configurable timeout duration (default 3 hours)
  - Keep-alive mechanism to maintain session

### Internationalization

- **Dual Language Support**:
  - English and French translations
  - Uses `shiny.i18n` package
  - Separate JSON files for translations
  - Dynamic loading of language-specific data files

### File Structure

```
aqbat/
├── data/
│   ├── pmReferences.json
│   ├── valuationReferences.json
│   └── valuationReferences_fr.json
├── templates/
│   └── clear-data-button.html
├── www/
│   ├── scripts/
│   │   └── utilities.js
│   └── styles/
└── app.R
```

### Development Workflow

- Local development supports both standalone R server and full app testing
- Template changes require app restart (no hot reloading)
- Deployment targets include Netlify (static) and Shinyapps.io (R server)

## Docker Setup and Usage

This project uses Docker to containerize the Shiny application for production. Here's how to work with the Docker setup:

### Prerequisites
- Docker Desktop installed on your system
- Git for version control

### Key Docker Commands

1. **Build the Docker image**:
   ```bash
   docker build -t aqbat .
   ```

2. **Run the container**:
   ```bash
   docker run -p 3838:3838 aqbat
   ```
   This will make the app available at `http://localhost:3838`

3. **View running containers**:
   ```bash
   docker ps
   ```

4. **Stop a container**:
   ```bash
   docker stop <container_id>
   ```

5. **Remove a container**:
   ```bash
   docker rm <container_id>
   ```

6. **Remove an image**:
   ```bash
   docker rmi aqbat
   ```

### Package Management with renv

The project uses `renv` for reproducible package management. Here's how to work with it:

1. **Initial Setup**:
   ```R
   # Install renv
   install.packages("renv")
   
   # Initialize renv in the project
   renv::init()
   
   # Restore all packages from the lockfile
   renv::restore()
   ```

2. **Adding New Packages**:
   ```R
   # Install a new package
   install.packages("new-package")
   
   # Snapshot the current state to update renv.lock
   renv::snapshot()
   ```

3. **Updating Packages**:
   ```R
   # Update all packages
   renv::update()
   
   # Snapshot the changes
   renv::snapshot()
   ```

4. **Working with renv.lock**:
   - The `renv.lock` file contains the exact versions of all R packages needed
   - It should be committed to version control
   - Other developers can use `renv::restore()` to get the same package versions

5. **In Docker**:
   - The Dockerfile automatically handles renv setup
   - It copies the `renv.lock` file and restores packages during build
   - No manual package installation is needed in the container

6. **Troubleshooting renv**:
   - If packages fail to install, try:
     ```R
     # Clear the renv cache
     renv::purge()
     
     # Restore packages again
     renv::restore()
     ```
   - For system dependency issues, check the Dockerfile for required system libraries

### Important Notes

1. **File Structure**: The Dockerfile copies all necessary files into the container:
   - R scripts and app files
   - Data files (CSV, JSON)
   - Static files (HTML, CSS, JS)
   - Map data

2. **Resource Paths**: The app uses `shiny::addResourcePath()` to make static files available to the Shiny server.

3. **Port Mapping**: The app runs on port 3838 inside the container and is mapped to the same port on your host machine.

4. **Development vs Production**: 
   - For development, you can run the app directly in RStudio
   - For production deployment, use the Docker container

### Troubleshooting

1. If the app doesn't show up in the browser:
   - Check if the container is running (`docker ps`)
   - Verify the port mapping
   - Check the container logs (`docker logs <container_id>`)

2. If package installation fails:
   - Check the `renv.lock` file for compatibility issues
   - Verify system dependencies in the Dockerfile

3. If static files aren't loading:
   - Verify the `www` directory is properly copied
   - Check the resource paths in the app code

## Workflows

- **File:** [`.github/workflows/docker-image.yml`](.github/workflows/docker-image.yml) — Deploys the AQBAT Docker image to Azure Container Registry (ACR).
- **Purpose:** Build the project Docker image, tag it with the run id and `latest`, and push both tags to ACR.
- **Trigger:** Manual (`workflow_dispatch`).
- **Jobs:**
   - `createRunner`: calls the reusable runner workflow [`hs/createEphemeralUbuntuRunner/.github/workflows/createEphemeralUbuntuRunner.yml@main`](https://github.hc-sc.gc.ca/hs/createEphemeralUbuntuRunner/blob/main/.github/workflows/createEphemeralUbuntuRunner.yml) to provision an ephemeral Ubuntu runner.
   - `build-and-push`: runs on the ephemeral runner, builds the image with `docker build`, tags, and pushes to the registry.
- **Key actions & dependencies:** `actions/checkout@v4`, `docker/setup-docker-action@v4`, `azure/login@v2`, and the org-provided `hs/createEphemeralUbuntuRunner` reusable workflow.
- **Required secrets / inputs:** The caller maps a repository secret to `AZURE_CREDENTIALS` (example secret name in this repo: `DTB-DEVOPS-LZ-CICDLZSP-DT`). That secret must contain valid Azure service principal JSON including a `subscriptionId` field.
- **Troubleshooting notes:**
   - The UI lists workflows from the repository default branch; to see the workflow in the Actions sidebar or use the manual "Run workflow" UI, ensure the file is present on the default branch (e.g., `main`).
   - The reusable runner expects an input named `environment` (lowercase). The caller must use the same input name.
   - The reusable workflow uses `fromJSON(secrets.AZURE_CREDENTIALS).subscriptionId` during evaluation — this will error if the secret is missing, empty, or not valid JSON (common when running PRs from forks where secrets are unavailable).
   - Ensure the runner label produced by the reusable workflow matches the `runs-on` label used by `build-and-push`.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

```

```
