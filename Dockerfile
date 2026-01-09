# Minimal sanity check Dockerfile for Azure App Service
# This keeps the container running so you can access it via Azure Portal Console or az webapp ssh
FROM alpine:latest

# Install basic utilities for debugging
RUN apk add --no-cache bash curl

# Create a simple message file
RUN echo "Container is running! You can now SSH into this container via Azure App Service." > /message.txt

# Expose port (Azure App Service may require this)
EXPOSE 80

# Keep container running indefinitely
# This allows you to access it via:
# - Azure Portal: App Service -> Development Tools -> Console
# - Azure CLI: az webapp ssh --name <app-name> --resource-group <rg-name>
CMD ["sh", "-c", "echo 'Container started. Access via Azure Portal Console or az webapp ssh' && cat /message.txt && tail -f /dev/null"]
