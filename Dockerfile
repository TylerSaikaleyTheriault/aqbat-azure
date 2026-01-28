# Simple, persistent container to serve a static HTML page
# nginx runs in the foreground so the container never exits
 
FROM nginx:alpine
 
# Copy the HTML file into nginx's default web root
COPY index-test.html /usr/share/nginx/html/index.html

# Expose the port nginx listens on
EXPOSE 80
 
# Run nginx in the foreground (this keeps the container alive)
CMD ["nginx", "-g", "daemon off;"]
