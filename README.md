# Photos Organizer

This is a tool to manage local digital photos imported from a camera.

## Usage

In nginx.conf:

```conf
    server {
        listen [::]:80;
        root /path/to/photos-organizer/web;
        index index.html;
        server_name photos.mydomain;
        location / {
            try_files $uri $uri/ =404;
        }
        location /api/ {
            proxy_pass http://photos.localhost:8081/;
        }
    }

```

In another shell:

```
stack build && ./run.sh
```
