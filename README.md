# Scalerl

A scale manager for Kubernetes to ensure safe and cost effective HPA configurations

(Currently in development)

# Deploying

TODO

## Enabling Scalerl for a deployment

Add the annotation `scaler: "enable"` to your deployment:

```
---
apiVersion: apps/v1
kind: Deployment
metadata:
  annotations:
    scalerl: "enable"
...
```


## Deploying

* Grab a Github personal access token
  With: *repo *read:packages

* Create you secret to pull the image

* kubectl -n scalerl create secret docker-registry regcred --docker-server=docker.pkg.github.com --docker-username=$GITHUB_USER --docker-password=$GITHUB_TOKEN
