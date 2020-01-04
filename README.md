# Scalerl

A scale manager for Kubernetes to ensure safe and cost effective HPA configurations

At the moment this is more of a proof-of concept.

Still TODO:
* HPA target customization
* Configuration of:
    * Prometheus location
    * Starting HPA settings
    * Re-evaluation frequency
    * Customizable Prometheus queries
* Ensure broken `watch` commands are noticed/handled
* Deal with multiple metric lists for an HPA size (grouping in the query should work)

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
