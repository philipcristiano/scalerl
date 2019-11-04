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
