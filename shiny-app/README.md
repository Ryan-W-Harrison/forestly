# Forestly Shiny App

This directory is the deployable Shiny app unit. It is intentionally isolated
from the upstream forestly package source in the repository root.

Run locally from the repository root:

```r
shiny::runApp("shiny-app")
```

For Posit Connect, deploy from this directory. If using `renv`, create or
restore the lockfile here, then write the deployment manifest:

```r
renv::snapshot()
rsconnect::writeManifest()
```

The app accepts `sas7bdat`, `csv`, `tsv`, `xlsx`, and `parquet` uploads, lets
users map AE analysis variables, previews the uploaded data, and generates an
interactive forestly plot.
