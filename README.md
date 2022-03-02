
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DBCA Turtle Data Viewer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dbca-wa/turtleviewer2/workflows/R-CMD-check/badge.svg)](https://github.com/dbca-wa/turtleviewer2/actions)
[![docker](https://github.com/dbca-wa/turtleviewer2/workflows/docker/badge.svg)](https://github.com/dbca-wa/turtleviewer2/actions)
<!-- badges: end -->

Interactive turtle data preview for QA and analysis hosted at
<https://turtledata.dbca.wa.gov.au/> for DBCA staff.

## Architecture

Data shapshots are locally saved as an `.rds` file for WAStD data,
WAMTRAM data, and WAStD sites. The app accesses these snapshots through
file paths relative to its working directory,
e.g. `here:here("inst/wastd_site.rds")`.

In development, the workdir is the top level folder of the app’s source
code, and `inst` refers to that folder inside the source code.

In production, the `WORKDIR` is defined as `/app` in the Dockerfile, and
data snapshots are written to `/app/inst` in the running container.

To persist the data snapshots, a volume should be mounted to the
internal path `/app/inst`.

## Development

  - Write code, add tests where needed
  - Update dependencies from code: `attachment::att_amend_desc()`
    (careful: drops unused Imports)
  - Update Dockerfile from dependencies:
    `golem::add_dockerfile(from="rocker/geospatial:4.1.2")` (careful:
    rotate existing Dockerfile to Dockerfile\_old to keep manual edits)
  - Build Docker image for local testing: `system("docker build . -t
    dbca-wa/turtleviewer2:latest")`
  - Run e.g. through [Portainer](https://www.portainer.io) with
      - Name: e.g. “tv”
      - Image: advanced mode, `dbca-wa/turtleviewer2:latest`
      - Always pull image: off
      - Publish ports: on
      - Volumes: container path `/app/inst`, use named volume or bind
        mount to a local directory
      - env: paste environment variables from your .env (warning:
        sensitive credentials). A template is given at
        `inst/.env.template`.
  - If the Docker image does not work, inspect the docker logs, or debug
    with `docker run -it dbca-wa/turtleviewer2:latest /bin/bash -c
    "export TERM=xterm; exec bash"`. The console runs inside the Docker
    container, bypassing the Shiny app. There, you can run R and test
    whether all libraries (especially `turtleviewer2`) load
    successfully.
  - If the app as well as the Docker image work, commit and push.

## Deployment

Once app and Docker image work, create a new version, tag, and push the
tag.

    # One of these (R console)
    usethis::use_version(which="patch")
    usethis::use_version(which="minor")
    usethis::use_version(which="major")
    
    # Create a git tag (terminal) - use the current version
    git tag -a 'v0.0.1' -m 'v0.0.1'
    git push --tags

Pushing a new tag will build and publish a new Docker image to the
GitHub container registry.

In the Rancher console, edit the “turtleviewer” workload and update the
image to the latest tag.
