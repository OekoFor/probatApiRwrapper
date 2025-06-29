# probat API R wrapper

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of this R Package is to conveniently interact with the probatAPI in your R-Projects. One can GET, PATCH or POST data (also for authentification).


## Installation

You can install the development version of probatApiRWrapper from
[GitHub](https://github.com/) with: You will need your Github PAT for
authentication (privat repo).

Install a specific version

``` r
devtools::install_github("OekoFor/probatApiRWrapper@0.1.3", auth_token = {GITHUB_PAT})
```

OR Install the latest commit from the main branch

``` r
# install.packages("devtools")
devtools::install_github("OekoFor/probatApiRWrapper", auth_token = {GITHUB_PAT})
```

## Example

To interact with the API, you need a valid key. It is recommended to
store it as an environmental variable in `.Renviron` in the root
directory of th project. The envar must be named `ECOPI_API_KEY`.

``` r
library(probatApiRWrapper)
get_detections_list(params = list("project_name" = "017_neeri", "datetime__month" = 3))
get_latest_detections()
get_latest_detections_by_project(project_name = "017_neeri")
get_latest_detections_by_recorder(recorder_name = "00000000d76d0bf9")
get_latest_detections_by_recordergroup(project_name = "053_sion", recordergroup_name = "lapwing")
get_recordings_list(params = list("project_name" = "017_neeri", "datetime__month" = 3))
```
