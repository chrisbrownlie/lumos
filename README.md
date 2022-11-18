
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lumos

<!-- badges: start -->

![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange)
![version:
0.0.0.9000](https://img.shields.io/badge/version-0.0.0.9000-blueviolet)
<!-- badges: end -->

{lumos} is an R package and shiny app that enable exploration of stage
lighting in candelit theatres and how different configurations affect
the light levels across different areas of the stage.

## Installation

You can install the development version of lumos from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chrisbrownlie/lumos")
```

## Classes

The package provides a set of R6 classes (stage, candelabra and candle)
which are primarily for use in the lumos app - and as such are
reactive-aware - but can also be used outside the app if desired.

## Lighting app

You can launch the lumos app with the code below, or visit it at
chrisbrownlie.com/lumos (not yet available).

``` r
library(lumos)

lumos::run_app()
```

## Contact

For more information, contact [Chris
Brownlie](mailto:chris.brownlie@hotmail.co.uk)
