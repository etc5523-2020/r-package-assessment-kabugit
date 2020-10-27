
# covtrack
<!-- badges: start -->
[![R build status](https://github.com/etc5523-2020/r-package-assessment-kabugit/workflows/R-CMD-check/badge.svg)](https://github.com/etc5523-2020/r-package-assessment-kabugit/actions)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

covtrack was designed to provide a quick overview of COVID 19 cases and deaths in four major countries - USA, India, Brazil and Australia. An app is embedded inside this package that contains separate tabs for analysis and a "User guide" section that will help you understand how to execute visualisations.

An app such as this can be used by any entity that needs a proper understanding of the pandemic in the above mentioned countries. Not only can this help with getting a general overview of the COVID 19 story so far, but also influence decision making for future endeavours.

## Installation

You can install this package from github by typing the below code in your console:

``` r
devtools::install_github("https://github.com/etc5523-2020/r-package-assessment-kabugit.git")
```

## Some useful funcctions

Following functions will make your life simpler!

```r
launch_app()
```
A shortcut to launch the app.

```r
inputFun(id, label)
```
Creates drop down selection options in app UI to visualise a specific country that you can choose.

```r
leafSel(country, df)
```
Generates a leaflet map base, upon which you can include case or death numbers on.