
- <a href="#hermione" id="toc-hermione">Hermione</a>
  - <a href="#installation" id="toc-installation">Installation</a>
  - <a href="#how-to-run-the-hermione-dashboard"
    id="toc-how-to-run-the-hermione-dashboard">How to run the Hermione
    dashboard</a>
- <a href="#references" id="toc-references">References</a>
- <a href="#hermione-dev-team" id="toc-hermione-dev-team">HERMIONE dev
  team</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Hermione

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The HERMIONE shiny dashboard, developed as part of the Social Inequality
Observatory of the MUHAI project, allows you to interactively filter and
explore co-occurrence entity networks, fine-grained narratives analysis,
and case studies on inequality perception using online data from
Twitter, collected in MUHAI’s Observatory Knowledge Graph. With HERMIONE
you can: (i) Gain insights into how specific forms of inequality are
perceived, and understood across time and contexts. (ii) Understand
reactions and dialectical challenges to inequality by individuals,
institutions, and organizations. (iii) Access powerful methods for
retrieving, filtering, aggregating, and analyzing online debates about
inequality from different perspectives.

## Installation

You can install the development version of Hermione from GitHub (main
branch) as follows:

``` r
install.packages("remotes")
library(remotes)
remotes::install_github("carlosantagiustina/HERMIONE")
```

## How to run the Hermione dashboard

This is a basic example which shows you how to solve a common problem:

``` r
#Load Hermione library
library(Hermione)
#run Hermione app
Hermione::run_app()
#Enjoy!
```

# References

# HERMIONE dev team

- (creator & maintainer) Carlo R. M. A. Santagiustina
  <carlo.santagiustina@univiu.org>

- (contributor) Laura Spillner <laura.spillner@uni-bremen.de>

  Hermoine queries and consumes in real time data from MUHAI's OKG. Source code and information available at this link: (https://github.com/muhai-project/okg_media_discourse)[https://github.com/muhai-project/okg_media_discourse]
