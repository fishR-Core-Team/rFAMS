---
title: "CRAN Comments"
author: "Jason Doll"
date: "2026-01-13"
output: html_document
editor_options: 
  markdown: 
    wrap: 72
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

-   This updates the existing rFAMS package on CRAN with changes
    described in NEWS and minor changes.

## Notes

-   There may be a notification of invalid URLs (see below). These are
    not invalid URLs but rather the websites have an automated bot
    detector that does not let the winbuilder check access the sites. I
    have confirmed the links work when accessed by the browser.

-   Found the following (possibly) invalid URLs:

    -   URL: <https://fisheries.org>

    -   From: README.md

    -   Status: 403 Message: Forbidden

-   URL: <https://units.fisheries.org/fits>

    -   From: README.md

    -   Status: 403

    -   Message: Forbidden

## Testing Environments

-   My Windows machine.
-   Win Builder -- old-release, release, and development.
-   Mac Builder
-   GitHub Action (R-CMD-check.yaml)

## R CMD check results

There were no ERRORs or WARNINGs or other NOTEs.

## Downstream dependencies

There are currently no downstream dependencies for this package.
