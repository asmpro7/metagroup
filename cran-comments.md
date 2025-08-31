## CRAN Submission for metagroup 1.0.0

This is a new submission.

The `metagroup` package provides tools to perform a two-step "meaningful subgrouping" in meta-analysis. First, it partitions studies into statistically homogeneous clusters. Second, it provides functions to analyze and interpret the composition of these new subgroups based on study-level characteristics.

## Test Environments
* local: Windows 10, R 4.3.3
* R-hub builder:
  * Windows Server 2022, R-devel, GCC
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * macOS 12.6.5 Monterey, R-release, clang
* CRAN `win-builder`: R-devel

## R CMD check results
There were no ERRORs or WARNINGs on any platform.

There were two NOTEs on the `win-devel` check:

1.  `checking CRAN incoming feasibility ... NOTE`
    * This is a new package submission.

2.  `checking for future file timestamps ... NOTE`
    * This is a known issue related to the build environment and not a problem with the package code itself.

## Reverse dependencies
This is a new package, so there are no reverse dependencies.

---

Thank you for your time and consideration.
