## Test environments

* Local: Windows 11 x64 (R-4.5.0)
* Win-builder: R-oldrelease 4.4.3; R-release 4.5.1;
  R-devel (Windows Server 2022)
* R-hub: Ubuntu 24.04 (R-devel); macOS Ventura 13.7 (R-devel);
  macOS Sequoia 15.5 (R-devel); Ubuntu 22.04 (R-4.5.1 Patched, R-4.5.1);
  Windows Server 2022 (R-devel)

## R CMD check results

0 errors | 0 warnings | 1-2 notes on Win-builder (see below)

## Submission

* Initial release to CRAN. No reverse dependencies.

## Notes for CRAN editors

* NOTE (Win-builder): “Diachronic/diachronic” in DESCRIPTION is a correct
  technical term referring to diachronic analysis.

* NOTE (Win-builder, R-oldrelease) about `Author:` field: current DESCRIPTION
  does not contain `Author:` and matches `Authors@R`.