## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: Compilation used the following non-portable flag: `-Wa,-mbig-obj`.
  This flag is present only in `src/Makevars.win` (Windows-specific) and is
  required to compile the large TMB C++ object file on Windows. There is no
  portable alternative; other TMB-based CRAN packages (e.g. glmmTMB) carry
  the same note on Windows.

## Test environments

* Local Windows 10, R 4.5.0
* win-builder R 4.6.0 alpha (2026-03-28 r89737) — 1 NOTE (compilation flag)
* win-builder Debian R-devel — 0 NOTEs
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## Downstream dependencies

None.
