## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: Possibly misspelled words in DESCRIPTION: `Deslauriers`, `et`, `al`.
  These words are part of the author citation `Deslauriers et al. (2017)`
  added to comply with CRAN policy requiring references in the form
  `authors (year) <doi:...>`. They are not misspellings.

* NOTE: Compilation used the following non-portable flag: `-Wa,-mbig-obj`.
  This flag is present only in `src/Makevars.win` (Windows-specific) and is
  required to compile the large TMB C++ object file on Windows. There is no
  portable alternative; other TMB-based CRAN packages (e.g. glmmTMB) carry
  the same note on Windows.

## Notes on par() / on.exit() usage

All exported plotting functions save graphics parameters with
`oldpar <- par(no.readonly = TRUE)` and restore them via `on.exit(par(oldpar))`
as the first statement after the `par()` call.

The internal helper `setup_plot_layout()` (not exported, `@keywords internal`)
intentionally modifies `par()` and returns the saved settings to the caller.
Every caller of this helper restores the settings through its own `on.exit()`
call. The helper itself cannot use `on.exit()` because its purpose is to leave
the layout active for the duration of the surrounding function.

## Test environments

* Local Windows 11, R 4.5.1
* win-builder R 4.6.0 alpha (2026-03-28 r89737) — 2 NOTEs (same as above)
* win-builder Debian R-devel — 0 NOTEs
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## Downstream dependencies

None.
