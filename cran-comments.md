## R CMD check results

0 errors | 0 warnings | 1 or 2 notes (see below; see also submission comments)

* NOTE: Possibly misspelled words in DESCRIPTION (`Deslauriers`, `et`, `al`).
  These are not errors: they are the standard author citation
  `Deslauriers et al. (2017)` written to satisfy CRAN policy
  (`authors (year) <doi:...>` in the Description field). The same explanation
  is provided in this file for reviewers.

* NOTE (Windows only): Compilation used the non-portable flag `-Wa,-mbig-obj`.
  Present only in `src/Makevars.win`; required so the TMB-generated C++ object
  stays within the default COFF section limit on Windows. Not used on
  Debian/Linux. No portable alternative; other TMB packages often show the
  same NOTE on Windows.

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
* win-builder incoming pre-test — spelling NOTE (citation in Description) on
  all flavours; additional NOTE on Windows only (`-Wa,-mbig-obj`).
* win-builder Debian R-devel
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## Downstream dependencies

None.
