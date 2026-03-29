## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: Compilation used the following non-portable flag: `-Wa,-mbig-obj`.
  This flag is required on Windows to handle large object files generated
  by TMB (Template Model Builder). No portable alternative exists for
  this platform-specific constraint.

* NOTE: Found no calls to `R_registerRoutines`, `R_useDynamicSymbols`.
  This is the standard pattern for TMB packages, which handle native
  routine registration internally via their own mechanism.

## Test environments

* Local Windows 10, R 4.5.0
* win-builder R 4.6.0 alpha (2026-03-28) — 2 NOTEs, same as above
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## Downstream dependencies

None.