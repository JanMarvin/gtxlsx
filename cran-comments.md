## Test environments

* local Ubuntu 24.04, R 4.3.3

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

`gtxlsx` calls one internal function of `gt`, `build_data()`, through
`utils::getFromNamespace()`. `gt` has no public accessor for a built table,
and reproducing that step would mean reimplementing a large part of the
package. The call is isolated in a single helper that checks the function is
available and fails with an explanatory message if it is not, and a test
asserts that every component gtxlsx reads is present, so a change on gt's side
surfaces as a test failure rather than as silently wrong output.
