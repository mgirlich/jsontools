## Resubmission
This is a resubmission. In this version I have:

* Documented return values in `json_merge.Rd`, `parse_json.Rd`, `read_json.Rd`,
  and `vec_cast.json2.Rd`.
* In the following documenting the return type does not really make sense:
  * `object.Rd`: This is not a documentation of a function or data but only a
    separate topic to be shown in the help.
  * `pipe.Rd`: Documentation of the magrittr pipe `%>%`.

## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
