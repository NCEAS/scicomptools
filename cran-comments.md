## Resubmission
This is a resubmission.

### Attempt 2
In this version I have:

* Added an entry in the Authors@R field of the DESCRIPTION file that references the University of California, Santa Barbara as the copyright holder.

### Attempt 1
In this version I have:

* Added more content to the Description field in the DESCRIPTION file.
* Defined the NCEAS acronym in the DESCRIPTION and README.
* Added a @return tag to `token_check.R`.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
There were no ERRORs or WARNINGs. There were 4 NOTEs.

```
❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), and fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [12s] NOTE
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    NCEAS (3:27)
  Maintainer: 'Angel Chen <anchen@nceas.ucsb.edu>'
```
This is the first submission of this package to CRAN. Also, "NCEAS" is not a typo.

```
❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
```
As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this note can be safely ignored. 

```
❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

```
❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
```
We cannot update `tidy` on these external test environments. 


0 errors ✔ | 0 warnings ✔ | 4 notes ✖


## Downstream dependencies

There are currently no downstream dependencies for this package.