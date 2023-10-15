## Resubmission

Previous CRAN Comments: 

Please always write package names, software names and API (application 
programming interface) names in single quotes in title and description. 
e.g: --> 'Tensorflow', 'Keras'
Please note that package names are case sensitive.

* The references to R Tensorflow and Keras have been changed to 'tensorflow' in accordance with the R package name.

Please proof-read your description text.
Currently it reads: "... and present a novel way of  learning and ..."
Probably it should be: "... and presents a novel way of  learning and  ..."

* The description has been proofread, and the subjec-verb agreements have been revised.

## R CMD check results

0 errors | 0 warnings | 4 notes

The following 2 notes only appear on Windows (Server 2022, R-devel 64-bit).

```
 * checking for detritus in the temp directory ... NOTE
 Found the following files/directories:
   'lastMiKTeXException'
```

* This has been noted as an issue with R-hub here: [R-hub issue #503](https://github.com/r-hub/rhub/issues/503)


```
 * checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```
* This also seems to be an issue with R-hub, as noted here: [R-hub issue #560](https://github.com/r-hub/rhub/issues/560)

The following note only appears on Fedora Linux (R-devel, clang, gfortran)

```
 * checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

* The HTML version of the manual is able to be validated locally and on Windows (Server 2022, R-devel 64-bit). This error does not seem critical.

The following note appears in some form for all operating systems, as this is a new submission.

```
 * checking CRAN incoming feasibility ... NOTE
Maintainer: 'Shayaan Emran <shayaan.emran@gmail.com>'

New submission
```
