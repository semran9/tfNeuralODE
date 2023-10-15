## R CMD check results

0 errors | 0 warnings | 3 notes

These notes only appear on Windows (Server 2022, R-devel 64-bit).

```
 * checking for detritus in the temp directory ... NOTE
 Found the following files/directories:
   'lastMiKTeXException'
```

This has been noted as an issue with R-hub here: [R-hub issue #503](https://github.com/r-hub/rhub/issues/503)


```
 * checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```
This also seems to be an issue with R-hub, as noted here: [R-hub issue #560](https://github.com/r-hub/rhub/issues/560)

```
 * checking CRAN incoming feasibility ... NOTE
Maintainer: 'Shayaan Emran <shayaan.emran@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Keras (8:50)
  Tensorflow (3:59, 8:35)
  al (9:55)
  et (9:52)''
```

* This is a new release.
