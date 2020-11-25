
## Resubmission
This is a resubmission. In this version I have:

* Added dependencies on methods to remove the NOTE

* Added dependency on the Bioconductor Basilisk package.

As a consequence of using the Basilisk package, the package now has three notes:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Hector Roux de Bezieux <hector.rouxdebezieux@berkeley.edu>’
New submission 

* checking whether package ‘Ecume’ can be installed ... NOTE
Found the following notes/warnings:
  Non-staged installation was used

* checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
             user system elapsed
  mmd_test 91.541    8.2 118.757

The last two notes come form the Basilisk package, which requires non-staged installation and need to install the python dependencies the first time
an exemple is run. 

## Test enviromnments

Github action: 

* os: windows-latest, r: 'release', bioc: '3.12'
* os: macOS-latest, r: 'release', bioc: '3.12'
* os: macOS-latest, r: 'devel', bioc: 'devel'
* os: ubuntu-20.04, r: 'release', bioc: '3.12'
* os: ubuntu-20.04, r: 'devel', bioc: 'devel'