## demutils
Demutils is demscore's internal R package, including all translation functions, 
functions to generate the reference documents, functions to generate data, etc. 
The functions are called in various scripts available in the infrastructure_construction
repository.
The demutils package needs to be updated when new output units are created or we want to 
make existing datasets and variables available in new/more output units. 
We also need to update 
the functions in this package whenever we add new features to the download interface, 
such as excluding empty rows,  filtering customized datasets by countries and years, 
etc. Updating demutils and enuring that all existing functionalities continue 
working is usually the most time consuming and challenging part of each update cycle. 

The functions and tables in the database need to be updated correctly for the 
secondary data generation to start. 
Each directory contains the complete code for the R package of the respective 
version of Demscore.

Currently, this repository only includes the functions we use, but is not structured
as an R package that can be installed by Demscore users, as several functions load *impure* 
in a sense that they load locally stored files. We are however working on creating
a public R package.

If you have any questions, please do not hesitate to contact us via email:
contact@demscore.se or melina.liethmann@demscore.se

License
Shield: [![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
