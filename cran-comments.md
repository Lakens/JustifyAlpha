#Test environments

local windows 10: R 4.1.0 

macOs-latest, ubuntu 20.4, and ubuntu 20.4 (devel) with github actions.

#R CMD check results

There is one warning about qpdf needed for size reduction of pdfs in the examples. However, this warning is likely spurious as (1) qpdf is imported (2) it is not actually needed as there are no pdfs in the examples. 

Otherwise no warnings, notes, or errors. 


#Downstream dependencies

There are no downstream dependencies for this package.