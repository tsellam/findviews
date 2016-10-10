## Test environments
* local OS X install, R 3.3.0
* win-builder (R Under development 2016-10-08 r71479)

## R CMD check results
There were no ERRORs and no WARNINGs.
One NOTE: "Days since last update: 1" - indeed, this patch is necessary because 
the previous version failed for many users (a dependecy was wrong).


## Downstream dependencies
I have run R CMD check on all the downstream dependencies of findviews.
No ERRORs or WARNINGs were found.
