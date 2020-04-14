# What this it?
Python wrapper for WASP (water quality) libhydrolink

## Why?
Need to output from EFDC-MPI (https://github.com/fearghalodonncha/EFDC-MPI) to WASP8 via hydrolink!

## Work from
1. hydrolink.f95
2. https://www.epa.gov/sites/production/files/2018-05/documents/stream-transport-user-guide.pdf

## Functions
* hladddescription
* hlclose
* hlgetcompfact
* hlgetcreator
* hlgetcurrentmoment
* hlgetdebug
* hlgetdescription
* hlgetendmoment
* hlgetflowinfo
* hlgetflowpath
* hlgetfpconsttype
* hlgethydtimestep
* hlgethydtowaspratio
* hlgetlanguage
* hlgetlasterror
* hlgetnthmoment
* hlgetnumdescriptions
* hlgetnumflowpaths
* hlgetnumfpconsts
* hlgetnumlayers
* hlgetnummoments
* hlgetnumsegconsts
* hlgetnumsegments
* hlgetseedmoment
* hlgetsegconsttype
* hlgetseginfo
* hlgetsegname
* hlgettimestep
* hlgetupdateint
* hlgetvartimestep
* hlmomentcomplete
* hlopen
* hlsetcompressionlevel
* hlsetcreator
* hlsetdebug
* hlsetflowinfo
* hlsetflowpath
* hlsetfpconsttype
* hlsethydtimestep
* hlsethydtowaspratio
* hlsetlanguage
* hlsetlasterror
* hlsetnumflowpaths
* hlsetnumfpconsts
* hlsetnumlayers
* hlsetnumsegconsts
* hlsetnumsegments
* hlsetseedmoment
* hlsetsegconsttype
* hlsetseginfo
* hlsetsegname
* hlsettimestep
* hlsetupdateint
* hlsetvartimestep

## From EFDC-MPI to WASP8
1. EFDC-MPI outputs are NetCDF4 files
2. You need others

## Test
Put lihydrolink.so in your path or in LD_LIBRAY_PATH:
* export LD_LIBRARY_PATH=/disco_local/rpacheco/USEPA-WASP8.32/wasp/lib:/disco_local/rpacheco/workspace/hydrolink/old
* python -m unittest
