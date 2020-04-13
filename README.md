# Contents

## To compile:
gfortran -fno-underscoring -L. hydrolink.f95 -lhydrolink -o hydrolink

¿libhydrolink.so? From WASP8 intallation directory.

## hydrolink.ctl
Commend file for hydrolink (lines):
1. Read type: ASCII or Binary;
2. Source program: EFDMPI = M or m;
3. Input file: Input_Name;
4. Output file (generate): Output_Name.HYD (to be read from WASP8);
5. Enter Start Day (dd)
6. Enter Start Year (yyyy)
7. Enter Start Hour (hh)
8. Enter Start Minute (mm)
9. Enter Start Second (ss)



