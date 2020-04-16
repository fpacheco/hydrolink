import sys

# Hydrolink library name
if 'linux' in sys.platform:
    LIB_NAME = 'libhydrolink.so'
elif 'win' in sys.platform:
    LIB_NAME = 'hydrolink.dll'
elif 'darwin' in sys.platform:
    # Dont know!
    LIB_NAME = 'libhydrolink.so'
else:
    print('Dont know your platform!')

# Open files for
OPEN_READ = 0
OPEN_WRITE = 1
# Languages (base arrays)
LANG_C = 0
LANG_FORTRAN = 1
# Creator (to inform Wasp)
CREATOR_EFDC = 1
CREATOR_EFDCMPI = 1
CREATOR_DYNHYD = 2
CREATOR_EPDRIV1 = 3
CREATOR_HECRAS = 2
# Number of constituents in HYD files
NUM_MAX_SEGCONST = 5        # VOLUMEN, DEPTH, VELOCITY, TEMPERATURE, SALINITY
NUM_MAX_FPCONST = 3         # ADVECTIVE FLOW, DISPERSIVE FLOW, ??
# Segment data type
# 1=Segment Volume (m3), 2=Segment Depth (m), 3=Segment Velocity (m/sec)
SEG_VOL = 1
SEG_DEP = 2
SEG_VEL = 3
SEG_TEM = 4
SEG_SAL = 5
# Flow data type
# 1 = Advective flow, 2 = Dispersive flow, 3 = Direction
FLOWP_ADV = 1
FLOWP_DIS = 2
FLOWP_DIR = 3
