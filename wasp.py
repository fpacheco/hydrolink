"""This module is a wrapper for libhydrogeo (WASP 8)"""
import sys
import os
import datetime
import ctypes as ct

# Hydrolink library name
if 'linux' in sys.platform:
    LIB_NAME = 'libhydrolink.so'
elif 'win' in sys.platform:
    LIB_NAME = 'hydrolink.dll'
elif 'darwin' in sys.platform:
    # Dont know!
    LIB_NAME = 'hydrolink.dll'
else:
    print('Dont know your platform!')

# Open files for
OPEN_READ = 0
OPEN_WRITE = 1
# Languages
LANG_C = 0
LANG_FORTRAN = 1
# Creator
CREATOR_EFDC = 1
CREATOR_EFDCMPI = 1
CREATOR_DYNHYD = 2
CREATOR_EPDRIV1 = 3
CREATOR_HECRAS = 2
# Segment data type
SEG_VOLS = 1
SEG_DEPS = 2
SEG_VELS = 3

class Wasp(object):

    def __init__(self, libname=LIB_NAME, libpath=None):
        """Initialization for class

        Seach and load the library ... else exit.

        Args:
            libname (str): The library name (libhydrolink.so on GNU/Linux)
            libpath (str): The path where we search for the file

        """
        if not libpath:
            # Get LD_LIBRARY_PATH
            ldp = list()
            if 'LD_LIBRARY_PATH' in os.environ:
                ldp = os.environ['LD_LIBRARY_PATH'].split(':')
            # Append current directory
            ldp.append(os.getcwd())
            # Is there?
            for p in ldp:
                fp = os.path.join(p, libname)
                if os.path.exists(fp):
                    libpath = p
                    break
        if not libpath:
            print('Can NOT find {}'.format(LIB_NAME))
            sys.exit(-1)
        self.lpath = os.path.join(libpath, libname)
        self.hydro = ct.cdll.LoadLibrary(self.lpath)
        self.cfpath = None
        self.hndl = None
        self.dlevel = -1

    def setDebug(self, dlevel=0):
        """Set the libhydrogeo debug level

        Seach and load the library.

        Args:
            dlevel (int): Set the level. Greather than O to get log outputs in log files.
        """
        d = ct.c_int(dlevel)
        self.hydro.hlsetdebug(ct.byref(d))
        self.dlevel = dlevel

    def getLastError(self):
        """Get last libhydrogeo error.

        Returns:
            str: The string with the error
        """
        m = ct.c_char_p(b''*72)
        self.hydro.hlgetlasterror(m)
        return m.value.decode('utf-8')

    def open(self, fpath, fmode=OPEN_WRITE):
        """Open file for reading or writing

        Args:
            fpath (str): The file path for open.
            fmode (int): 0 = read; 1 = write.
        """
        fp = ct.c_char_p(fpath.encode())
        fm =  ct.c_int(fmode)
        fh = ct.c_int(0)
        ie = ct.c_int(0)
        self.hydro.hlopen(fp, ct.byref(fm), ct.byref(fh), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())
            self.hndl = -1
        else:
            # Raw fh
            self.cfpath = fpath
            # By ref!
            self.hndl = ct.byref(fh)

    def close(self):
        """Close an opened file"""
        ie = ct.c_int(0)
        if self.hndl:
            self.hydro.hlclose(self.hndl, ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())

    def compactFactor(self):
        """Get compression factor for the file.

        Returns:
            int: The compact factor
        """
        cf = ct.c_int(1)
        ie = ct.c_int(0)
        if self.hndl:
            self.hydro.hlgetcompfact(self.hndl, ct.byref(cf), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.cf = cf.value*100

    def setLang(self, lang=LANG_C):
        """Set programming language for the library

        Fortran use base 1 arrays. C, C++, python use base 0.

        Args:
            lang (int): 0 = C; 1 = Fortran.
        """
        fl = ct.c_int(lang)
        ie = ct.c_int(0)
        self.hydro.hlsetlanguage(self.hndl, ct.byref(fl), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())

    def setCreator(self, creator=CREATOR_EFDCMPI):
        """Inform the library who is the creator for this HYD file

        libhydrolink recognizes EFDC, HECRAS, DYNHYD, EPDRIV1 = 3

        Args:
            creator (int): 1 = EFDC and EFDCMPI ; 2 = HECRAS and DYNHYD, 3=EPDRIV1.
        """
        fc =  ct.c_int(creator)
        ie = ct.c_int(0)
        self.hydro.hlsetcreator(self.hndl, ct.byref(fc), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())

    def setDescriptions(self, desc=list()):
        """Add descriptions to the HYD file

        Args:
            desc (list): List of string with descriptions
        """
        if len(desc) > 0:
            n = len(desc)
            for i in range(n):
                fd =  ct.c_char_p(desc[i].encode())
                # 0 for decriptions
                dd = ct.c_int(0)
                ie = ct.c_int(0)
                self.hydro.hladddescription(
                    self.hndl,
                    ct.byref(dd),
                    fd,
                    ct.byref(ie)
                )
                if ie.value > 0:
                    print(self.getLastError())
        self.desc = desc

    def setAuthor(self, author):
        """Add autor to the HYD file

        Args:
            author (str): The autor name
        """
        fd =  ct.c_char_p(author.encode())
        # 1 for modeller name
        dd = ct.c_int(1)
        ie = ct.c_int(0)
        self.hydro.hladddescription(
            self.hndl,
            ct.byref(dd),
            fd,
            ct.byref(ie)
        )
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.author = author

    def setMoment(self, dt):
        """Set the initial time and date for th hydrodynamic information in the linkage file.

        Args:
            dt (datetime.datetime): The date and time.
        """
        ida = ct.c_int(dt.day)
        imo = ct.c_int(dt.month)
        iye = ct.c_int(dt.year)
        iho = ct.c_int(dt.hour)
        imi = ct.c_int(dt.minute)
        ise = ct.c_int(dt.second)
        ie = ct.c_int(0)
        self.hydro.hlsetseedmoment(
            self.hndl,
            ct.byref(imo),
            ct.byref(ida),
            ct.byref(iye),
            ct.byref(iho),
            ct.byref(imi),
            ct.byref(ise),
            ct.byref(ie)
        )
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.moment = dt

    def setNumLay(self, nlays=1):
        """Set the number of layers for the hydrodynamic information in the linkage file.

        Args:
            nlays (int): The number of layers in the model.
        """
        fn = ct.c_int(nlays)
        ie = ct.c_int(0)
        self.hydro.hlsetnumlayers(self.hndl, ct.byref(fn), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.nlays = nlays

    def setNumSeg(self, nsegs):
        """Set the number of segments for the hydrodynamic information in the linkage file.

        Args:
            nsegs (int): The number of segments in the model.
        """
        fn = ct.c_int(nsegs)
        ie = ct.c_int(0)
        self.hydro.hlsetnumsegments(self.hndl, ct.byref(fn), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.nsegs = nsegs

    def setSegNames(self, segsn=list()):
        """Set segments names.

        Args:
            segsn (list): A list with segments names.
        """
        if len(segsn) > 0:
            n = len(segsn)
            for i in range(n):
                sn = ct.c_char_p(segsn[i].encode())
                se = ct.c_int(i)
                ie = ct.c_int(0)
                self.hydro.hlsetsegname(
                    self.hndl,
                    ct.byref(se),
                    sn,
                    ct.byref(ie)
                )
                if ie.value > 0:
                    print(self.getLastError())
        self.segsn = segsn

    def setTimeStep(self, ts):
        """specifies  the  timestep  that  is  used  for  the  hydrodynamic  model  simulation.

        This value only needs to be set once

        Args:
            ts (int): The time step used in the hydrodynamic models.
        """
        ts = ct.c_int(ts)
        ie = ct.c_int(0)
        self.hydro.hlsethydtimestep(self.hndl, ct.byref(ts), ct.byref(ie))
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.ts = ts

    def _setSegData(self, stype, data):
        """Set segments data.

        Args:
            stype (int): Type information. 1=Segment Volume (m3), 2=Segment Depth (m), 3=Segment Velocity (m/sec)
            data (list): List of floats with data
        """
        n = len(data)
        av = (ct.c_double * n)()
        for i in range(n):
            av[i] = data[i]
        si = ct.c_int(stype)
        ie = ct.c_int(0)
        self.hydro.hlsetseginfo(
            self.hndl,
            ct.byref(si),
            ct.byref(av),
            ct.byref(ie)
        )
        if ie.value > 0:
            print(self.getLastError())
        else:
            self.ts = ts

    def setSegVolume(self, vols):
        """Set segments volumes.

        Args:
            vols (list): List of floats with segments volumes
        """
        self._setSegData(SEG_VOLS, vols)

    def setSegDepth(self, deps):
        """Set segments depths.

        Args:
            deps (list): List of floats with segments depths
        """
        self._setSegData(SEG_DEPS, deps)

    def setSegVelocity(self, vels):
        """Set segments velocities.

        Args:
            vels (list): List of floats with segments velocities
        """
        self._setSegData(SEG_VELS, vels)
