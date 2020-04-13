       Program HydroLink
!=======================================================================
!      interface
!      subroutine hlsetdebug(hl_debug)
!         !ms$attributes c,dllimport,alias:'__hlsetdebug'::hlsetdebug
!         Integer hl_debug
!         !ms$attributes reference :: hl_debug
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlgetlasterror(message)
!         !ms$attributes c,dllimport,alias:'__hlgetlasterror'::hlgetlasterror
!         character*(*) message
!         !ms$attributes reference :: message
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlopen(FName, hl_mode, hl_handle, ierror)
!         !ms$attributes c,dllimport,alias:'__hlopen'::hlopen
!      character*(*) Fname
!      Integer hl_mode, hl_handle, ierror
!         !ms$attributes reference :: FName, hl_mode, hl_handle, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetlanguage(hl_handle, hl_language, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetlanguage'::hlsetlanguage
!      Integer hl_handle, hl_language, ierror
!         !ms$attributes reference :: hl_handle, hl_language, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetcreator(hl_handle, hl_creator, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetcreator'::hlsetcreator
!      Integer hl_handle, hl_creator, ierror
!        !ms$attributes reference :: hl_handle, hl_creator, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hladddescription(hl_handle, id, string, ierror)
!         !ms$attributes c,dllimport,alias:'__hladddescription'::hladddescription
!      Integer hl_handle, id, ierror
!      character *(*) string
!        !ms$attributes reference :: hl_handle, id, string, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetseedmoment(hl_handle, month, day, year, hour,
!     .           minute,second,ierror)
!      !ms$attributes c,dllimport,alias:'__hlsetseedmoment'::hlsetseedmoment
!     Integer hl_handle, month, day, year, hour, minute,second,ierror
!      !ms$attributes reference :: hl_handle, month, day, year, hour
!     !ms$attributes reference :: minute,second,ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetnumsegments(hl_handle, numsegs, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetnumsegments'::hlsetnumsegments
!      Integer hl_handle, numsegs, ierror
!        !ms$attributes reference :: hl_handle, numsegs,ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetsegname(hl_handle,index, segname, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetsegname'::hlsetsegname
!      Integer hl_handle, index, ierror
!      Character *(*) segname
!        !ms$attributes reference :: hl_handle, index, segname,ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetnumflowpaths(hl_handle,numfp, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetnumflowpaths'::hlsetnumflowpaths
!      Integer hl_handle, numfp, ierror
!        !ms$attributes reference :: hl_handle, numfp, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetnumsegconsts(hl_handle,numsc, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetnumsegconsts'::hlsetnumsegconsts
!      Integer hl_handle, numsc, ierror
!        !ms$attributes reference :: hl_handle, numsc, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetnumfpconsts(hl_handle,numfpc, ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetnumfpconsts'::hlsetnumfpconsts
!      Integer hl_handle, numfpc, ierror
!        !ms$attributes reference :: hl_handle, numfpc, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetsegconsttype(hl_handle,sc_index, sc_type,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetsegconsttype'::hlsetsegconsttype
!      Integer hl_handle, sc_index, sc_type, ierror
!        !ms$attributes reference :: hl_handle, sc_index, sc_type, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetfpconsttype(hl_handle,fp_index, fp_type,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetfpconsttype'::hlsetfpconsttype
!      Integer hl_handle, fp_index, fp_type, ierror
!        !ms$attributes reference :: hl_handle, fp_index, fp_type, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetvartimestep(hl_handle,vardt,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetvartimestep'::hlsetvartimestep
!      Integer hl_handle, vardt, ierror
!        !ms$attributes reference :: hl_handle,vardt, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsethydtimestep(hl_handle,timestep,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsethydtimestep'::hlsethydtimestep
!      Integer hl_handle, ierror
!      Real timestep
!        !ms$attributes reference :: hl_handle,timestep, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetupdateint(hl_handle,updateinterval,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetupdateint'::hlsetupdateint
!      Integer hl_handle, ierror
!      Real updateinterval
!        !ms$attributes reference :: hl_handle,updateinterval, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsethydtowaspratio(hl_handle,iratio,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsethydtowaspratio'::hlsethydtowaspratio
!      Integer hl_handle, iratio, ierror
!        !ms$attributes reference :: hl_handle,iratio, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetnumlayers(hl_handle,numlayers,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetnumlayers'::hlsetnumlayers
!      Integer hl_handle, numlayers, ierror
!        !ms$attributes reference :: hl_handle,numlayers, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetflowpath(hl_handle,flow_index,from_seg, to_seg,
!     .           direction, ierror)
!      !ms$attributes c,dllimport,alias:'__hlsetflowpath'::hlsetflowpath
!      Integer hl_handle, flow_index,from_seg, to_seg,direction,ierror
!      !ms$attributes reference :: hl_handle,flow_index,from_seg
!     !ms$attributes reference :: to_seg,direction,ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetflowinfo(hl_handle,index,value,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsetflowinfo'::hlsetflowinfo
!      Integer hl_handle, index, ierror
!      Real value
!        !ms$attributes reference :: hl_handle,index, value, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsetseginfo(hl_handle,index,value,ierror)
!      !ms$attributes c,dllimport,alias:'__hlsetseginfo'::hlsetseginfo
!     Integer hl_handle, index, ierror
!     Real value
!      !ms$attributes reference :: hl_handle,index, value, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlsettimestep(hl_handle,value,ierror)
!         !ms$attributes c,dllimport,alias:'__hlsettimestep'::hlsettimestep
!      Integer hl_handle, ierror
!      Real value
!        !ms$attributes reference :: hl_handle,value, ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlmomentcomplete(hl_handle,ierror)
!         !ms$attributes c,dllimport,alias:'__hlmomentcomplete'::hlmomentcomplete
!      Integer hl_handle, ierror
!        !ms$attributes reference :: hl_handle,ierror
!      end subroutine
!      end interface
!=======================================================================
!      interface
!      subroutine hlclose(hl_handle,ierror)
!         !ms$attributes c,dllimport,alias:'__hlclose'::hlclose
!      Integer hl_handle, ierror
!      Real value
!        !ms$attributes reference :: hl_handle,ierror
!      end subroutine
!      end interface
!=======================================================================
      Integer, Allocatable, Dimension (:):: IQ, JQ, IFLOWDIR
      Real, Allocatable, Dimension (:):: SegVolume, SegDepth, SegVel, Flow, crnu, brintt,SegSal,SegTemp
      integer*4 Ihl_handle
      Character(len=4):: SEGID
      character*1 ANS
      character*30 segname
      character*256 HLFILE,INFILE,segfile
      character*256 DESCRIPTION(10)
      character*256 MODELERNAME
      character*256 errstring
      logical binary, ConfigFile, EFDC, EFDCMPI, DYNHYD, EPDRIV1, HECRAS
!=======================================================================
!      Opening Message
!=======================================================================
!      write(6,6120)
!6120  format(3(/),62('-'),/,
!     'The purpose of this program is to convert a previously created',/ &
!     'hydrodynamic linkage file from EFDC, DYNHYD, EPDRIV1 to the   ',/ &
!     'new HYDROLINK method.  This is required for the latest version',/ &
!     'of WASP.  The user must specify the path and filename of all  ',/ &
!     'files to be created, must specify the file type and from which',/ &
!     'hydrodynamic model created the old file.  The user must also  ',/ &
!     'specify the start time (Gregorian Format) of the hydrodynamic ',/ &
!     'linkage file.                                                 ',/ &
!      62('-'),2(/))
!=======================================================================
!      hlopen parameters
!=======================================================================
      Ihl_language    = 1
      Ihl_creator     = 1
      Ihl_handle      = 0
      Ihl_debug       = 1
      Ihl_mode        = 1
      inumsegconsts   = 5
      inumfpconsts    = 3
      binary          = .true.
      ConfigFile      = .false.
      EFDC            = .false.
      EFDCMPI         = .false.
      DYNHYD          = .false.
      EPDRIV1         = .false.
      HECRAS          = .false.
!=======================================================================
!    Open the Control File
!=======================================================================
      open (unit=10,file='hydrolink.ctl',status='old',iostat=istat)
      if (istat .eq. 0) then
!         write(6,*)'Previous Control File Found'
!         write(6,*)'Do you want to read from File (Y=Yes, N=No)'
!         read(5,*)ANS
!         If(ANS .eq. 'Y' .or. ANS .eq. 'y')ConfigFile=.true.
!         If(ANS .eq. 'A' .or. ANS .eq. 'a')ConfigFile=.false.
         ConfigFile=.true.
         if (ConfigFile) then
            write(6,*)'Reading Information from HYDROLINK.CTL'
         else
            close(unit=10)
         endif
      endif
!=======================================================================
      if (ConfigFile)then

         read(10,*)ANS
         If(ANS .eq. 'A' .or. ANS .eq. 'a')binary=.false.
         If(ANS .eq. 'B' .or. ANS .eq. 'b')binary=.true.

         read(10,*)ANS
         If(ANS .eq. 'E' .or. ANS .eq. 'e')EFDC=.true.
         If(ANS .eq. 'M' .or. ANS .eq. 'm')EFDCMPI=.true.
         If(ANS .eq. 'D' .or. ANS .eq. 'd')DYNHYD=.true.
         If(ANS .eq. 'R' .or. ANS .eq. 'r')EPDRIV1=.true.
         If(ANS .eq. 'H' .or. ANS .eq. 'h')HECRAS=.true.

         read(10,1000)INFILE
         if (binary) then
            open(unit=1,file=INFILE,form='unformatted',status='old',  &
     	  iostat=istat)
            if (istat .gt. 0) then
               write(6,*)'HYD File not Found: '
               stop
            endif
         else
            open(unit=1,file=INFILE,status='old',iostat=istat)
            if (istat .gt. 0) then
               write(6,*)'HYD File not Found: '
               stop
            endif
         endif

         read(10,1000)HLFILE
         HLFILE=trim((HLFILE)//CHAR(0))
         read(10,*)istartmonth
         read(10,*)istartday
         read(10,*)istartyear
         read(10,*)istarthour
         read(10,*)istartminute
         read(10,*)istartsecond
         read(10,1000)segfile
         READ(10,*)NUM_DESCRIPTIONS
         do i=1,NUM_DESCRIPTIONS
            READ(10,1111)DESCRIPTION(I)
1111        format(A256)
         end do
1000     format(A64)
      else
!=======================================================================
         open (unit=10,file='hydrolink.ctl',status='unknown')
         write(6,*)'Is Old HYD ASCII or Binary (A=ASCII, B=Binary)'
         read(5,*)ANS
         write(10,1060)ANS
1060     format(A1)
         If(ANS .eq. 'A' .or. ANS .eq. 'a')binary=.false.
         If(ANS .eq. 'B' .or. ANS .eq. 'b')binary=.true.
!=======================================================================
         write(6,*)'Enter File Type (E=EFDC, D=DYNHYD, R=EPDRIV1, H=HECRAS)'
         read(5,*)ANS
         write(10,1060)ANS
         If(ANS .eq. 'E' .or. ANS .eq. 'e')EFDC=.true.
         If(ANS .eq. 'D' .or. ANS .eq. 'd')DYNHYD=.true.
         If(ANS .eq. 'R' .or. ANS .eq. 'r')EPDRIV1=.true.
         If(ANS .eq. 'H' .or. ANS .eq. 'h')HECRAS=.true.
!=======================================================================
         write(6,*)'Enter Name of Previous Version HYD File to Convert'
         read(5,*)INFILE
         write(10,1000)INFILE
         if (binary) then
            open(unit=1,file=INFILE,form='unformatted',status='old', iostat=istat)
            if (istat .gt. 0) then
               write(6,*)'HYD File not Found: '
               stop
            endif
         else
            open(unit=1,file=INFILE,status='old',iostat=istat)
            if (istat .gt. 0) then
               write(6,*)'HYD File not Found: '
               stop
            endif
         endif
!=======================================================================
         write(6,*)'Enter Name of HYDROLINK HYD File to Create'
         read(5,*)HLFILE
         write(10,1000)HLFILE
!=======================================================================
         Write(6,6600)
6600     format('Seed Time Information Needed for the Start of  ',/'  Hydrodynamic Linkage File mm/dd/yyyy hh:mm:ss')
         write(6,*)'Enter Start Month (mm)'
         read(5,*)istartmonth
         write(6,*)'Enter Start Day (dd)'
         read(5,*)istartday
         write(6,*)'Enter Start Year (yyyy)'
         read(5,*)istartyear
         write(6,*)'Enter Start Hour (hh)'
         read(5,*)istarthour
         write(6,*)'Enter Start Minute (mm)'
         read(5,*)istartminute
         write(6,*)'Enter Start Second (ss)'
         read(5,*)istartsecond
         write(10,*)istartmonth
         write(10,*)istartday
         write(10,*)istartyear
         write(10,*)istarthour
         write(10,*)istartminute
         write(10,*)istartsecond
         write(6,*)'Enter Name of Segment Name File (Type NONE)'
         read(5,1000)segfile
         Write(10,1000)segfile
         write(6,6610)
6610     format('How Many Description Lines would you like to add to the file (0=None)')
         read(5,*)NUM_DESCRIPTIONS
         write(10,*) NUM_DESCRIPTIONS
         do i=1, NUM_Descriptions
            write(6,*)'Enter Description:',I
            read(5,1111)description(i)
            write(10,1111)description(i)
         end do
         close(unit=10)
      End If
      if(EFDC)MODTYPE    =1
      if(DYNHYD)MODTYPE  =2
      if(EPDRIV1)MODTYPE =3
      if(HECRAS)MODTYPE  =2
!=======================================================================
      HLFILE=(TRIM(HLFILE)//CHAR(0))
      write(6,*)'About to Create Hydrolink File'
      call hlopen(HLFILE, Ihl_mode, Ihl_handle, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      write(6,*)'Hydrolink File Created'
!=======================================================================
!     Set the language to FORTRAN
!=======================================================================
      call hlsetlanguage(Ihl_handle, 1, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!     Store a description string
!======================================================================
      write(6,*)'Storing Descriptions'
      do i =1,NUM_DESCRIPTIONS
         call hladddescription(Ihl_handle,0,DESCRIPTION(I),ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
      end do
!=======================================================================
!     Store the modeler name
!=======================================================================
!      call hladddescription(Ihl_handle,1,MODELERNAME,ierror)
!      if(ierror .gt. 0)then
!          call hlgetlasterror(errstring)
!          write(6,6000) ierror, errstring
!          stop
!      end if
!=======================================================================
!     Set the creator
!=======================================================================
      write(6,*)'Setting Creator'
      call hlsetcreator(Ihl_handle, MODTYPE, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!     Set the seed moment
!=======================================================================
      write(6,*)'Setting Seed Moment'
      IF (.not. HECRAS) THEN
      call hlsetseedmoment(Ihl_handle, istartmonth, istartday,istartyear, istarthour, istartminute, istartsecond, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      ENDIF
!=======================================================================
! Read the Header from Hydrodynamic Linkage File
!=======================================================================
      write(6,*)'Starting to Process Hydrodynamic File'
      IF(BINARY)THEN
         IF (EFDC)READ(1)NOSEG,NUMFLOW, NUMDHT,HDT, START,END,NUM_LAYER
         IF (DYNHYD)READ(1)NOSEG,NUMFLOW, HDT, START,END,NUM_LAYER
      ELSE
         IF(EFDC)READ(1,*)NOSEG,NUMFLOW, NUMDHT, HDT, START,END, NUM_LAYER
         IF(DYNHYD)READ(1,*)NOSEG,NUMFLOW, HDT, START,END,NUM_LAYER
         IF(HECRAS)Then
            READ(1,*)NOSEG,NUMFLOW, HDT, IMON, IDAY, IYEAR, IHour, Imin,NUM_LAYER
            call hlsetseedmoment(Ihl_handle, IMON, IDAY, IYEAR, IHour,Imin, 0, ierror)
            NUMDHT = 1
      Endif
      END IF
!=======================================================================
      Allocate (IQ(NUMFLOW))
      Allocate (JQ(NUMFLOW))
      Allocate (IFLOWDIR(NUMFLOW))
      Allocate (SEGVOLUME(NOSEG))
      Allocate (SEGDEPTH(NOSEG))
      Allocate (SEGVEL(NOSEG))
      Allocate (Flow(NUMFLOW))
      Allocate (CRNU(NUMFLOW))
      Allocate (BRINTT(NUMFLOW))
      Allocate (SegSal(NOSEG))
      Allocate (SegTemp(NOSEG))      
!=======================================================================
      if (num_layer .lt. 1) then
         num_layer=1
         numdht=1
      endif
      if (numdht .lt. 1)numdht=1
!=======================================================================
      call hlsetnumlayers(Ihl_handle,num_layer,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!     Set the number of segments
!=======================================================================
      call hlsetnumsegments(Ihl_handle, noseg, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!               Get Segment Name Map File if exists
!=======================================================================
      open(unit=15,file=segfile,status='old', iostat=istat)
      write(6,*)'Opening  segment map file; iostat = ',istat            !3/11/08,rba                           !
      if (istat.eq.0) then
         write(6,*)'Segment Map Text file is: ',segfile                 !
         do i=1, NOSEG
            read(15,4000)SEGNAME
            SEGNAME =(TRIM(SEGNAME)//CHAR(0))                           !
            call hlsetsegname(ihl_handle,i,segname,ierror)
            write(6,*)'Segment ',i,' name: ',segname,' err code ',ierror                  !
4000        format(A30)
         end do
      Else
         do i=1, NOSEG
            SEGID=repeat(char(0),4)
            IF (I .lt. 9)write (SEGID,'(I1)') I
            IF (I .gt. 9 .and. I .le. 99)write (SEGID,'(I2)') I
            IF (I .gt. 99 .and. I .lt. 999 )write (SEGID,'(I3)') I
            IF (I .gt. 999)write (SEGID,'(I4)') I
            SEGID=Trim(SEGID)//char(0)
            Segname='WASP-SEG'//'-'//SEGID
            Segname=Trim(Segname)//char(0)
            call hlsetsegname(ihl_handle,i,Segname,ierror)
         end do

      endif
!=======================================================================
!     Set the number of flow paths
!=======================================================================
      call hlsetnumflowpaths(Ihl_handle, numflow, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!     Set the number of segment constituents
!=======================================================================
      call hlsetnumsegconsts(Ihl_handle, inumsegconsts, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!     Set the number of flow path constituents
!=======================================================================
      if(EFDC)call hlsetnumfpconsts (Ihl_handle, 3, ierror)
!      if(DYNHYD)call hlsetnumfpconsts (Ihl_handle, 1, ierror)
      if(DYNHYD)call hlsetnumfpconsts (Ihl_handle, 2, ierror)           !6/10/08,rba
      if(HECRAS)call hlsetnumfpconsts (Ihl_handle, 2, ierror)           !6/10/08,rba
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
!     Now we will set all the constitituent types
!=======================================================================
      call hlsetsegconsttype(Ihl_handle, 1, 0, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
      if(EFDC)then
         call hlsetsegconsttype(Ihl_handle, 2, 1, ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         call hlsetsegconsttype(Ihl_handle, 3, 2, ierror)
         if(ierror .gt. 0)then
           call hlgetlasterror(errstring)
           write(6,6000) ierror, errstring
           stop
         end if
         call hlsetsegconsttype(Ihl_handle, 4, 3, ierror)
         if(ierror .gt. 0)then
           call hlgetlasterror(errstring)
           write(6,6000) ierror, errstring
           stop
         end if
         call hlsetsegconsttype(Ihl_handle, 5, 4, ierror)
         if(ierror .gt. 0)then
           call hlgetlasterror(errstring)
           write(6,6000) ierror, errstring
           stop
         end if
      endif
!=======================================================================
      call hlsetfpconsttype(Ihl_handle, 1, 0, ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      if (DYNHYD)then                                                   !6/10/08,rba
         call hlsetfpconsttype(Ihl_handle, 2, 0, ierror)                !
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         endif
      end if
      if (HECRAS)then                                                   !6/10/08,rba
         call hlsetfpconsttype(Ihl_handle, 1, 0, ierror)                !
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         endif
      end if
      if (EFDC)then
         call hlsetfpconsttype(Ihl_handle, 1, 1, ierror)                !shouldn't this be 2,1
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         endif
         call hlsetfpconsttype(Ihl_handle, 1, 2, ierror)                !shouldn't this be 3,1
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
      end if
!=======================================================================
      call hlsetvartimestep(Ihl_handle,0,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      timestep=(hdt/86400.)
!=======================================================================
      IF (HECRAS)then
          call hlsettimestep(Ihl_handle,timestep,ierror)
      Else If (DYNHYD) then
          call hlsettimestep(Ihl_handle,timestep,ierror)
      Else
          call hlsettimestep(Ihl_handle,timestep,ierror)
      End If
!=======================================================================
      call hlsethydtimestep(Ihl_handle,hdt,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      rinterval=(hdt*numdht)/86400.
      IF (DYNHYD) THEN
         rinterval=(hdt*numdht)/86400.
      ENDIF
!=======================================================================
      call hlsetupdateint(Ihl_handle,rinterval,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
!=======================================================================
      call hlsethydtowaspratio(Ihl_handle,numdht,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      idt=1
!=======================================================================
      do i=1,numflow
         if (binary) then
            read(1)JQ(I), IQ(I)
         else
            IF(EFDC)read(1,1001)JQ(I), IQ(I)
            IF(DYNHYD)read(1,1001)JQ(I), IQ(I)
            IF(HECRAS)read(1,*)JQ(I), IQ(I)
1001        format(2(I5))
         endif
      end do
!=======================================================================
      NOCYCLES= END-START
      NOCYCLES=NOCYCLES/(HDT*numdht)
      IF(HECRAS)nocycles=1000000
      write(6,*)'nocycles=',nocycles
      write(6,*)'Noseg=',Noseg
      Do itime=1,nocycles
         write(6,*)'itime=',itime
         do k=1,Noseg
            if (itime .eq. 1) then
               if (binary) then
                  if(EFDC)Read(1,end=999)SegVolume(k),SegDepth(k), SegVel(k)
                  if(DYNHYD)read(1,end=999)SegVolume(k),rjunk, SegDepth(k),SegVel(k)
               else
                  if(EFDC)read(1,*,end=999)SegVolume(k),SegDepth(k),SegVel(k)
                  if(DYNHYD)read(1,*,end=999)SegVolume(k),rjunk, SegDepth(k),SegVel(k)
                  if(HECRAS)read(1,*,end=999)SegVolume(k),SegDepth(k), SegVel(k)
               endif
1020           Format(5x,F15.0,5x,F15.0,5x,F15.0)
            else
               if (binary) then
                  if(EFDC)Read(1,end=999)SegVolume(k),SegDepth(k), SegVel(k),SegTemp(K),SegSal(k)
                  if(DYNHYD)read(1,end=999)SegVolume(k),rjunk,SegDepth(k),SegVel(k)
               else
                  if(EFDC)read(1,*,end=999)SegVolume(k),SegDepth(k), SegVel(k),SegTemp(K),SegSal(k)
                  if(DYNHYD)read(1,*,end=999)SegVolume(k),rjunk, SegDepth(k),SegVel(k)
                  if(HECRAS)read(1,*,end=999)SegVolume(k),SegDepth(k),SegVel(k)
!                  Write(6,*)'Segment = ',k
               endif
1011           format(4(F17.0))
            endif
         end do
!=======================================================================
         do k=1,numflow
            if (binary) then
               if(EFDC)read(1)Flow(k),crnu(k),iflowdir(k)
!c               IF(DYNHYD)READ(1)Flow(k)
               IF(DYNHYD)READ(1)Flow(k),brintt(k)                       !6/10/08,rba
            else
               if (EFDC)read(1,*)Flow(k),crnu(k),brintt(k),iflowdir(k)
!c               IF (DYNHYD)read(1,*)Flow(k)
               IF (DYNHYD)read(1,1012)Flow(k),brintt(k)                 !6/10/08,rba
               IF (HECRAS)read(1,*)Flow(k)
            endif
1012        format(2F20.0)                                              !6/10/08,rba
1010        format(3(F17.0),I5)
         end do
!=======================================================================
         if(itime .eq. 1)then
            do k=1,numflow
               if(EFDC)call hlsetflowpath(Ihl_handle,k,jq(k),iq(k),iflowdir(k),ierror)
               if(DYNHYD)call hlsetflowpath(Ihl_handle,k,jq(k),iq(k),1,ierror)
               if(HECRAS)call hlsetflowpath(Ihl_handle,k,jq(k),iq(k),1,ierror)
               if(ierror .gt. 0)then
                  call hlgetlasterror(errstring)
                  write(6,6000) ierror, errstring
                  stop
               end if
            end do
         end if
!=======================================================================
         call hlsetseginfo(Ihl_handle,1,SegVolume,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         call hlsetseginfo(Ihl_handle,2,SegDepth,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         call hlsetseginfo(Ihl_handle,3,SegVel,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         call hlsetseginfo(Ihl_handle,4,SegTemp,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         call hlsetseginfo(Ihl_handle,5,SegSal,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if                 
!=======================================================================
         call hlsetflowinfo(Ihl_handle,1,Flow,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         if(DYNHYD)Then                                                 !6/10/08,rba
            call hlsetflowinfo(Ihl_handle,2,brintt,ierror)
            if(ierror .gt. 0)then
               call hlgetlasterror(errstring)
               write(6,6000) ierror, errstring
               stop
            end if
         end if
         if(HECRAS)Then
            DO Itemp=1,numflow
              brintt(Itemp)=0.00
            End Do                                                   !6/10/08,rba
            call hlsetflowinfo(Ihl_handle,2,brintt,ierror)
            if(ierror .gt. 0)then
               call hlgetlasterror(errstring)
               write(6,6000) ierror, errstring
               stop
            end if
         end if
         if(EFDC)Then
            call hlsetflowinfo(Ihl_handle,2,crnu,ierror)
            if(ierror .gt. 0)then
               call hlgetlasterror(errstring)
               write(6,6000) ierror, errstring
               stop
            end if
            call hlsetflowinfo(Ihl_handle,3,brintt,ierror)
            if(ierror .gt. 0)then
               call hlgetlasterror(errstring)
               write(6,6000) ierror, errstring
               stop
            end if
         end if
         time=RINTERVAL*itime
         write(6,*)'Time is: ',itime,time
!CTAW         call hlsettimestep(Ihl_handle,time,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
         call hlmomentcomplete(Ihl_Handle,ierror)
         if(ierror .gt. 0)then
            call hlgetlasterror(errstring)
            write(6,6000) ierror, errstring
            stop
         end if
      end do
999   continue
6000  format('Error ',I10, ' : ', A)
      call hlgetcompfact(Ihl_handle,compact,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      compact=compact*100
      write(6,6040)compact
6040  format('Compaction Ratio is: ',F8.4)
      call hlclose(Ihl_handle,ierror)
      if(ierror .gt. 0)then
          call hlgetlasterror(errstring)
          write(6,6000) ierror, errstring
          stop
      end if
      stop
      end
