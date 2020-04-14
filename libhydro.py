from ctypes import byref, c_int, c_char, c_char_p, cdll
"""
000000000000e420 T hladddescription
0000000000016fc0 T hlclose
00000000000169c0 T hlgetcompfact
000000000000e130 T hlgetcreator
00000000000172a0 T hlgetcurrentmoment
000000000000cfd0 T hlgetdebug
000000000000ea40 T hlgetdescription
000000000000f6c0 T hlgetendmoment
00000000000154c0 T hlgetflowinfo
0000000000014e20 T hlgetflowpath
0000000000012940 T hlgetfpconsttype
0000000000013580 T hlgethydtimestep
00000000000141b0 T hlgethydtowaspratio
000000000000db40 T hlgetlanguage
000000000000d280 T hlgetlasterror
000000000000fbe0 T hlgetnthmoment
000000000000e750 T hlgetnumdescriptions
00000000000110c0 T hlgetnumflowpaths
0000000000011c90 T hlgetnumfpconsts
0000000000014790 T hlgetnumlayers
00000000000166d0 T hlgetnummoments
00000000000116b0 T hlgetnumsegconsts
0000000000010450 T hlgetnumsegments
000000000000f210 T hlgetseedmoment
00000000000122c0 T hlgetsegconsttype
0000000000015ae0 T hlgetseginfo
0000000000010a80 T hlgetsegname
0000000000016100 T hlgettimestep
0000000000013ba0 T hlgetupdateint
0000000000012f80 T hlgetvartimestep
0000000000016410 T hlmomentcomplete
000000000000d440 T hlopen
0000000000016cd0 T hlsetcompressionlevel
000000000000de30 T hlsetcreator
000000000000cef0 T hlsetdebug
00000000000151b0 T hlsetflowinfo
0000000000014a80 T hlsetflowpath
0000000000012600 T hlsetfpconsttype
0000000000013270 T hlsethydtimestep
0000000000013eb0 T hlsethydtowaspratio
000000000000d840 T hlsetlanguage
000000000000d0b0 T hlsetlasterror
0000000000010dc0 T hlsetnumflowpaths
00000000000119a0 T hlsetnumfpconsts
00000000000144a0 T hlsetnumlayers
00000000000113b0 T hlsetnumsegconsts
0000000000010150 T hlsetnumsegments
000000000000edb0 T hlsetseedmoment
0000000000011f80 T hlsetsegconsttype
00000000000157d0 T hlsetseginfo
0000000000010740 T hlsetsegname
0000000000015df0 T hlsettimestep
0000000000013890 T hlsetupdateint
0000000000012c80 T hlsetvartimestep
"""
# Load the library 
lhydro = cdll.LoadLibrary("libhydrolink.so")

def hlsetdebug(dlevel):
	d = c_int(dlevel)
	return lhydro.hlsetdebug(byref(d))	

def hlgetlasterror():
	m = c_char_p('')
	g = lhydro.hlgetlasterror(byref(m))
	return m

def hlopen(fpath, ):
	pass

def hlclose(hdl):
	pass


d = c_int(1)
lhydro.hlsetdebug(byref(d))

lhydro.hlopen ('Pocho', 0, Ihl_handle,Ierror)

