rem This program needs the x64 Vs toolset cmd line


mkdir build

cl /O2 /EHsc /Fe:build/ec.exe ec.c Resource.res /link /SUBSYSTEM:windows /ENTRY:mainCRTStartup
cl /O2 /EHsc /Fe:build/ecn.exe ecn.c Resource.res /link /SUBSYSTEM:windows /ENTRY:mainCRTStartup
