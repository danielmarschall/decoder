@echo off
cd /d %~dp0
del Output\win32\*.rsm
del Output\win32\*.dcu
del Output\win64\*.rsm
del Output\win64\*.dcu
del *.local
del *.identcache
del *.~*
del *.o
del *.rsm
del Translation\*.xlat.$*
attrib __history -h
if exist __history rmdir /s /q __history
attrib __recovery -h
if exist __recovery rmdir /s /q __recovery
pause.
