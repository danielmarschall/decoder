@echo off
cd /d %~dp0
del bin\*.rsm
del *.dcu
del *.local
del *.identcache
del *.~*
attrib __history -h
if exist __history rmdir /s /q __history
attrib __recovery -h
if exist __recovery rmdir /s /q __recovery
