@echo off
cd /d %~dp0
brcc32 Quelltext\Coder.rc
brcc32 Quelltext\Activator.rc
brcc32 Quelltext\ShlExt.rc
brcc32 Quelltext\ShlErase.rc
brcc32 Quelltext\SecureMoveExt.rc
pause.
