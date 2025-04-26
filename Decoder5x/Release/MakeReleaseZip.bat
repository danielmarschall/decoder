@echo off

cd /d "%~dp0"

if not exist "c:\Program Files\7-Zip\7z.exe" (
    echo Please install 7-zip!
    pause.
    exit /b 1
)

if not exist ..\..\README.md (
    echo Missing: ..\..\README.md
		pause.
		exit /b 1
)

if not exist ..\..\LICENSE (
    echo Missing: ..\..\LICENSE
		pause.
		exit /b 1
)

if not exist ..\Output\win32\CoderCLI.exe (
    echo Missing: Output\win32\CoderCLI.exe
		pause.
		exit /b 1
)

if not exist ..\Output\win32\CoderCLI.de (
    echo Missing: Output\win32\CoderCLI.de
		pause.
		exit /b 1
)

if not exist ..\Output\win32\CoderFMX.exe (
    echo Missing: Output\win32\CoderFMX.exe
		pause.
		exit /b 1
)

if not exist ..\Output\win32\CoderFMX.de (
    echo Missing: Output\win32\CoderFMX.de
		pause.
		exit /b 1
)

if not exist ..\Output\win32\7z.dll (
    echo Missing: Output\win32\7z.dll
		pause.
		exit /b 1
)

if not exist ..\Output\win64\CoderCLI.exe (
    echo Missing: Output\win64\CoderCLI.exe
		pause.
		exit /b 1
)

if not exist ..\Output\win64\CoderCLI.de (
    echo Missing: Output\win64\CoderCLI.de
		pause.
		exit /b 1
)

if not exist ..\Output\win64\CoderFMX.exe (
    echo Missing: Output\win64\CoderFMX.exe
		pause.
		exit /b 1
)

if not exist ..\Output\win64\CoderFMX.de (
    echo Missing: Output\win64\CoderFMX.de
		pause.
		exit /b 1
)

if not exist ..\Output\win64\7z.dll (
    echo Missing: Output\win64\7z.dll
		pause.
		exit /b 1
)

rem ---

if exist WindowsRelease.zip del WindowsRelease.zip

if exist README.txt del README.txt
copy ..\..\README.md README.txt

if exist LICENSE.txt del LICENSE.txt
copy ..\..\LICENSE LICENSE.txt

if exist Windows_32Bit rmdir /s /q Windows_32Bit
mkdir Windows_32Bit
copy ..\Output\win32\CoderCLI.exe Windows_32Bit
copy ..\Output\win32\CoderCLI.de Windows_32Bit
copy ..\Output\win32\CoderFMX.exe Windows_32Bit
copy ..\Output\win32\CoderFMX.de Windows_32Bit
copy ..\Output\win32\7z.dll Windows_32Bit

if exist Windows_64Bit rmdir /s /q Windows_64Bit
mkdir Windows_64Bit
copy ..\Output\win64\CoderCLI.exe Windows_64Bit
copy ..\Output\win64\CoderCLI.de Windows_64Bit
copy ..\Output\win64\CoderFMX.exe Windows_64Bit
copy ..\Output\win64\CoderFMX.de Windows_64Bit
copy ..\Output\win64\7z.dll Windows_64Bit

"c:\Program Files\7-Zip\7z.exe" a -tzip "WindowsRelease.zip" README.txt LICENSE.txt Windows_32Bit Windows_64Bit

explorer /select,WindowsRelease.zip

del README.txt
del LICENSE.txt
rmdir /s /q Windows_32Bit
rmdir /s /q Windows_64Bit

rem pause.
