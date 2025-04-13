# Release procedure

## Windows release

- Make sure DecoderConst.pas contains the latest version date
- In Delphi, compile CoderFMX and CoderCLI in Release mode for Windows x86 and Windows x64
- Sign it using `AuthentiCode Sign\sign_single.bat`
- Publish CoderFMX.32.exe, CoderFMX.64.exe, CoderCLI.32.exe, CoderCLI.64.exe in a ZIP file

## Mac release

- Make sure DecoderConst.pas contains the latest version date
- In Delphi, compile CoderFMX and CoderCLI in Release mode for macOS x64
- Run both using the debugger, so that it gets copied to the Mac computer via PA Server
- On Mac, run the following to make a DMG (adjust paths): `hdiutil create -volname "(De)Coder 5.1" -srcfolder /Users/daniel/PAServer/scratch-dir/danie-MacBook/CoderFMX.app -ov -format UDZO /Users/daniel/PAServer/scratch-dir/danie-MacBook/CoderFMX.dmg`
- Publish CoderFMX.dmg and the file CoderCLI (which is found in the app package CoderCLI.app/Contents/MacOS/CoderCLI, Package contents) in a ZIP file
