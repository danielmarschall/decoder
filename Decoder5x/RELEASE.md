# Release procedure

## Windows release

- Make sure DecoderConst.pas contains the latest version date
- In Delphi, compile both CoderFMX and CoderCLI in Release mode for Windows x86 and Windows x64
- Open all 4 xlat files in Trnslation using Better Translation Manager, then
  * Click "Update"
	* Update translations if required (use Translation Memory to transfer translations between XLAT file)
	* Build the file ("de" file for German).
- Check in to SVN and sync SVN to GitHub
- Run Release\MakeReleaseZip.bat to sign the files using AuthentiCode and pack the ZIP file
- Publish Release\WindowsRelease.zip to GitHub and ViaThinkSoft

## Mac release

- Make sure DecoderConst.pas contains the latest version date
- In Delphi, compile CoderFMX and CoderCLI in Release mode for macOS x64
- Run both using the debugger, so that it gets copied to the Mac computer via PA Server
- On Mac, run the following to make a DMG (adjust paths): `hdiutil create -volname "(De)Coder 5.1.1" -srcfolder /Users/daniel/PAServer/scratch-dir/danie-MacBook/CoderFMX.app -ov -format UDZO /Users/daniel/PAServer/scratch-dir/danie-MacBook/CoderFMX.dmg`
- Publish CoderFMX.dmg and the file CoderCLI (which is found in the app package CoderCLI.app/Contents/MacOS/CoderCLI, Package contents) in a ZIP file
