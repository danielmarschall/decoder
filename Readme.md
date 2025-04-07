
# ViaThinkSoft (De)Coder 5.1

(De)Coder is an encryption software developed by Daniel Marschall.
It uses the high-security AES encryption.


## New and improved features in version 5.1

- Modern design
- Newer algorithms
- Backwards compatible with ALL previous (De)Coder versions
- Inclusion of metadata (filename, size, date) is now optional
- Available as GUI ("CoderFMX") or as a command-line tool ("CoderCLI")
- 32-bit and 64-bit binaries for Windows and 64-bit binaries for Mac OS X
- Secure deletion of files and folders (only in the CLI tool)
- Drag and drop support
- Completely re-written with the latest development tools (Delphi 12.3, DEC 6.5)
- Encrypt and decrypt files or folders (integrated 7zip packer)
- Internally compresses data using ZLib, but only if the file is not already compressed or has a high entropy
- Translated to English (currently, no German translation available anymore)

A few features like shell integration and installer have been removed,
to keep the product simple and clean.


## Screenshots

![Original file](Decoder5X/Private/Screenshot1.png)

![Encoded file](Decoder5X/Private/Screenshot2.png)

![Command line tool](Decoder5X/Private/Screenshot3.png)


## Disclaimer

Use this software at your own risk! ViaThinkSoft is not responsible for any damages which
may be caused by the usage of this software. Especially when encrypting files, a damaged file,
a forgotten password, or a mistyped password will lead to an irreversible loss of your data.
The developers are not liable for any program bugs or data loss.

Please always backup your unencrypted data before using (De)Coder, or if you choose to delete the
original unencrypted files, then check at least if the decryption works as expected.
Please do not download or use the software if you do not agree with these conditions.


## License and Credits

(De)Coder is licensed under the terms of the Apache 2.0 license,
which means you can use it for free (also commercially),
and the [source code is publicly available](https://github.com/danielmarschall/decoder) for everyone to use/learn/extend.

This product uses the following third-party components:

- [Delphi Encryption Compendium](https://github.com/MHumm/DelphiEncryptionCompendium/) (License: Apache 2.0)
- [7zip](https://7-zip.org/) DLL files by Igor Pavlov (License: LGPL)
- [7zip Wrapper for Delphi](https://github.com/danielmarschall/d7zip/) by Henri Gourvest (License: MPL1.1)
