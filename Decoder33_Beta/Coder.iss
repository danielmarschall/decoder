; (De)Coder 3.3 Setup Script for InnoSetup 3.0.6
; by Daniel Marschall

; http://www.d-m-home.de/

; Shut-Down QuickStarter?!

[Setup]
AppName=(De)Coder
AppVerName=(De)Coder 3.3
AppVersion=3.3
AppCopyright=(C)Copyright 2001 - 2003 Daniel Marschall. Alle Rechte vorbehalten!
AppPublisher=Daniel Marschall Computersoftware
AppPublisherURL=http://www.d-m-home.de/
AppSupportURL=http://www.d-m-home.de/
AppUpdatesURL=http://www.d-m-home.de/
DefaultDirName={pf}\(De)Coder
DefaultGroupName=(De)Coder
InfoBeforeFile=G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\Hilfe.rtf
Compression=zip/9
; MinVersion=4,3.51

[LangOptions]
LanguageName=Deutsch
LanguageID=$0407

[Tasks]
Name: "desktopicon"; Description: "Erstelle eine Verknüpfung auf dem &Desktop"; GroupDescription: "Programmverknüpfungen:"; MinVersion: 4,4
Name: "contextmenu"; Description: "Erstelle Eintrag im &Kontextmenü der Dateien"; GroupDescription: "Registrierungseinträge:"; MinVersion: 4,4
Name: "quickstarter"; Description: "Installiere den (De)Coder &QuickStarter"; GroupDescription: "Zusatzprogramme:"; MinVersion: 4,4

[Files]
Source: "G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\Coder.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\QuickStart.exe"; DestDir: "{app}"; Flags: ignoreversion; Tasks: quickstarter
; Source: "G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\Bilder\Background.jpg"; DestDir: "{app}\Bilder"; Flags: ignoreversion
Source: "G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\Bilder\Info.bmp"; DestDir: "{app}\Bilder"; Flags: ignoreversion
Source: "G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\Bilder\QuickStarter.bmp"; DestDir: "{app}\Bilder"; Flags: ignoreversion; Tasks: quickstarter
Source: "G:\Programmierung\Veröffentlichte Programme\(De)Coder 3.3\Icons\TrayIcon.ico"; DestDir: "{app}\Icons"; Flags: ignoreversion; Tasks: quickstarter

[Icons]
Name: "{group}\(De)Coder"; Filename: "{app}\Coder.exe"
Name: "{group}\Daniel Marschall Computersoftware"; Filename: "http://www.d-m-home.de/"
Name: "{userdesktop}\(De)Coder"; Filename: "{app}\Coder.exe"; MinVersion: 4,4; Tasks: desktopicon
Name: "{group}\(De)Coder deinstallieren"; Filename: "{uninstallexe}"

; Must uninstall!
[Registry]
Root: HKCR; Subkey: "*\Shell\(De)Coder"; ValueType: string; ValueName: ""; ValueData: "(De)&Coder 3.3 starten..."; Flags: uninsdeletekey; Tasks: contextmenu
; WITH %1 PARAMETER!
Root: HKCR; Subkey: "*\Shell\(De)Coder\Command"; ValueType: string; ValueName: ""; ValueData: "{app}\Coder.exe"; Flags: uninsdeletekey; Tasks: contextmenu

[Run]
Filename: "{app}\Coder.exe"; Description: "(De)Coder starten"; Flags: nowait postinstall skipifsilent
Filename: "{app}\QuickStart.exe"; Flags: nowait; Tasks: quickstarter

