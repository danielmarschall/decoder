; (De)Coder Script für InnoSetup 5.1.9
; Fehler bei Uninstallation: ReadOnly, Anwendung in Benutzung

[Setup]
AppName=(De)Coder
AppVerName=(De)Coder 4.1 Public Beta 4b
AppVersion=4.1.0.0
AppCopyright=© Copyright 2001 - 2007 ViaThinkSoft
AppPublisher=ViaThinkSoft
AppPublisherURL=https://www.viathinksoft.de/
AppSupportURL=https://www.daniel-marschall.de/
AppUpdatesURL=https://www.viathinksoft.de/
DefaultDirName={autopf}\(De)Coder
DefaultGroupName=(De)Coder
UninstallDisplayIcon={app}\Coder.exe
VersionInfoCompany=ViaThinkSoft
VersionInfoCopyright=© Copyright 2001 - 2007 ViaThinkSoft
VersionInfoDescription=(De)Coder 4.1 Setup
VersionInfoTextVersion=1.0.0.0
VersionInfoVersion=4.1.0.0
WizardImageFile=Images\Large.bmp
WizardSmallImageFile=Images\Small.bmp
OutputDir=.
OutputBaseFilename=DeCoder_Setup
SetupIconFile=Icon.ico
; Configure Sign Tool in InnoSetup at "Tools => Configure Sign Tools" (adjust the path to your SVN repository location)
; Name    = sign_single   
; Command = "C:\SVN\...\sign_single.bat" $f
SignTool=sign_single
SignedUninstaller=yes

[Languages]
Name: de; MessagesFile: "compiler:Languages\German.isl"; LicenseFile: "License\German.txt"
Name: en; MessagesFile: "compiler:Default.isl"; LicenseFile: "License\English.txt"
Name: fr; MessagesFile: "compiler:Languages\French.isl"; LicenseFile: "License\French.txt"

[Files]
; Allgemein
Source: "..\Quelltext\Coder.exe"; DestDir: "{app}"; Flags: restartreplace ignoreversion signonce
Source: "..\Quelltext\Activator.exe"; DestDir: "{app}"; Flags: restartreplace ignoreversion signonce
Source: "..\Quelltext\style.css"; DestDir: "{app}"; Flags: restartreplace
Source: "..\Quelltext\ShlExt.dll"; DestDir: "{app}"; Flags: restartreplace ignoreversion signonce
Source: "..\Quelltext\ShlErase.dll"; DestDir: "{app}"; Flags: restartreplace ignoreversion signonce
Source: "..\Quelltext\SecureMoveExt.dll"; DestDir: "{app}"; Flags: restartreplace ignoreversion signonce
; Deutsch
Source: "..\Quelltext\Help.html"; DestDir: "{app}"; Flags: isreadme; DestName: "Help.html"; Languages: de
Source: "..\Quelltext\funktionsweise_1.gif"; DestDir: "{app}"; DestName: "funktionsweise_1.gif"; Languages: de
Source: "..\Quelltext\funktionsweise_2.gif"; DestDir: "{app}"; DestName: "funktionsweise_2.gif"; Languages: de
Source: "..\Quelltext\funktionsweise_3.gif"; DestDir: "{app}"; DestName: "funktionsweise_3.gif"; Languages: de
Source: "..\Quelltext\Language.ini"; DestDir: "{app}"; Languages: de
; Englisch
Source: "..\Quelltext\Help-Eng.html"; DestDir: "{app}"; Flags: isreadme; DestName: "Help.html"; Languages: en
Source: "..\Quelltext\funktionsweise_1.gif"; DestDir: "{app}"; Flags: isreadme; DestName: "funktionsweise_1.gif"; Languages: en
Source: "..\Quelltext\funktionsweise_2.gif"; DestDir: "{app}"; Flags: isreadme; DestName: "funktionsweise_2.gif"; Languages: en
Source: "..\Quelltext\funktionsweise_3.gif"; DestDir: "{app}"; Flags: isreadme; DestName: "funktionsweise_3.gif"; Languages: en
Source: "..\Quelltext\Language-Eng.ini"; DestDir: "{app}"; DestName: "Language.ini"; Languages: en
; Französisch
Source: "..\Quelltext\Help-Fr.html"; DestDir: "{app}"; Flags: isreadme; DestName: "Help.html"; Languages: fr
Source: "..\Quelltext\funktionsweise_1.gif"; DestDir: "{app}"; Flags: isreadme; DestName: "funktionsweise_1.gif"; Languages: fr
Source: "..\Quelltext\funktionsweise_2.gif"; DestDir: "{app}"; Flags: isreadme; DestName: "funktionsweise_2.gif"; Languages: fr
Source: "..\Quelltext\funktionsweise_3.gif"; DestDir: "{app}"; Flags: isreadme; DestName: "funktionsweise_3.gif"; Languages: fr
Source: "..\Quelltext\Language-Fr.ini"; DestDir: "{app}"; DestName: "Language.ini"; Languages: fr

[CustomMessages]
; Deutsch
de.dateienvernichten=Temporäre Dateien vernichten
de.hilfe=%1 Hilfe
de.webseiten=Webseiten
; Englisch
en.dateienvernichten=Erase tempoary files
en.hilfe=%1 Help
en.webseiten=Websites (German)
; Französisch
fr.dateienvernichten=Effacez les dossiers tempoary
fr.hilfe=L'aide pour %1
fr.webseiten=Web (en allemand)

;[Folders]
;Name: "{group}\Webseiten"; Languages: de
;Name: "{group}\Websites (German)"; Languages: en
;Name: "{group}\Web (en allemand)"; Languages: fr

[Icons]
; Allgemein
Name: "{group}\(De)Coder"; Filename: "{app}\Coder.exe"
;Name: "{userdesktop}\(De)Coder"; Filename: "{app}\Coder.exe"; Tasks: "desktopicon"
Name: "{group}\{cm:UninstallProgram,(De)Coder}"; Filename: "{uninstallexe}"
Name: "{group}\{cm:dateienvernichten}"; Filename: "{app}\Coder.exe"; Parameters: "/clean"; IconFilename: "{app}\Coder.exe"; IconIndex: "2"
Name: "{group}\{cm:hilfe,(De)Coder}"; Filename: "{app}\Coder.exe"; Parameters: "/?"; IconFilename: "{app}\Coder.exe"; IconIndex: "3"
; Deutsch
;Name: "{group}\{cm:webseiten}\Daniel Marschalls Webportal"; Filename: "https://www.daniel-marschall.de/"; Languages: de
;Name: "{group}\{cm:webseiten}\ViaThinkSoft"; Filename: "https://www.viathinksoft.de/"; Languages: de
;Name: "{group}\{cm:webseiten}\Projektseite auf ViaThinkSoft"; Filename: "https://www.viathinksoft.de/index.php?page=projektanzeige&seite=projekt-18"; Languages: de
; Englisch
;Name: "{group}\{cm:webseiten}\Daniel Marschalls web portal"; Filename: "https://www.daniel-marschall.de/"; Languages: en
;Name: "{group}\{cm:webseiten}\ViaThinkSoft"; Filename: "https://www.viathinksoft.de/"; Languages: en
;Name: "{group}\{cm:webseiten}\Projectpage on ViaThinkSoft"; Filename: "https://www.viathinksoft.de/index.php?page=projektanzeige&seite=projekt-18"; Languages: en
; Französisch
;Name: "{group}\{cm:webseiten}\Daniel Marschalls portail de Web"; Filename: "https://www.daniel-marschall.de/"; Languages: fr
;Name: "{group}\{cm:webseiten}\ViaThinkSoft"; Filename: "https://www.viathinksoft.de/"; Languages: fr
;Name: "{group}\{cm:webseiten}\Page de projet dans ViaThinkSoft"; Filename: "https://www.viathinksoft.de/index.php?page=projektanzeige&seite=projekt-18"; Languages: fr

[Tasks]
; Allgemein
;Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; MinVersion: 4,4
; Deutsch
Name: "registerserver"; Description: "Dateien und Ordner einen &Kontextmenüeintrag geben"; GroupDescription: "Voreinstellungen (später unter Konfiguration änderbar):"; MinVersion: 4,4; Languages: de
Name: "filetype"; Description: "DC4-Dateien mit (De)Coder &assoziieren"; GroupDescription: "Voreinstellungen (später unter Konfiguration änderbar):"; MinVersion: 4,4; Languages: de
; Englisch
Name: "registerserver"; Description: "Give files and folders a &contextmenuentry"; GroupDescription: "Settings (changeable after installation):"; MinVersion: 4,4; Languages: en
Name: "filetype"; Description: "&Associate DC4-Files with (De)Coder"; GroupDescription: "Settings (changeable after installation):"; MinVersion: 4,4; Languages: en
; Französisch
Name: "registerserver"; Description: "Donnez les fichies et les dossiers une entrée de menu &contextuel"; GroupDescription: "Arrangements (variables après installation):"; MinVersion: 4,4; Languages: fr
Name: "filetype"; Description: "&Associé DC4-Files avec (De)Coder"; GroupDescription: "Arrangements (variables après installation):"; MinVersion: 4,4; Languages: fr

[Registry]
; DC4-Dateityp registrieren
Root: HKCR; Subkey: ".dc4"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: ".dc4"; ValueType: string; ValueName: ""; ValueData: "DeCoder4-File"; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File"; Flags: uninsdeletekey; Tasks: filetype
; Deutsch
Root: HKCR; Subkey: "DeCoder4-File"; ValueType: string; ValueName: ""; ValueData: "(De)Coder 4 Datei"; Languages: de; Tasks: filetype
; Englisch
Root: HKCR; Subkey: "DeCoder4-File"; ValueType: string; ValueName: ""; ValueData: "(De)Coder 4 File"; Languages: en; Tasks: filetype
; Französisch
Root: HKCR; Subkey: "DeCoder4-File"; ValueType: string; ValueName: ""; ValueData: "Fichier (De)Coder 4"; Languages: fr; Tasks: filetype
; Allgemein
Root: HKCR; Subkey: "DeCoder4-File\DefaultIcon"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Coder.exe,1"; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell\open"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell\open\command"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Coder.exe"" ""%1"""; Tasks: filetype
Root: HKLM; Subkey: "SOFTWARE\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers"; ValueType: string; ValueName: "{app}\Activator.exe"; ValueData: "RUNASADMIN"
Root: HKLM; Subkey: "SOFTWARE\Microsoft\Windows\CurrentVersion\Run"; ValueType: string; ValueName: "(De)Coder Cleaner"; ValueData: "{app}\Coder.exe /clean /silent"

[Run]
Filename: "{app}\Coder.exe"; Description: "{cm:LaunchProgram,(De)Coder}"; Flags: nowait postinstall skipifsilent

[Code]
function InitializeSetup(): Boolean;
begin
  if CheckForMutexes('DeCoder41Setup') = false then
  begin
    Createmutex('DeCoder41Setup');
    Result := true;
  end
  else
  begin
    Result := False;
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if (CurPageID = wpFinished) and IsTaskSelected('registerserver') then RegisterServer(false, ExpandConstant('{app}\ShlExt.dll'), false);
  if (CurPageID = wpFinished) and IsTaskSelected('registerserver') then RegisterServer(false, ExpandConstant('{app}\ShlErase.dll'), false);
  if (CurPageID = wpFinished) and IsTaskSelected('registerserver') then RegisterServer(false, ExpandConstant('{app}\SecureMoveExt.dll'), false);
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  case CurUninstallStep of
    usUninstall:
    begin
      UnRegisterServer(false, ExpandConstant('{app}\ShlExt.dll'), false);
      UnRegisterServer(false, ExpandConstant('{app}\ShlErase.dll'), false);
      UnRegisterServer(false, ExpandConstant('{app}\SecureMoveExt.dll'), false);
      // Die Werte werden nicht gelöscht, wenn der Task bei der Installation nicht gewählt wurden...
      if RegKeyExists(HKEY_CLASSES_ROOT, '.dc4') then
        RegDeleteKeyIncludingSubkeys(HKEY_CLASSES_ROOT, '.dc4');
      if RegKeyExists(HKEY_CLASSES_ROOT, 'DeCoder4-File') then
        RegDeleteKeyIncludingSubkeys(HKEY_CLASSES_ROOT, 'DeCoder4-File');
    end;
  end;
end;

