; (De)Coder Script für InnoSetup 5.1.6
; Fehler bei Uninstallation: ReadOnly, Anwendung in Benutzung

[Setup]
AppName=(De)Coder
AppVerName=(De)Coder 4.0
AppVersion=4.0
AppCopyright=© Copyright 2001 - 2006 ViaThinkSoft.
AppPublisher=ViaThinkSoft
AppPublisherURL=http://www.viathinksoft.de/
AppSupportURL=http://www.daniel-marschall.de/
AppUpdatesURL=http://www.viathinksoft.de/
DefaultDirName={pf}\(De)Coder 4.0
DefaultGroupName=(De)Coder 4.0
UninstallDisplayIcon={app}\Coder.exe
VersionInfoCompany=ViaThinkSoft
VersionInfoCopyright=© Copyright 2001 - 2006 ViaThinkSoft.
VersionInfoDescription=(De)Coder 4.0 Setup
VersionInfoTextVersion=1.0.0.0
VersionInfoVersion=4.0
WizardImageFile=Private\Gizeh\Setup_2d.bmp
WizardSmallImageFile=Private\Gizeh\SmallSetup-2.bmp
Compression=zip/9

[Languages]
Name: de; MessagesFile: "compiler:Languages\German.isl"
Name: en; MessagesFile: "compiler:Default.isl"

[Files]
; Allgemein
Source: "Coder.exe"; DestDir: "{app}"
Source: "Logo.gif"; DestDir: "{app}"
Source: "ShlExt.dll"; DestDir: "{app}"
; Deutsch
Source: "Hilfe.html"; DestDir: "{app}"; Flags: isreadme; Languages: de
Source: "Language.ini"; DestDir: "{app}"; Languages: de
; Englisch
Source: "Help.html"; DestDir: "{app}"; Flags: isreadme; Languages: en
Source: "Language-Eng.ini"; DestDir: "{app}"; DestName: "Language.ini"; Languages: en

[Folders]
Name: "{group}\Webseiten"; Languages: de
Name: "{group}\Websites (German)"; Languages: en

[Icons]
; Allgemein
Name: "{group}\(De)Coder 4.0"; Filename: "{app}\Coder.exe"
Name: "{userdesktop}\(De)Coder 4.0"; Filename: "{app}\Coder.exe"; Tasks: "desktopicon"
; Deutsch
Name: "{group}\(De)Coder Hilfe"; Filename: "{app}\Hilfe.html"; Languages: de
Name: "{group}\(De)Coder deinstallieren"; Filename: "{uninstallexe}"; Languages: de
Name: "{group}\Webseiten\Daniel Marschalls Webportal"; Filename: "http://www.daniel-marschall.de/"; Languages: de
Name: "{group}\Webseiten\ViaThinkSoft"; Filename: "http://www.viathinksoft.de/"; Languages: de
Name: "{group}\Webseiten\Projektseite auf ViaThinkSoft"; Filename: "http://www.viathinksoft.de/index.php?page=projektanzeige&seite=projekt-18"; Languages: de
; Englisch
Name: "{group}\(De)Coder Help"; Filename: "{app}\Help.html"; Languages: en
Name: "{group}\Uninstall (De)Coder"; Filename: "{uninstallexe}"; Languages: en
Name: "{group}\Websites (German)\Daniel Marschalls Webportal"; Filename: "http://www.daniel-marschall.de/"; Languages: en
Name: "{group}\Websites (German)\ViaThinkSoft"; Filename: "http://www.viathinksoft.de/"; Languages: en
Name: "{group}\Websites (German)\Projektseite on ViaThinkSoft"; Filename: "http://www.viathinksoft.de/index.php?page=projektanzeige&seite=projekt-18"; Languages: en

[Tasks]
; Deutsch
Name: "desktopicon"; Description: "Erstelle eine Verknüpfung auf dem &Desktop"; GroupDescription: "Programmverknüpfungen:"; MinVersion: 4,4; Languages: de
Name: "registerserver"; Description: "Dateien und Ordner einen &Kontextmenüeintrag geben"; GroupDescription: "Voreinstellungen (später unter Konfiguration änderbar):"; MinVersion: 4,4; Languages: de
Name: "filetype"; Description: "DC4-Dateien mit (De)Coder &assoziieren"; GroupDescription: "Voreinstellungen (später unter Konfiguration änderbar):"; MinVersion: 4,4; Languages: de
; Englisch
Name: "desktopicon"; Description: "Create a shortcut on the &Desktop"; GroupDescription: "Shortcuts:"; MinVersion: 4,4; Languages: en
Name: "registerserver"; Description: "Give files and folders a &contextmenuentry"; GroupDescription: "Settings (changeable after installation):"; MinVersion: 4,4; Languages: en
Name: "filetype"; Description: "&Associate DC4-Files with (De)Coder"; GroupDescription: "Settings (changeable after installation):"; MinVersion: 4,4; Languages: en

[Registry]
; DC4-Dateityp registrieren
Root: HKCR; Subkey: ".dc4"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: ".dc4"; ValueType: string; ValueName: ""; ValueData: "DeCoder4-File"; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File"; ValueType: string; ValueName: ""; ValueData: "(De)Coder 4 Datei"; Languages: de; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File"; ValueType: string; ValueName: ""; ValueData: "(De)Coder 4 File"; Languages: en; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\DefaultIcon"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "C:\Programme\(De)Coder 4.0\Coder.exe,1"; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell\open"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell\open\command"; Flags: uninsdeletekey; Tasks: filetype
Root: HKCR; Subkey: "DeCoder4-File\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """C:\Programme\(De)Coder 4.0\Coder.exe"" ""%1"""; Tasks: filetype

[Run]
Filename: "{app}\Coder.exe"; Description: "(De)Coder starten"; Flags: nowait postinstall skipifsilent; Languages: de
Filename: "{app}\Coder.exe"; Description: "Run (De)Coder"; Flags: nowait postinstall skipifsilent; Languages: en

[Code]
function InitializeSetup(): Boolean;
begin
  if CheckForMutexes('DeCoder40Setup')=false then
  begin
    Createmutex('DeCoder40Setup');
    Result := true;
  end
  else
  begin
    Result := False;
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  case CurPageID of
    wpFinished:
      if IsTaskSelected('registerserver') then RegisterServer(false, ExpandConstant('{app}\ShlExt.dll'), false);
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  case CurUninstallStep of
    usUninstall:
    begin
      UnregisterServer(false, ExpandConstant('{app}\ShlExt.dll'), false);
      // Die Werte werden nicht gelöscht, wenn der Task bei der Installation nicht gewählt wurden...
      if RegKeyExists(HKEY_CLASSES_ROOT, '.dc4') then
        RegDeleteKeyIncludingSubkeys(HKEY_CLASSES_ROOT, '.dc4');
      if RegKeyExists(HKEY_CLASSES_ROOT, 'DeCoder4-File') then
      RegDeleteKeyIncludingSubkeys(HKEY_CLASSES_ROOT, 'DeCoder4-File');
    end;
  end;
end;

