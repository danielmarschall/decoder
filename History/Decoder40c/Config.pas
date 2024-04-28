unit Config;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, shellapi, activeX, registry, inifiles;

type
  TLanguageEntry = record
    name: string;
    text: string;
  end;

  TConfigForm = class(TForm)
    btn_close: TButton;
    btn_reg1: TButton;
    btn_unreg1: TButton;
    btn_reg2: TButton;
    btn_unreg2: TButton;
    lbl_label1: TLabel;
    lbl_label2: TLabel;
    lbl_label3: TLabel;
    lbl_label4: TLabel;
    grp_system: TGroupBox;
    procedure btn_reg1_click(Sender: TObject);
    procedure btn_unreg1_click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn_close_click(Sender: TObject);
    procedure btn_reg2_click(Sender: TObject);
    procedure btn_unreg2_click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    LangArray: array of TLanguageEntry;
    function GetLangEntry(name: string): string;
  end;

var
  ConfigForm: TConfigForm;

implementation

uses Main;

{$R *.dfm}


function TConfigForm.GetLangEntry(name: string): string;
var
  i: integer;
begin
  for i := 0 to high(LangArray) do
  begin
    if LangArray[i].name = name then
    begin
      result := LangArray[i].text;
      break;
    end;
  end;
end;

procedure TConfigForm.btn_reg1_click(Sender: TObject);
var
  Reg: TRegistry;
  Fehler: boolean;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    Fehler := false;

    if Reg.OpenKey('\.dc4', true) then
    begin
      Reg.WriteString('', 'DeCoder4-File');
      if Reg.ReadString('') <> 'DeCoder4-File' then
        Fehler := true;
      Reg.CloseKey();
    end
    else
      Fehler := true;

    if Reg.OpenKey('\DeCoder4-File', true) then
    begin
      Reg.WriteString('', GetLangEntry('dc4file'));
      if Reg.ReadString('') <> GetLangEntry('dc4file') then
        Fehler := true;
      Reg.CloseKey();
    end
    else
      Fehler := true;

    if Reg.OpenKey('\DeCoder4-File\DefaultIcon', true) then
    begin
      Reg.WriteString('', application.ExeName+',1');
      if Reg.ReadString('') <> application.ExeName+',1' then
        Fehler := true;
      Reg.CloseKey();
    end
    else
      Fehler := true;

    if Reg.OpenKey('\DeCoder4-File\shell\open\command', true) then
    begin
      Reg.WriteString('', '"'+application.ExeName+'" "%1"');
      if reg.ReadString('') <> '"'+application.ExeName+'" "%1"' then
        Fehler := true;
      Reg.CloseKey();
    end
    else
      Fehler := true;

  finally
    Reg.Free;
  end;

  if Fehler then
    Application.MessageBox(pchar(GetLangEntry('regerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
  else
    Application.MessageBox(pchar(GetLangEntry('succeed')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION);
end;

procedure TConfigForm.btn_unreg1_click(Sender: TObject);
var
  Reg: TRegistry;
  temp: string;
  Fehler: boolean;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    Fehler := false;

    if Reg.OpenKey('\.dc4', false) then
    begin
      temp := Reg.ReadString('');
      Reg.CloseKey();
      if (Reg.KeyExists('\'+temp) and not Reg.DeleteKey('\'+temp)) or
        (Reg.KeyExists('\.dc4') and not Reg.DeleteKey('\.dc4')) then
          Fehler := true;
    end;
  finally
    Reg.free;
  end;

  if Fehler then
    Application.MessageBox(pchar(GetLangEntry('unregerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
  else
    Application.MessageBox(pchar(GetLangEntry('succeed')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION);
end;

procedure TConfigForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // mainform.btn_config.down := false;
end;

procedure TConfigForm.btn_close_click(Sender: TObject);
begin
  close;
end;

procedure RegisterServer (const Filename: String);
var
  LibHandle: THandle;
  RegisterServerProc: function: HRESULT; stdcall;
begin
  LibHandle := LoadLibrary(PChar(Filename));
  if LibHandle <> 0 then
    try
      @RegisterServerProc := GetProcAddress(LibHandle, 'DllRegisterServer');
      if Assigned(@RegisterServerProc) then
        RegisterServerProc;
    finally
      FreeLibrary (LibHandle);
    end;
end;

procedure UnRegisterServer (const Filename: String);
var
  LibHandle: THandle;
  UnRegisterServerProc: function: HRESULT; stdcall;
begin
  LibHandle := LoadLibrary(PChar(Filename));
  if LibHandle <> 0 then
    try
      @UnRegisterServerProc := GetProcAddress(LibHandle, 'DllUnregisterServer');
      if Assigned(@UnRegisterServerProc) then
        UnRegisterServerProc;
    finally
      FreeLibrary (LibHandle);
    end;
end;

procedure TConfigForm.btn_reg2_click(Sender: TObject);
begin
  CoInitialize (nil);

  try
    RegisterServer(extractfilepath(application.exename)+'ShlExt.dll');
  finally
    CoUninitialize;
  end;

  Application.MessageBox(pchar(GetLangEntry('executed')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION);

  // Ich verwende RegSvr32.exe nicht, weil es bei der ersten Veröffentlichung von Windows 95 nicht dabei war
  // ShellExecute(Handle, 'open', PChar('regsvr32.exe'), pchar('"'+extractfilepath(application.exename)+'ShlExt.dll"'), nil, SW_SHOW);
end;

procedure TConfigForm.btn_unreg2_click(Sender: TObject);
begin
  CoInitialize (nil);

  try
    UnRegisterServer(extractfilepath(application.exename)+'ShlExt.dll');
  finally
    CoUninitialize;
  end;

  Application.MessageBox(pchar(GetLangEntry('executed')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION);

  // Ich verwende RegSvr32.exe nicht, weil es bei der ersten Veröffentlichung von Windows 95 nicht dabei war
  // ShellExecute(Handle, 'open', PChar('regsvr32.exe'), pchar('/u "'+extractfilepath(application.exename)+'ShlExt.dll"'), nil, SW_SHOW);
end;

procedure TConfigForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i: integer;
begin
  // Sprachdatei auslesen

  ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'Language.ini');
  str := TStringList.Create();
  try
    ini.ReadSection(Name, str);
    for i := 0 to str.count-1 do
    begin
      setlength(LangArray, length(LangArray)+1);
      LangArray[length(LangArray)-1].name := str.strings[i];
      LangArray[length(LangArray)-1].text := ini.ReadString(name, str.strings[i], '');
      LangArray[length(LangArray)-1].text := StringReplace(LangArray[length(LangArray)-1].text, '###', #13#10, [rfReplaceAll]);
    end;
  finally
    ini.free;
    str.Free;
  end;

  // Formular vorbereiten

  btn_reg1.Caption := GetLangEntry('reregister');
  btn_reg2.Caption := GetLangEntry('reregister');
  btn_unreg1.Caption := GetLangEntry('unregister');
  btn_unreg2.Caption := GetLangEntry('unregister');
  lbl_label1.Caption := GetLangEntry('confinfo1');
  lbl_label2.Caption := GetLangEntry('confinfo2');
  lbl_label3.Caption := GetLangEntry('confinfo3');
  lbl_label4.Caption := GetLangEntry('confinfo4');
  grp_system.Caption := GetLangEntry('operatingsystem');
  btn_close.Caption := GetLangEntry('close');
  caption := GetLangEntry('caption');
end;

end.
