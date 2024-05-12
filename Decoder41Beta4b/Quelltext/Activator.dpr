// Activator.exe wird für Windows Vista benötigt und aktiviert Konfigurationseinstellungen
// Diese Anwendung besitzt durch den Kompatibilitätsmodus volle Adminrechte

program Activator;

uses
  SysUtils,
  Windows,
  IniFiles,
  Classes,
  Registry;

{$R Activator.res}

type
  TLanguageEntry = record
    name: string;
    text: string;
  end;

var
  LangArray: array of TLanguageEntry;
  ini: TIniFile;
  str: TStringList;
  i: integer;

const
  Name: String = 'Activator';
  DC4Ver: String = '4.1';

function GetLangEntry(name: string): string;
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

function DLLRegister(dll: string; doregister: boolean): boolean;
type
  DllReg = function: HResult; stdcall;
var
  hDll: THandle;
  dr: DllReg;
begin
  result := false;
  hDll := LoadLibrary(PChar(dll));
  if hDll <> 0 then
  begin
    if doregister then
      @dr := GetProcAddress(hDll, 'DllRegisterServer')
    else
      @dr := GetProcAddress(hDll, 'DllUnregisterServer');
    if assigned(dr) then
    begin
      dr;
      result := true;
    end;
  end;
end;

function TypeRegistry(App, IconIndex, Extension, InternalName, Name: string): boolean;
var
  reg: TRegistry;
  c1, c2, c3, c4, l1, l2, l3, l4: boolean;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    l1 := false;
    if Reg.OpenKey('\'+Extension, true) then
    begin
      Reg.WriteString('', InternalName);
      l1 := reg.ReadString('') = InternalName;
      Reg.CloseKey();
    end;
    c1 :=  reg.KeyExists('\'+Extension);

    l2 := false;
    if Reg.OpenKey('\'+InternalName, true) then
    begin
      Reg.WriteString('', Name);
      l2 := reg.ReadString('') = Name;
      Reg.CloseKey();
    end;
    c2 := reg.KeyExists('\'+InternalName);

    l3 := false;
    if Reg.OpenKey('\'+InternalName+'\DefaultIcon', true) then
    begin
      Reg.WriteString('', App+','+IconIndex);
      l3 := reg.ReadString('') = App+','+IconIndex;
      Reg.CloseKey();
    end;
    c3 := reg.KeyExists('\'+InternalName+'\DefaultIcon');

    l4 := false;
    if Reg.OpenKey('\'+InternalName+'\shell\open\command', true) then
    begin
      Reg.WriteString('', '"'+App+'" "%1"');
      l4 := reg.ReadString('') = '"'+App+'" "%1"';
      Reg.CloseKey();
    end;
    c4 := reg.KeyExists('\'+InternalName+'\shell\open\command');
  finally
    Reg.Free;
  end;

  result := c1 and c2 and c3 and c4 and l1 and l2 and l3 and l4;
end;

function TypeUnregistry(Extension: string): boolean;
var
  reg: TRegistry;
  temp: string;
  c1, c2: boolean;
begin
  c1 := false;
  c2 := false;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    if Reg.OpenKey('\'+Extension, false) then
    begin
      temp := Reg.ReadString('');
      Reg.CloseKey();
      if reg.KeyExists('\'+temp) then Reg.DeleteKey('\'+temp);
      c1 := not reg.KeyExists('\'+temp);
      if reg.KeyExists('\'+Extension) then Reg.DeleteKey('\'+Extension);
      c2 := not reg.KeyExists('\'+Extension);
    end;
  finally
    Reg.Free;
  end;

  result := c1 and c2;
end;

begin
  // Sprachdatei auslesen

  ini := TIniFile.Create(ExtractFilePath(paramstr(0))+'Language.ini');
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

  // Ausführung

  if paramstr(1) = '/dllreg' then
  begin
    if not DLLRegister(paramstr(2), true) then
      MessageBox(0, pchar(GetLangEntry('failed')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR);
  end;
  if paramstr(1) = '/dllunreg' then
  begin
    if not DLLRegister(paramstr(2), false) then
      MessageBox(0, pchar(GetLangEntry('failed')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR);
  end;
  if paramstr(1) = '/typereg' then
  begin
    if not TypeRegistry(paramstr(3), paramstr(4), paramstr(2), paramstr(5), paramstr(6)) then
      MessageBox(0, pchar(GetLangEntry('failed')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR);
  end;
  if paramstr(1) = '/typeunreg' then
  begin
    if not TypeUnregistry(paramstr(2)) then
      MessageBox(0, pchar(GetLangEntry('failed')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR);
  end;
end.
