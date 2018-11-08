unit Config;

interface

uses
  Windows, Messages, SysUtils, Forms, shellapi, registry, inifiles, DCConst,
  ExtCtrls, StdCtrls, ComCtrls, Controls, Classes;

type
  TConfigForm = class(TForm)
    btn_close: TButton;
    lbl_info1: TLabel;
    lbl_info2: TLabel;
    grp_system: TGroupBox;
    img_information: TImage;
    tmr_timer: TTimer;
    sys_checkbox1: TCheckBox;
    sys_checkbox2: TCheckBox;
    sys_checkbox3: TCheckBox;
    sys_checkbox4: TCheckBox;
    grp_keylogger: TGroupBox;
    lbl_key_info: TLabel;
    edt_garbarge: TEdit;
    upd_garbarge: TUpDown;
    lbl_bytes: TLabel;
    procedure btn_close_click(Sender: TObject);
    procedure form_create(Sender: TObject);
    procedure timer_timer(Sender: TObject);
    procedure sys_checkbox1_click(Sender: TObject);
    procedure sys_checkbox2_click(Sender: TObject);
    procedure sys_checkbox3_click(Sender: TObject);
    procedure sys_checkbox4_click(Sender: TObject);
    procedure form_show(Sender: TObject);
    procedure form_hide(Sender: TObject);
    procedure upd_garbargeClick(Sender: TObject; Button: TUDBtnType);
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

procedure TConfigForm.timer_timer(Sender: TObject);
var
  Reg: TRegistry;
begin
  sys_checkbox1.OnClick := nil;
  sys_checkbox2.OnClick := nil;
  sys_checkbox3.OnClick := nil;
  sys_checkbox4.OnClick := nil;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    sys_checkbox1.checked := Reg.KeyExists('.dc4') and Reg.KeyExists('DeCoder4-File');
    sys_checkbox2.checked := Reg.KeyExists('*\shellex\ContextMenuHandlers\(De)Coder') and Reg.KeyExists('*\shellex\ContextMenuHandlers\(De)Coder');
    sys_checkbox3.checked := Reg.KeyExists('*\shellex\ContextMenuHandlers\(De)Coder-Erase') and Reg.KeyExists('*\shellex\ContextMenuHandlers\(De)Coder-Erase');
    sys_checkbox4.checked := Reg.KeyExists('CLSID\{54069E5A-C471-4B68-835C-FC845E64040B}');
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\ViaThinkSoft\(De)Coder\', true) then
    begin
      if not reg.ValueExists('GarbargeCount') then
        reg.WriteInteger('GarbargeCount', StdGarbarge);
      upd_garbarge.Position := reg.ReadInteger('GarbargeCount');
      edt_garbarge.Text := inttostr(upd_garbarge.Position);
      reg.CloseKey;
    end;
  finally
    Reg.free;
  end;
  sys_checkbox1.OnClick := sys_checkbox1_click;
  sys_checkbox2.OnClick := sys_checkbox2_click;
  sys_checkbox3.OnClick := sys_checkbox3_click;
  sys_checkbox4.OnClick := sys_checkbox4_click;
end;

procedure TConfigForm.upd_garbargeClick(Sender: TObject; Button: TUDBtnType);
var
  reg: tregistry;
begin
  edt_garbarge.Text := inttostr(upd_garbarge.Position);
  reg := tregistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('SOFTWARE\ViaThinkSoft\(De)Coder\', true) then
    begin
      reg.WriteInteger('GarbargeCount', upd_garbarge.Position);
      reg.CloseKey;
    end;
  finally
    reg.free;
  end;
end;

procedure TConfigForm.btn_close_click(Sender: TObject);
begin
  close;
end;

procedure TConfigForm.sys_checkbox1_click(Sender: TObject);
begin
  if fileexists(extractfilepath(application.ExeName)+activator) then
  begin
    if sys_checkbox1.Checked then
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/typereg .dc4 "'+application.exename+'" 1 DeCoder4-File "'+Format(GetLangEntry('dc4file'), [DC4Ver])+'"'), pchar(extractfilepath(application.exename)), sw_normal)
    else
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/typeunreg .dc4'), pchar(extractfilepath(application.exename)), sw_normal);
  end;
end;

procedure TConfigForm.sys_checkbox2_click(Sender: TObject);
begin
  if fileexists(extractfilepath(application.ExeName)+activator) then
  begin
    if sys_checkbox2.Checked then
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/dllreg "'+extractfilepath(application.exename)+'ShlExt.dll"'), pchar(extractfilepath(application.ExeName)), sw_normal)
    else
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/dllunreg "'+extractfilepath(application.exename)+'ShlExt.dll"'), pchar(extractfilepath(application.ExeName)), sw_normal);
  end;
end;

procedure TConfigForm.sys_checkbox3_click(Sender: TObject);
begin
  if fileexists(extractfilepath(application.ExeName)+activator) then
  begin
    if sys_checkbox3.Checked then
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/dllreg "'+extractfilepath(application.exename)+'ShlErase.dll"'), pchar(extractfilepath(application.ExeName)), sw_normal)
    else
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/dllunreg "'+extractfilepath(application.exename)+'ShlErase.dll"'), pchar(extractfilepath(application.ExeName)), sw_normal);
  end;
end;

procedure TConfigForm.sys_checkbox4_click(Sender: TObject);
begin
  if fileexists(extractfilepath(application.ExeName)+activator) then
  begin
    if sys_checkbox4.Checked then
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/dllreg "'+extractfilepath(application.exename)+'SecureMoveExt.dll"'), pchar(extractfilepath(application.ExeName)), sw_normal)
    else
      shellexecute(application.Handle, 'open', pchar(extractfilepath(application.ExeName)+activator), pchar('/dllunreg "'+extractfilepath(application.exename)+'SecureMoveExt.dll"'), pchar(extractfilepath(application.ExeName)), sw_normal);
  end;
end;

procedure TConfigForm.form_hide(Sender: TObject);
begin
  tmr_timer.Enabled := false;
end;

procedure TConfigForm.form_show(Sender: TObject);
begin
  tmr_timer.Enabled := true;
  timer_timer(self);
end;

procedure TConfigForm.form_create(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i, temp: integer;
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
      LangArray[length(LangArray)-1].text := ini.ReadString(name, str.strings[i], '?');
      LangArray[length(LangArray)-1].text := StringReplace(LangArray[length(LangArray)-1].text, '###', #13#10, [rfReplaceAll]);
    end;
  finally
    ini.free;
    str.Free;
  end;

  // Formular vorbereiten

  lbl_info1.Caption := GetLangEntry('confinfo1');
  lbl_info2.Caption := GetLangEntry('restartinfo');
  lbl_key_info.Caption := Format(GetLangEntry('keyinfo'), [StdGarbarge]);
  grp_system.Caption := GetLangEntry('operatingsystem');
  grp_keylogger.Caption := GetLangEntry('keycapt');
  btn_close.Caption := GetLangEntry('close');
  lbl_bytes.Caption := GetLangEntry('bytes');

  sys_checkbox1.Caption := GetLangEntry('confinfo2');
  sys_checkbox2.Caption := GetLangEntry('confinfo3');
  sys_checkbox3.Caption := GetLangEntry('confinfo4');
  sys_checkbox4.Caption := GetLangEntry('confinfo5');

  caption := GetLangEntry('caption');
  icon.LoadFromResourceID(hInstance, 103);
  img_information.Picture.Bitmap.LoadFromResourceName(hInstance, 'Information');

  // Bei verschiedenen Sprachen gibt es verschiedene Label-Größen!
  // Leider gibt es bei TCheckBox keine Zeilenumbrüche oder AutoSize

  // Vertikale Anpassung

  sys_checkbox1.Top := lbl_info1.Top + lbl_info1.Height + 16;
  sys_checkbox2.top := sys_checkbox1.top + sys_checkbox1.Height;
  sys_checkbox3.top := sys_checkbox2.top + sys_checkbox2.Height;
  sys_checkbox4.top := sys_checkbox3.top + sys_checkbox3.Height;
  img_information.top := sys_checkbox4.top + sys_checkbox4.height + 16;
  lbl_info2.top := img_information.top;
  grp_system.Height := lbl_info2.Top + lbl_info2.Height + 16;
  upd_garbarge.Top := lbl_key_info.Top + lbl_key_info.Height + 8;
  grp_keylogger.Height := upd_garbarge.Top + upd_garbarge.Height + 16;
  edt_garbarge.Top := upd_garbarge.Top;
  upd_garbarge.Height := edt_garbarge.Height;
  grp_keylogger.top := grp_system.Top + grp_system.Height + 8;
  btn_close.top := grp_keylogger.Top + grp_keylogger.Height + 8;
  clientheight := btn_close.Top + btn_close.Height + 8;
  lbl_bytes.top := upd_garbarge.top + upd_garbarge.Height div 2 - lbl_bytes.Height div 2;

  // Horizontale Anpassung
  
  temp := max_3(lbl_info1.width, lbl_info2.Width+img_information.width+(lbl_info2.left-img_information.left-img_information.width), lbl_key_info.Width);
  sys_checkbox1.Width := temp;
  sys_checkbox2.Width := temp;
  sys_checkbox3.Width := temp;
  sys_checkbox4.Width := temp;
  grp_system.Width := temp + 32;
  grp_keylogger.Width :=  grp_system.Width;
  upd_garbarge.Left := edt_garbarge.Left + edt_garbarge.Width + 1;
  clientwidth := grp_system.Left + grp_system.Width + 8;
  btn_close.Left := clientwidth div 2 - btn_close.width div 2;

  // Werte

  edt_garbarge.Text := inttostr(upd_garbarge.Position);
  timer_timer(self);
end;

end.
