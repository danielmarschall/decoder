unit Main;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HCMngr, ExtCtrls, ImgList, ElImgLst, ElBtnCtl, ElPopBtn,
  ElXPThemedControl, ElEdits, ElHTMLLbl, StdCtrls, ElCLabel, ElLabel,
  ElPanel, Windows, ShellAPI, ElHTMLView, Registry, ShlObj, IniFiles,
  MSI_STORAGE, HSLUnit, ComCtrls;

{
$I VER.INC
}

type
  TSystemDirectory = (SD_NO,
                      SD_DESKTOP,
                      SD_IE,
                      SD_SMPROGRAMS,
                      SD_SETTINGS,
                      SD_PRINTER,
                      SD_PERSONAL,
                      SD_FAVORITES,
                      SD_AUTOSTART,
                      SD_RECENT,
                      SD_SENDTO,
                      SD_PAPERBASKED,
                      SD_STARTMENU,
                      SD_MUSIC,
                      SD_VIDEOS,
                      SD_COMPUTER,
                      SD_NETWORK,
                      SD_FONTS,
                      SD_SHELLNEW,
                      SD_AUDESKTOP,
                      SD_APPDATA,
                      SD_PRINTHOOD,
                      SD_TEMPINETFILES,
                      SD_COOKIES,
                      SD_COURSE,
                      SD_AUAPPDATA,
                      SD_WINDOWS,
                      SD_SYSTEM,
                      SD_PROGRAMS,
                      SD_PICTURES,
                      SD_AUFILES);

  TMainForm = class(TForm)
    BgPanel: TElPanel;
    ULab1: TElLabel;
    CLab1: TElHTMLLabel;
    MenuPanel: TElPanel;
    MBev1: TBevel;
    MBev2: TBevel;
    DLab1: TElLabel;
    DImg: TImage;
    DLab3: TElLabel;
    DLab3b: TElLabel;
    DLab2: TElLabel;
    DLab1b: TElLabel;
    DLab2b: TElLabel;
    BlinkTimer: TTimer;
    WaitTmr1: TTimer;
    OpenDlg: TOpenDialog;
    DLab4: TElHTMLLabel;
    DImg2: TImage;
    DImg3: TImage;
    CipherManager1: TCipherManager;
    HashManager1: THashManager;
    Capt: TElPanel;
    CaptLabel: TElLabel;
    ExBtn: TElPanel;
    OpenBtn: TElPanel;
    CloseBtn: TElPanel;
    EncBtn: TElPanel;
    DecBtn: TElPanel;
    MBev3: TBevel;
    HelpBtn: TElPanel;
    ExitBtn: TElPanel;
    VBox: TElPanel;
    VLab1: TElLabel;
    PwdEdit: TElEdit;
    VLab2: TElHTMLLabel;
    InfoBtn: TElPanel;
    OptionBtn: TElPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    Button1: TButton;
    Label1: TLabel;
    procedure CLab1LinkClick(Sender: TObject; HRef: TElFString);
    procedure BlinkTimerTimer(Sender: TObject);
    procedure WaitTmr1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CaptMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExBtnMouseLeave(Sender: TObject);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseLeave(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure ExBtnMouseEnter(Sender: TObject);
  public
    BlinkVar: integer;
    procedure dc4_CloseFile;
    procedure dc4_OpenFile(filename: string);
    procedure dc4_Showmessage(msg: string);
    procedure ChangeButtonState(Mode: boolean; Obj: TObject);
    procedure ClickHandler(Sender: TObject);
    procedure DrawDesign;
    procedure Standardfarben;
    procedure DesignInit;
    function GetFolder(FolderID: TSystemDirectory): string;
  end;

var
  MainForm: TMainForm;
  MBDown: boolean;
    Design_Button_Active1,
    Design_Button_Active2,
    Design_Button_Leuchtend1,
    Design_Button_Leuchtend2,
    Design_Button_Inactive1,
    Design_Button_Inactive2,
    Design_Form1,
    Design_Form2,
    Design_Caption1,
    Design_Caption2,
    Design_Caption_Active1,
    Design_Caption_Active2,
    Design_Menu1,
    Design_Menu2: TColor;
    Design_Button_Active1_lock,
    Design_Button_Active2_lock,
    Design_Button_Leuchtend1_lock,
    Design_Button_Leuchtend2_lock,
    Design_Button_Inactive1_lock,
    Design_Button_Inactive2_lock,
    Design_Form1_lock,
    Design_Form2_lock,
    Design_Caption1_lock,
    Design_Caption2_lock,
    Design_Caption_Active1_lock,
    Design_Caption_Active2_lock,
    Design_Menu1_lock,
    Design_Menu2_lock: boolean;

implementation

uses Options, RepeatPassword, Message, FileInfo;

var
  HSLRange: integer = 240;

{$R *.DFM}

function GetHWID(): string;
var
  Storage: TStorage;
begin
  Storage:=TStorage.Create;
  Storage.GetInfo;
  result := Storage.Devices[0].Model+'/'+Storage.Devices[0].Revision+'/'+Storage.Devices[0].SerialNumber;
  Storage.Free;
end;

procedure TMainForm.ChangeButtonState(Mode: boolean; Obj: TObject);
begin
  TElPanel(Obj).Enabled := Mode;
  if TElPanel(Obj).Enabled then
  begin
    TElPanel(Obj).GradientEndColor := Design_Button_Active1;
    TElPanel(Obj).GradientStartColor := Design_Button_Active2;
    TElPanel(Obj).Tag := 1;
  end
  else
  begin
    TElPanel(Obj).GradientEndColor := Design_Button_Inactive1;
    TElPanel(Obj).GradientStartColor := Design_Button_Inactive2;
    TElPanel(Obj).Tag := 0;
  end;
end;

// Entnommen von FileCtrl
procedure CutFirstDirectory(var S: TFileName);
var
  Root: Boolean;
  P: Integer;
begin
  if S = '\' then
    S := ''
  else
  begin
    if S[1] = '\' then
    begin
      Root := True;
      Delete(S, 1, 1);
    end
    else
      Root := False;
    if S[1] = '.' then
      Delete(S, 1, 4);
    P := AnsiPos('\',S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
    if Root then
      S := '\' + S;
  end;
end;

// Entnommen von FileCtrl
function MinimizeName(const Filename: TFileName; Canvas: TCanvas;
  MaxLen: Integer): TFileName;
var
  Drive: TFileName;
  Dir: TFileName;
  Name: TFileName;
begin
  Result := FileName;
  Dir := ExtractFilePath(Result);
  Name := ExtractFileName(Result);

  if (Length(Dir) >= 2) and (Dir[2] = ':') then
  begin
    Drive := Copy(Dir, 1, 2);
    Delete(Dir, 1, 2);
  end
  else
    Drive := '';
  while ((Dir <> '') or (Drive <> '')) and (Canvas.TextWidth(Result) > MaxLen) do
  begin
    if Dir = '\...\' then
    begin
      Drive := '';
      Dir := '...\';
    end
    else if Dir = '' then
      Drive := ''
    else
      CutFirstDirectory(Dir);
    Result := Drive + Dir + Name;
  end;
end;

procedure TMainForm.dc4_Showmessage(msg: string);
begin
  MessageForm.Message.caption := msg;
  MessageForm.showmodal;
end;

function TMainForm.GetFolder(FolderID: TSystemDirectory): string;
var
  pidl: PItemIDList;
  Path: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  i := 0;
  case FolderID of
    SD_NO             : Exit;
    SD_DESKTOP        : i := $00;
    SD_IE             : i := $01;
    SD_SMPROGRAMS     : i := $02;
    SD_SETTINGS       : i := $03;
    SD_PRINTER        : i := $04;
    SD_PERSONAL       : i := $05;
    SD_FAVORITES      : i := $06;
    SD_AUTOSTART      : i := $07;
    SD_RECENT         : i := $08;
    SD_SENDTO         : i := $09;
    SD_PAPERBASKED    : i := $0A;
    SD_STARTMENU      : i := $0B;
    SD_MUSIC          : i := $0C;
    SD_VIDEOS         : i := $0D;
    SD_COMPUTER       : i := $11;
    SD_NETWORK        : i := $12;
    SD_FONTS          : i := $14;
    SD_SHELLNEW       : i := $15;
    SD_AUDESKTOP      : i := $19;
    SD_APPDATA        : i := $1A;
    SD_PRINTHOOD      : i := $1B;
    SD_TEMPINETFILES  : i := $20;
    SD_COOKIES        : i := $21;
    SD_COURSE         : i := $22;
    SD_AUAPPDATA      : i := $23;
    SD_WINDOWS        : i := $24;
    SD_SYSTEM         : i := $25;
    SD_PROGRAMS       : i := $26;
    SD_PICTURES       : i := $27;
    SD_AUFILES        : i := $28;
  end;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, i, pidl)) then
  begin
    SHGetPathFromIDList(pidl, Path);
    Result := Path;
  end;
end;

procedure TMainForm.dc4_CloseFile;
begin
  {* Objekteigenschaften ändern *}
  DLab1.caption := 'Keine Datei geöffnet.';
  VBox.visible := false;
  DLab3.visible := false;
  DLab3b.visible := false;
  DLab2.visible := false;
  DLab1b.visible := false;
  DLab2b.visible := false;
  DLab4.visible := false;
  ChangeButtonState(false, InfoBtn);
  ChangeButtonState(false, CloseBtn);
  ChangeButtonState(false, EncBtn);
  ChangeButtonState(false, DecBtn);
  DImg.Visible := false;
  DImg2.Visible := false;
  DImg3.Visible := true;
  RepeatForm.B2CipherCombo.ItemIndex := 1;
  RepeatForm.B2CipherComboChange(nil);
  RepeatForm.B2ModeCombo.ItemIndex := 0;
  RepeatForm.B2ModeComboChange(nil);

  Label1.Visible := true;
end;

procedure TMainForm.dc4_OpenFile(filename: string);
var
  Reg: TRegistry;
  temp: string;
  INIDatei: TIniFile;
begin
  {* Dateityp herausfinden *}
  if ExtractFileExt(filename) <> '' then
  begin
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey(ExtractFileExt(filename), false) then
    begin
      temp := Reg.ReadString('');
      Reg.CloseKey();
      if temp <> '' then
      begin
        Reg.OpenKey(temp, false);
        temp := Reg.ReadString('');
        Reg.CloseKey();
        if temp <> '' then
          DLab2b.caption := temp
        else
          DLab2b.caption := 'Unbekannt'; {* Weiterleitung hat keinen Namen für den Dateityp *}
      end
      else
        DLab2b.caption := 'Unbekannt'; {* Konnte keine Weiterleitung in der Registry finden *}
    end
    else
      DLab2b.caption := 'Unbekannt'; {* Keinen Eintrag der Erweiterung in der Registry gefunden *}
    Reg.free;
  end
  else
    DLab2b.caption := 'Unbekannt'; {* Keine Erweiterung *}
  {* Objekteigenschaften ändern *}
  DLab1.caption := 'Geöffnete Datei:';
  // DLab1b.caption := copy(ExtractFileName(filename), 0, length(ExtractFileName(filename))-length(ExtractFileExt(filename)));
  DLab1b.caption := ExtractFileName(filename);
  DLab1b.visible := true;
  DLab2b.visible := true;
  VBox.visible := true;
  PwdEdit.text := '';
  DLab3.visible := true;
  DLab3b.visible := true;
  DLab3b.caption := MinimizeName(ExtractFilePath(filename), MainForm.Canvas, DLab3b.Width);
  DLab2.visible := true;
  ChangeButtonState(true, CloseBtn);
  ChangeButtonState((ExtractFileExt(filename) <> '.dc4'), EncBtn);
  ChangeButtonState((ExtractFileExt(filename) = '.dc4'), DecBtn);
  ChangeButtonState((ExtractFileExt(filename) = '.dc4'), InfoBtn);
  DImg.visible := (ExtractFileExt(filename) <> '.dc4');
  DImg2.visible := (ExtractFileExt(filename) = '.dc4');
  DImg3.Visible := false;
  DLab4.visible := true;
  VLab1.Caption := 'Passwort zum ';
  if ExtractFileExt(filename) = '.dc4' then
  begin
    DLab4.caption := 'Die Datei wurde bereits verschlüsselt.';
    VLab1.Caption := VLab1.Caption + 'Entschlüsseln:';
  end
  else
  begin
    DLab4.caption := 'Diese Datei wurde noch nicht verschlüsselt.';
    VLab1.Caption := VLab1.Caption + 'Verschlüsseln:';
  end;
  PwdEdit.SetFocus; // ist das gut?
  {* Header-Daten auslesen *}
  INIDatei := TIniFile.Create(filename);
  InfoForm.InfoLbl1b.Caption := INIDatei.ReadString('Header', 'Software', '?');
  InfoForm.InfoLbl2b.Caption := INIDatei.ReadString('Header', 'User', '?');
  InfoForm.InfoLbl3b.Caption := INIDatei.ReadString('Header', 'OriginalFileName', '?');
  InfoForm.InfoLbl4b.Caption := INIDatei.ReadString('Header', 'Algotythmus', '?');
  InfoForm.InfoLbl5b.Caption := INIDatei.ReadString('Header', 'Mode', '?');
  INIDatei.Free;
  // ToDo: Prüfung der Header einbauen!

  Label1.Visible := false;
end;

procedure TMainForm.CLab1LinkClick(Sender: TObject; HRef: TElFString);
begin
  if href = 'home' then
    shellexecute(handle, 'open', 'http://www.d-m-home.de/', '', '', 1);
  if href = 'email' then
    shellexecute(handle, 'open', 'mailto:info@daniel-marschall.de?subject=(De)Coder 4.0', '', '', 1);
end;

procedure TMainForm.BlinkTimerTimer(Sender: TObject);
begin
  if not MessageForm.visible then
  begin
    inc(BlinkVar);
    if BlinkVar > 6 then
    begin
      BlinkVar := 0;
      BlinkTimer.Enabled := false;
    end;
    {* Lieber mit gleich oder ungleich abprüfen? *}
    if (BlinkVar = 1) or (BlinkVar = 3) or (BlinkVar = 5) then
    begin
      VLab1.Font.Color := clRed;
      // VLab2.Font.Color := clRed;
    end;
    if (BlinkVar = 2) or (BlinkVar = 4) or (BlinkVar = 6) then
    begin
      VLab1.Font.Color := $00000A32;
      // VLab2.Font.Color := clMaroon;
    end;
  end;
end;

function GetRegUser(): string;
var
  Reg: TRegistry;
  temp: string;
begin
  {* Benutzernamen herausfinden *}
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion', false) then
  begin
    temp := Reg.ReadString('RegisteredOwner');
    Reg.CloseKey();
  end;
  if temp = '' then
  begin
    if Reg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion', false) then
    begin
      temp := Reg.ReadString('RegisteredOwner');
      Reg.CloseKey();
    end;
    if temp = '' then
    begin
      result := ''; // Unbekanntes System: Kein Benutzername gefunden!
    end
    else
    begin
      result := temp; // NT-System: Benutzername gefunden!
    end;
  end
  else
  begin
    result := temp; // 9x-System: Benutzername gefunden!
  end;
end;

procedure TMainForm.WaitTmr1Timer(Sender: TObject);
var
  INIDatei: TIniFile;
  Daten: TextFile;
  DateiName, DC4Open: string;
  dstr: TStream;
  FromF: file;
  NumRead, NumWritten: longint;
  Buf: array[1..40000] of byte;
begin
  WaitTmr1.enabled := false;
  DC4Open := RepeatForm.B4Filename.Caption;
    // VERSCHLÜSSELUNG
    if ExtractFileExt(DC4Open) <> '.dc4' then DateiName := DC4Open + '.dc4' else DateiName := DC4Open;
    AssignFile(daten, DateiName);
    ReWrite(daten);
    CloseFile(daten);
    INIDatei := TIniFile.Create(DateiName);
    INIDatei.WriteString('Header', 'Software', '(De)Coder 4.0');
    INIDatei.WriteString('Header', 'OriginalFileName', DLab1b.Caption);
    INIDatei.WriteString('Header', 'Hardware-ID', GetHWID());
    INIDatei.WriteString('Header', 'User', GetRegUser());
    INIDatei.WriteString('Header', 'Algotythmus', CipherManager1.Algorithm);
    INIDatei.WriteString('Header', 'Mode', '?'); // !!!
    INIDatei.WriteString('Header', 'Compress', '?'); // !!!
    INIDatei.WriteString('Header', 'Hash', HashManager1.Algorithm);
    INIDatei.Free;
    AssignFile(daten, DateiName);
    Append(daten);
    WriteLN(daten, '');
    // Sektion nicht mit INIFILE schreiben?
    WriteLN(daten, '[Data]');
    WriteLN(daten, '<hier der Code>');

    AssignFile(FromF, DC4Open);
    Reset(FromF, 1);
    dstr := TStream.Create;
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      dstr.Write(Buf, SizeOf(Buf));
    until (NumRead = 0);
    closefile(FromF);

    repeat
      dstr.ReadBuffer(Buf, SizeOf(Buf));
    until dstr.Position >= dstr.Size;

   // CipherManager1.EncodeStream(dstr, dstr, dstr.size);
    dstr.Free;

    CloseFile(daten);
    dc4_Showmessage('Datei wurde erfolgreich verschlüsselt!');
end;

procedure TMainForm.DrawDesign;
begin
  BgPanel.GradientEndColor := Design_Form1;
  BgPanel.GradientStartColor := Design_Form2;
  Capt.GradientEndColor := Design_Caption1;
  Capt.GradientStartColor := Design_Caption2;
  ExBtn.GradientEndColor := Design_Caption1;
  ExBtn.GradientStartColor := Design_Caption2;
  MenuPanel.GradientEndColor := Design_Menu1;
  MenuPanel.GradientStartColor := Design_Menu2;
  VBox.GradientEndColor := Design_Menu1;
  VBox.GradientStartColor := Design_Menu2;

  if (OpenBtn.Tag = 1) then ChangeButtonState(true, OpenBtn) else ChangeButtonState(false, OpenBtn);
  if (CloseBtn.Tag = 1) then ChangeButtonState(true, CloseBtn) else ChangeButtonState(false, CloseBtn);
  if (OpenBtn.Tag = 1) then ChangeButtonState(true, OpenBtn) else ChangeButtonState(false, OpenBtn);
  if (InfoBtn.Tag = 1) then ChangeButtonState(true, InfoBtn) else ChangeButtonState(false, InfoBtn);
  if (EncBtn.Tag = 1) then ChangeButtonState(true, EncBtn) else ChangeButtonState(false, EncBtn);
  if (DecBtn.Tag = 1) then ChangeButtonState(true, DecBtn) else ChangeButtonState(false, DecBtn);
  if (OptionBtn.Tag = 1) then ChangeButtonState(true, OptionBtn) else ChangeButtonState(false, OptionBtn);
  if (HelpBtn.Tag = 1) then ChangeButtonState(true, HelpBtn) else ChangeButtonState(false, HelpBtn);
  if (ExitBtn.Tag = 1) then ChangeButtonState(true, ExitBtn) else ChangeButtonState(false, ExitBtn);
end;

procedure TMainForm.Standardfarben;
var
  H, S, L: integer;
begin
  Design_Form1 := clYellow; Design_Form1_lock := false;
  Design_Form2 := $000080FF; Design_Form2_lock := false;

  Design_Button_Active1 := Design_Form1; Design_Button_Active1_lock := false;
  Design_Button_Active2 := Design_Form2; Design_Button_Active2_lock := false;

    RGBtoHSLRange(Design_Button_Active1, H, S, L); Design_Button_Active1_lock := false;
      if (L+30 > HSLRange) then L := HSLRange-30;
    Design_Button_Leuchtend1 := HSLRangeToRGB(H, S, L+30); Design_Button_Leuchtend1_lock := false;
    RGBtoHSLRange(Design_Button_Active2, H, S, L); Design_Button_Active2_lock := false;
      if (L+30 > HSLRange) then L := HSLRange-30;
    Design_Button_Leuchtend2 := HSLRangeToRGB(H, S, L+30); Design_Button_Leuchtend2_lock := false;

  Design_Button_Inactive1 := clWhite; Design_Button_Inactive1_lock := true;
  Design_Button_Inactive2 := clSilver; Design_Button_Inactive2_lock := true;

  Design_Caption1 := clRed; Design_Caption1_lock := false;
  Design_Caption2 := clMaroon; Design_Caption2_lock := false;
  Design_Caption_Active1 := clRed; Design_Caption_Active1_lock := false;
  Design_Caption_Active2 := clRed; Design_Caption_Active2_lock := false;

  Design_Menu1 := clWhite; Design_Menu1_lock := true;
  Design_Menu2 := $00D8E9EC; Design_Menu2_lock := true;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  //INIDatei: TIniFile;
  Hgt: integer;
begin
  // Design Init
  Standardfarben();
  DrawDesign();

  OpenDlg.InitialDir := GetFolder(SD_PERSONAL);
  // Lese INI-Datei...
  { INIDatei := TIniFile.Create(DateiName);
  INIDatei.ReadString('Properties', 'User', '?');
  INIDatei.Free; }
  ChangeButtonState(true, OpenBtn);
  ChangeButtonState(false, CloseBtn);
  ChangeButtonState(true, OptionBtn);
  ChangeButtonState(false, EncBtn);
  ChangeButtonState(false, DecBtn);
  ChangeButtonState(false, HelpBtn);
  ChangeButtonState(true, ExitBtn);
  ChangeButtonState(false, InfoBtn);

  // Titelleiste á la Windows
  //Hgt := GetSystemMetrics(SM_CYCAPTION);
  Hgt := 25;
  Capt.Height := Hgt;
  ExBtn.Height := Hgt;
  ExBtn.Width := Hgt;
  ExBtn.Left := Capt.Width - ExBtn.Width;
  CaptLabel.Top := Capt.Height div 2 - CaptLabel.Height div 2;
end;

procedure TMainForm.CaptMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  perform(WM_SysCommand, $F012, 0);
end;

procedure TMainForm.ExBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption1;
    TElPanel(Sender).GradientStartColor := Design_Caption2;
  end;
end;

procedure TMainForm.BtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TElPanel(Sender).BevelOuter := bvLowered;
    MBDown := true;
  end;
end;

procedure TMainForm.BtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and MBDown then
  begin
    TElPanel(Sender).BevelOuter := bvRaised;
    MBDown := false;
    // Standardfarbe
    TElPanel(Sender).GradientEndColor := Design_Button_Active1;
    TElPanel(Sender).GradientStartColor := Design_Button_Active2;
    // Ausfürhung
    ClickHandler(Sender);
  end;
end;

procedure TMainForm.ClickHandler(Sender: TObject);
begin;
  if (Sender = OpenBtn) then
  begin
    if OpenDlg.Execute then
    begin
      if CloseBtn.Enabled then dc4_CloseFile;
      dc4_OpenFile(OpenDlg.FileName);
    end;
  end
  else if (Sender = DecBtn) then
  begin
    if PwdEdit.Text = '' then
    begin
      MessageForm.Message.caption := 'Sie müssen ein Passwort zum Verschlüsseln /'+#13#10+'Entschlüsseln eingeben!';
      PwdEdit.SetFocus;
      BlinkVar := 0;
      BlinkTimer.Enabled := true;
      MessageForm.showmodal;
    end
    else
      showmessage('Entschlüsselung...');
  end
  else if (Sender = ExitBtn) or (Sender = ExBtn) then
  begin
    if CloseBtn.Enabled then
      dc4_CloseFile;
    close;
  end
  else if (Sender = HelpBtn) then
  begin
    // Nichts da
  end
  else if (Sender = OptionBtn) then
  begin
    OptionsForm.showmodal;
  end
  else if (Sender = EncBtn) then
  begin
    if PwdEdit.Text = '' then
    begin
      dc4_Showmessage('Sie müssen ein Passwort zum Verschlüsseln /'+#13#10+'Entschlüsseln eingeben!');
      PwdEdit.SetFocus;
      BlinkVar := 0;
      BlinkTimer.Enabled := true;
    end
    else
      RepeatForm.showmodal;
  end
  else if (Sender = InfoBtn) then
  begin
    InfoForm.showmodal;
  end
  else if (Sender = CloseBtn) then
  begin
    dc4_CloseFile;
  end;
end;

procedure TMainForm.MouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Active1;
    TElPanel(Sender).GradientStartColor := Design_Button_Active2;
  end;
end;

procedure TMainForm.DesignInit;
var
  H, S, L: integer;
begin
  Standardfarben;
  RGBtoHSLRange(Design_Form1, H, S, L);
  TrackBar1.Max := ((HSLRange-1) - H);
  TrackBar1.Min := -H;
  TrackBar1.Position := TrackBar1.Min + H;
  TrackBar2.Position := S;
  TrackBar3.Position := L;
end;

procedure TMainForm.TrackBar3Change(Sender: TObject);
var
  H, S, L, W1, W2, W3: integer;
  tmp: real;
begin
    Standardfarben;

    W1 := TrackBar1.Position;
    W2 := TrackBar2.Position;
    W3 := TrackBar3.Position;

    if not Design_Form1_lock then begin
    RGBtoHSLRange(Design_Form1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Form1 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Form2_lock then begin
    RGBtoHSLRange(Design_Form2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Form2 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Button_Active1_lock then begin
    RGBtoHSLRange(Design_Button_Active1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Button_Active1 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Button_Active2_lock then begin
    RGBtoHSLRange(Design_Button_Active2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Button_Active2 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Button_Inactive1_lock then begin
    RGBtoHSLRange(Design_Button_Inactive1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Button_Inactive1 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Button_Inactive2_lock then begin
    RGBtoHSLRange(Design_Button_Inactive2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Button_Inactive2 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Button_Leuchtend1_lock then begin
    RGBtoHSLRange(Design_Button_Leuchtend1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Button_Leuchtend1 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Button_Leuchtend2_lock then begin
    RGBtoHSLRange(Design_Button_Leuchtend2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Button_Leuchtend2 := HSLRangeToRGB(H, S, L);
    end;

    // Neu
    if not Design_Caption1_lock then begin
    RGBtoHSLRange(Design_Caption1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    if TrackBar3.Position = 0 then
      tmp := 0
    else
      tmp := TrackBar3.Max / TrackBar3.Position;
    L := round((tmp/100)*L);
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Caption1 := HSLRangeToRGB(H, S, L);
    end;

    // Neu
    if not Design_Caption2_lock then begin
    RGBtoHSLRange(Design_Caption2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := S + W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    if TrackBar3.Position = 0 then
      tmp := 0
    else
      tmp := TrackBar3.Max / TrackBar3.Position;
    L := round((tmp/100)*L);
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Caption2 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Caption_Active1_lock then begin
    RGBtoHSLRange(Design_Caption_Active1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Caption_Active1 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Caption_Active2_lock then begin
    RGBtoHSLRange(Design_Caption_Active2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Caption_Active2 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Menu1_lock then begin
    RGBtoHSLRange(Design_Menu1, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Menu1 := HSLRangeToRGB(H, S, L);
    end;

    if not Design_Menu2_lock then begin
    RGBtoHSLRange(Design_Menu2, H, S, L);
    H := H + W1;
    while (H > HSLRange-1) do H := H - (HSLRange-1);
    while (H < 0)          do H := H + (HSLRange-1);
    S := W2;
    if (S > HSLRange) then S := HSLRange;
    if (S < 0) then S := 0;
    L := W3;
    if (L > HSLRange) then L := HSLRange;
    if (L < 0) then L := 0;
    Design_Menu2 := HSLRangeToRGB(H, S, L);
    end;

    DrawDesign;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  DesignInit;
end;

procedure TMainForm.MouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Leuchtend1;
    TElPanel(Sender).GradientStartColor := Design_Button_Leuchtend2;
  end;
end;

procedure TMainForm.ExBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption_Active1;
    TElPanel(Sender).GradientStartColor := Design_Caption_Active2;
  end;
end;

end.

