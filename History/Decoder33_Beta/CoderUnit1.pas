unit CoderUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ShellAPI, JPEG, Gauges, Buttons, CoolGauge,
  SRWave, DropSource, DropTarget, unit_DialogsEx, ShlObj,
  ElXPThemedControl, ElBtnCtl, ElPopBtn, ElACtrls, Mask, ElMaskEdit,
  ElCLabel, ElLabel, ElEdits, ElPanel;

type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    BgPnl: TElPanel;
    CopyrightLbl1: TLabel;
    CopyrightLbl2: TLabel;
    StatusLbl1: TLabel;
    StatusLbl2: TLabel;
    CopyrightBvl: TBevel;
    TitelLbl: TElLabel;
    ProgressPnl: TPanel;
    ProgressGge: TGauge;
    OpenBtn: TElPopupButton;
    ExitBtn: TElPopupButton;
    CloseBtn: TElPopupButton;
    CryptBtn: TElPopupButton;
    WaitTmr2: TTimer;
    BlinkTmr: TTimer;
    DragDrop: TDropFileTarget;
    WaitTmr1: TTimer;
    OpenDlg: TOpenDialogEx;
    SaveDlg: TSaveDialogEx;
    KeyPnl: TElPanel;
    KeyLbl: TLabel;
    KeyEdt: TElEdit;
    FileNameEdt: TEdit;
    procedure ExitBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure CryptBtnClick(Sender: TObject);
    procedure StatusBarClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WaitTmr2Timer(Sender: TObject);
    procedure BlinkTmrTimer(Sender: TObject);
    procedure OpenBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CloseBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CryptBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ExitBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure DragDropDrop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure WaitTmr1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    verz: string;
    OpenedFile: string;
    procedure OpenFile(filename: string);
  end;

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

var
  MainForm: TMainForm;

implementation

uses CoderUnit2, CoderUnit3;

{$R *.DFM}

var
  mem: TMemoryStream;
  a: char;
  j: integer;
  Blinked: integer;
  FileProtection: textfile;

function GetFolder(FolderID: TSystemDirectory): string;
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

procedure TMainForm.OpenFile(filename: string);
begin
  if not fileexists(filename) then
  begin
    //MessageDLG('Datei nicht gefunden!', mtInformation, [mbOK], 0);
    DlgForm.DialogLbl.caption := 'Datei nicht gefunden!';
    DlgForm.showmodal;
    exit;
  end;
  assignfile(FileProtection, filename);
  reset(FileProtection);
  FileNameEdt.text := ExtractFileName(FileName);
  fileNameEdt.enabled := true;
  CryptBtn.Enabled := true;
  ProgressGge.showhint := false;
  ProgressGge.enabled := false;
  KeyEdt.Enabled:=true;
  KeyEdt.ShowHint := true;
  KeyLbl.Enabled:=true;
  CloseBtn.enabled := true;
  SaveDlg.filename := FileName;
  CloseBtn.showhint := true;
  CryptBtn.showhint := true;
  OpenedFile := filename;
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  ExitBtn.font.color := clWindowText;
  MainForm.close;
end;

procedure TMainForm.OpenBtnClick(Sender: TObject);
begin
  OpenBtn.font.color := clWindowText;
  if OpenDlg.Execute then OpenFile(OpenDlg.filename);
end;

procedure TMainForm.CryptBtnClick(Sender: TObject);
begin
  CryptBtn.font.color := clWindowText;
  if KeyEdt.Text = '' then
  begin
    //MessageDLG('Sie müssen ein Passwort zum Verschlüsseln / Entschlüsseln eingeben!', mtInformation, [mbOK], 0);
    DlgForm.DialogLbl.caption := 'Sie müssen ein Passwort zum' + #13#10 + 'Verschlüsseln / Entschlüsseln eingeben!';
    DlgForm.showmodal;
    BlinkTmr.enabled := true;
  end
  else
    PasswordDlg.showmodal;
end;

procedure TMainForm.StatusBarClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.d-m-home.de/', '', '', 1);
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  CloseBtn.font.color := clWindowText;
  closefile(FileProtection);
  ProgressGge.progress := 0;
  FilenameEdt.text := 'Keine Datei geöffnet';
  FilenameEdt.enabled := false;
  CryptBtn.Enabled := false;
  KeyEdt.Text := '';
  KeyEdt.Enabled := false;
  KeyEdt.ShowHint := false;
  KeyLbl.Enabled := false;
  ProgressGge.showhint := false;
  ProgressGge.enabled := false;
  CloseBtn.enabled := false;
  CloseBtn.showhint := false;
  CryptBtn.showhint := false;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  verz := extractfilepath(paramstr(0));
  MainForm.caption := application.title;
  TitelLbl.caption := application.title;
  TitelLbl.left := ((MainForm.ClientWidth-(TitelLbl.width)) div 2);
  StatusLbl2.left := ((MainForm.ClientWidth-(StatusLbl2.width)) div 2);
  CopyrightBvl.left := ((MainForm.ClientWidth-(CopyrightBvl.width)) div 2);
  ProgressPnl.left := ((MainForm.ClientWidth-(ProgressPnl.width)) div 2);
  KeyPnl.left := ((MainForm.ClientWidth-(KeyPnl.width)) div 2);
  DragDrop.register(mainform);
  SaveDlg.InitialDir := GetFolder(SD_PERSONAL);
  OpenDlg.InitialDir := GetFolder(SD_PERSONAL);
end;

procedure TMainForm.WaitTmr2Timer(Sender: TObject);
var
  i: integer;
  temp: string;
begin
  WaitTmr2.enabled := false;
  mem := TMemoryStream.Create;
  mem.LoadFromfile(OpenedFile);
  ProgressGge.MaxValue := mem.size;
  for i := 0 to mem.Size - 1 do
  begin
    mem.Position := i;
    mem.Read(a, 1);
    inc(j);
    if j > length(KeyEdt.text) then j := 1;
    temp := copy(KeyEdt.text, j, 1);
    a := CHR(byte(a) xor byte(temp[1]));
    mem.Position := i;
    mem.Write(a, 1);
    ProgressGge.Progress := ProgressGge.Progress + 1;
  end;
  j := 0;
  if SaveDlg.Filename = OpenDlg.Filename then closefile(fileprotection);
  mem.SaveToFile(SaveDlg.FileName);
  if SaveDlg.Filename = OpenDlg.Filename then openfile(OpenDlg.Filename);
  mem.Free;
  FilenameEdt.enabled := true;
  CryptBtn.Enabled := true;
  KeyEdt.Enabled := true;
  KeyLbl.Enabled := true;
  CloseBtn.enabled := true;
  OpenBtn.enabled := true;
  TitelLbl.Cursor := crDefault;
  mainform.Cursor := crDefault;
  exitbtn.Cursor := crDefault;
  closebtn.Cursor := crDefault;
  openbtn.Cursor := crDefault;
  cryptbtn.Cursor := crDefault;
  keyedt.Cursor := crDefault;
  filenameedt.Cursor := crDefault;
  keypnl.Cursor := crDefault;
  keylbl.Cursor := crDefault;
  progressgge.Cursor := crDefault;
  copyrightbvl.Cursor := crDefault;
  copyrightlbl1.Cursor := crDefault;
  copyrightlbl2.Cursor := crDefault;
  statusbar.Cursor := crDefault;
  StatusLbl2.caption := 'Programm bereit!';
  StatusLbl1.font.color := clGreen;
  StatusLbl2.font.color := clGreen;
  //MessageDLG('Datei wurde erfolgreich Verschlüsselt / Entschlüsselt!', mtInformation, [mbOK], 0);
  DlgForm.DialogLbl.caption := 'Datei wurde erfolgreich' + #13#10 + 'Verschlüsselt / Entschlüsselt!';
  DlgForm.showmodal;
  ProgressGge.Progress := ProgressGge.MinValue;
  ProgressGge.showhint := false;
  ProgressGge.enabled := false;
end;

procedure TMainForm.BlinkTmrTimer(Sender: TObject);
begin
  inc(Blinked);
  if Blinked = 7 then
  begin
    Blinked := 0;
    BlinkTmr.enabled := false;
    exit;
  end
  else
  begin
    if PasswordDlg.visible then
    begin
      if (Blinked = 1) or (Blinked = 3) or (Blinked = 5) then
        PasswordDlg.KeyLbl.font.color := clMaroon
      else
        PasswordDlg.KeyLbl.font.color := clWindowText;
    end
    else
    begin
      if (Blinked = 1) or (Blinked = 3) or (Blinked = 5) then
        KeyLbl.font.color := clMaroon
      else
        KeyLbl.font.color := clWindowText;
    end;
  end;
end;

procedure TMainForm.OpenBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  OpenBtn.font.color := clnavy;
  CloseBtn.font.color := clWindowText;
  CryptBtn.font.color := clWindowText;
  ExitBtn.font.color := clWindowText;
end;

procedure TMainForm.BackgroundMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  OpenBtn.font.color := clWindowText;
  CloseBtn.font.color := clWindowText;
  CryptBtn.font.color := clWindowText;
  ExitBtn.font.color := clWindowText;
end;

procedure TMainForm.CloseBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CloseBtn.font.color := clnavy;
  OpenBtn.font.color := clWindowText;
  CryptBtn.font.color := clWindowText;
  ExitBtn.font.color := clWindowText;
end;

procedure TMainForm.CryptBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CryptBtn.font.color := clnavy;
  CloseBtn.font.color := clWindowText;
  OpenBtn.font.color := clWindowText;
  ExitBtn.font.color := clWindowText;
end;

procedure TMainForm.ExitBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ExitBtn.font.color := clnavy;
  CryptBtn.font.color := clWindowText;
  CloseBtn.font.color := clWindowText;
  OpenBtn.font.color := clWindowText;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if ParamStr(1) <> '' then OpenFile(ParamStr(1));
end;

procedure TMainForm.DragDropDrop(Sender: TObject; ShiftState: TShiftState;
  Point: TPoint; var Effect: Integer);
begin
  if DragDrop.files.count>1 then
  begin
    //MessageDLG('Es kann nur eine Datei gleichzeitig geöffnet werden!', mtInformation, [mbOK], 0);
    DlgForm.DialogLbl.caption := 'Es kann nur eine Datei' + #13#10 + 'gleichzeitig geöffnet werden!';
    DlgForm.showmodal;
  end
  else openfile(DragDrop.files[0]);
end;

procedure TMainForm.WaitTmr1Timer(Sender: TObject);
begin
  WaitTmr1.enabled := false;
  if not SaveDlg.Execute then exit;
  CryptBtn.Enabled := false;
  KeyEdt.Enabled := false;
  KeyLbl.Enabled := false;
  ProgressGge.showhint := true;
  ProgressGge.enabled := true;
  CloseBtn.enabled := false;
  OpenBtn.enabled := false;
  TitelLbl.Cursor := crHourGlass;
  mainform.Cursor := crHourGlass;
  exitbtn.Cursor := crHourGlass;
  closebtn.Cursor := crHourGlass;
  openbtn.Cursor := crHourGlass;
  cryptbtn.Cursor := crHourGlass;
  keyedt.Cursor := crHourGlass;
  filenameedt.Cursor := crHourGlass;
  keypnl.Cursor := crHourGlass;
  keylbl.Cursor := crHourGlass;
  progressgge.Cursor := crHourGlass;
  copyrightbvl.Cursor := crHourGlass;
  copyrightlbl1.Cursor := crHourGlass;
  copyrightlbl2.Cursor := crHourGlass;
  statusbar.Cursor := crHourGlass;
  StatusLbl2.caption := 'Programm arbeitet...';
  StatusLbl1.font.color := clMaroon;
  StatusLbl2.font.color := clMaroon;
  WaitTmr2.enabled := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DragDrop.unregister;
end;

end.

