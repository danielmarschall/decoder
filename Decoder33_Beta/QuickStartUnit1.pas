unit QuickStartUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TaskBarIcon, TaskIcon, ShellAPI, StdCtrls, Buttons, ExtCtrls, SRWave,
  Menus, antTaskbarIcon, ElBaseComp, ElTray, ElXPThemedControl, ElBtnCtl,
  ElPopBtn, ElCLabel, ElLabel;

type
  TMainForm = class(TForm)
    DialogImg: TImage;
    TrayPopup: TPopupMenu;
    DeCoderstarten1: TMenuItem;
    N1: TMenuItem;
    QuickStarterbeenden1: TMenuItem;
    QuickStarterinfo: TMenuItem;
    TrayIcon: TantTaskbarIcon;
    NoBtn: TElPopupButton;
    YesBtn: TElPopupButton;
    DialogLbl: TElLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NoBtnClick(Sender: TObject);
    procedure YesBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QuickStarterbeenden1Click(Sender: TObject);
    procedure QuickStarterinfo2Click(Sender: TObject);
    procedure YesBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure NoBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormHide(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  public
    verz: string;
  end;

var
  MainForm: TMainForm;

implementation

uses
  QuickStartUnit2;

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
var
  Sem: THandle;
begin
  Sem := CreateSemaphore(nil, 0, 1, 'PROGRAM_NAME');
  if (Sem <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    CloseHandle(Sem);
    ShowMessage('Der QuickStarter wurde schon gestartet.');
    Halt;
  end;
  verz := extractfilepath(paramstr(0));
  if not fileexists(verz+'Coder.exe') then Decoderstarten1.enabled := false;
  if fileexists(verz+'Bilder\Info.bmp') then DialogImg.Picture.LoadFromFile(verz+'Bilder\Info.bmp');
  if fileexists(verz+'Icons\TrayIcon.ico') then TrayIcon.Icon.LoadFromFile(verz+'Icons\TrayIcon.ico');
  DialogLbl.caption := 'Möchten Sie den QuickStarter von (De)Coder 3.3' + #13#10 + 'wirklich beenden?';
  TrayIcon.visible := true;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TrayIcon.visible := false;
end;

procedure TMainForm.NoBtnClick(Sender: TObject);
begin
  NoBtn.font.color := clwindowtext;
  MainForm.Hide;
end;

procedure TMainForm.YesBtnClick(Sender: TObject);
begin
  YesBtn.font.color := clwindowtext;
  MainForm.close;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Beep;
  MainForm.TrayIcon.enabled := false;
end;

procedure TMainForm.QuickStarterbeenden1Click(Sender: TObject);
begin
  MainForm.Show;
end;

procedure TMainForm.QuickStarterinfo2Click(Sender: TObject);
begin
  InfoForm.show;
end;

procedure TMainForm.YesBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  YesBtn.font.color := clnavy;
  NoBtn.font.color := clwindowtext;
end;

procedure TMainForm.NoBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  YesBtn.font.color := clwindowtext;
  NoBtn.font.color := clnavy;
end;

procedure TMainForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  YesBtn.font.color := clwindowtext;
  NoBtn.font.color := clwindowtext;
end;

procedure TMainForm.FormHide(Sender: TObject);
begin
  MainForm.TrayIcon.enabled := true;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  if Decoderstarten1.enabled then shellexecute(handle, 'open',
    pchar(verz+'Coder.exe'), '', '', 1);
end;

end.

