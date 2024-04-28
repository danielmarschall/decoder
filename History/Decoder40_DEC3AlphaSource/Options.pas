unit Options;

interface

uses
  Windows, Messages, Classes, Forms, ElPopBtn, Registry, ElXPThemedControl,
  ElEdits, Controls, StdCtrls, ElBtnCtl, ElCLabel, ElLabel, ExtCtrls,
  ElPanel, Graphics, ElSideBar;

type
  TOptionsForm = class(TForm)
    Capt: TElPanel;
    CaptLabel: TElLabel;
    ExBtn: TElPanel;
    MainPanel: TElPanel;
    UserLbl: TLabel;
    UserEdt: TElEdit;
    OKBtn: TElPanel;
    ElSideBar1: TElSideBar;
    procedure FormCreate(Sender: TObject);
    procedure ExBtnMouseLeave(Sender: TObject);
    procedure CaptMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure ExBtnMouseEnter(Sender: TObject);
    procedure ExBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseLeave(Sender: TObject);
    procedure BtnMouseEnter(Sender: TObject);
  public
    procedure ClickHandler(Sender: TObject);
    procedure DrawDesign();
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses Main;

{$R *.DFM}

procedure TOptionsForm.FormCreate(Sender: TObject);
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
      UserEdt.Text := ''; // Unbekanntes System: Kein Benutzername gefunden!
    end
    else
    begin
      UserEdt.Text := temp; // NT-System: Benutzername gefunden!
    end;
  end
  else
  begin
    UserEdt.Text := temp; // 9x-System: Benutzername gefunden!
  end;
end;

procedure TOptionsForm.ExBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption1;
    TElPanel(Sender).GradientStartColor := Design_Caption2;
  end;
end;

procedure TOptionsForm.CaptMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  perform(WM_SysCommand, $F012, 0);
end;

procedure TOptionsForm.BtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TElPanel(Sender).BevelOuter := bvLowered;
    MBDown := true;
  end;
end;

procedure TOptionsForm.BtnMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TOptionsForm.FormShow(Sender: TObject);
var
  Hgt: integer;
begin
  DrawDesign();

  MainForm.ChangeButtonState(true, OKBtn);

  // Titelleiste á la Windows
  //Hgt := GetSystemMetrics(SM_CYCAPTION);
  Hgt := MainForm.Capt.Height;
  Capt.Height := Hgt;
  ExBtn.Height := Hgt;
  ExBtn.Width := Hgt;
  ExBtn.Left := Capt.Width - ExBtn.Width;
  CaptLabel.Top := Capt.Height div 2 - CaptLabel.Height div 2;
end;

procedure TOptionsForm.ExBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption_Active1;
    TElPanel(Sender).GradientStartColor := Design_Caption_Active2;
  end;
end;

procedure TOptionsForm.ExBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TElPanel(Sender).BevelOuter := bvLowered;
    MBDown := true;
  end;
end;

procedure TOptionsForm.ClickHandler(Sender: TObject);
begin;
  if (Sender = OkBtn) or (Sender = ExBtn) then
    close;
end;

procedure TOptionsForm.ExBtnMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TOptionsForm.DrawDesign();
begin
  MainPanel.GradientEndColor := Design_Form1;
  MainPanel.GradientStartColor := Design_Form2;
  Capt.GradientEndColor := Design_Caption1;
  Capt.GradientStartColor := Design_Caption2;
  ExBtn.GradientEndColor := Design_Caption1;
  ExBtn.GradientStartColor := Design_Caption2;

  if (OkBtn.Tag = 1) then MainForm.ChangeButtonState(true, OkBtn) else MainForm.ChangeButtonState(false, OkBtn);
end;

procedure TOptionsForm.BtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Active1;
    TElPanel(Sender).GradientStartColor := Design_Button_Active2;
  end;
end;

procedure TOptionsForm.BtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Leuchtend1;
    TElPanel(Sender).GradientStartColor := Design_Button_Leuchtend2;
  end;
end;

end.

