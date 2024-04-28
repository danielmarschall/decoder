unit FileInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ElXPThemedControl, ElBtnCtl, ElPopBtn, StdCtrls, ElCLabel,
  ElLabel, ExtCtrls, ElPanel;

type
  TInfoForm = class(TForm)
    Capt: TElPanel;
    CaptLabel: TElLabel;
    ExBtn: TElPanel;
    MainPanel: TElPanel;
    OKBtn: TElPanel;
    InfoLbl0: TElLabel;
    InfoLbl1: TElLabel;
    InfoLbl1b: TElLabel;
    InfoLbl2: TElLabel;
    InfoLbl2b: TElLabel;
    InfoLbl3: TElLabel;
    InfoLbl3b: TElLabel;
    InfoLbl4: TElLabel;
    InfoLbl4b: TElLabel;
    InfoLbl5: TElLabel;
    InfoLbl5b: TElLabel;
    InfoImg: TImage;
    ElLabel1: TElLabel;
    procedure CaptMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExBtnMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnMouseEnter(Sender: TObject);
    procedure OKBtnMouseLeave(Sender: TObject);
    procedure ExBtnMouseEnter(Sender: TObject);
  public
    procedure DrawDesign();
    procedure ClickHandler(Sender: TObject);
  end;

var
  InfoForm: TInfoForm;

implementation

uses Main;

{$R *.dfm}

procedure TInfoForm.ClickHandler(Sender: TObject);
begin;
  if (Sender = OkBtn) or (Sender = ExBtn) then
    close;
end;

procedure TInfoForm.CaptMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  perform(WM_SysCommand, $F012, 0);
end;

procedure TInfoForm.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TElPanel(Sender).BevelOuter := bvLowered;
    MBDown := true;
  end;
end;

procedure TInfoForm.MouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TInfoForm.DrawDesign();
begin
  MainPanel.GradientEndColor := Design_Form1;
  MainPanel.GradientStartColor := Design_Form2;
  Capt.GradientEndColor := Design_Caption1;
  Capt.GradientStartColor := Design_Caption2;
  ExBtn.GradientEndColor := Design_Caption1;
  ExBtn.GradientStartColor := Design_Caption2;

  if (OkBtn.Tag = 1) then MainForm.ChangeButtonState(true, OkBtn) else MainForm.ChangeButtonState(false, OkBtn);
end;

procedure TInfoForm.ExBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption1;
    TElPanel(Sender).GradientStartColor := Design_Caption2;
  end;
end;

procedure TInfoForm.FormShow(Sender: TObject);
var
  Hgt: integer;
begin
  DrawDesign;

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

procedure TInfoForm.OKBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Leuchtend1;
    TElPanel(Sender).GradientStartColor := Design_Button_Leuchtend2;
  end;
end;

procedure TInfoForm.OKBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Active1;
    TElPanel(Sender).GradientStartColor := Design_Button_Active2;
  end;
end;

procedure TInfoForm.ExBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption_Active1;
    TElPanel(Sender).GradientStartColor := Design_Caption_Active2;
  end;
end;

end.
