unit Message;

interface

uses
  Windows, Messages, SysUtils, Forms, ElLabel, ElCLabel, StdCtrls, ElPopBtn,
  ElBtnCtl, ElXPThemedControl, Controls, Graphics, Classes, ExtCtrls,
  ElPanel;

type
  TMessageForm = class(TForm)
    Capt: TElPanel;
    CaptLabel: TElLabel;
    ExBtn: TElPanel;
    MainPanel: TElPanel;
    OKBtn: TElPanel;
    Message: TElLabel;
    MsgImg: TImage;
    procedure FormShow(Sender: TObject);
    procedure CaptMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExBtnMouseLeave(Sender: TObject);
    procedure OKBtnMouseLeave(Sender: TObject);
    procedure OKBtnMouseEnter(Sender: TObject);
    procedure ExBtnMouseEnter(Sender: TObject);
  public
    procedure DrawDesign();
    procedure ClickHandler(Sender: TObject);
  end;

var
  MessageForm: TMessageForm;

implementation

uses Main;

{$R *.DFM}

procedure TMessageForm.DrawDesign();
begin
  MainPanel.GradientEndColor := Design_Form1;
  MainPanel.GradientStartColor := Design_Form2;
  Capt.GradientEndColor := Design_Caption1;
  Capt.GradientStartColor := Design_Caption2;
  ExBtn.GradientEndColor := Design_Caption1;
  ExBtn.GradientStartColor := Design_Caption2;

  if (OkBtn.Tag = 1) then MainForm.ChangeButtonState(true, OkBtn) else MainForm.ChangeButtonState(false, OkBtn);
end;

procedure TMessageForm.FormShow(Sender: TObject);
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

  beep;
end;

procedure TMessageForm.CaptMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  perform(WM_SysCommand, $F012, 0);
end;

procedure TMessageForm.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TElPanel(Sender).BevelOuter := bvLowered;
    MBDown := true;
  end;
end;

procedure TMessageForm.MouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TMessageForm.ExBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption1;
    TElPanel(Sender).GradientStartColor := Design_Caption2;
  end;
end;

procedure TMessageForm.ClickHandler(Sender: TObject);
begin;
  if (Sender = OkBtn) or (Sender = ExBtn) then
    close;
end;

procedure TMessageForm.OKBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Active1;
    TElPanel(Sender).GradientStartColor := Design_Button_Active2;
  end;
end;

procedure TMessageForm.OKBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Leuchtend1;
    TElPanel(Sender).GradientStartColor := Design_Button_Leuchtend2;
  end;
end;

procedure TMessageForm.ExBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption_Active1;
    TElPanel(Sender).GradientStartColor := Design_Caption_Active2;
  end;
end;

end.
