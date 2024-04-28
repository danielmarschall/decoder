unit RepeatPassword;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElACtrls, ExtCtrls, ElPanel, ElGroupBox, Cipher, Cipher1, ElCLabel, ElLabel,
  ElBtnCtl, ElPopBtn, ElXPThemedControl, ElEdits, StdCtrls, ElCheckCtl,
  Hash, FileCtrl;

type
  TRepeatForm = class(TForm)
    SaveDlg: TSaveDialog;
    Capt: TElPanel;
    CaptLabel: TElLabel;
    ExBtn: TElPanel;
    MainPanel: TElPanel;
    UebLabel: TElLabel;
    CnclBtn: TElPanel;
    BackBtn: TElPanel;
    NextBtn: TElPanel;
    Box1: TElPanel;
    B1Image: TImage;
    B1Label: TElLabel;
    B1PwdEdit: TElEdit;
    B1Status: TElLabel;
    Box2: TElPanel;
    B2Label: TElLabel;
    B2Image: TImage;
    B2CipherCombo: TElAdvancedComboBox;
    B2CipherLbl: TElLabel;
    B2CipherStandard: TElLabel;
    B2ModeLbl: TElLabel;
    B2ModeCombo: TElAdvancedComboBox;
    B2ModeStandard: TElLabel;
    B2HashLabel: TElLabel;
    B2HashCombo: TElAdvancedComboBox;
    B2HashStandard: TElLabel;
    B2CipherExplain: TElLabel;
    B2ModeExplain: TElLabel;
    Box3: TElPanel;
    B3CheckBox1: TElCheckBox;
    B3Image: TImage;
    B3CheckBox2: TElCheckBox;
    Box4: TElPanel;
    B4Image: TImage;
    B4Label: TElLabel;
    B4Filename: TLabel;
    B2HashExplain: TElLabel;
    ChBtn: TElPanel;
    procedure B1PwdEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure B2CipherComboChange(Sender: TObject);
    procedure B2ModeComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ElPanel2Click(Sender: TObject);
    procedure ExBtnMouseLeave(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaptMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure B2HashComboChange(Sender: TObject);
    procedure ExBtnMouseEnter(Sender: TObject);
    procedure BtnMouseEnter(Sender: TObject);
    procedure BtnMouseLeave(Sender: TObject);
  public
    aktion: integer;
    procedure ActFunc();
    procedure DrawDesign();
    procedure ClickHandler(Sender: TObject);
  end;

var
  RepeatForm: TRepeatForm;

implementation

uses Main;

{$R *.DFM}

procedure TRepeatForm.ActFunc();
begin
  Box1.Visible := aktion = 1;
  Box2.Visible := aktion = 2;
  Box3.Visible := aktion = 3;
  Box4.Visible := aktion = 3;
  if aktion = 4 then
  begin
    close;
    MainForm.WaitTmr1.enabled := true;
  end;
  if aktion <> 3 then
    NextBtn.Caption := 'Weiter >>'
  else
    NextBtn.Caption := 'Fertigstellen';
  MainForm.ChangeButtonState(aktion <> 1, BackBtn);
  UebLabel.Caption := 'Schritt ' + inttostr(aktion) + ' von 3 - ';
  if aktion = 1 then UebLabel.Caption := UebLabel.Caption + 'Kennwortbestätigung';
  if aktion = 2 then UebLabel.Caption := UebLabel.Caption + 'Konfiguration';
  if aktion = 3 then UebLabel.Caption := UebLabel.Caption + 'Fertigstellung';
end;

procedure TRepeatForm.B1PwdEditChange(Sender: TObject);
begin
  MainForm.ChangeButtonState(B1PwdEdit.Text = MainForm.PwdEdit.Text, NextBtn);
  if B1PwdEdit.Text = MainForm.PwdEdit.Text then
  begin
    B1Status.Font.Color := $00008000;
    B1Status.Caption := 'Passwort OK!';
  end
  else
  begin
    B1Status.Font.Color := $00000080;
    B1Status.Caption := 'Keine Übereinstimmung...';
  end;
end;

procedure TRepeatForm.FormShow(Sender: TObject);
var
  Hgt: integer;
begin
  DrawDesign();

  aktion := 1;
  ActFunc();
  MainForm.ChangeButtonState(false, NextBtn);
  B1PwdEdit.Text := '';
  B1PwdEdit.SetFocus;
  B1Status.Font.Color := $00000080;
  B1Status.Caption := 'Keine Übereinstimmung...';
  B4Filename.Caption := MinimizeName(MainForm.GetFolder(SD_PERSONAL) + '\' + copy(ExtractFileName(MainForm.OpenDlg.FileName), 0, length(ExtractFileName(MainForm.OpenDlg.FileName))-length(ExtractFileExt(MainForm.OpenDlg.FileName)))+ '.dc4', Box4.Canvas, B4Filename.Width);
  MainForm.ChangeButtonState(true, CnclBtn);
  MainForm.ChangeButtonState(false, BackBtn);
  MainForm.ChangeButtonState(false, NextBtn);
  MainForm.ChangeButtonState(true, ChBtn);

  // Titelleiste á la Windows
  //Hgt := GetSystemMetrics(SM_CYCAPTION);
  Hgt := MainForm.Capt.Height;
  Capt.Height := Hgt;
  ExBtn.Height := Hgt;
  ExBtn.Width := Hgt;
  ExBtn.Left := Capt.Width - ExBtn.Width;
  CaptLabel.Top := Capt.Height div 2 - CaptLabel.Height div 2;
end;

procedure TRepeatForm.B2CipherComboChange(Sender: TObject);
begin
  // B2CipherCombo.ItemIndex := B2CipherCombo.ItemIndex;
  MainForm.CipherManager1.Algorithm := B2CipherCombo.Text;
  B2CipherExplain.Caption := MainForm.CipherManager1.Description;
  try
    if not MainForm.CipherManager1.CipherClass.SelfTest then
      MessageBox(Handle, 'Self Test failed', 'Cipher Self Test', mb_Ok);
  except
    Application.HandleException(Self);
  end;
end;

procedure TRepeatForm.B2ModeComboChange(Sender: TObject);
const
  sMode : array[TCipherMode] of String =
    ('Cipher Text Stealing', 'Cipher Block Chaining', 'Cipher Feedback',
     'Output Feedback', 'Electronic Code Book', 'CBC MAC', 'CTS MAC', 'CFB MAC');
begin
  MainForm.CipherManager1.Mode := TCipherMode(B2ModeCombo.ItemIndex);
  B2ModeExplain.Caption := sMode[MainForm.CipherManager1.Mode];
end;

procedure TRepeatForm.FormCreate(Sender: TObject);
begin
  HashNames(B2HashCombo.Items);
  B2HashCombo.ItemIndex := 1;
  B2HashComboChange(nil);
  CipherNames(B2CipherCombo.Items);
  B2CipherCombo.ItemIndex := 1;
  B2CipherComboChange(nil);
  B2ModeCombo.ItemIndex := 0;
  B2ModeComboChange(nil);
  B2CipherStandard.caption := 'Standard: ' + B2CipherCombo.Text;
  B2ModeStandard.caption := 'Standard: ' + B2ModeCombo.Text;
  B2HashStandard.caption := 'Standard: ' + B2HashCombo.Text;
end;

procedure TRepeatForm.ElPanel2Click(Sender: TObject);
begin
  close;
end;

procedure TRepeatForm.ExBtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption1;
    TElPanel(Sender).GradientStartColor := Design_Caption2;
  end;
end;

procedure TRepeatForm.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TElPanel(Sender).BevelOuter := bvLowered;
    MBDown := true;
  end;
end;

procedure TRepeatForm.MouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TRepeatForm.CaptMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  perform(WM_SysCommand, $F012, 0);
end;

procedure TRepeatForm.B2HashComboChange(Sender: TObject);
begin
  // B2HashCombo.ItemIndex := B2HashCombo.ItemIndex;
  MainForm.HashManager1.Algorithm := B2HashCombo.Text;
  B2HashExplain.Caption := MainForm.HashManager1.Description;
  try
    if not MainForm.HashManager1.HashClass.SelfTest then
      MessageBox(Handle, 'Self Test failed', 'Hash Self Test', mb_Ok);
  except
    Application.HandleException(Self);
  end;
end;

procedure TRepeatForm.ExBtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Caption_Active1;
    TElPanel(Sender).GradientStartColor := Design_Caption_Active2;
  end;
end;

procedure TRepeatForm.BtnMouseEnter(Sender: TObject);
begin
  // Aufleuchten
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Leuchtend1;
    TElPanel(Sender).GradientStartColor := Design_Button_Leuchtend2;
  end;
end;

procedure TRepeatForm.BtnMouseLeave(Sender: TObject);
begin
  TElPanel(Sender).BevelOuter := bvRaised;
  MBDown := false;
  if (TElPanel(Sender).Tag = 1) then
  begin
    TElPanel(Sender).GradientEndColor := Design_Button_Active1;
    TElPanel(Sender).GradientStartColor := Design_Button_Active2;
  end;
end;

procedure TRepeatForm.ClickHandler(Sender: TObject);
begin;
  if (Sender = NextBtn) then
  begin
    inc(aktion);
    ActFunc();
  end else if (Sender = BackBtn) then
  begin
    dec(aktion);
    ActFunc();
  end else if (Sender = CnclBtn) then
  begin
    close;
  end else if (Sender = chBtn) then
  begin
    if SaveDlg.Execute then
      B4Filename.Caption := MinimizeName(SaveDlg.FileName, Box4.Canvas, B4Filename.Width);
  end;
end;

procedure TRepeatForm.DrawDesign();
begin
  MainPanel.GradientEndColor := Design_Form1;
  MainPanel.GradientStartColor := Design_Form2;
  Capt.GradientEndColor := Design_Caption1;
  Capt.GradientStartColor := Design_Caption2;
  ExBtn.GradientEndColor := Design_Caption1;
  ExBtn.GradientStartColor := Design_Caption2;
  Box1.GradientStartColor := Design_Menu2;
  Box1.GradientEndColor := Design_Menu1;
  Box2.GradientStartColor := Design_Menu2;
  Box2.GradientEndColor := Design_Menu1;
  Box3.GradientStartColor := Design_Menu2;
  Box3.GradientEndColor := Design_Menu1;
  Box4.GradientStartColor := Design_Menu2;
  Box4.GradientEndColor := Design_Menu1;

  if (ChBtn.Tag = 1) then MainForm.ChangeButtonState(true, ChBtn) else MainForm.ChangeButtonState(false, ChBtn);
  if (CnclBtn.Tag = 1) then MainForm.ChangeButtonState(true, CnclBtn) else MainForm.ChangeButtonState(false, CnclBtn);
  if (NextBtn.Tag = 1) then MainForm.ChangeButtonState(true, NextBtn) else MainForm.ChangeButtonState(false, NextBtn);
  if (BackBtn.Tag = 1) then MainForm.ChangeButtonState(true, BackBtn) else MainForm.ChangeButtonState(false, BackBtn);
end;

end.

