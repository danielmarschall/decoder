unit CoderUnit2;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, Dialogs, ExtCtrls, ElBtnCtl, ElPopBtn, ElXPThemedControl,
  ElEdits, ElCLabel, ElLabel;

type
  TPasswordDlg = class(TForm)
    KeyLbl: TElLabel;
    KeyEdt: TElEdit;
    CancelBtn: TElPopupButton;
    OKBtn: TElPopupButton;
    procedure OKBtnClick(Sender: TObject);
    procedure KeyEdtChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CancelBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CancelBtnClick(Sender: TObject);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  end;

var
  PasswordDlg: TPasswordDlg;

implementation

uses CoderUnit1;

{$R *.DFM}

procedure TPasswordDlg.OKBtnClick(Sender: TObject);
begin
  OKBtn.font.color := clWindowText;
  MainForm.WaitTmr1.enabled := true;
end;

procedure TPasswordDlg.KeyEdtChange(Sender: TObject);
begin
  if KeyEdt.Text <> MainForm.KeyEdt.Text then
    OKBtn.enabled := false
  else
    OKBtn.enabled := true;
end;

procedure TPasswordDlg.FormShow(Sender: TObject);
begin
  KeyEdt.Text := '';
  KeyEdtChange(Sender);
end;

procedure TPasswordDlg.OKBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  OKBtn.font.color := clNavy;
  CancelBtn.font.color := clWindowText;
end;

procedure TPasswordDlg.CancelBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CancelBtn.font.color := clNavy;
  OKBtn.font.color := clWindowText;
end;

procedure TPasswordDlg.CancelBtnClick(Sender: TObject);
begin
  CancelBtn.font.color := clWindowText;
end;

procedure TPasswordDlg.BackgroundMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  OKBtn.font.color := clWindowText;
  CancelBtn.font.color := clWindowText;
end;

end.
 
