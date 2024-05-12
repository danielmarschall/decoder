unit CoderUnit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, SRWave, ElXPThemedControl, ElBtnCtl,
  ElPopBtn, ElCLabel, ElLabel;

type
  TDlgForm = class(TForm)
    DialogImg: TImage;
    OKBtn: TElPopupButton;
    DialogLbl: TElLabel;
    procedure OKBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OKBtnClick(Sender: TObject);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  DlgForm: TDlgForm;

implementation

uses CoderUnit1;

{$R *.DFM}

procedure TDlgForm.OKBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  OKBtn.font.color := clNavy;
end;

procedure TDlgForm.OKBtnClick(Sender: TObject);
begin
  OKBtn.font.color := clwindowText;
end;

procedure TDlgForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  OKBtn.font.color := clwindowText;
end;

procedure TDlgForm.FormShow(Sender: TObject);
begin
  Beep;
end;

procedure TDlgForm.FormCreate(Sender: TObject);
begin
  if fileexists(MainForm.verz+'Bilder\Info.bmp') then DialogImg.Picture.loadfromfile(MainForm.verz+'Bilder\Info.bmp');
end;

end.

