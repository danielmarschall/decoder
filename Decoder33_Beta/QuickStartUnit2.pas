unit QuickStartUnit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ElXPThemedControl, ElBtnCtl, ElPopBtn,
  ElCLabel, ElLabel, ElPanel;

type
  TInfoForm = class(TForm)
    OKBtn: TElPopupButton;
    ElPanel1: TElPanel;
    InfoLbl5: TElLabel;
    InfoLbl4: TElLabel;
    InfoLbl2: TElLabel;
    InfoLbl1: TElLabel;
    InfoLbl3: TElLabel;
    InfoImg: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  end;

var
  InfoForm: TInfoForm;

implementation

uses QuickStartUnit1;

{$R *.DFM}

procedure TInfoForm.FormShow(Sender: TObject);
begin
  MainForm.TrayIcon.enabled := false;
end;

procedure TInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.TrayIcon.enabled := true;
end;

procedure TInfoForm.OKBtnClick(Sender: TObject);
begin
  OKBtn.Font.color := clblack;
  InfoForm.close;
end;

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  if fileexists(mainform.verz+'Bilder\QuickStarter.bmp') then InfoImg.Picture.LoadFromFile(mainform.verz+'Bilder\QuickStarter.bmp');
end;

procedure TInfoForm.OKBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  OKBtn.Font.color := clnavy;
end;

procedure TInfoForm.BackgroundMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  OKBtn.Font.color := clblack;
end;

end.
