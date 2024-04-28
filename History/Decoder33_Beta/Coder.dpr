program Coder;

{$Description '(De)Coder 3.3'}

uses
  Forms,
  Dialogs,
  Windows,
  Graphics,
  CoderUnit1 in 'CoderUnit1.pas' {MainForm},
  CoderUnit2 in 'CoderUnit2.pas' {PasswordDlg},
  CoderUnit3 in 'CoderUnit3.pas' {DlgForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(De)Coder 3.3';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPasswordDlg, PasswordDlg);
  Application.CreateForm(TDlgForm, DlgForm);
  Application.Run;
end.

