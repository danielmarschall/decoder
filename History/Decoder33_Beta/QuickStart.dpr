program QuickStart;

{$Description '(De)Coder 3.3 - QuickStarter'}

uses
  Forms,
  QuickStartUnit1 in 'QuickStartUnit1.pas' {MainForm},
  QuickStartUnit2 in 'QuickStartUnit2.pas' {InfoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.ShowMainForm := false;
  Application.Title := '(De)Coder 3.3 QuickStarter';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.
