program Coder;

uses
  Forms,
  Warten in 'Warten.pas' {WartenForm},
  Main in 'Main.pas' {MainForm},
  Config in 'Config.pas' {ConfigForm},
  About in 'About.pas' {AboutForm},
  Elemente in 'Elemente.pas' {ElementeForm};

{$R Coder.res}

begin
  Application.Initialize;
  Application.Title := '(De)Coder 4.1';
  Application.ShowMainForm := false; // wichtig, damit fenster bei befehlszeile nicht aufflackert
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TWartenForm, WartenForm);
  Application.CreateForm(TConfigForm, ConfigForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TElementeForm, ElementeForm);
  MainForm.Start;
  Application.Run;
  //halt(mainform.errorlevel);
end.
