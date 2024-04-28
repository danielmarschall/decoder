program Coder;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Config in 'Config.pas' {ConfigForm};

{$R Coder.res}

begin
  Application.Initialize;
  Application.Title := '(De)Coder 4.0';
  Application.ShowMainForm := false; // wichtig, damit fenster bei befehlszeile nicht aufflackert
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConfigForm, ConfigForm);
  Application.Run;
  halt(mainform.errorlevel);
end.
