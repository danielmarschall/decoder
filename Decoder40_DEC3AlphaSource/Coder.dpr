program Coder;

{%ToDo 'Coder.todo'}

{$Description '(De)Coder 4.0'}

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Options in 'Options.pas' {OptionsForm},
  RepeatPassword in 'RepeatPassword.pas' {RepeatForm},
  Message in 'Message.pas' {MessageForm},
  FileInfo in 'FileInfo.pas' {InfoForm},
  HSLUnit in 'HSLUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(De)Coder 4.0';
  Application.ShowMainForm := false;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TRepeatForm, RepeatForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.

