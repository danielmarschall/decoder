library ShlErase;

uses
  Windows,
  ComServ,
  ShellEraseMain in 'ShellEraseMain.pas';

{$R ShlErase.res}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
