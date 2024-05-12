library ShellExt;

uses
  Windows,
  ComServ,
  ShellExtMain in 'ShellExtMain.pas';

{$R ShlExt.res}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
