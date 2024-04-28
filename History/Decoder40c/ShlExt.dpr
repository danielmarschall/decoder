library ShellExt;

uses
  Windows,
  ComServ,
  Shellmain in 'Shellmain.pas';

{$R ShlExt.res}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
