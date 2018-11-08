library SecureMoveExt;

uses
  ComServ,
  SecureMoveMain in 'SecureMoveMain.pas' {DataModuleDragDropHandler: TDataModule};

{$R SecureMoveExt.res}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
