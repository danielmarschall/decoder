unit SecureMoveMain;

interface

uses
  DragDropHandler, SysUtils, IniFiles, Menus, ShellAPI, Windows, Classes,
  ComServ, ComObj, Registry, Forms, ShlObj, ComCtrls;

{$ifndef VER13_PLUS}
type
  TDataModule = TForm;
{$endif}

type
  TDataModuleDragDropHandler = class(TDataModule, IUnknown, IShellExtInit, IContextMenu)
    PopupMenu1: TPopupMenu;
    MenuEncrypt: TMenuItem;
    MenuLine1: TMenuItem;
    procedure MenuEncryptClick(Sender: TObject);
    procedure DragDropHandler1Popup(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FFiles: TStrings;
    DragDropHandler1: TDragDropHandler;
    procedure MoveFile(const Filename: string);
  public
    property ContextMenuHandler: TDragDropHandler read DragDropHandler1
      implements IShellExtInit, IContextMenu;
  end;

implementation

{$R *.DFM}

type
  TDragDropHandlerFactoryAbs = class(TDragDropHandlerFactory)
  public
    procedure UpdateRegistry(Register: boolean); override;
  end;

const
  CLSID_DragDropHandler: TGUID = '{54069E5A-C471-4B68-835C-FC845E64040B}';

var
  sTitle: string;
  sDescription: string;
  langini: TIniFile;
  path: string;
  mydocuments: string;

resourcestring
  sFileClass = 'Folder';
  sFileExtension = '';
  sClassName = 'SecureMove';

function ExecuteWithExitCode(filename, params, dir: string): integer;
var
  proc_info: TProcessInformation;
  startinfo: TStartupInfo;
  ExitCode: longword;
begin
  FillChar(proc_info, sizeof(TProcessInformation), 0);
  FillChar(startinfo, sizeof(TStartupInfo), 0);
  startinfo.cb := sizeof(TStartupInfo);

  if CreateProcess(nil, pchar(Format('"%s" %s', [filename, TrimRight(pchar(params))])), nil,
      nil, false, NORMAL_PRIORITY_CLASS, nil, pchar(path),
       startinfo, proc_info) <> False then
  begin
    // WaitForSingleObject(proc_info.hProcess, INFINITE);
    while WaitForSingleObject(proc_info.hProcess, 0) = WAIT_TIMEOUT do
    begin
      // ProcessMessages(0);
      Sleep(50);
    end;
    GetExitCodeProcess(proc_info.hProcess, ExitCode);
    CloseHandle(proc_info.hThread);
    CloseHandle(proc_info.hProcess);
    result := ExitCode;
  end
  else
  begin
    result := -1;
  end;
end;

procedure TDragDropHandlerFactoryAbs.UpdateRegistry(Register: boolean);
var
  ClassID: string;
begin
  ClassID := GUIDToString(CLSID_DragDropHandler);

  if Register then
  begin
    inherited UpdateRegistry(Register);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved', True);
          WriteString(ClassID, '(De)Coder-SecureMove');
          CloseKey;
        finally
          Free;
        end;
  end
  else
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved', True);
          DeleteValue(ClassID);
          CloseKey;
        finally
          Free;
        end;

    inherited UpdateRegistry(Register);
  end;
end;

// http://www.delphipraxis.net/topic1451_dateioperationen+mit+shfileoperation.html
function DoFileWork(aOperation: FILEOP_FLAGS; aFrom, aTo: AnsiString;
    Flags: FILEOP_FLAGS): Integer; 
var
  FromPath, ToPath: AnsiString; 
  SHFileOpStruct: TSHFileOpStruct;
begin 
  FromPath := aFrom + #0#0;
  ToPath := aTo + #0#0;
  with SHFileOpStruct do
  begin 
    Wnd := 0;
    wFunc := aOperation;
    pFrom := PAnsiChar(FromPath);
    if ToPath <> '' then 
    begin 
      pTo := PAnsiChar(ToPath) 
    end else begin // target available 
      pTo := nil;
    end; // target not available
    fFlags := Flags; 
  end; // structure 
  Result := SHFileOperationA(SHFileOpStruct);
end;

procedure TDataModuleDragDropHandler.DataModuleCreate(Sender: TObject);
begin
  FFiles := TStringList.Create;
  DragDropHandler1 := TDragDropHandler.Create(self);
  DragDropHandler1.OnPopup := DragDropHandler1Popup;
  DragDropHandler1.ContextMenu := popupmenu1;
  popupmenu1.Items.Items[0].Hint := sDescription;
  popupmenu1.Items.Items[0].Caption := sTitle;
end;

procedure TDataModuleDragDropHandler.DataModuleDestroy(Sender: TObject);
begin
  FFiles.Free;
  DragDropHandler1.free;
end;

procedure TDataModuleDragDropHandler.MoveFile(const Filename: string);
var
  delform: TForm;
  ani: TAnimate;
  cd: integer;
begin
  // undo ist leider nicht möglich, weil es sich um keine move-aktion handelt
  cd := DoFileWork(FO_COPY, Filename, DragDropHandler1.Folder+'\'+extractfilename(Filename), 0);
  if cd = 0 then
  begin
    delform := TForm.Create(nil);
    try
      delform.caption := langini.ReadString('SecureMove', 'wait', '?');
      delform.BorderStyle := bsSingle;
      delform.BorderIcons := [biSystemMenu];
      delform.Position := poDesktopCenter;

      ani := TAnimate.Create(delform);
      try
        ani.Parent := delform;
        ani.CommonAVI := aviDeleteFile;
        ani.Active := true;
        ani.Top := 8;
        ani.Left := 8;
        ani.Visible := true;

        delform.ClientWidth := ani.Width + 16;
        delform.ClientHeight := ani.Height + 16;
        delform.visible := true;

        // shellexecute(handle, 'open', pchar(path+'Coder.exe'), pchar('"'+Filename+'" /e'), pchar(path), SW_NORMAL);
        if ExecuteWithExitCode(path+'Coder.exe', '"'+Filename+'" /e', path) = 8 then
        begin
          Messagebox(handle, pchar(Format(
          langini.ReadString('SecureMove', 'error_del', '?'),
          [Filename]
          )), pchar(langini.ReadString('ShlErase', 'error', '?')), MB_ICONERROR or MB_OK);
        end;
      finally
        ani.free;
      end;
    finally
      delform.Free;
    end;
  end
  else
  begin
    // fehlercodes für abbruch beim überschreiben: 138, 7
    Messagebox(handle, pchar(Format(
    langini.ReadString('SecureMove', 'error_copy', '?'),
    [Filename, DragDropHandler1.Folder+'\', cd]
    )), pchar(langini.ReadString('ShlErase', 'error', '?')), MB_ICONERROR or MB_OK);
  end;
end;

procedure TDataModuleDragDropHandler.MenuEncryptClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to FFiles.Count-1 do
    MoveFile(FFiles[i]);
end;

procedure TDataModuleDragDropHandler.DragDropHandler1Popup(Sender: TObject);

  procedure ClearItem(Item: TMenuItem);
  begin
  {$ifdef VER13_PLUS}
    Item.Clear;
  {$else}
    while (Item.Count > 0) do
      Item[0].Free;
  {$endif}
  end;

var
  i: integer;
begin
  FFiles.Assign(DragDropHandler1.Files);

  for i := 0 to FFiles.Count-1 do
  begin
    // AUSNAHMEN - KEIN VERSCHIEBEN MÖGLICH
    if  // nicht-existierendes (z.b. arbeitsplatz)
        (not fileexists(ffiles.Strings[i]) and not directoryexists(ffiles.Strings[i])) or
        // "eigene dateien"-ordner
       (uppercase(ffiles.Strings[i]) = uppercase(mydocuments)) or
       // innerhalb des selben orderns verschieben
       (DragDropHandler1.Folder+'\' = extractfilepath(ffiles.Strings[i])) or
       // verzeichnis in sich selbst kopieren
       (ffiles.Strings[i] = DragDropHandler1.Folder) or
       // unbekannt
       (FFiles.Count = 0) then
    begin
      ClearItem(PopupMenu1.Items);
    end;
  end;
end;

initialization
  path := '';
  mydocuments := '';

  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\(De)Coder_is1');
      path := ReadString('InstallLocation');
      CloseKey;
      if path = '' then
      begin
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\(De)Coder_is1');
        path := ReadString('InstallLocation');
        CloseKey;
      end;

      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders');
      mydocuments := ReadString('Personal');
      CloseKey;
    finally
      free;
    end;

  if copy(mydocuments, length(mydocuments), 1) = '\' then
    mydocuments := copy(mydocuments, 0, length(mydocuments)-1);

  langini := TIniFile.Create(path+'Language.ini');

  sTitle := langini.ReadString('SecureMove', 'title', '?');
  sDescription := langini.ReadString('SecureMove', 'description', '?');

  TDragDropHandlerFactoryAbs.Create(ComServer, TDataModuleDragDropHandler,
    CLSID_DragDropHandler, sClassName, sDescription, sFileClass,
    sFileExtension, ciMultiInstance);

finalization
  langini.Free;
end.

