unit ShellMain;

interface

uses
  ComServ, SysUtils, ShellAPI, Windows, Registry, ActiveX, ComObj, ShlObj, Graphics, classes;

const
  GUID_TDFKontextMenuShellExt: TGUID = '{C30DC498-38EA-4DED-8AD4-E302CE094892}';

type
  TDFKontextMenuShellExt = class(TComObject, IShellExtInit, IContextMenu)
    protected
      function IShellExtInit.Initialize = SEInitialize;
      function SEInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY): HResult; stdcall;
      function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uflags: UINT): HResult; stdcall;
      function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
      function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

implementation

var
  hBmp: TBitmap;
  handle: hwnd;
  vsl: tstringlist;

type
  TDFKontextMenuShellExtFactory = class(TComObjectFactory)
    public
      procedure UpdateRegistry(Register: boolean); override;
  end;

function TDFKontextMenuShellExt.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HResult;
begin
  try

  if(idCmd = 0) then
  begin
    if(uType = GCS_HELPTEXT) then
      StrCopy(pszName, 'Datei mit (De)Coder öffnen');

    Result := NOERROR;
  end
  else
    Result := E_INVALIDARG;

  except
    Result := E_UNEXPECTED;
  end;
end;

function TDFKontextMenuShellExt.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
var
  path: string;
  i, res: integer;
begin
  Result := E_FAIL;
  if (HiWord(Integer(lpici.lpVerb)) <> 0) then
    Exit;

  if not (LoWord(lpici.lpVerb) in [0, 1, 2]) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if vsl.Count > 1 then
    res := Messagebox(handle, 'Sie haben mehrere Dateien oder Ordner ausgewählt. Für jedes Element wird sich ein neues Fenster von (De)Coder öffnen. Möchten Sie den Vorgang wirklich fortsetzen?', 'Öffnen mehrerer Dateien', MB_ICONQUESTION or MB_YESNO)
  else
    res := ID_YES;

  if res = ID_YES then
  begin
    with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey('SOFTWARE\ViaThinkSoft\(De)Coder 4.0', False);
        path := ReadString('Path');
      finally
        free;
      end;

    if not fileexists(path+'\Coder.exe') then
      Messagebox(handle, 'Konnte (De)Coder nicht finden. Eine Neuinstallation der Anwendung könnte das Problem beheben.', 'Fehler', MB_ICONERROR or MB_OK)
    else
    begin
      for i := 0 to vsl.count-1 do
        ShellExecute(handle, 'open', PChar(path+'\Coder.exe'), PChar('"'+vsl.Strings[i]+'"'), pchar(path), SW_NORMAL);
    end;

  end;

  Result := NOERROR;
end;

function TDFKontextMenuShellExt.QueryContextMenu(Menu: HMENU; indexMenu,
  idCmdFirst, idCmdLast, uflags: UINT): HResult;
begin
  Result := 0;
 
  if ((uFlags and $0000000F) = CMF_NORMAL) or
      ((uFlags and CMF_EXPLORE) <> 0) or
      ((uFlags and CMF_VERBSONLY <> 0)) then
  begin
    InsertMenu(Menu, indexMenu, MF_STRING or MF_BYPOSITION, idCmdFirst, 'Mit (De)Coder öffnen');
 
    if hBmp.Handle <> 0 then
      SetMenuItemBitmaps(Menu, indexMenu, MF_BYPOSITION, hBmp.Handle, hBmp.Handle);
 
    Result := 1;
  end;
end;

function TDFKontextMenuShellExt.SEInitialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
  FFileName: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  if (lpdobj = nil) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  with FormatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
  end;

  Result := lpdobj.GetData(FormatEtc, StgMedium);
  if Failed(Result) then
    Exit;

  vsl := tstringlist.Create;

  vSL.Clear;
  for i := 0 to DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0) - 1 do
  begin
    DragQueryFile(StgMedium.hGlobal, i, FFileName, SizeOf(FFileName));
    vSl.Add(FFileName);
  end;
 
  ReleaseStgMedium(StgMedium);
  Result := NOERROR;
end;

procedure TDFKontextMenuShellExtFactory.UpdateRegistry(Register: boolean);
var
  ClassID: string;
begin
  if Register then
  begin
    inherited UpdateRegistry(Register);

    ClassID := GUIDToString(GUID_TDFKontextMenuShellExt);

    CreateRegKey('Folder\shellex', '', '');
    CreateRegKey('Folder\shellex\ContextMenuHandlers', '', '');
    CreateRegKey('Folder\shellex\ContextMenuHandlers\(De)Coder', '', ClassID);

    CreateRegKey('*\shellex', '', '');
    CreateRegKey('*\shellex\ContextMenuHandlers', '', '');
    CreateRegKey('*\shellex\ContextMenuHandlers\(De)Coder', '', ClassID);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved', True);
          WriteString(ClassID, '(De)Coder');
          CloseKey();
        finally
          Free;
        end;
  end
  else
  begin
    DeleteRegKey('Folder\shellex\ContextMenuHandlers\(De)Coder');
    DeleteRegKey('Folder\shellex\ContextMenuHandlers');
    DeleteRegKey('Folder\shellex');

    DeleteRegKey('*\shellex\ContextMenuHandlers\(De)Coder');
    DeleteRegKey('*\shellex\ContextMenuHandlers');
    DeleteRegKey('*\shellex');

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved', True);
          DeleteValue(ClassID);
          CloseKey();
        finally
          Free;
        end;

    inherited UpdateRegistry(Register);
  end;
end;

initialization
  TDFKontextMenuShellExtFactory.Create(ComServer, TDFKontextMenuShellExt, GUID_TDFKontextMenuShellExt,
    '', '(De)Coder', ciMultiInstance, tmApartment);
  hBmp := TBitmap.Create;
  hBmp.LoadFromResourceName(hInstance, 'KONTEXTICON');
finalization
  hBmp.Free;
end.
