// Abgeändert für (De)Coder 4.1

unit OneInst;

interface

uses
  Windows, Messages, SysUtils, DCConst;

var
  SecondInstMsgId: UINT = 0;

function ParamBlobToStr(lpData: Pointer): string;
function ParamStrToBlob(out cbData: DWORD): Pointer;

implementation

const
  TimeoutWaitForReply = 5000;

var
  UniqueName: array [0..MAX_PATH] of Char = 'ViaThinkSoft-DeCoder4100'#0;
  MutexHandle: THandle = 0;

function ParamBlobToStr(lpData: Pointer): string;
var
  pStr: PChar;
begin
  Result := '';
  pStr := lpData;
  while pStr[0] <> #0 do
  begin
    if pStr <> '/newinstance' then
      Result := Result + string(pStr) + #13#10;
    pStr := @pStr[lstrlen(pStr) + 1];
  end;
end;

function ParamStrToBlob(out cbData: DWORD): Pointer;
var
  Loop: Integer;
  pStr: PChar;
begin
  cbData := Length(ParamStr(1)) + 3;
  for Loop := 2 to ParamCount do
    cbData := cbData + DWORD(Length(ParamStr(Loop)) + 1);
  Result := GetMemory(cbData);
  ZeroMemory(Result, cbData);
  pStr := Result;
  for Loop := 1 to ParamCount do
  begin
    lstrcpy(pStr, PChar(ParamStr(Loop)));
    pStr := @pStr[lstrlen(pStr) + 1];
  end;
end;

procedure HandleSecondInstance;
var
  Run: DWORD;
  Now: DWORD;
  Msg: TMsg;
  Wnd: HWND;
  Dat: TCopyDataStruct;
begin
  SendMessage(HWND_BROADCAST, SecondInstMsgId, GetCurrentThreadId, 0);

  Wnd := 0;
  Run := GetTickCount;
  while True do
  begin
    if PeekMessage(Msg, 0, SecondInstMsgId, SecondInstMsgId, PM_NOREMOVE) then
    begin
      GetMessage(Msg, 0, SecondInstMsgId, SecondInstMsgId);
      if Msg.message = SecondInstMsgId then
      begin
        Wnd := Msg.wParam;
        Break;
      end;
    end;
    Now := GetTickCount;
    if Now < Run then
      Run := Now;
    if Now - Run > TimeoutWaitForReply then
      Break;
  end;

  if (Wnd <> 0) and IsWindow(Wnd) then
  begin
    Dat.dwData := SecondInstMsgId;
    Dat.lpData := ParamStrToBlob(Dat.cbData);
    SendMessage(Wnd, WM_COPYDATA, 0, LPARAM(@Dat));
    FreeMemory(Dat.lpData);
  end;
end;

procedure CheckForSecondInstance;
var
  Loop: Integer;
begin
  for Loop := lstrlen(UniqueName) to MAX_PATH - 1 do
  begin
    MutexHandle := CreateMutex(nil, False, UniqueName);
    if (MutexHandle = 0) and (GetLastError = INVALID_HANDLE_VALUE) then
      lstrcat(UniqueName, '_')
    else
      Break;
  end;

  case GetLastError of
    0:
      begin

      end;
    ERROR_ALREADY_EXISTS:
      begin
        try
          HandleSecondInstance;
        finally
          Halt(10);
        end;
      end;
  else

  end;
end;

initialization

  SecondInstMsgId := RegisterWindowMessage(UniqueName);

  if (paramstr_firstposition('/newinstance') = -1) and
     (paramstr_firstposition('/c') = -1) and
     (paramstr_firstposition('/x') = -1) and
     (paramstr_firstposition('/e') = -1) and
     (paramstr_firstposition('/?') = -1) and
     (paramstr_firstposition('/clean') = -1) then
    CheckForSecondInstance;

finalization

  if MutexHandle <> 0 then
  begin
    ReleaseMutex(MutexHandle);
    MutexHandle := 0;
  end;

end.
