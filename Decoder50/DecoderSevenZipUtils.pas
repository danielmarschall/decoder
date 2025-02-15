unit DecoderSevenZipUtils;

interface

uses
  System.SysUtils, DecoderFuncs{$IFNDEF Console}, Fmx.Forms{$ENDIF};

procedure SevenZipFolder(const AFolderName, AArchFile: string; AOnProgress: TDcProgressEvent=nil);
procedure SevenZipExtract(const AArchFile, AFolder: string; AOnProgress: TDcProgressEvent=nil);

implementation

{$IFDEF MsWindows}
{$WARN UNIT_PLATFORM OFF}
uses
  SevenZip;
{$WARN UNIT_PLATFORM ON}
{$ENDIF}

const
  E_ABORT = HRESULT($80004004); // taken from Winapi.Windows.pas

type
  TSevenZipProgressContext = packed record
    DecProgress: TDcProgressEvent;
    Max: Int64;
    Task: string;
    StartSent: boolean;
    FinishedSent: boolean
  end;
  PSevenZipProgressContext = ^TSevenZipProgressContext;

function SevenZipProgress(sender: Pointer; total: boolean; value: int64): HRESULT; stdcall;
var
  state: TDcProgressState;
  docall: boolean;
begin
  if total then
  begin
    PSevenZipProgressContext(sender)^.Max := value;
  end
  else if Assigned(PSevenZipProgressContext(sender)^.DecProgress) then
  begin
    docall := true;
    if value = 0 then
    begin
      state := TDcProgressState.Started;
      PSevenZipProgressContext(sender)^.FinishedSent := false;
    end
    else if value = PSevenZipProgressContext(sender)^.Max then
    begin
      state := TDcProgressState.Finished;
    end
    else
    begin
      state := TDcProgressState.Processing;
    end;
    if state = TDcProgressState.Started then
    begin
      if PSevenZipProgressContext(sender)^.StartSent then
        state := TDcProgressState.Processing
      else
        PSevenZipProgressContext(sender)^.StartSent := true;
    end;
    if state = TDcProgressState.Finished then
    begin
      if PSevenZipProgressContext(sender)^.FinishedSent then
        docall := false // state := TDcProgressState.Processing
      else
        PSevenZipProgressContext(sender)^.FinishedSent := true;
    end;
    if docall then
      PSevenZipProgressContext(sender)^.DecProgress(PSevenZipProgressContext(sender)^.Max, value, PSevenZipProgressContext(sender)^.Task, state);
  end;
  {$IFDEF Console}
  Result := S_OK;
  {$ELSE}
  if Assigned(Application) and Application.Terminated then
    Result := E_ABORT
  else
    Result := S_OK;
  {$ENDIF}
end;

function SevenZipGetDll: string;
resourcestring
  SDllFileNotFound = 'File %s not found. Therefore, cannot compress or uncompress folders.';
begin
  // Extract 7z.dll from the 32 an 64 bit installer binaries
  // Do not take the 7za.dll ones from the extra archive
  {$IFDEF Win64}
  result := '7z.64.dll';
  {$ELSE}
  result := '7z.32.dll';
  {$ENDIF}
  if not FileExists(Result) then
    raise Exception.CreateFmt(SDllFileNotFound, [result]);
end;

procedure SevenZipFolder(const AFolderName, AArchFile: string; AOnProgress: TDcProgressEvent=nil);
{$IFDEF MsWindows}
var
  Arch: I7zOutArchive;
  ProgressCtx: TSevenZipProgressContext;
resourcestring
  SPackFolderTask = '7zip Pack folder';
  SFileExtMissingForFolderPack = 'File extension missing for SevenZipFolder()';
begin
  FillChar(ProgressCtx, Sizeof(ProgressCtx), 0);
  ProgressCtx.Task := SPackFolderTask;
  ProgressCtx.DecProgress := AOnProgress;
  if AArchFile.EndsWith('.7z', true) then
    Arch := CreateOutArchive(CLSID_CFormat7z, SevenZipGetDll)
  else if AArchFile.EndsWith('.zip', true) then
    Arch := CreateOutArchive(CLSID_CFormatZip, SevenZipGetDll)
  else
    raise Exception.Create(SFileExtMissingForFolderPack);
  Arch.AddFiles(AFolderName, '', '*', true);
  SetCompressionLevel(Arch, 5);
  SevenZipSetCompressionMethod(Arch, m7BZip2);
  Arch.SetProgressCallback(@ProgressCtx, SevenZipProgress);
  Arch.SaveToFile(AArchFile);
{$ELSE}
begin
  raise Exception.Create('7zip-Packen/Entpacken ist auf diesem System nicht möglich');
{$ENDIF}
end;

procedure SevenZipExtract(const AArchFile, AFolder: string; AOnProgress: TDcProgressEvent=nil);
{$IFDEF MsWindows}
var
  Arch: I7zInArchive;
  ProgressCtx: TSevenZipProgressContext;
resourcestring
  SUnpackFolderTask = '7zip Unpack folder';
  SFileExtMissingForFolderUnpack = 'File extension missing for SevenZipExtract()';
begin
  FillChar(ProgressCtx, Sizeof(ProgressCtx), 0);
  ProgressCtx.Task := SUnpackFolderTask;
  ProgressCtx.DecProgress := AOnProgress;
  if AArchFile.EndsWith('.7z', true) then
    Arch := CreateInArchive(CLSID_CFormat7z, SevenZipGetDll)
  else if AArchFile.EndsWith('.zip', true) then
    Arch := CreateInArchive(CLSID_CFormatZip, SevenZipGetDll)
  else
    raise Exception.Create(SFileExtMissingForFolderUnpack);
  Arch.SetProgressCallback(@ProgressCtx, SevenZipProgress);
  Arch.OpenFile(AArchFile);
  Arch.ExtractTo(AFolder);
{$ELSE}
begin
  raise Exception.Create('7zip-Packen/Entpacken ist auf diesem System nicht möglich');
{$ENDIF}
end;

end.
