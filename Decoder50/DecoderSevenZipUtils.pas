unit DecoderSevenZipUtils;

interface

uses
  SysUtils, Windows, DecoderFuncs{$IFNDEF Console}, Fmx.Forms{$ENDIF};

procedure SevenZipFolder(const AFolderName, AArchFile: string; AOnProgress: TDcProgressEvent=nil);
procedure SevenZipExtract(const AArchFile, AFolder: string; AOnProgress: TDcProgressEvent=nil);

implementation

uses
  SevenZip;

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
begin
  // Extract 7z.dll from the 32 an 64 bit installer binaries
  // Do not take the 7za.dll ones from the extra archive
  {$IFDEF Win64}
  result := '7z.64.dll';
  {$ELSE}
  result := '7z.32.dll';
  {$ENDIF}
  if not FileExists(Result) then
    raise Exception.CreateFmt('File %s not found. Therefore, cannot compress or uncompress folders.', [result]);
end;

procedure SevenZipFolder(const AFolderName, AArchFile: string; AOnProgress: TDcProgressEvent=nil);
var
  Arch: I7zOutArchive;
  ProgressCtx: TSevenZipProgressContext;
begin
  ZeroMemory(@ProgressCtx, Sizeof(ProgressCtx));
  ProgressCtx.Task := '7zip Pack folder';
  ProgressCtx.DecProgress := AOnProgress;
  if AArchFile.EndsWith('.7z', true) then
    Arch := CreateOutArchive(CLSID_CFormat7z, SevenZipGetDll)
  else if AArchFile.EndsWith('.zip', true) then
    Arch := CreateOutArchive(CLSID_CFormatZip, SevenZipGetDll)
  else
    raise Exception.Create('File extension missing for SevenZipFolder()');
  Arch.AddFiles(AFolderName, '', '*', true);
  SetCompressionLevel(Arch, 5);
  SevenZipSetCompressionMethod(Arch, m7BZip2);
  Arch.SetProgressCallback(@ProgressCtx, SevenZipProgress);
  Arch.SaveToFile(AArchFile);
end;

procedure SevenZipExtract(const AArchFile, AFolder: string; AOnProgress: TDcProgressEvent=nil);
var
  Arch: I7zInArchive;
  ProgressCtx: TSevenZipProgressContext;
begin
  ZeroMemory(@ProgressCtx, Sizeof(ProgressCtx));
  ProgressCtx.Task := '7zip Unpack folder';
  ProgressCtx.DecProgress := AOnProgress;
  if AArchFile.EndsWith('.7z', true) then
    Arch := CreateInArchive(CLSID_CFormat7z, SevenZipGetDll)
  else if AArchFile.EndsWith('.zip', true) then
    Arch := CreateInArchive(CLSID_CFormatZip, SevenZipGetDll)
  else
    raise Exception.Create('File extension missing for SevenZipExtract()');
  Arch.SetProgressCallback(@ProgressCtx, SevenZipProgress);
  Arch.OpenFile(AArchFile);
  Arch.ExtractTo(AFolder);
end;

end.
