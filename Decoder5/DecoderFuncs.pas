unit DecoderFuncs;

interface

uses
  Windows, DECTypes, Classes, SysUtils, Math, Forms,





  dialogs;

procedure SecureDeleteFile(AFileName: string);
function IsCompressedFileType(AFileName: string): boolean;
function ShannonEntropy(const filename: string; OnProgressProc: TDECProgressEvent=nil): Extended;

implementation

uses
  DECUtil, DECRandom;

{$IFDEF Unicode}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeW';
{$ELSE}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeA';
{$ENDIF}

procedure SecureDeleteFile(AFileName: string);

  function GetClusterSize(Drive: String): integer;
  var
    SectorsPerCluster, BytesPerSector, dummy: Cardinal;
  begin
    SectorsPerCluster := 0;
    BytesPerSector := 0;
    GetDiskFreeSpace(PChar(Drive), SectorsPerCluster, BytesPerSector, dummy, dummy);

    Result := SectorsPerCluster * BytesPerSector;
  end;

  function RandFileName(len: integer): string;
  const
    str = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  var
    i: UInt32;
  begin
    Result := '';
    repeat
      i := RandomLong mod UInt32(Length(str));
      Result := Result + str[Low(str) + i];
    until Length(Result) = len;
  end;

  function RelToAbs(const RelPath, BasePath: string): string;
  var
    Dst: array[0..MAX_PATH-1] of char;
  begin
    PathCanonicalize(@Dst[0], PChar(IncludeTrailingPathDelimiter(BasePath) + RelPath));
    result := Dst;
  end;

var
  drive: string;
  fs: TFileStream;
  AFileName2: string;
  ClusterSize: integer;
begin
  ClusterSize := 32000; // max available in Windows format dialog
  try
    drive := ExtractFileDrive(RelToAbs(AFileName, GetCurrentDir));
    if drive <> '' then
      ClusterSize := GetClusterSize(drive);
  except
    // If we can't get it, that's not a problem
  end;

  fs := TFileStream.Create(AFileName, fmOpenReadWrite);
  try
    try
      // Try to grow the file to avoid guessing the file size by looking at the rest of the block
      // Also erases information at the rest of the block
      fs.Size := Ceil(fs.Size / ClusterSize) * ClusterSize;
    except
      // We might be out of disk space
    end;

    // secure wipe contents
    ProtectStream(fs, fs.Size-fs.Position);

    // avoid that undelete tools see file size
    fs.Size := 0;
  finally
    FreeAndNil(fs);
  end;

  // Avoid that undelete tools see file name...
  AFileName2 := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName))+RandFileName(Length(ExtractFileName(AFileName)));
  if RenameFile(AFileName, AFileName2) then AFileName := AFileName2;

  // ... or the size of the name
  AFileName2 := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName))+'_';
  if RenameFile(AFileName, AFileName2) then AFileName := AFileName2;

  // now delete the file
  DeleteFile(AFileName);
end;

function IsCompressedFileType(AFileName: string): boolean;
begin
  result :=
    SameText(ExtractFileExt(AFileName), '.zip') or
    SameText(ExtractFileExt(AFileName), '.7z') or
    SameText(ExtractFileExt(AFileName), '.rar') or
    SameText(ExtractFileExt(AFileName), '.gz') or
    SameText(ExtractFileExt(AFileName), '.xz') or
    SameText(ExtractFileExt(AFileName), '.mp3') or
    SameText(ExtractFileExt(AFileName), '.mp4') or
    SameText(ExtractFileExt(AFileName), '.png') or
    SameText(ExtractFileExt(AFileName), '.gif') or
    SameText(ExtractFileExt(AFileName), '.jpg') or
    SameText(ExtractFileExt(AFileName), '.jpeg') or
    SameText(ExtractFileExt(AFileName), '.docx') or
    SameText(ExtractFileExt(AFileName), '.xlsx') or
    SameText(ExtractFileExt(AFileName), '.pptx');
end;

function ShannonEntropy(const filename: string; OnProgressProc: TDECProgressEvent=nil): Extended;
var
  fs: TFileStream;

  procedure Read(var Value; Size: Integer);
  begin
    fs.ReadBuffer(Value, Size);
  end;

  function ReadRaw(leng: integer): RawByteString;
  begin
    SetLength(Result, leng);
    Read(Result[Low(Result)], Length(Result));
  end;

var
  p: Extended;
  i: int64;
  counts: array[0..255] of int64;
  filesize: int64;
  rbs: RawByteString;
  ProgrSize, ProgrPos: Int64;
const
  chunksize = 4096; // bigger = faster
begin
  for i := Low(counts) to High(counts) do
    Counts[i] := 0;

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    filesize := fs.Size;
    ProgrPos := 0;
    ProgrSize := Filesize div chunksize;
    if Assigned(OnProgressProc) then OnProgressProc(ProgrSize, ProgrPos, Started);
    while fs.Position < fs.Size do
    begin
      rbs := ReadRaw(Min(chunksize,fs.Size-fs.Position));
      for i := Low(rbs) to High(rbs) do
        Inc(counts[Ord(rbs[i])]);
      Inc(ProgrPos);
      if Assigned(OnProgressProc) then OnProgressProc(ProgrSize, ProgrPos, Processing);
      Application.ProcessMessages;
      if Application.Terminated then Abort;
    end;
    if Assigned(OnProgressProc) then OnProgressProc(ProgrSize, ProgrSize, Finished);

    // Shannon's entropy
    // https://stackoverflow.com/questions/990477/how-to-calculate-the-entropy-of-a-file
    result := 0;
    for i := Low(counts) to High(counts) do
    begin
      p := Counts[i] / filesize;
      if p > 0 then result := result - p*Log2(p)
    end;
  finally
    FreeAndNil(fs);
  end;
end;

end.
