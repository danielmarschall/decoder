unit DecoderFuncs;

interface

uses
  Windows, {$IFNDEF Console}Forms, {$ENDIF} DECTypes, Classes, SysUtils,
  Math, DECHashBase, DECHashAuthentication, IOUtils;

type
  TStreamHelper = class helper for TStream
  public
    procedure Read(var Value; Size: Integer);
    function ReadByte: Byte;
    function ReadLongBE: LongWord;
    function ReadInt32: Int32;
    function ReadInt64: Int64;
    function ReadRawByteString(len: integer): RawByteString;
    function ReadRawBytes(len: integer): TBytes;
    procedure Write(var Value; Size: Integer);
    procedure WriteByte(b: Byte);
    procedure WriteLongBE(lw: LongWord);
    procedure WriteInt32(i32: Int32);
    procedure WriteInt64(i64: Int64);
    procedure WriteRawByteString(rb: RawByteString);
    procedure WriteRawBytes(b: TBytes);
  end;

  // https://github.com/MHumm/DelphiEncryptionCompendium/issues/62
  TDECHashExtendedAuthentication = class helper for TDECHashAuthentication
    class function HMACFile(const Key: TBytes; const FileName: string;
      const OnProgress:TDECProgressEvent = nil): TBytes;
    class function HMACStream(const Key: TBytes; const Stream: TStream; Size: Int64;
      const OnProgress:TDECProgressEvent): TBytes;
  end;

procedure ZLib_Compress(InputFileName, OutputFileName: string; OnProgressProc: TDECProgressEvent=nil);
procedure Zlib_Decompress(InputFileName, OutputFileName: string; OnProgressProc: TDECProgressEvent=nil);
function SecureDeleteFile(AFileName: string): boolean;
function SecureDeleteFolder(ADirName: string): boolean;
function IsCompressedFileType(const AFileName: string): boolean;
function ShannonEntropy(const filename: string; OnProgressProc: TDECProgressEvent=nil): Extended;
function BytesToRawByteString(const Bytes: TBytes): RawByteString; inline;
function FileSizeHumanReadable(Bytes: Int64): string;
function GetBuildTimestamp(const ExeFile: string): TDateTime;
function GetOwnBuildTimestamp: TDateTime;

implementation

uses
  DECUtil, DECRandom, ZLib, DateUtils;

{$IFDEF Unicode}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeW';
{$ELSE}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeA';
{$ENDIF}

procedure ZLib_Compress(InputFileName, OutputFileName: string; OnProgressProc: TDECProgressEvent=nil);
var
  CompressInputStream: TFileStream;
  CompressOutputStream: TFileStream;
  CompressionStream: TCompressionStream;
  rbs: RawByteString;
const
  ChunkSize = $100000; // value from System.Classes
begin
  CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, 0, TDECProgressState.Started);
    CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
    try
      CompressionStream:=TCompressionStream.Create(clMax, CompressOutputStream);
      try
        if Assigned(OnProgressProc) then
        begin
          OnProgressProc(CompressInputStream.Size, CompressInputStream.Position, TDECProgressState.Processing);
          rbs := CompressInputStream.ReadRawByteString(Min(ChunkSize,CompressInputStream.Size-CompressInputStream.Position));
          CompressionStream.WriteRawByteString(rbs);
        end
        else
        begin
          CompressionStream.CopyFrom(CompressInputStream, CompressInputStream.Size);
        end;
      finally
        FreeAndNil(CompressionStream);
      end;
    finally
      FreeAndNil(CompressOutputStream);
    end;
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, CompressInputStream.Size, TDECProgressState.Finished);
  finally
    FreeAndNil(CompressInputStream);
  end;
end;

procedure Zlib_Decompress(InputFileName, OutputFileName: string; OnProgressProc: TDECProgressEvent=nil);
var
  CompressInputStream: TFileStream;
  CompressOutputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
  rbs: RawByteString;
const
  ChunkSize = $100000; // value from System.Classes
begin
  CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, 0, TDECProgressState.Started);
    CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
    try
      DecompressionStream := TDecompressionStream.Create(CompressInputStream);
      try
        if Assigned(OnProgressProc) then
        begin
          OnProgressProc(DecompressionStream.Size, DecompressionStream.Position, TDECProgressState.Processing);
          rbs := DecompressionStream.ReadRawByteString(Min(ChunkSize,DecompressionStream.Size-DecompressionStream.Position));
          CompressOutputStream.WriteRawByteString(rbs);
        end
        else
        begin
          CompressOutputStream.CopyFrom(DecompressionStream, DecompressionStream.Size);
        end;
      finally
        FreeAndNil(DecompressionStream);
      end;
    finally
      FreeAndNil(CompressOutputStream);
    end;
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, CompressInputStream.Size, TDECProgressState.Finished);
  finally
    FreeAndNil(CompressInputStream);
  end;
end;

function RandStringFileNameFriendly(len: integer): string;
const
  str = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
  i: UInt32;
begin
  Result := '';
  repeat
    i := RandomLong mod UInt32(Length(str));
    Result := Result + str[UInt32(Low(str)) + i];
  until Length(Result) = len;
end;

function SecureDeleteFile(AFileName: string): boolean;

  function GetClusterSize(Drive: String): integer;
  var
    SectorsPerCluster, BytesPerSector, dummy: Cardinal;
  begin
    SectorsPerCluster := 0;
    BytesPerSector := 0;
    GetDiskFreeSpace(PChar(Drive), SectorsPerCluster, BytesPerSector, dummy, dummy);

    Result := SectorsPerCluster * BytesPerSector;
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
  if not FileExists(AFileName) then Exit(False);

  {$IFDEF Console}
  WriteLn('Delete file: ' + AFileName);
  {$ENDIF}

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
  AFileName2 := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName))+RandStringFileNameFriendly(Length(ExtractFileName(AFileName)));
  if RenameFile(AFileName, AFileName2) then AFileName := AFileName2;

  // ... or the size of the name
  AFileName2 := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName))+'_';
  if RenameFile(AFileName, AFileName2) then AFileName := AFileName2;

  // now delete the file
  result := DeleteFile(AFileName);
end;

function SecureDeleteFolder(ADirName: string): boolean;
var
  F: TSearchRec;
  ADirName2: string;
begin
  if not DirectoryExists(ADirName) then Exit(False);
  result := true;

  {$IFDEF Console}
  WriteLn('START Delete folder: ' + ADirName);
  {$ENDIF}

  if FindFirst(IncludeTrailingPathDelimiter(ADirName) + '*', faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then
        begin
          if (F.Name <> '.') and (F.Name <> '..') then
            result := result and SecureDeleteFolder(IncludeTrailingPathDelimiter(ADirName) + F.Name);
        end
        else
          result := result and SecureDeleteFile(IncludeTrailingPathDelimiter(ADirName) + F.Name);
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;

    if TDirectory.IsEmpty(ADirName) then
    begin
      ADirName := ExcludeTrailingPathDelimiter(ADirName);

      // Avoid that undelete tools see directory name...
      ADirName2 := IncludeTrailingPathDelimiter(ExtractFileDir(ADirName))+RandStringFileNameFriendly(Length(ExtractFileName(ADirName)));
      if RenameFile(ADirName, ADirName2) then ADirName := ADirName2;

      // ... or the size of the name
      ADirName2 := IncludeTrailingPathDelimiter(ExtractFileDir(ADirName))+'_';
      if RenameFile(ADirName, ADirName2) then ADirName := ADirName2;

      // and now delete empty directory
      if not RemoveDir(ADirName) then
      begin
        {$IFDEF Console}
        WriteLn('ERROR Deleting empty folder: ' + ADirName);
        {$ENDIF}
        result := false;
      end;
    end
    else
    begin
      {$IFDEF Console}
      WriteLn('ERROR Deleting folder contents: ' + ADirName);
      {$ENDIF}
      result := false;
    end;
  end;
end;

function IsCompressedFileType(const AFileName: string): boolean;
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
      {$IFNDEF Console}
      Application.ProcessMessages;
      if Application.Terminated then Abort;
      {$ENDIF}
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

function BytesToRawByteString(const Bytes: TBytes): RawByteString; inline;
begin
  SetString(Result, PAnsiChar(pointer(Bytes)), length(Bytes));
end;

// https://stackoverflow.com/questions/30548940/correct-way-to-convert-size-in-bytes-to-kb-mb-gb-delphi
function FileSizeHumanReadable(Bytes: Int64): string;
const
  Description: Array [0 .. 8] of string = ('Bytes', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB', 'YiB');
var
  i: Integer;
begin
  i := 0;
  while Bytes > Power(1024, i + 1) do
    Inc(i);
  Result := FormatFloat('###0.##', Bytes / IntPower(1024, i)) + ' ' + Description[i];
end;

function GetBuildTimestamp(const ExeFile: string): TDateTime;
var
  fs: TFileStream;
  unixTime: integer;
  peOffset: Integer;
begin
  try
    fs := TFileStream.Create(ExeFile, fmOpenRead or fmShareDenyNone);
    try
      fs.Seek($3C, soFromBeginning);
      fs.Read(peOffset, 4);

      fs.Seek(peOffset+8, soFromBeginning);
      fs.Read(unixTime, 4);

      result := UnixToDateTime(unixTime, false);
    finally
      FreeAndNil(fs);
    end;
  except
    // Sollte nicht passieren
    if not FileAge(ExeFile, result) then
      raise Exception.CreateFmt('GetBuildTimestamp(%s) fehlgeschlagen', [ExeFile]);
  end;
end;

function GetOwnBuildTimestamp: TDateTime;
begin
  result := GetBuildTimestamp(ParamStr(0));
end;

{ TStreamHelper }

procedure TStreamHelper.Read(var Value; Size: Integer);
begin
  Self.ReadBuffer(Value, Size);
end;

function TStreamHelper.ReadByte: Byte;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamHelper.ReadLongBE: LongWord;
begin
  Read(Result, SizeOf(Result));
  Result := Result shl 24 or Result shr 24 or Result shl 8 and $00FF0000 or Result shr 8 and $0000FF00;
end;

function TStreamHelper.ReadInt32: Int32;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamHelper.ReadInt64: Int64;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamHelper.ReadRawByteString(len: integer): RawByteString;
begin
  SetLength(Result, len);
  Read(Result[Low(Result)], Length(Result));
end;

function TStreamHelper.ReadRawBytes(len: integer): TBytes;
begin
  result := BytesOf(ReadRawByteString(len));
end;

procedure TStreamHelper.Write(var Value; Size: Integer);
begin
  Self.WriteBuffer(Value, Size);
end;

procedure TStreamHelper.WriteByte(b: Byte);
begin
  Write(b, SizeOf(b));
end;

procedure TStreamHelper.WriteLongBE(lw: LongWord);
begin
  lw := lw shl 24 or lw shr 24 or lw shl 8 and $00FF0000 or lw shr 8 and $0000FF00;
  Write(lw, SizeOf(lw));
end;

procedure TStreamHelper.WriteInt32(i32: Int32);
begin
  Write(i32, SizeOf(i32));
end;

procedure TStreamHelper.WriteInt64(i64: Int64);
begin
  Write(i64, SizeOf(i64));
end;

procedure TStreamHelper.WriteRawByteString(rb: RawByteString);
begin
  Write(rb[1], Length(rb));
end;

procedure TStreamHelper.WriteRawBytes(b: TBytes);
begin
  WriteRawByteString(BytesToRawByteString(b));
end;

{ TDECHashExtendedAuthentication }

class function TDECHashExtendedAuthentication.HMACFile(const Key: TBytes;
  const FileName: string; const OnProgress: TDECProgressEvent): TBytes;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    HMACStream(Key, fs, fs.Size, OnProgress);
  finally
    FreeAndNil(fs);
  end;
end;

class function TDECHashExtendedAuthentication.HMACStream(const Key: TBytes;
  const Stream: TStream; Size: Int64;
  const OnProgress: TDECProgressEvent): TBytes;
const
  CONST_UINT_OF_0x36 = $3636363636363636;
  CONST_UINT_OF_0x5C = $5C5C5C5C5C5C5C5C;
var
  HashInstance: TDECHashAuthentication;
  InnerKeyPad, OuterKeyPad: array of Byte;
  I, KeyLength, BlockSize, DigestLength: Integer;
begin
  // Taken from TDECHashAuthentication.HMAC and changed HashInstance.Calc to HashInstance.CalcStream for the message
  HashInstance := TDECHashAuthenticationClass(self).Create;
  try
    BlockSize    := HashInstance.BlockSize; // 64 for sha1, ...
    DigestLength := HashInstance.DigestSize;
    KeyLength    := Length(Key);

    SetLength(InnerKeyPad, BlockSize);
    SetLength(OuterKeyPad, BlockSize);

    I := 0;

    if KeyLength > BlockSize then
    begin
      Result    := HashInstance.CalcBytes(Key);
      KeyLength := DigestLength;
    end
    else
      Result := Key;

    while I <= KeyLength - SizeOf(NativeUInt) do
    begin
      PNativeUInt(@InnerKeyPad[I])^ := PNativeUInt(@Result[I])^ xor NativeUInt(CONST_UINT_OF_0x36);
      PNativeUInt(@OuterKeyPad[I])^ := PNativeUInt(@Result[I])^ xor NativeUInt(CONST_UINT_OF_0x5C);
      Inc(I, SizeOf(NativeUInt));
    end;

    while I < KeyLength do
    begin
      InnerKeyPad[I] := Result[I] xor $36;
      OuterKeyPad[I] := Result[I] xor $5C;
      Inc(I);
    end;

    while I <= BlockSize - SizeOf(NativeUInt) do
    begin
      PNativeUInt(@InnerKeyPad[I])^ := NativeUInt(CONST_UINT_OF_0x36);
      PNativeUInt(@OuterKeyPad[I])^ := NativeUInt(CONST_UINT_OF_0x5C);
      Inc(I, SizeOf(NativeUInt));
    end;

    while I < BlockSize do
    begin
      InnerKeyPad[I] := $36;
      OuterKeyPad[I] := $5C;
      Inc(I);
    end;

    HashInstance.Init;
    HashInstance.Calc(InnerKeyPad[0], BlockSize);
    if Size > 0 then
      TDECHashExtended(HashInstance).CalcStream(Stream, Size, OnProgress, false);
    HashInstance.Done;
    Result := HashInstance.DigestAsBytes;

    HashInstance.Init;
    HashInstance.Calc(OuterKeyPad[0], BlockSize);
    HashInstance.Calc(Result[0], DigestLength);
    HashInstance.Done;

    Result := HashInstance.DigestAsBytes;
  finally
    HashInstance.Free;
  end;
end;

end.
