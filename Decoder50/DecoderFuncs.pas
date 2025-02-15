unit DecoderFuncs;

interface

uses
  {$IFNDEF Console}Fmx.Forms, {$ENDIF} DECTypes, System.Classes, System.SysUtils,
  System.Math, DECHashBase, DECHashAuthentication, System.IOUtils;

type
  TDcProgressState = (Started, Processing, Finished);
  TDcProgressEvent = reference to procedure(Size, Pos: Int64; const Task: string; State: TDcProgressState);

  TextProgressCallback = reference to procedure(const Text: string);

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
      const OnProgress: TDECProgressEvent = nil): TBytes;
    class function HMACStream(const Key: TBytes; const Stream: TStream; Size: Int64;
      const OnProgress: TDECProgressEvent = nil): TBytes;
  end;

procedure ZLib_Compress(const InputFileName, OutputFileName: string; OnProgressProc: TDcProgressEvent=nil);
procedure Zlib_Decompress(const InputFileName, OutputFileName: string; OnProgressProc: TDcProgressEvent=nil);
function RandStringFileNameFriendly(len: integer): string;
function RelToAbs(RelPath: string; BasePath: string=''): string;
function SecureDeleteFile(const AFileName: string; pcb: TextProgressCallback=nil): boolean;
function SecureDeleteFolder(const ADirName: string; pcb: TextProgressCallback=nil): boolean;
function IsCompressedFileType(const AFileName: string): boolean;
function ShannonEntropy(const filename: string; OnProgressProc: TDcProgressEvent=nil): Extended;
function BytesToRawByteString(const Bytes: TBytes): RawByteString; inline;
function FileSizeHumanReadable(Bytes: Int64): string;
procedure ExplorerNavigateToFile(const AFileName: string);
function GetFileTypename(const FileName: string): string;
function GetBuildTimestamp(const ExeFile: string): TDateTime;
function GetOwnBuildTimestamp: TDateTime;
procedure PlayEmptyRecycleBinSound;

{$IFDEF Console}
procedure CountDown(const msg: string; timer: integer);
{$ENDIF}

implementation

uses
  DECUtil, DECRandom, System.ZLib, System.DateUtils, System.StrUtils,
  {$IFDEF MsWindows}
  Winapi.Windows, WinApi.ShellAPI, System.Win.Registry, Winapi.MMSystem
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.SysStatVFS
  {$ENDIF};

{$IFDEF Console}
procedure CountDown(const msg: string; timer: integer);
const
  interval = 100;
begin
  timer := timer * 1000;
  while timer > 0 do
  begin
    Sleep(interval);
    Dec(timer, interval);
    Write('     ' + Format(msg, [round(timer/1000)]) + '    ' + #13);
  end;
  WriteLn('');
end;
{$ENDIF}

procedure ZLib_Compress(const InputFileName, OutputFileName: string; OnProgressProc: TDcProgressEvent=nil);
var
  CompressInputStream: TFileStream;
  CompressOutputStream: TFileStream;
  CompressionStream: TCompressionStream;
  rbs: RawByteString;
const
  ChunkSize = $100000; // value from System.Classes
resourcestring
  ProgrTask = 'ZLib compress';
begin
  CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, 0, ProgrTask, TDcProgressState.Started);
    CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
    try
      CompressionStream:=TCompressionStream.Create(clMax, CompressOutputStream);
      try
        if Assigned(OnProgressProc) then
        begin
          OnProgressProc(CompressInputStream.Size, CompressInputStream.Position, ProgrTask, TDcProgressState.Processing);
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
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, CompressInputStream.Size, ProgrTask, TDcProgressState.Finished);
  finally
    FreeAndNil(CompressInputStream);
  end;
end;

procedure Zlib_Decompress(const InputFileName, OutputFileName: string; OnProgressProc: TDcProgressEvent=nil);
var
  CompressInputStream: TFileStream;
  CompressOutputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
  rbs: RawByteString;
const
  ChunkSize = $100000; // value from System.Classes
resourcestring
  ProgrTask = 'ZLib decompress';
begin
  CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, 0, ProgrTask, TDcProgressState.Started);
    CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
    try
      DecompressionStream := TDecompressionStream.Create(CompressInputStream);
      try
        if Assigned(OnProgressProc) then
        begin
          OnProgressProc(DecompressionStream.Size, DecompressionStream.Position, ProgrTask, TDcProgressState.Processing);
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
    if Assigned(OnProgressProc) then OnProgressProc(CompressInputStream.Size, CompressInputStream.Size, ProgrTask, TDcProgressState.Finished);
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

function GetClusterSize(Drive: String): integer;
{$IF Defined(MsWindows)} // Windows
var
  SectorsPerCluster, BytesPerSector, dummy: Cardinal;
begin
  SectorsPerCluster := 0;
  BytesPerSector := 0;
  if GetDiskFreeSpace(PChar(Drive), SectorsPerCluster, BytesPerSector, dummy, dummy) then
    Result := SectorsPerCluster * BytesPerSector
  else
    Result := 512; // 1 sector
{$ELSEIF Defined(POSIX)} // macOS, Linux, Android
var
  StatFS: statvfs;
begin
  if statvfs(PAnsiChar(AnsiString(ExtractFileDrive(Path))), StatFS) = 0 then
    Result := StatFS.f_frsize // Fragment size (equivalent to cluster size)
  else
    Result := 512; // 1 sector
end;
{$ELSE}
begin
  Result := 512; // 1 sector
{$ENDIF}
end;

{$IFDEF MsWindows}
  {$IFDEF Unicode}
  function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
    external 'shlwapi.dll' name 'PathCanonicalizeW';
  {$ELSE}
  function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
    external 'shlwapi.dll' name 'PathCanonicalizeA';
  {$ENDIF}
{$ENDIF}

function RelToAbs(RelPath: string; BasePath: string=''): string;
begin
  // If no base path is provided, use the current directory
  if BasePath.IsEmpty then
    BasePath := TDirectory.GetCurrentDirectory;

  // Combine and resolve to absolute path
  Result := TPath.GetFullPath(TPath.Combine(BasePath, RelPath));
end;

function SecureDeleteFile(const AFileName: string; pcb: TextProgressCallback=nil): boolean;
var
  drive: string;
  fs: TFileStream;
  AFileNameTest: string;
  AFileNameParent: string;
  AFileNameRenamed: string;
  ClusterSize: integer;
resourcestring
  SDeleteFile_S = 'Delete file: %s';
begin
  if not FileExists(AFileName) then Exit(False);

  if Assigned(pcb) then pcb(Format(SDeleteFile_S, [AFileName]));

  ClusterSize := 32000; // max available in Windows format dialog
  try
    drive := ExtractFileDrive(RelToAbs(AFileName));
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

  AFileNameRenamed := AFileName;
  AFileNameParent := ExtractFileDir(AFileNameRenamed);
  if AFileNameParent <> '' then AFileNameParent := IncludeTrailingPathDelimiter(AFileNameParent);

  // Avoid that undelete tools see file name...
  AFileNameTest := AFileNameParent + RandStringFileNameFriendly(Length(ExtractFileName(AFileNameRenamed)));
  if RenameFile(AFileNameRenamed, AFileNameTest) then AFileNameRenamed := AFileNameTest;

  // ... or the size of the name
  AFileNameTest := AFileNameParent + '_';
  if RenameFile(AFileNameRenamed, AFileNameTest) then AFileNameRenamed := AFileNameTest;

  // Change file attributes and modification dates to also destroy this info from undelete tools
  try
    TFile.SetCreationTimeUtc(AFileNameRenamed, 0);
    TFile.SetLastWriteTimeUtc(AFileNameRenamed, 0);
    TFile.SetLastAccessTimeUtc(AFileNameRenamed, 0);
    TFile.SetAttributes(AFileNameRenamed, []);
  except
  end;

  // now delete the file
  result := System.SysUtils.DeleteFile(AFileNameRenamed);
end;

function Occurrences(const Substring, Text: string): integer;
var
  offset: integer;
begin
  result := 0;
  offset := PosEx(Substring, Text, 1);
  while offset <> 0 do
  begin
    inc(result);
    offset := PosEx(Substring, Text, offset + length(Substring));
  end;
end;

function SecureDeleteFolder(const ADirName: string; pcb: TextProgressCallback=nil): boolean;
var
  F: TSearchRec;
  ADirNameTest: string;
  ADirNameParent: string;
  ADirNameRenamed: string;
  ADirNameAbs: string;
  IsDriveOrShareRoot: boolean;
resourcestring
  SNoNuke = 'Sorry, but this program will not destroy your computer.';
  SStartDeleteFolder_S = 'START Delete folder: %s';
  SErrorDeletingEmptyFolder_S = 'ERROR Deleting empty folder: %s';
  SDoneDeletingFolder_S = 'DONE Deleting folder: %s';
  SErrorDeletingFolderContents_S = 'ERROR Deleting folder contents: %s';
begin
  if SameText(ADirName, 'C:\') or
     SameText(ADirName, 'C:/') or
     SameText(ADirName, 'C:') or
     SameText(ADirName, '/') then
  begin
    raise Exception.Create(SNoNuke);
  end;

  if not DirectoryExists(ADirName) then Exit(False);
  result := true;

  ADirNameAbs := RelToAbs(ADirName);

  IsDriveOrShareRoot :=
    // are we trying to delete a drive root D:\, \, \\?\D:\, D:
    EndsStr(':\', ADirNameAbs) or
    EndsStr(':/', ADirNameAbs) or
    EndsStr(':', ADirNameAbs) or
    (ADirNameAbs = '/') or
    (ADirNameAbs = '\') or
    // Is this a root UNC shared?  \\server1\share\
    (
      StartsStr('\\', ADirNameAbs) and
      (Occurrences('\', IncludeTrailingPathDelimiter(ADirNameAbs)) = 4) and
      EndsStr('\', IncludeTrailingPathDelimiter(ADirNameAbs))
    );

  if Assigned(pcb) then pcb(Format(SStartDeleteFolder_S, [ADirName]));

  if FindFirst(IncludeTrailingPathDelimiter(ADirName) + '*', faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then
        begin
          if (F.Name <> '.') and (F.Name <> '..') then
            result := result and SecureDeleteFolder(IncludeTrailingPathDelimiter(ADirName) + F.Name, pcb);
        end
        else
          result := result and SecureDeleteFile(IncludeTrailingPathDelimiter(ADirName) + F.Name, pcb);
      until FindNext(F) <> 0;
    finally
      System.SysUtils.FindClose(F);
    end;

    if not IsDriveOrShareRoot then
    begin
      if TDirectory.IsEmpty(ADirName) then
      begin
        ADirNameRenamed := ExcludeTrailingPathDelimiter(ADirName);
        ADirNameParent := ExtractFileDir(ADirNameRenamed);
        if ADirNameParent <> '' then ADirNameParent := IncludeTrailingPathDelimiter(ADirNameParent);

        // Avoid that undelete tools see directory name...
        ADirNameTest := ADirNameParent + RandStringFileNameFriendly(Length(ExtractFileName(ADirNameRenamed)));
        if RenameFile(ADirNameRenamed, ADirNameTest) then ADirNameRenamed := ADirNameTest;

        // ... or the size of the name
        ADirNameTest := ADirNameParent + '_';
        if RenameFile(ADirNameRenamed, ADirNameTest) then ADirNameRenamed := ADirNameTest;

        // Change folder attributes and modification dates to also destroy this info from undelete tools
        try
          TDirectory.SetCreationTimeUtc(ADirNameRenamed, 0);
          TDirectory.SetLastWriteTimeUtc(ADirNameRenamed, 0);
          TDirectory.SetLastAccessTimeUtc(ADirNameRenamed, 0);
          TDirectory.SetAttributes(ADirNameRenamed, []);
        except
        end;

        // and now delete empty directory
        if not RemoveDir(ADirNameRenamed) then
        begin
          // Undo renaming
          if ADirName <> ADirNameRenamed then
            RenameFile(ADirNameRenamed, ADirName);
          if Assigned(pcb) then pcb(Format(SErrorDeletingEmptyFolder_S, [ADirName]));
          result := false;
        end
        else
        begin
          if Assigned(pcb) then pcb(Format(SDoneDeletingFolder_S, [ADirName]));
          result := true;
        end;
      end
      else
      begin
        if Assigned(pcb) then pcb(Format(SErrorDeletingFolderContents_S, [ADirName]));
        result := false;
      end;
    end;
  end;
end;

function IsCompressedFileType(const AFileName: string): boolean;
var
  ext: string;
begin
  ext := ExtractFileExt(AFileName);
  result :=
    // Compressed archive formats
    // TODO: Get more formats here (but only the compressed ones!): https://en.wikipedia.org/wiki/List_of_archive_formats
    SameText(ext, '.zip')  or // .zip - ZIP archive
    SameText(ext, '.7z')   or // .7z - 7-Zip archive
    SameText(ext, '.rar')  or // .rar - RAR archive
    SameText(ext, '.gz')   or // .gz - GZIP file (often combined with tar)
    SameText(ext, '.xz')   or // .xz - XZ archive (often combined with tar)
    SameText(ext, '.bz2')  or // .bz2 - BZ2 archive (often combined with tar)
    SameText(ext, '.tgz')  or // .tgz - TAR archive compressed with GZIP.
    SameText(ext, '.zst')  or // .zst - Zstandard compressed file
    SameText(ext, '.cab')  or // .cab - Microsoft Cabinet file
    // Microsoft Office Formats
    SameText(ext, '.docx') or // .docx - Microsoft Word document
    SameText(ext, '.docm') or // .docm - Microsoft Word document macro-enabled
    SameText(ext, '.dotx') or // .dotx - Word templates
    SameText(ext, '.dotm') or // .dotm - Word template macro-enabled
    SameText(ext, '.xlsx') or // .xlsx - Microsoft Excel spreadsheet
    SameText(ext, '.xlsm') or // .xlsm - Microsoft Excel spreadsheet macro-enabled
    SameText(ext, '.xlam') or // .xlam - Excel macro-enabled add-ins
    SameText(ext, '.pptx') or // .pptx - Microsoft PowerPoint presentation
    SameText(ext, '.pptm') or // .pptm - Microsoft PowerPoint presentation macro-enabled
    SameText(ext, '.potx') or // .potx - PowerPoint templates
    SameText(ext, '.potm') or // .potm - PowerPoint templates macro-enabled
    SameText(ext, '.vsdx') or // .vsdx - Microsoft Visio diagram
    // OpenDocument Formats (but they could also be a single XML file according to Wikipedia?!)
    SameText(ext, '.odt')  or // .odt - OpenDocument Text document
    SameText(ext, '.ods')  or // .ods - OpenDocument Spreadsheet
    SameText(ext, '.odp')  or // .odp - OpenDocument Presentation
    SameText(ext, '.odg')  or // .odg - OpenDocument Graphics
    SameText(ext, '.ott')  or // .ott - OpenDocument Text document template
    SameText(ext, '.ots')  or // .ots - OpenDocument Spreadsheet template
    SameText(ext, '.otp')  or // .otp - OpenDocument Presentation template
    SameText(ext, '.oxt')  or // .oxt - LibreOffice extensions
    // Audio formats
    SameText(ext, '.mp3')  or // .mp3 - MPEG audio
    SameText(ext, '.aac')  or // .aac - Advanced Audio Codec
    SameText(ext, '.flac') or // .flac - Free Lossless Audio Codec
    SameText(ext, '.ogg')  or // .ogg - Ogg Vorbis
    SameText(ext, '.wma')  or // .wma - Windows Media Audio (lossy, sometimes compressed)
    // Video formats
    SameText(ext, '.mp4')  or // .mp4 - MPEG-4 video
    SameText(ext, '.webm') or // .webm - WebM video
    SameText(ext, '.mkv')  or // .mkv - Matroska video
    SameText(ext, '.avi')  or // .avi - Audio Video Interleave. Uncompressed video is rare; often uses lossy codecs.
    SameText(ext, '.wmv')  or // .wmv - Windows Media Video (lossy, compressed)
    // Picture formats
    SameText(ext, '.png')  or // .png - Portable Network Graphics
    SameText(ext, '.gif')  or // .gif - Graphics Interchange Format
    SameText(ext, '.jpg')  or // .jpg, .jpeg, .jfif - JPEG image
    SameText(ext, '.jpeg') or // .jpg, .jpeg, .jfif - JPEG image
    SameText(ext, '.jfif') or // .jpg, .jpeg, .jfif - JPEG image
    SameText(ext, '.webp') or // .webp - WebP image
    SameText(ext, '.svgz') or // .svgz - Compressed SVG (Scalable Vector Graphics)
    // Other formats
    SameText(ext, '.epub') or // .epub - eBook format
    SameText(ext, '.jar')  or // .jar - Java Archive
    SameText(ext, '.apk')  or // .apk - Android application package
    SameText(ext, '.kmz')  or // .kmz - Google Earth data file (compressed version of KML)
    SameText(ext, '.xpi')  or // .xpi - Mozilla Firefox browser extension
    SameText(ext, '.war')  or // .war / .ear - Java web applications
    SameText(ext, '.ear')  or // .war / .ear - Java web applications
    SameText(ext, '.appx') or // .appx / .msix - Windows application package formats
    SameText(ext, '.msix') or // .appx / .msix - Windows application package formats
    SameText(ext, '.ipa')  or // .ipa - iOS application archive
    SameText(ext, '.cb7')  or // .cb7 - Comic Book 7z archive
    SameText(ext, '.cba')  or // .cba - Comic Book ACE archive
    SameText(ext, '.cbr')  or // .cbr - Comic Book RAR archive
    SameText(ext, '.cbt')  or // .cbt - Comic Book TAR archive
    SameText(ext, '.cbz')  or // .cbz - Comic Book ZIP archive
    SameText(ext,'.pkpass') or// .pkpass - Apple Wallet pass files, ZIP based
    SameText(ext, '.azw')  or // .azw - Amazon Kindle eBook format (old), compressed except metadata
    SameText(ext, '.azw3') or // .azw3 - Amazon Kindle eBook format "HF8", ZIP based
    SameText(ext, '.kfx');    // .kfx - Amazon Kindle eBook format, compressed
end;

function ShannonEntropy(const filename: string; OnProgressProc: TDcProgressEvent=nil): Extended;
var
  fs: TFileStream;
var
  p: Extended;
  i: int64;
  counts: array[0..255] of int64;
  filesize: int64;
  rbs: RawByteString;
  ProgrSize, ProgrPos: Int64;
const
  chunksize = 4096; // bigger = faster
resourcestring
  ProgrTask = 'Calc shannon entropy';
begin
  for i := Low(counts) to High(counts) do
    Counts[i] := 0;

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    filesize := fs.Size;
    ProgrPos := 0;
    ProgrSize := Filesize div chunksize;
    if Assigned(OnProgressProc) then OnProgressProc(ProgrSize, ProgrPos, ProgrTask, Started);
    while fs.Position < fs.Size do
    begin
      rbs := fs.ReadRawByteString(Min(chunksize,fs.Size-fs.Position));
      for i := Low(rbs) to High(rbs) do
        Inc(counts[Ord(rbs[i])]);
      Inc(ProgrPos);
      if Assigned(OnProgressProc) then OnProgressProc(ProgrSize, ProgrPos, ProgrTask, Processing);
      {$IFNDEF Console}
      Application.ProcessMessages;
      if Application.Terminated then Abort;
      {$ENDIF}
    end;
    if Assigned(OnProgressProc) then OnProgressProc(ProgrSize, ProgrSize, ProgrTask, Finished);

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

function GetFileTypename(const Filename: string): string;
{$IFDEF MsWindows}
var
  Info: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(Filename), 0, Info, SizeOf(Info), SHGFI_TYPENAME) <> 0 then
    Result := Info.szTypeName
  else
    Result := '';
{$ELSE}
resourcestring
  SSFile = '%s-File';
  SUnknown = 'Unknown';
var
  tmp: string;
begin
  tmp := ExtractFileExt(Filename);
  if tmp = '' then
  begin
    Result := SUnknown;
  end
  else
  begin
    tmp := Copy(tmp, 1);
    Result := Format(SSFile, [tmp.ToUpper]);
  end;
{$ENDIF}
end;

procedure ExplorerNavigateToFile(const AFileName: string);
begin
{$IFDEF MsWindows}
  ShellExecute(0, 'open', 'explorer.exe', PChar('/select,'+AFileName), nil, SW_SHOWNORMAL);
{$ELSE}
  // Not implemented for this OS
{$ENDIF}
end;

function GetBuildTimestamp(const ExeFile: string): TDateTime;
var
  fs: TFileStream;
  unixTime: integer;
  peOffset: Integer;
resourcestring
  SGetBuildTimestampFailed = 'GetBuildTimestamp(%s) failed';
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
      raise Exception.CreateFmt(SGetBuildTimestampFailed, [ExeFile]);
  end;
end;

function GetOwnBuildTimestamp: TDateTime;
begin
  result := GetBuildTimestamp(ParamStr(0));
end;

procedure PlayEmptyRecycleBinSound;
{$IFDEF MsWindows}
var
  reg: TRegistry;
  soundFile: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('AppEvents\Schemes\Apps\Explorer\EmptyRecycleBin\.Current') then
    begin
      soundFile := reg.ReadString('');
      if soundFile <> '' then
        PlaySound(PChar(soundFile), 0, SND_FILENAME or SND_ASYNC or SND_NODEFAULT);
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
{$ELSE}
begin
  // Not implemented for this OS
{$ENDIF}
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
  if len = 0 then exit;  
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
  if rb = '' then exit;
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
  const OnProgress: TDECProgressEvent = nil): TBytes;
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
    FreeAndNil(HashInstance);
  end;
end;

end.
