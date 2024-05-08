unit DecoderFuncs;

interface

uses
  DECTypes, Classes, SysUtils, Math, Forms;

procedure SecureDeleteFile(AFileName: string);
function IsCompressedFileType(AFileName: string): boolean;
function ShannonEntropy(const filename: string; OnProgressProc: TDECProgressEvent=nil): Extended;

implementation

procedure SecureDeleteFile(AFileName: string);
begin
  // TODO: Implement SecureDeleteFile()
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
