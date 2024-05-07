unit DecoderMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  DecoderEncDec, DECTypes, DecoderOldCiphers, DECCipherBase, DECCiphers,
  DECCipherFormats, Math;

{$R *.dfm}

procedure OnProgressProc(Size, Pos: Int64; State: TDECProgressState);
begin
  FormMain.ProgressBar1.Min := 0;
  FormMain.ProgressBar1.Max := Size;

  if (State = Finished) then
    FormMain.ProgressBar1.Position := FormMain.ProgressBar1.Max
  else
    FormMain.ProgressBar1.Position := Pos;
end;

function CalcEntropy(const filename: string; OnProgressProc: TDECProgressEvent=nil): Extended;
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
      rbs := ReadRaw(Max(chunksize,fs.Size-fs.Position));
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

procedure TFormMain.Button1Click(Sender: TObject);
var
  entropy: Extended;
begin
  // EXE          5,7816594231572
  // PAS          5,15501756140616
  // DC4          7,98892287038652
  // Pure Random  7,98539387290228
  // Same Byte    0
  entropy := CalcEntropy('Coder.exe', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := CalcEntropy('DecoderEncDec.pas', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := CalcEntropy('TestData\schloss.dc4', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := CalcEntropy('random.bin', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := CalcEntropy('zeroent.bin', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
end;

(*
procedure TFormMain.Button2Click(Sender: TObject);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
const
  IdentityBase = $1259D82A; // DC 5.0
begin
  Memo1.Clear;
  for p in TDECHash.ClassList do
  begin
    c := p.Value;
    cn := c.ClassName;
    Memo1.Lines.Add(
      '0x'+IntToHex(DEC51_Identity(IdentityBase, cn), 8) + #9 +
      cn +
      ' (DigestSize: '+IntToStr(TDECHashClass(c).DigestSize) +
      ', BlockSize: '+IntToStr(TDECHashClass(c).BlockSize) + ')'
    );
  end;
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
const
  IdentityBase = $1259D82A; // DC 5.0
begin
  Memo1.Clear;
  for p in TDECCipher.ClassList do
  begin
    c := p.Value;
    cn := c.ClassName;
    Memo1.Lines.Add(
      '0x'+IntToHex(DEC51_Identity(IdentityBase, cn), 8) + #9 +
      cn +
      ' (KeySize: '+IntToStr(TDECCipherClass(c).Context.KeySize) +
      ', BlockSize: '+IntToStr(TDECCipherClass(c).Context.BlockSize) +
      ', BufferSize: '+IntToStr(TDECCipherClass(c).Context.BufferSize) + ')'
    );
  end;
end;
*)

procedure TFormMain.Button3Click(Sender: TObject);

  function Are2FilesEqual(const File1, File2: TFileName): Boolean;
  var
    ms1, ms2: TMemoryStream;
  begin
    Result := False;
    ms1 := TMemoryStream.Create;
    try
      ms1.LoadFromFile(File1);
      ms2 := TMemoryStream.Create;
      try
        ms2.LoadFromFile(File2);
        if ms1.Size = ms2.Size then
          Result := CompareMem(ms1.Memory, ms2.memory, ms1.Size);
      finally
        ms2.Free;
      end;
    finally
      ms1.Free;
    end
  end;
var
  fi: TDC4FileInfo;

begin
  Memo1.Lines.Clear;

  DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(fvHagenReddmannExample));
  DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(fvDc40));
  DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(fvDc41Beta));
  DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled));
  DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(fvDc50Wip));
  Memo1.Lines.Add('DeCoder4X_ValidateParameterBlock OK');
  Memo1.Lines.Add('');

  DeCoder20_EncodeFile('TestData\dc20_256zero_in.txt', 'TestData\dc20_256zero_out.tmp', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc20_256zero_out.txt', 'TestData\dc20_256zero_out.tmp'));
  DeleteFile('TestData\dc20_256zero_out.tmp');
  DeCoder20_EncodeFile('TestData\dc20_test_in.txt', 'TestData\dc20_test_out.tmp', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc20_test_out.txt', 'TestData\dc20_test_out.tmp'));
  DeleteFile('TestData\dc20_test_out.tmp');
  Memo1.Lines.Add('DC20 Encode OK');
  Memo1.Lines.Add('');

  DeCoder22_EncodeFile('TestData\dc22_256zero_in.txt', 'TestData\dc22_256zero_out_61.tmp', 61, OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc22_256zero_out_61.txt', 'TestData\dc22_256zero_out_61.tmp'));
  DeleteFile('TestData\dc22_256zero_out_61.tmp');
  DeCoder22_EncodeFile('TestData\dc22_test_in.txt', 'TestData\dc22_test_out_61.tmp', 61, OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc22_test_out_61.txt', 'TestData\dc22_test_out_61.tmp'));
  DeleteFile('TestData\dc22_test_out_61.tmp');
  Memo1.Lines.Add('DC22 Encode OK');
  Memo1.Lines.Add('');

  DeCoder30_EncodeFile('TestData\dc30_256zero_in.txt', 'TestData\dc30_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc30_256zero_out_foobar.txt', 'TestData\dc30_256zero_out_foobar.tmp'));
  DeleteFile('TestData\dc30_256zero_out_foobar.tmp');
  DeCoder30_EncodeFile('TestData\dc30_test_in.txt', 'TestData\dc30_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc30_test_out_foobar.txt', 'TestData\dc30_test_out_foobar.tmp'));
  DeleteFile('TestData\dc30_test_out_foobar.tmp');
  Memo1.Lines.Add('DC30 Encode OK');
  Memo1.Lines.Add('');

  DeCoder32_EncodeFile('TestData\dc32_256zero_in.txt', 'TestData\dc32_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc32_256zero_out_foobar.txt', 'TestData\dc32_256zero_out_foobar.tmp'));
  DeleteFile('TestData\dc32_256zero_out_foobar.tmp');
  DeCoder32_EncodeFile('TestData\dc32_256zero_in.txt', 'TestData\dc32_256zero_out_abcdefg.tmp', 'abcdefg', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc32_256zero_out_abcdefg.txt', 'TestData\dc32_256zero_out_abcdefg.tmp'));
  DeleteFile('TestData\dc32_256zero_out_abcdefg.tmp');
  DeCoder32_EncodeFile('TestData\dc32_test_in.txt', 'TestData\dc32_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc32_test_out_foobar.txt', 'TestData\dc32_test_out_foobar.tmp'));
  DeleteFile('TestData\dc32_test_out_foobar.tmp');
  Memo1.Lines.Add('DC32 Encode OK');
  Memo1.Lines.Add('');

  DeCoder4X_DecodeFile('TestData\schloss.dc4', 'schloss_decoded.bmp', 'test', false, OnProgressProc);
  Memo1.Lines.Add('Decode DC41 Beta OK');
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver0.dc5', 'test', DeCoder4X_GetDefaultParameters(fvHagenReddmannExample), OnProgressProc);
  DeCoder4X_DecodeFile('schloss_ver0.dc5', 'schloss_decoded_dc5_ver0.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver0.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver0.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver0.bmp');
  DeleteFile('schloss_ver0.dc5');
  Memo1.Lines.Add('Hagen Example OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver1.dc5', 'test', DeCoder4X_GetDefaultParameters(fvDc40), OnProgressProc);
  DeCoder4X_DecodeFile('schloss_ver1.dc5', 'schloss_decoded_dc5_ver1.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver1.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver1.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver1.bmp');
  DeleteFile('schloss_ver1.dc5');
  Memo1.Lines.Add('DC40 OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver2.dc5', 'test', DeCoder4X_GetDefaultParameters(fvDc41Beta), OnProgressProc);
  DeCoder4X_DecodeFile('schloss_ver2.dc5', 'schloss_decoded_dc5_ver2.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver2.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver2.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver2.bmp');
  DeleteFile('schloss_ver2.dc5');
  Memo1.Lines.Add('DC41 Beta OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver3.dc5', 'test', DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled), OnProgressProc);
  DeCoder4X_DecodeFile('schloss_ver3.dc5', 'schloss_decoded_dc5_ver3.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver3.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver3.bmp');
  DeleteFile('schloss_ver3.dc5');
  Memo1.Lines.Add('DC41 Final OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver4.dc5', 'test', DeCoder4X_GetDefaultParameters(fvDc50Wip), OnProgressProc);
  DeCoder4X_DecodeFile('schloss_ver4.dc5', 'schloss_decoded_dc5_ver4.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver4.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver4.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver4.bmp');
  DeleteFile('schloss_ver4.dc5');
  Memo1.Lines.Add('DC50 OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeleteFile('schloss_decoded.bmp');

  ShowMessage('Alles OK');
end;

end.
