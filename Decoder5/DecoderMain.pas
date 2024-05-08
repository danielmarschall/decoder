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
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  DecoderEncDec, DECTypes, DecoderOldCiphers, DECCipherBase, DECCiphers,
  DECCipherFormats, Math, ZLib, System.Generics.Collections,
  DecoderFuncs;

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

procedure TFormMain.Button1Click(Sender: TObject);
var
  entropy: Extended;
begin
  // EXE          5,7816594231572
  // PAS          5,15501756140616
  // Bitmap       6,42988844900519
  // DC4          7,98892287038652
  // Pure Random  7,98539387290228
  // Same Byte    0
  entropy := ShannonEntropy('Coder.exe', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := ShannonEntropy('DecoderEncDec.pas', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := ShannonEntropy('TestData\schloss.dc4', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := ShannonEntropy('random.bin', OnProgressProc);
  memo1.Lines.Add(FloatTostr(entropy));
  entropy := ShannonEntropy('zeroent.bin', OnProgressProc);
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

procedure TFormMain.Button2Click(Sender: TObject);

  type
    TEntropyRatio = record
      entropySum: Extended;
      ratioSum: Extended;
      num: integer;
    end;

  var
    FileExtAnalysis: TDictionary<string, TEntropyRatio>;

  function ZLibCompressRatio(InputFileName: string): Extended;
  var
    CompressInputStream: TFileStream;
    CompressOutputStream: TMemoryStream;
    CompressionStream: TCompressionStream;
  begin
    CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
    try
      CompressOutputStream:=TMemoryStream.Create;
      try
        CompressionStream:=TCompressionStream.Create(clMax, CompressOutputStream);
        try
          CompressionStream.CopyFrom(CompressInputStream, CompressInputStream.Size);
        finally
          FreeAndNil(CompressionStream);
        end;
        result := CompressOutputStream.Size / CompressInputStream.Size;
      finally
        FreeAndNil(CompressOutputStream);
      end;
    finally
      FreeAndNil(CompressInputStream);
    end;
  end;

  procedure AnalyzeFile(AFileName: string);
  var
    entropy, ratio: Extended;
    er: TEntropyRatio;
    FileExt: string;
  begin
    Caption := AFileName;
    Application.ProcessMessages;
    entropy := ShannonEntropy(AFileName, OnProgressProc);
    ratio := ZLibCompressRatio(AFileName);
    FileExt := AnsiUpperCase(ExtractFileExt(AFileName));
    if not FileExtAnalysis.ContainsKey(FileExt) then
    begin
      FileExtAnalysis.Add(FileExt, er);
      er.entropySum := 0;
      er.ratioSum := 0;
      er.num := 0;
    end
    else
      er := FileExtAnalysis[FileExt];
    er.entropySum := er.entropySum + entropy;
    er.ratioSum := er.ratioSum + ratio;
    er.num := er.num + 1;
    FileExtAnalysis[FileExt] := er;
    memo1.Lines.Add(AFileName+#9+ExtractfileExt(AFileName)+#9+FloatTostr(entropy)+#9+FloatTostr(ratio));
    if Application.Terminated then
    begin
      Memo1.Lines.SaveToFile('result.csv');
      Abort;
    end;
  end;

  procedure AnalyzeDir(DirName: string);
  var
    searchResult: TSearchRec;
  begin
    if FindFirst(dirName+'\*', faAnyFile, searchResult)=0 then begin
      try
        repeat
          if (searchResult.Attr and faDirectory)=0 then begin
            //if SameText(ExtractFileExt(searchResult.Name), '.ini') then begin
            try
              AnalyzeFile(IncludeTrailingPathDelimiter(dirName)+searchResult.Name);
            except
              on E: Exception do
              begin
                Memo1.Lines.Add(IncludeTrailingPathDelimiter(dirName)+searchResult.Name+#9+E.Message);
              end;
            end;
            //end;
          end else if (searchResult.Name<>'.') and (searchResult.Name<>'..') then begin
            AnalyzeDir(IncludeTrailingPathDelimiter(dirName)+searchResult.Name);
          end;
        until FindNext(searchResult)<>0
      finally
        FindClose(searchResult);
      end;
    end;
    Caption := 'Fertig';
  end;

begin
  FileExtAnalysis := TDictionary<string, TEntropyRatio>.Create;
  try
    // AnalyzeFile('Coder.exe');
    AnalyzeDir('C:\');

    Memo1.Lines.Clear;
    Memo1.Lines.Add('File Ext'+#9+'EntropyAvg'+#9+'RatioAvg'+#9+'Num');
    for var Enum in FileExtAnalysis do
    begin
      Memo1.Lines.Add(Enum.Key + #9 + FloatToStr(Enum.Value.entropySum/Enum.Value.num) + #9 + FloatToStr(Enum.Value.ratioSum/Enum.Value.num) + #9 + IntToStr(Enum.Value.num));
    end;
    Memo1.Lines.SaveToFile('result.csv');

  finally
    FileExtAnalysis.Free;
  end;
end;

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
  Memo1.Lines.Add('DC41 Final OK (Filename encrypted):');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  fi := DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled);
  fi.FileNameUserPasswordEncrypted := false;
  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver3.dc5', 'test', fi, OnProgressProc);
  DeCoder4X_DecodeFile('schloss_ver3.dc5', 'schloss_decoded_dc5_ver3.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver3.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver3.bmp');
  DeleteFile('schloss_ver3.dc5');
  Memo1.Lines.Add('DC41 Final OK (Filename not encrypted):');
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

  fi := DeCoder4X_GetDefaultParameters(fvDc50Wip);
  fi.CipherClass := TCipher_AES128;
  fi.CipherMode := cmGCM;
  fi.GCMAuthTagSizeInBytes := 16;
  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver4.dc5', 'test', fi, OnProgressProc);
  fi := DeCoder4X_DecodeFile('schloss_ver4.dc5', 'schloss_decoded_dc5_ver4.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver4.bmp'));
  DeleteFile('schloss_decoded_dc5_ver4.bmp');
  DeleteFile('schloss_ver4.dc5');
  Memo1.Lines.Add('DC50 GCM OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  fi := DeCoder4X_GetDefaultParameters(fvDc50Wip);
  fi.CipherClass := TCipher_Blowfish;
  fi.IVSizeInBytes := fi.CipherClass.Context.BufferSize;
  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver4.dc5', 'test', fi, OnProgressProc);
  fi := DeCoder4X_DecodeFile('schloss_ver4.dc5', 'schloss_decoded_dc5_ver4.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver4.bmp'));
  DeleteFile('schloss_decoded_dc5_ver4.bmp');
  DeleteFile('schloss_ver4.dc5');
  Memo1.Lines.Add('DC50 Blowfish OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  fi := DeCoder4X_GetDefaultParameters(fvDc50Wip);
  fi.CipherClass := TCipher_Blowfish;
  fi.CipherMode := cmECBx;
  fi.IVSizeInBytes := fi.CipherClass.Context.BufferSize;
  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver4.dc5', 'test', fi, OnProgressProc);  // works only because ZLib provided a size that can be divided by 8.
  fi := DeCoder4X_DecodeFile('schloss_ver4.dc5', 'schloss_decoded_dc5_ver4.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver4.bmp'));
  DeleteFile('schloss_decoded_dc5_ver4.bmp');
  DeleteFile('schloss_ver4.dc5');
  Memo1.Lines.Add('DC50 Blowfish ECB OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeleteFile('schloss_decoded.bmp');

  ShowMessage('Alles OK');
end;

end.
