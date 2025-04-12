unit TestUnit1;

interface

uses
  SysUtils, Classes, DUnitX.TestFramework;

type
  [TestFixture]
  TDecoderTestObject = class
  public
    [Test]
    procedure Test_DeCoder4X_ValidateParameterBlock;
    [Test]
    procedure Test_DeCoder10_Encode;
    [Test]
    procedure Test_DeCoder10_Decode;
    [Test]
    procedure Test_DeCoder20_Encode;
    [Test]
    procedure Test_DeCoder20_Decode;
    [Test]
    procedure Test_DeCoder22_Encode;
    [Test]
    procedure Test_DeCoder22_Decode;
    [Test]
    procedure Test_DeCoder30_Encode;
    [Test]
    procedure Test_DeCoder30_Decode;
    [Test]
    procedure Test_DeCoder32_Encode;
    [Test]
    procedure Test_DeCoder32_Decode;
    [Test]
    procedure Test_DeCoder45_Testcases;
  end;

implementation

uses
  DECTypes,
  DECCiphers,
  DECCipherBase,
  DecoderEncDec,
  DecoderFuncs,
  DecoderOldCiphers,
  DecoderSevenZipUtils,
  DecoderConst;

procedure TDecoderTestObject.Test_DeCoder4X_ValidateParameterBlock;
var
  v: TDc4FormatVersion;
  iOk, iFail: integer;
begin
  iOk := 0;
  iFail := 0;
  for v := Low(TDc4FormatVersion) to High(TDc4FormatVersion) do
  begin
    DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(v));
    try
      Log('DeCoder4X_ValidateParameterBlock v'+IntToStr(Integer(v))+' OK');
      Inc(iOk);
    except
      on E: Exception do
      begin
        Log('DeCoder4X_ValidateParameterBlock v'+IntToStr(Integer(v))+' Fail: ' + E.Message);
        Inc(iFail);
      end;
    end;
  end;
  if iFail > 0 then
    Assert.Fail(Format('DeCoder4X_ValidateParameterBlock: %d OK, %d FAIL', [iOk, iFail]))
  else
    Assert.Pass(Format('DeCoder4X_ValidateParameterBlock: %d OK, %d FAIL', [iOk, iFail]));
end;

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
      FreeAndNil(ms2);
    end;
  finally
    FreeAndNil(ms1);
  end
end;

procedure OnProgressProc(Size, Pos: Int64; const Task: string; State: TDcProgressState);
begin
  (*
  case State of
    TDcProgressState.Started:    Write(  #13 + Format('%6.2f',[0.00])+'% ... ' + Task);
    TDcProgressState.Processing: Write(  #13 + Format('%6.2f',[Pos/Size*100])+'% ... ' + Task);
    TDcProgressState.Finished:   WriteLn(#13 + Format('%6.2f',[100.00])+'% ... ' + Task + ' = Done');
  end;
  *)
end;

procedure TDecoderTestObject.Test_DeCoder10_Encode;
begin
  DeCoder10_EncodeFile('..\TestData\dc10_example_in.txt', '..\TestData\dc10_example_out.tmp', True, OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc10_example_out.tmp', '..\TestData\dc10_example_out.txt'));
  DeleteFile('..\TestData\dc10_example_out.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder10_Decode;
begin
  DeCoder10_DecodeFile('..\TestData\dc10_example_out.txt', '..\TestData\dc10_example_in.tmp', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc10_example_in.tmp', '..\TestData\dc10_example_in.txt'));
  DeleteFile('..\TestData\dc10_example_in.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder20_Encode;
begin
  DeCoder20_EncodeFile('..\TestData\dc20_256zero_in.txt', '..\TestData\dc20_256zero_out.tmp', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc20_256zero_out.txt', '..\TestData\dc20_256zero_out.tmp'));
  DeleteFile('..\TestData\dc20_256zero_out.tmp');
  DeCoder20_EncodeFile('..\TestData\dc20_test_in.txt', '..\TestData\dc20_test_out.tmp', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc20_test_out.txt', '..\TestData\dc20_test_out.tmp'));
  DeleteFile('..\TestData\dc20_test_out.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder20_Decode;
begin
  Test_DeCoder20_Encode; // due to XOR, Encode and Decode is the same
end;

procedure TDecoderTestObject.Test_DeCoder22_Encode;
begin
  DeCoder22_EncodeFile('..\TestData\dc22_256zero_in.txt', '..\TestData\dc22_256zero_out_61.tmp', 61, OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc22_256zero_out_61.txt', '..\TestData\dc22_256zero_out_61.tmp'));
  DeleteFile('..\TestData\dc22_256zero_out_61.tmp');
  DeCoder22_EncodeFile('..\TestData\dc22_test_in.txt', '..\TestData\dc22_test_out_61.tmp', 61, OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc22_test_out_61.txt', '..\TestData\dc22_test_out_61.tmp'));
  DeleteFile('..\TestData\dc22_test_out_61.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder22_Decode;
begin
  Test_DeCoder22_Encode; // due to XOR, Encode and Decode is the same
end;

procedure TDecoderTestObject.Test_DeCoder30_Encode;
begin
  DeCoder30_EncodeFile('..\TestData\dc30_256zero_in.txt', '..\TestData\dc30_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc30_256zero_out_foobar.txt', '..\TestData\dc30_256zero_out_foobar.tmp'));
  DeleteFile('..\TestData\dc30_256zero_out_foobar.tmp');
  DeCoder30_EncodeFile('..\TestData\dc30_test_in.txt', '..\TestData\dc30_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc30_test_out_foobar.txt', '..\TestData\dc30_test_out_foobar.tmp'));
  DeleteFile('..\TestData\dc30_test_out_foobar.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder30_Decode;
begin
  Test_DeCoder30_Encode; // due to XOR, Encode and Decode is the same
end;

procedure TDecoderTestObject.Test_DeCoder32_Encode;
begin
  DeCoder32_EncodeFile('..\TestData\dc32_256zero_in.txt', '..\TestData\dc32_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc32_256zero_out_foobar.txt', '..\TestData\dc32_256zero_out_foobar.tmp'));
  DeleteFile('..\TestData\dc32_256zero_out_foobar.tmp');
  DeCoder32_EncodeFile('..\TestData\dc32_256zero_in.txt', '..\TestData\dc32_256zero_out_abcdefg.tmp', 'abcdefg', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc32_256zero_out_abcdefg.txt', '..\TestData\dc32_256zero_out_abcdefg.tmp'));
  DeleteFile('..\TestData\dc32_256zero_out_abcdefg.tmp');
  DeCoder32_EncodeFile('..\TestData\dc32_test_in.txt', '..\TestData\dc32_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('..\TestData\dc32_test_out_foobar.txt', '..\TestData\dc32_test_out_foobar.tmp'));
  DeleteFile('..\TestData\dc32_test_out_foobar.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder32_Decode;
begin
  Test_DeCoder32_Encode; // due to XOR, Encode and Decode is the same
end;

// TODO: Split into smaller test cases and remove WriteLn()
procedure TDecoderTestObject.Test_DeCoder45_Testcases;
var
  fp: TDC4Parameters;
  fi: TDC4FileInfo;
  sl: TStringList;
  OutputFile: string;
  v: TDc4FormatVersion;
begin
  sl := TStringList.Create;

  OutputFile := 'schloss_decoded.bmp';
  DeCoder4X_DecodeFile('..\TestData\schloss.dc4', OutputFile, 'test', OnProgressProc);
  WriteLn('Decode DC41 Beta OK');
  WriteLn('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver0.dc5', 'test', DeCoder4X_GetDefaultParameters(fvHagenReddmannExample), OnProgressProc);
  OutputFile := 'schloss_decoded_dc5_ver0.bmp';
  DeCoder4X_DecodeFile('schloss_ver0.dc5', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver0.bmp'));
  fi := DeCoder4X_FileInfo('schloss_ver0.dc5', '', OnProgressProc);
  DeleteFile('schloss_decoded_dc5_ver0.bmp');
  DeleteFile('schloss_ver0.dc5');
  WriteLn('Hagen Example OK:');
  sl.Clear;
  DeCoder4X_PrintFileInfo(fi, sl);
  WriteLn(sl.Text);
  WriteLn('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver1.dc5', 'test', DeCoder4X_GetDefaultParameters(fvDc40), OnProgressProc);
  OutputFile := 'schloss_decoded_dc5_ver1.bmp';
  DeCoder4X_DecodeFile('schloss_ver1.dc5', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver1.bmp'));
  fi := DeCoder4X_FileInfo('schloss_ver1.dc5', '', OnProgressProc);
  DeleteFile('schloss_decoded_dc5_ver1.bmp');
  DeleteFile('schloss_ver1.dc5');
  WriteLn('DC40 OK:');
  sl.Clear;
  DeCoder4X_PrintFileInfo(fi, sl);
  WriteLn(sl.Text);
  WriteLn('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver2.dc5', 'test', DeCoder4X_GetDefaultParameters(fvDc41Beta), OnProgressProc);
  OutputFile := 'schloss_decoded_dc5_ver2.bmp';
  DeCoder4X_DecodeFile('schloss_ver2.dc5', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver2.bmp'));
  fi := DeCoder4X_FileInfo('schloss_ver2.dc5', '', OnProgressProc);
  DeleteFile('schloss_decoded_dc5_ver2.bmp');
  DeleteFile('schloss_ver2.dc5');
  WriteLn('DC41 Beta OK:');
  sl.Clear;
  DeCoder4X_PrintFileInfo(fi, sl);
  WriteLn(sl.Text);
  WriteLn('');

  fp := DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled);
  fp.ContainFileOrigName := fpEncryptWithUserKey;
  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver3.dc5', 'test', fp, OnProgressProc);
  OutputFile := 'schloss_decoded_dc5_ver3.bmp';
  DeCoder4X_DecodeFile('schloss_ver3.dc5', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
  fi := DeCoder4X_FileInfo('schloss_ver3.dc5', '', OnProgressProc);
  DeleteFile('schloss_decoded_dc5_ver3.bmp');
  DeleteFile('schloss_ver3.dc5');
  WriteLn('DC41 Final OK (Filename encrypted):');
  sl.Clear;
  DeCoder4X_PrintFileInfo(fi, sl);
  WriteLn(sl.Text);
  WriteLn('');

  fp := DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled);
  fp.ContainFileOrigName := fpExpose;
  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver3.dc5', 'test', fp, OnProgressProc);
  OutputFile := 'schloss_decoded_dc5_ver3.bmp';
  DeCoder4X_DecodeFile('schloss_ver3.dc5', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
  fi := DeCoder4X_FileInfo('schloss_ver3.dc5', '', OnProgressProc);
  DeleteFile('schloss_decoded_dc5_ver3.bmp');
  DeleteFile('schloss_ver3.dc5');
  WriteLn('DC41 Final OK (Filename not encrypted):');
  sl.Clear;
  DeCoder4X_PrintFileInfo(fi, sl);
  WriteLn(sl.Text);
  WriteLn('');

  for v in [fvDc50, fvDc51] do
  begin
    fp := DeCoder4X_GetDefaultParameters(v);
    fp.ContainFileOrigName := fpHide;
    fp.ContainFileOrigSize := false;
    fp.ContainFileOrigDate := false;
    DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver'+IntToStr(Integer(v))+'.dc5', 'test', fp, OnProgressProc);
    OutputFile := 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp';
    DeCoder4X_DecodeFile('schloss_ver'+IntToStr(Integer(v))+'.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
    fi := DeCoder4X_FileInfo('schloss_ver'+IntToStr(Integer(v))+'.dc5', '', OnProgressProc);
    DeleteFile('schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp');
    DeleteFile('schloss_ver'+IntToStr(Integer(v))+'.dc5');
    if v = fvDc50 then Write('DC50') else if v = fvDc51 then Write('DC51') else Write('DC??');
    WriteLn(' OK (without file name/date/time):');
    sl.Clear;
    DeCoder4X_PrintFileInfo(fi, sl);
    WriteLn(sl.Text);
    WriteLn('');

    fp := DeCoder4X_GetDefaultParameters(v);
    fp.ContainFileOrigName := fpExpose;
    fp.ContainFileOrigSize := true;
    fp.ContainFileOrigDate := true;
    DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver'+IntToStr(Integer(v))+'.dc5', 'test', fp, OnProgressProc);
    OutputFile := 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp';
    DeCoder4X_DecodeFile('schloss_ver'+IntToStr(Integer(v))+'.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
    fi := DeCoder4X_FileInfo('schloss_ver'+IntToStr(Integer(v))+'.dc5', '', OnProgressProc);
    DeleteFile('schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp');
    DeleteFile('schloss_ver'+IntToStr(Integer(v))+'.dc5');
    if v = fvDc50 then Write('DC50') else if v = fvDc51 then Write('DC51') else Write('DC??');
    WriteLn(' OK (with file name/date/time):');
    sl.Clear;
    DeCoder4X_PrintFileInfo(fi, sl);
    WriteLn(sl.Text);
    WriteLn('');

    fp := DeCoder4X_GetDefaultParameters(v);
    fp.CipherClass := TCipher_AES128;
    fp.CipherMode := cmGCM;
    fp.GCMAuthTagSizeInBytes := 16;
    DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver'+IntToStr(Integer(v))+'.dc5', 'test', fp, OnProgressProc);
    OutputFile := 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp';
    fi := DeCoder4X_DecodeFile('schloss_ver'+IntToStr(Integer(v))+'.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
    DeleteFile('schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp');
    DeleteFile('schloss_ver'+IntToStr(Integer(v))+'.dc5');
    if v = fvDc50 then Write('DC50') else if v = fvDc51 then Write('DC51') else Write('DC??');
    WriteLn(' GCM OK:');
    sl.Clear;
    DeCoder4X_PrintFileInfo(fi, sl);
    WriteLn(sl.Text);
    WriteLn('');

    fp := DeCoder4X_GetDefaultParameters(v);
    fp.CipherClass := TCipher_Blowfish;
    fp.IVSizeInBytes := fp.CipherClass.Context.BufferSize;
    DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver'+IntToStr(Integer(v))+'.dc5', 'test', fp, OnProgressProc);
    OutputFile := 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp';
    fi := DeCoder4X_DecodeFile('schloss_ver'+IntToStr(Integer(v))+'.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
    DeleteFile('schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp');
    DeleteFile('schloss_ver'+IntToStr(Integer(v))+'.dc5');
    if v = fvDc50 then Write('DC50') else if v = fvDc51 then Write('DC51') else Write('DC??');
    WriteLn(' Blowfish OK:');
    sl.Clear;
    DeCoder4X_PrintFileInfo(fi, sl);
    WriteLn(sl.Text);
    WriteLn('');

    fp := DeCoder4X_GetDefaultParameters(v);
    fp.CipherClass := TCipher_Blowfish;
    fp.CipherMode := cmECBx;
    fp.IVSizeInBytes := fp.CipherClass.Context.BufferSize;
    DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver'+IntToStr(Integer(v))+'.dc5', 'test', fp, OnProgressProc);  // works only because ZLib provided a size that can be divided by 8.
    OutputFile := 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp';
    fi := DeCoder4X_DecodeFile('schloss_ver'+IntToStr(Integer(v))+'.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
    DeleteFile('schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp');
    DeleteFile('schloss_ver'+IntToStr(Integer(v))+'.dc5');
    if v = fvDc50 then Write('DC50') else if v = fvDc51 then Write('DC51') else Write('DC??');
    WriteLn(' Blowfish ECB OK:');
    sl.Clear;
    DeCoder4X_PrintFileInfo(fi, sl);
    WriteLn(sl.Text);
    WriteLn('');
  end;

  DeleteFile('schloss_decoded.bmp');

  WriteLn('All testcases passed');
end;


initialization
  TDUnitX.RegisterTestFixture(TDecoderTestObject);

end.
