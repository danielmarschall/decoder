unit TestUnit1;

interface

uses
  SysUtils, Classes, DUnitX.TestFramework;

type
  [TestFixture]
  TDecoderTestObject = class
  public
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
    procedure Test_DeCoder4X_ValidateDefaultParameterBlock;
    [Test]
    procedure Test_DeCoder40_Ver0_FileInfo;
    [Test]
    procedure Test_DeCoder40_Ver0_Decode;
    [Test]
    procedure Test_DeCoder40_Ver0_Encode;
    [Test]
    procedure Test_DeCoder40_Ver1_FileInfo;
    [Test]
    procedure Test_DeCoder40_Ver1_Decode;
    [Test]
    procedure Test_DeCoder40_Ver1_Encode;
    [Test]
    procedure Test_DeCoder41_Ver2_FileInfo;
    [Test]
    procedure Test_DeCoder41_Ver2_Decode;
    [Test]
    procedure Test_DeCoder41_Ver2_Encode;
    [Test]
    procedure Test_DeCoder41_Ver3_Enc_FileInfo;
    [Test]
    procedure Test_DeCoder41_Ver3_Enc_Decode;
    [Test]
    procedure Test_DeCoder41_Ver3_Enc_Encode;
    [Test]
    procedure Test_DeCoder41_Ver3_Exp_FileInfo;
    [Test]
    procedure Test_DeCoder41_Ver3_Exp_Decode;
    [Test]
    procedure Test_DeCoder41_Ver3_Exp_Encode;
    [Test]
    procedure Test_Decoder50_Ver4_HideFilename_AES_CTSx_FileInfo;
    [Test]
    procedure Test_Decoder50_Ver4_HideFilename_AES_CTSx_Decode;
    [Test]
    procedure Test_Decoder50_Ver4_HideFilename_AES_CTSx_Encode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_FileInfo;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_Decode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_Encode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_FileInfo;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_Decode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_Encode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_FileInfo;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_Decode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_Encode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_FileInfo;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_Decode;
    [Test]
    procedure Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_Encode;
    [Test]
    procedure Test_Decoder51_Ver5_HideFilename_AES_CTSx_FileInfo;
    [Test]
    procedure Test_Decoder51_Ver5_HideFilename_AES_CTSx_Decode;
    [Test]
    procedure Test_Decoder51_Ver5_HideFilename_AES_CTSx_Encode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_FileInfo;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_Decode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_Encode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_FileInfo;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_Decode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_Encode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_FileInfo;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_Decode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_Encode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_FileInfo;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_Decode;
    [Test]
    procedure Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_Encode;
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

{$REGION 'Helper functions'}

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

{$ENDREGION}

{$REGION '(De)Coder 1.0'}

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

{$ENDREGION}

{$REGION '(De)Coder 2.0'}

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

{$ENDREGION}

{$REGION '(De)Coder 2.2'}

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

{$ENDREGION}

{$REGION '(De)Coder 3.0'}

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

{$ENDREGION}

{$REGION '(De)Coder 3.2'}

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

{$ENDREGION}

{$REGION '(De)Coder 4.x/5.x Validate Default Parameter Block'}

procedure TDecoderTestObject.Test_DeCoder4X_ValidateDefaultParameterBlock;
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

{$ENDREGION}

{$REGION 'Hagen Reddmann Example (Ver 0)'}

procedure TDecoderTestObject.Test_DeCoder40_Ver0_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc40_ver0_out.dc4', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_DeCoder40_Ver0_Encode;
var
  OutputFile: string;
begin
  try
    Test_DeCoder40_Ver0_Decode;
  except
    Assert.Fail('This test case requires that its Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  DeCoder4X_EncodeFile('..\TestData\dc40_ver0_in.bmp', 'encode.tmp', 'test', DeCoder4X_GetDefaultParameters(fvHagenReddmannExample), OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc40_ver0_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder40_Ver0_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc40_ver0_out.dc4', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc40_ver0_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 4.0 (Ver 1)'}

procedure TDecoderTestObject.Test_DeCoder40_Ver1_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc40_ver1_out.dc4', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_DeCoder40_Ver1_Encode;
var
  OutputFile: string;
begin
  try
    Test_DeCoder40_Ver1_Decode;
  except
    Assert.Fail('This test case requires that its Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  DeCoder4X_EncodeFile('..\TestData\dc40_ver1_in.bmp', 'encode.tmp', 'test', DeCoder4X_GetDefaultParameters(fvDc40), OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc40_ver1_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder40_Ver1_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc40_ver1_out.dc4', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc40_ver1_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 4.1 Beta (Ver 2)'}

procedure TDecoderTestObject.Test_DeCoder41_Ver2_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc41_ver2_out.dc4', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_DeCoder41_Ver2_Encode;
var
  OutputFile: string;
begin
  try
    Test_DeCoder41_Ver2_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  DeCoder4X_EncodeFile('..\TestData\dc41_ver2_in.bmp', 'encode.tmp', 'test', DeCoder4X_GetDefaultParameters(fvDc41Beta), OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc41_ver2_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder41_Ver2_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc41_ver2_out.dc4', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc41_ver2_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 4.1 Final/Cancelled (Ver 3), Filename exposed'}

procedure TDecoderTestObject.Test_DeCoder41_Ver3_Exp_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc41_ver3_exp_out.dc4', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_DeCoder41_Ver3_Exp_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_DeCoder41_Ver3_Exp_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled);
  fp.ContainFileOrigName := fpEncryptWithUserKey;
  DeCoder4X_EncodeFile('..\TestData\dc41_ver3_exp_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc41_ver3_exp_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder41_Ver3_Exp_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc41_ver3_exp_out.dc4', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc41_ver3_exp_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 4.1 Final/Cancelled (Ver 3), Filename encrypted'}

procedure TDecoderTestObject.Test_DeCoder41_Ver3_Enc_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc41_ver3_enc_out.dc4', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_DeCoder41_Ver3_Enc_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_DeCoder41_Ver3_Enc_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc41FinalCancelled);
  fp.ContainFileOrigName := fpEncryptWithUserKey;
  DeCoder4X_EncodeFile('..\TestData\dc41_ver3_enc_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc41_ver3_enc_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_DeCoder41_Ver3_Enc_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc41_ver3_enc_out.dc4', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc41_ver3_enc_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.0 (Ver 4), without file name/date/time'}

procedure TDecoderTestObject.Test_Decoder50_Ver4_HideFilename_AES_CTSx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc50_ver4_hid_aes_ctsx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_HideFilename_AES_CTSx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder50_Ver4_HideFilename_AES_CTSx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc50);
  fp.ContainFileOrigName := fpHide;
  fp.ContainFileOrigSize := false;
  fp.ContainFileOrigDate := false;
  DeCoder4X_EncodeFile('..\TestData\dc50_ver4_hid_aes_ctsx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_hid_aes_ctsx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_HideFilename_AES_CTSx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc50_ver4_hid_aes_ctsx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_hid_aes_ctsx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.0 (Ver 4), with file name/date/time'}

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc50_ver4_exp_aes_ctsx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc50);
  fp.ContainFileOrigName := fpExpose;
  fp.ContainFileOrigSize := true;
  fp.ContainFileOrigDate := true;
  DeCoder4X_EncodeFile('..\TestData\dc50_ver4_exp_aes_ctsx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_aes_ctsx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_AES_CTSx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc50_ver4_exp_aes_ctsx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_aes_ctsx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.0 (Ver 4), with AES128 GCM'}

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc50_ver4_exp_aes128_gcm_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc50);
  fp.CipherClass := TCipher_AES128;
  fp.CipherMode := cmGCM;
  fp.GCMAuthTagSizeInBytes := 16;
  DeCoder4X_EncodeFile('..\TestData\dc50_ver4_exp_aes128_gcm_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_aes128_gcm_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_AES128_GCM_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc50_ver4_exp_aes128_gcm_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_aes128_gcm_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.0 (Ver 4), with Blowfish CTSx'}

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc50_ver4_exp_blowfish_ctsx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc50);
  fp.CipherClass := TCipher_Blowfish;
  fp.IVSizeInBytes := fp.CipherClass.Context.BufferSize;
  DeCoder4X_EncodeFile('..\TestData\dc50_ver4_exp_blowfish_ctsx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_blowfish_ctsx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_Blowfish_CTSx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc50_ver4_exp_blowfish_ctsx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_blowfish_ctsx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.0 (Ver 4), with Blowfish ECBx'}

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc50_ver4_exp_blowfish_ecbx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc50);
  fp.CipherClass := TCipher_Blowfish;
  fp.CipherMode := cmECBx;
  fp.IVSizeInBytes := fp.CipherClass.Context.BufferSize;
  DeCoder4X_EncodeFile('..\TestData\dc50_ver4_exp_blowfish_ecbx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_blowfish_ecbx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder50_Ver4_ExposeFilename_Blowfish_ECBx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc50_ver4_exp_blowfish_ecbx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc50_ver4_exp_blowfish_ecbx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.1 (Ver 5), without file name/date/time'}

procedure TDecoderTestObject.Test_Decoder51_Ver5_HideFilename_AES_CTSx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc51_ver5_hid_aes_ctsx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_HideFilename_AES_CTSx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder51_Ver5_HideFilename_AES_CTSx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc51);
  fp.ContainFileOrigName := fpHide;
  fp.ContainFileOrigSize := false;
  fp.ContainFileOrigDate := false;
  DeCoder4X_EncodeFile('..\TestData\dc51_ver5_hid_aes_ctsx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_hid_aes_ctsx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_HideFilename_AES_CTSx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc51_ver5_hid_aes_ctsx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_hid_aes_ctsx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.1 (Ver 5), with file name/date/time'}

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc51_ver5_exp_aes_ctsx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc51);
  fp.ContainFileOrigName := fpExpose;
  fp.ContainFileOrigSize := true;
  fp.ContainFileOrigDate := true;
  DeCoder4X_EncodeFile('..\TestData\dc51_ver5_exp_aes_ctsx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_aes_ctsx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_AES_CTSx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc51_ver5_exp_aes_ctsx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_aes_ctsx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.1 (Ver 5), with AES128 GCM'}

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc51_ver5_exp_aes128_gcm_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc51);
  fp.CipherClass := TCipher_AES128;
  fp.CipherMode := cmGCM;
  fp.GCMAuthTagSizeInBytes := 16;
  DeCoder4X_EncodeFile('..\TestData\dc51_ver5_exp_aes128_gcm_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_aes128_gcm_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_AES128_GCM_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc51_ver5_exp_aes128_gcm_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_aes128_gcm_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.1 (Ver 5), with Blowfish CTSx'}

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc51_ver5_exp_blowfish_ctsx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc51);
  fp.CipherClass := TCipher_Blowfish;
  fp.IVSizeInBytes := fp.CipherClass.Context.BufferSize;
  DeCoder4X_EncodeFile('..\TestData\dc51_ver5_exp_blowfish_ctsx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_blowfish_ctsx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_Blowfish_CTSx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc51_ver5_exp_blowfish_ctsx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_blowfish_ctsx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

{$REGION '(De)Coder 5.1 (Ver 5), with Blowfish ECBx'}

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_FileInfo;
var
  fi: TDC4FileInfo;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    fi := DeCoder4X_FileInfo('..\TestData\dc51_ver5_exp_blowfish_ecbx_out.dc5', '', OnProgressProc);
    DeCoder4X_PrintFileInfo(fi, sl); // Note: This currently just checks for a crash
    Log(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_Encode;
var
  OutputFile: string;
  fp: TDC4Parameters;
begin
  try
    Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_Decode;
  except
    Assert.Fail('This test case requires that is Decode part passes');
  end;

  // Since an encrypted output file contains random parts, we cannot encode and compare with an already encoded file. We need to Encode+Decode+Compare
  // (requires that the Decode procedure is OK)
  fp := DeCoder4X_GetDefaultParameters(fvDc51);
  fp.CipherClass := TCipher_Blowfish;
  fp.CipherMode := cmECBx;
  fp.IVSizeInBytes := fp.CipherClass.Context.BufferSize;
  DeCoder4X_EncodeFile('..\TestData\dc51_ver5_exp_blowfish_ecbx_in.bmp', 'encode.tmp', 'test', fp, OnProgressProc);
  OutputFile := 'decode.tmp';
  DeCoder4X_DecodeFile('encode.tmp', OutputFile, 'test', OnProgressProc);
  Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_blowfish_ecbx_in.bmp'));

  DeleteFile('encode.tmp');
  DeleteFile('decode.tmp');
end;

procedure TDecoderTestObject.Test_Decoder51_Ver5_ExposeFilename_Blowfish_ECBx_Decode;
var
  OutputFile: string;
begin
  try
    OutputFile := 'decode.tmp';
    DeCoder4X_DecodeFile('..\TestData\dc51_ver5_exp_blowfish_ecbx_out.dc5', OutputFile, 'test', OnProgressProc);
    Assert.IsTrue(Are2FilesEqual('decode.tmp', '..\TestData\dc51_ver5_exp_blowfish_ecbx_in.bmp'));
  finally
    DeleteFile('decode.tmp');
  end;
end;

{$ENDREGION}

initialization
  TDUnitX.RegisterTestFixture(TDecoderTestObject);

end.
