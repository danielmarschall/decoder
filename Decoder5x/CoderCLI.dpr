program CoderCLI;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DECTypes,
  DECCiphers,
  DECCipherBase,
  System.StrUtils,
  System.Classes,
  System.ZLib,
  Generics.Collections,
  DecoderEncDec in 'DecoderEncDec.pas',
  DecoderFuncs in 'DecoderFuncs.pas',
  DecoderOldCiphers in 'DecoderOldCiphers.pas',
  DecoderSevenZipUtils in 'DecoderSevenZipUtils.pas',
  DecoderConst in 'DecoderConst.pas';

procedure OnProgressProc(Size, Pos: Int64; const Task: string; State: TDcProgressState);
begin
  case State of
    TDcProgressState.Started:    Write(  #13 + Format('%6.2f',[0.00])+'% ... ' + Task);
    TDcProgressState.Processing: Write(  #13 + Format('%6.2f',[Pos/Size*100])+'% ... ' + Task);
    TDcProgressState.Finished:   WriteLn(#13 + Format('%6.2f',[100.00])+'% ... ' + Task + ' = Done');
  end;
end;

procedure CheckFileExists(const AFileName: string);
resourcestring
  SFileNotFound = 'File %s not found';
begin
  if not FileExists(AFileName) then
    raise Exception.CreateResFmt(@SFileNotFound, [AFileName]);
end;

procedure CheckDirectoryExists(const AFileName: string);
resourcestring
  SDirNotFound = 'Directory %s not found';
begin
  if not DirectoryExists(AFileName) then
    raise Exception.CreateResFmt(@SDirNotFound, [AFileName]);
end;

const
  Cmd_DC10_EnCrypt = 'DC10_EnCrypt';
  Cmd_DC10_DeCrypt = 'DC10_DeCrypt';
  Cmd_DC20_EnCrypt = 'DC20_EnCrypt';
  Cmd_DC20_DeCrypt = 'DC20_DeCrypt';
  Cmd_DC21_EnCrypt = 'DC21_EnCrypt';
  Cmd_DC21_DeCrypt = 'DC21_DeCrypt';
  Cmd_DC22_EnCrypt = 'DC22_EnCrypt';
  Cmd_DC22_DeCrypt = 'DC22_DeCrypt';
  Cmd_DC30_EnCrypt = 'DC30_EnCrypt';
  Cmd_DC30_DeCrypt = 'DC30_DeCrypt';
  Cmd_DC32_EnCrypt = 'DC32_EnCrypt';
  Cmd_DC32_DeCrypt = 'DC32_DeCrypt';
  Cmd_DC50_EnCrypt_NoInfo = 'DC50_EnCrypt';
  Cmd_DC50_EnCrypt_WithInfo = 'DC50_EnCrypt+';
  Cmd_DC50_DeCrypt = 'DC50_DeCrypt';
  Cmd_DC50_FileInfo = 'DC50_FileInfo';
  Cmd_SecureDeleteFile = 'DeleteFile';
  Cmd_SecureDeleteFolder = 'DeleteFolder';
  Cmd_Help = 'Help';
  {$IFDEF Debug}
  Cmd_Debug_Testcases = 'Debug_Testcases';
  Cmd_Debug_EntropyTest = 'Debug_EntropyTest';
  {$ENDIF}

{$REGION 'Debug methods'}
{$IFDEF Debug}

procedure Debug_Testcases;

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

var
  fp: TDC4Parameters;
  fi: TDC4FileInfo;
  sl: TStringList;
  OutputFile: string;
  v: TDc4FormatVersion;

begin
  sl := TStringList.Create;

  WriteLn('Reddmann Example Cipher Algos');
  sl.Clear;
  Debug_ListCipherAlgos(sl, fvHagenReddmannExample);
  WriteLn(sl.Text);
  WriteLn('');

  WriteLn('Reddmann Example Hash Algos');
  sl.Clear;
  Debug_ListHashAlgos(sl, fvHagenReddmannExample);
  WriteLn(sl.Text);
  WriteLn('');

  //WriteLn('DC 4.0 Cipher Algos');
  //sl.Clear;
  //Debug_ListCipherAlgos(sl, fvDc40);
  //WriteLn(sl.Text);
  //WriteLn('');

  //WriteLn('DC 4.0 Hash Algos');
  //sl.Clear;
  //Debug_ListHashAlgos(sl, fvDc40);
  //WriteLn(sl.Text);
  //WriteLn('');

  WriteLn('DC 4.1B Cipher Algos');
  sl.Clear;
  Debug_ListCipherAlgos(sl, fvDc41Beta);
  WriteLn(sl.Text);
  WriteLn('');

  WriteLn('DC 4.1B Hash Algos');
  sl.Clear;
  Debug_ListHashAlgos(sl, fvDc41Beta);
  WriteLn(sl.Text);
  WriteLn('');

  WriteLn('DC 4.1F Cipher Algos');
  sl.Clear;
  Debug_ListCipherAlgos(sl, fvDc41FinalCancelled);
  WriteLn(sl.Text);
  WriteLn('');

  WriteLn('DC 4.1F Hash Algos');
  sl.Clear;
  Debug_ListHashAlgos(sl, fvDc41FinalCancelled);
  WriteLn(sl.Text);
  WriteLn('');

  WriteLn('DC 5.x Cipher Algos');
  sl.Clear;
  Debug_ListCipherAlgos(sl, fvDc50);
  WriteLn(sl.Text);
  WriteLn('');

  WriteLn('DC 5.x Hash Algos');
  sl.Clear;
  Debug_ListHashAlgos(sl, fvDc50);
  WriteLn(sl.Text);
  WriteLn('');

  for v := Low(TDc4FormatVersion) to High(TDc4FormatVersion) do
  begin
    DeCoder4X_ValidateParameterBlock(DeCoder4X_GetDefaultParameters(v));
    WriteLn('DeCoder4X_ValidateParameterBlock v'+IntToStr(Integer(v))+' OK');
  end;
  WriteLn('');

  DeCoder10_EncodeFile('..\TestData\dc10_example_in.txt', '..\TestData\dc10_example_out.tmp', True, OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc10_example_out.tmp', '..\TestData\dc10_example_out.txt'));
  DeleteFile('..\TestData\dc10_example_out.tmp');
  WriteLn('DC10 Encode OK');
  WriteLn('');

  DeCoder10_DecodeFile('..\TestData\dc10_example_out.txt', '..\TestData\dc10_example_in.tmp', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc10_example_in.tmp', '..\TestData\dc10_example_in.txt'));
  DeleteFile('..\TestData\dc10_example_in.tmp');
  WriteLn('DC10 Decode OK');
  WriteLn('');

  DeCoder20_EncodeFile('..\TestData\dc20_256zero_in.txt', '..\TestData\dc20_256zero_out.tmp', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc20_256zero_out.txt', '..\TestData\dc20_256zero_out.tmp'));
  DeleteFile('..\TestData\dc20_256zero_out.tmp');
  DeCoder20_EncodeFile('..\TestData\dc20_test_in.txt', '..\TestData\dc20_test_out.tmp', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc20_test_out.txt', '..\TestData\dc20_test_out.tmp'));
  DeleteFile('..\TestData\dc20_test_out.tmp');
  WriteLn('DC20 Encode OK');
  WriteLn('');

  DeCoder22_EncodeFile('..\TestData\dc22_256zero_in.txt', '..\TestData\dc22_256zero_out_61.tmp', 61, OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc22_256zero_out_61.txt', '..\TestData\dc22_256zero_out_61.tmp'));
  DeleteFile('..\TestData\dc22_256zero_out_61.tmp');
  DeCoder22_EncodeFile('..\TestData\dc22_test_in.txt', '..\TestData\dc22_test_out_61.tmp', 61, OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc22_test_out_61.txt', '..\TestData\dc22_test_out_61.tmp'));
  DeleteFile('..\TestData\dc22_test_out_61.tmp');
  WriteLn('DC22 Encode OK');
  WriteLn('');

  DeCoder30_EncodeFile('..\TestData\dc30_256zero_in.txt', '..\TestData\dc30_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc30_256zero_out_foobar.txt', '..\TestData\dc30_256zero_out_foobar.tmp'));
  DeleteFile('..\TestData\dc30_256zero_out_foobar.tmp');
  DeCoder30_EncodeFile('..\TestData\dc30_test_in.txt', '..\TestData\dc30_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc30_test_out_foobar.txt', '..\TestData\dc30_test_out_foobar.tmp'));
  DeleteFile('..\TestData\dc30_test_out_foobar.tmp');
  WriteLn('DC30 Encode OK');
  WriteLn('');

  DeCoder32_EncodeFile('..\TestData\dc32_256zero_in.txt', '..\TestData\dc32_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc32_256zero_out_foobar.txt', '..\TestData\dc32_256zero_out_foobar.tmp'));
  DeleteFile('..\TestData\dc32_256zero_out_foobar.tmp');
  DeCoder32_EncodeFile('..\TestData\dc32_256zero_in.txt', '..\TestData\dc32_256zero_out_abcdefg.tmp', 'abcdefg', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc32_256zero_out_abcdefg.txt', '..\TestData\dc32_256zero_out_abcdefg.tmp'));
  DeleteFile('..\TestData\dc32_256zero_out_abcdefg.tmp');
  DeCoder32_EncodeFile('..\TestData\dc32_test_in.txt', '..\TestData\dc32_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('..\TestData\dc32_test_out_foobar.txt', '..\TestData\dc32_test_out_foobar.tmp'));
  DeleteFile('..\TestData\dc32_test_out_foobar.tmp');
  WriteLn('DC32 Encode OK');
  WriteLn('');

  OutputFile := 'schloss_decoded.bmp';
  DeCoder4X_DecodeFile('..\TestData\schloss.dc4', OutputFile, 'test', OnProgressProc);
  WriteLn('Decode DC41 Beta OK');
  WriteLn('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver0.dc5', 'test', DeCoder4X_GetDefaultParameters(fvHagenReddmannExample), OnProgressProc);
  OutputFile := 'schloss_decoded_dc5_ver0.bmp';
  DeCoder4X_DecodeFile('schloss_ver0.dc5', OutputFile, 'test', OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver0.bmp'));
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
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver1.bmp'));
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
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver2.bmp'));
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
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
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
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
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
    Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
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
    Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
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
    Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
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
    Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
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
    Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver'+IntToStr(Integer(v))+'.bmp'));
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

procedure Debug_EntroyTest(const ADirToTest, ACsvOutPutFile: string);

  type
    TEntropyRatio = record
      entropySum: Extended;
      ratioSum: Extended;
      num: integer;
    end;

  var
    FileExtAnalysis: TDictionary<string, TEntropyRatio>;

  function ZLibCompressRatio(const InputFileName: string): Extended;
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

  procedure AnalyzeFile(const AFileName: string);
  var
    entropy, ratio: Extended;
    er: TEntropyRatio;
    FileExt: string;
  begin
    WriteLn(AFileName);
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
    WriteLn(AFileName+#9+ExtractfileExt(AFileName)+#9+FloatTostr(entropy)+#9+FloatTostr(ratio));
  end;

  procedure AnalyzeDir(const DirName: string);
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
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                WriteLn(IncludeTrailingPathDelimiter(dirName)+searchResult.Name+#9+E.Message);
              end;
            end;
            //end;
          end else if (searchResult.Name<>'.') and (searchResult.Name<>'..') then begin
            AnalyzeDir(IncludeTrailingPathDelimiter(dirName)+searchResult.Name);
          end;
        until FindNext(searchResult)<>0
      finally
        System.SysUtils.FindClose(searchResult);
      end;
    end;
    WriteLn('Done');
  end;

var
  sl: TStringList;
begin
  FileExtAnalysis := TDictionary<string, TEntropyRatio>.Create;
  try
    AnalyzeDir(ADirToTest);

    sl := TStringList.Create;
    try
      sl.Add('File Ext'+#9+'EntropyAvg'+#9+'RatioAvg'+#9+'Num');
      for var Enum in FileExtAnalysis do
      begin
        sl.Add(Enum.Key + #9 + FloatToStr(Enum.Value.entropySum/Enum.Value.num) + #9 + FloatToStr(Enum.Value.ratioSum/Enum.Value.num) + #9 + IntToStr(Enum.Value.num));
      end;
      sl.SaveToFile(ACsvOutPutFile);
    finally
      FreeAndNil(sl);
    end;

  finally
    FreeAndNil(FileExtAnalysis);
  end;
end;

{$ENDIF}
{$ENDREGION}

procedure TextCallback(const Text: string);
begin
  WriteLn(Text);
end;

const
  DeleteFolderCountDown = 3;

var
  iKey: integer;
  fp: TDC4Parameters;
  fi: TDC4FileInfo;
  sl: TStringList;
  OutputFile: string;
  OwnName: string;

resourcestring
  SCautionDeleteFolder = 'Caution! You are about to delete this folder and all of its contents:';
  SCautionDeleteFolderCountdown_D = 'Press Ctrl+C to cancel or wait to continue ... %d seconds';
  SProductName = 'ViaThinkSoft (De)Coder 5.1';
  SVersion_S = 'Version %s';
  SDevelopedByDanielMarschall = 'Developed by Daniel Marschall';
  SDMHomepage = 'www.daniel-marschall.de';
  SLicenseLine = 'FREEWARE - Licensed under the terms of the Apache 2.0 License';
  SEncryptDecryptFilesAndFolders = 'Encrypting and decrypting files or folders';
  SEncryptDecryptFilesAndFolders_1 = 'Encrypts a file using (De)Coder 5.x';
  SEncryptDecryptFilesAndFolders_2 = 'Same as %s, but with metadata name+size+date';
  SEncryptDecryptFilesAndFolders_3 = 'Decrypts a (De)Coder 4.x or 5.x encrypted file';
  SEncryptDecryptFilesAndFolders_4 = 'Shows details of a (De)Coder 4.x or 5.x encrypted file';
  SSupportLegacyFormats = 'Support for legacy file formats';
  SSupportLegacyFormats_1 = 'Encrypts a file using the (De)Coder 1.0 format (INSECURE)';
  SSupportLegacyFormats_2 = 'Decrypts a file using the (De)Coder 1.0 format (INSECURE)';
  SSupportLegacyFormats_3 = 'Encrypts a file using the (De)Coder 2.0 format (INSECURE)';
  SSupportLegacyFormats_4 = 'Decrypts a file using the (De)Coder 2.0 format (INSECURE)';
  SSupportLegacyFormats_5 = 'Encrypts a file using the (De)Coder 2.1 format (INSECURE)';
  SSupportLegacyFormats_6 = 'Decrypts a file using the (De)Coder 2.1 format (INSECURE)';
  SSupportLegacyFormats_7 = 'Encrypts a file using the (De)Coder 2.2 format (INSECURE)';
  SSupportLegacyFormats_8 = 'Decrypts a file using the (De)Coder 2.2 format (INSECURE)';
  SSupportLegacyFormats_9 = 'Encrypts a file using the (De)Coder 3.0 format (INSECURE)';
  SSupportLegacyFormats_10 = 'Decrypts a file using the (De)Coder 3.0 format (INSECURE)';
  SSupportLegacyFormats_11 = 'Encrypts a file using the (De)Coder 3.2 format (INSECURE)';
  SSupportLegacyFormats_12 = 'Decrypts a file using the (De)Coder 3.2 format (INSECURE)';
  SExtras = 'Extras';
  SExtras_1 = 'Wipes a file from a disk in a secure way';
  SExtras_2 = 'Wipes a complete folder from a disk in a secure way';
  SExtras_3 = 'Shows this command listing';
  SError_S = 'ERROR: %s';
  SExitCode_D = 'Exit code: %d';
  SPressAnyKey = 'Press any key to exit ...';
  SInfoLegacyDecrypt = 'Please CHECK if the output file is what you expect. (With this legacy file format version, there is no possibility for (De)Coder to check if algorithm or password was okay.)';
  SInfoLegacyEncrypt = 'WARNING: Encrypting files with this legacy algorithm is EXTREMELY insecure!';

begin
  try
    {$REGION '(De)Coder 1.0'}
    if SameText(ParamStr(1), Cmd_DC10_EnCrypt) and (ParamCount = 3) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder10_EncodeFile(ParamStr(2), ParamStr(3), false, OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyEncrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC10_DeCrypt) and (ParamCount = 3) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder10_DecodeFile(ParamStr(2), ParamStr(3), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyDecrypt));
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION '(De)Coder 2.x'}
    else if SameText(ParamStr(1), Cmd_DC20_EnCrypt) and (ParamCount = 3) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder20_EncodeFile(ParamStr(2), ParamStr(3), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyEncrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC20_DeCrypt) and (ParamCount = 3) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder20_DecodeFile(ParamStr(2), ParamStr(3), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyDecrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC21_EnCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder21_EncodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyEncrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC21_DeCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder21_DecodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyDecrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC22_EnCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder22_EncodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyEncrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC22_DeCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder22_DecodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyDecrypt));
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION '(De)Coder 3.x'}
    else if SameText(ParamStr(1), Cmd_DC30_EnCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder30_EncodeFile(ParamStr(2), ParamStr(3), AnsiString(ParamStr(4)), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyEncrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC30_DeCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder30_DecodeFile(ParamStr(2), ParamStr(3), AnsiString(ParamStr(4)), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyDecrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC32_EnCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder32_EncodeFile(ParamStr(2), ParamStr(3), AnsiString(ParamStr(4)), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyEncrypt));
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC32_DeCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder32_DecodeFile(ParamStr(2), ParamStr(3), AnsiString(ParamStr(4)), OnProgressProc);
      WriteLn(LoadResString(@SInfoLegacyDecrypt));
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION '(De)Coder 4.x / 5.x'}
    else if SameText(ParamStr(1), Cmd_DC50_EnCrypt_NoInfo) and (ParamCount = 4) then
    begin
      fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
      fp.ContainFileOrigName := fpHide;
      fp.ContainFileOrigSize := false;
      fp.ContainFileOrigDate := false;
      DeCoder4X_EncodeFile(ParamStr(2), ParamStr(3), ParamStr(4), fp, OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC50_EnCrypt_WithInfo) and (ParamCount = 4) then
    begin
      fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
      fp.ContainFileOrigName := fpExpose;
      fp.ContainFileOrigSize := true;
      fp.ContainFileOrigDate := true;
      DeCoder4X_EncodeFile(ParamStr(2), ParamStr(3), ParamStr(4), fp, OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC50_DeCrypt) and (ParamCount = 4) then
    begin
      CheckFileExists(ParamStr(2));
      OutputFile := ParamStr(3);
      DeCoder4X_DecodeFile(ParamStr(2), OutputFile, ParamStr(4), OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC50_FileInfo) and (ParamCount = 2) then
    begin
      CheckFileExists(ParamStr(2));
      fi := DeCoder4X_FileInfo(ParamStr(2), '', nil{OnProgressProc}); // no progress bar, because people might want to pipe the output to a file
      sl := TStringList.Create;
      try
        DeCoder4X_PrintFileInfo(fi, sl);
        WriteLn(sl.Text);
      finally
        FreeAndNil(sl);
      end;
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION 'Debug commands'}
    {$IFDEF Debug}
    else if SameText(ParamStr(1), Cmd_Debug_Testcases) and (ParamCount = 1) then
    begin
      Debug_Testcases;
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_Debug_EntropyTest) and (ParamCount = 3) then
    begin
      CheckDirectoryExists(ParamStr(2));
      Debug_EntroyTest(ParamStr(2), ParamStr(3));
      ExitCode := 0;
    end
    {$ENDIF}
    {$ENDREGION}
    {$REGION 'Extras'}
    else if SameText(ParamStr(1), Cmd_SecureDeleteFile) and (ParamCount = 2) then
    begin
      CheckFileExists(ParamStr(2));
      SecureDeleteFile(ParamStr(2), TextCallback);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_SecureDeleteFolder) and (ParamCount = 2) then
    begin
      CheckDirectoryExists(ParamStr(2));
      WriteLn(LoadResString(@SCautionDeleteFolder));
      WriteLn(RelToAbs(ParamStr(2)));
      WriteLn('');
      CountDown(LoadResString(@SCautionDeleteFolderCountdown_D), DeleteFolderCountDown);
      WriteLn('');
      SecureDeleteFolder(ParamStr(2), TextCallback);
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION 'Illegal usage / Help page'}
    else
    begin
      OwnName := ChangeFileExt(Uppercase(ExtractFileName(ParamStr(0))),'');

      WriteLn(Format('%-35s %s', [LoadResString(@SProductName), Format(LoadResString(@SVersion_S), [GetDecoderVersion])]));
      WriteLn(Format('%-35s %s', [LoadResString(@SDevelopedByDanielMarschall), LoadResString(@SDMHomepage)]));
      WriteLn(Format(LoadResString(@SLicenseLine), []));
      WriteLn('');

      WriteLn('=== ' + LoadResString(@SEncryptDecryptFilesAndFolders) + ' ===');
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC50_EnCrypt_NoInfo, LoadResString(@SEncryptDecryptFilesAndFolders_1)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC50_EnCrypt_WithInfo, Cmd_DC50_EnCrypt_NoInfo, LoadResString(@SEncryptDecryptFilesAndFolders_2)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC50_DeCrypt, LoadResString(@SEncryptDecryptFilesAndFolders_3)]));
      WriteLn(Format('%s %-13s <InFile>                       -- %s', [OwnName, Cmd_DC50_FileInfo, LoadResString(@SEncryptDecryptFilesAndFolders_4)]));
      WriteLn('');

      WriteLn('=== ' + LoadResString(@SSupportLegacyFormats) + ' ===');
      WriteLn(Format('%s %-13s <InFile> <OutFile>             -- %s', [OwnName, Cmd_DC10_EnCrypt, LoadResString(@SSupportLegacyFormats_1)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile>             -- %s', [OwnName, Cmd_DC10_DeCrypt, LoadResString(@SSupportLegacyFormats_2)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile>             -- %s', [OwnName, Cmd_DC20_EnCrypt, LoadResString(@SSupportLegacyFormats_3)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile>             -- %s', [OwnName, Cmd_DC20_DeCrypt, LoadResString(@SSupportLegacyFormats_4)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Key1..255> -- %s', [OwnName, Cmd_DC21_EnCrypt, LoadResString(@SSupportLegacyFormats_5)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Key1..255> -- %s', [OwnName, Cmd_DC21_DeCrypt, LoadResString(@SSupportLegacyFormats_6)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Key1..256> -- %s', [OwnName, Cmd_DC22_EnCrypt, LoadResString(@SSupportLegacyFormats_7)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Key1..256> -- %s', [OwnName, Cmd_DC22_DeCrypt, LoadResString(@SSupportLegacyFormats_8)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC30_EnCrypt, LoadResString(@SSupportLegacyFormats_9)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC30_DeCrypt, LoadResString(@SSupportLegacyFormats_10)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC32_EnCrypt, LoadResString(@SSupportLegacyFormats_11)]));
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC32_DeCrypt, LoadResString(@SSupportLegacyFormats_12)]));
      WriteLn('');

      WriteLn('=== ' + LoadResString(@SExtras) + ' ===');
      WriteLn(Format('%s %-13s <File>     -- %s', [OwnName, Cmd_SecureDeleteFile, LoadResString(@SExtras_1)]));
      WriteLn(Format('%s %-13s <Folder>   -- %s', [OwnName, Cmd_SecureDeleteFolder, LoadResString(@SExtras_2)]));
      {$IFDEF Debug}
      WriteLn(Format('%s %-13s          -- Run internal testcases from folder ..\TestData', [OwnName, Cmd_Debug_Testcases]));
      WriteLn(Format('%s %-13s <DirName> <CSVResultFile> -- Run entropy test on directory', [OwnName, Cmd_Debug_EntropyTest]));
      {$ENDIF}
      WriteLn(Format('%s %-13s            -- %s', [OwnName, Cmd_Help, LoadResString(@SExtras_3)]));

      if ParamCount = 0 then
      begin
        ExitCode := 0;
        WriteLn('');
        Write(LoadResString(@SPressAnyKey));
        ReadLn;
        WriteLn('');
      end
      else if (ParamCount=1) and (SameText(ParamStr(1), Cmd_Help) or SameText(ParamStr(1), '--help') or SameText(ParamStr(1), '/?')) then
        ExitCode := 0
      else
        ExitCode := 1;
    end;
    {$ENDREGION}
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      ExitCode := 1;
      WriteLn(Format(LoadResString(@SError_S), [E.Message]));
      WriteLn('');
    end;
  end;

  {$IFDEF MsWindows}
  {$WARN SYMBOL_PLATFORM OFF}
  if DebugHook <> 0 then
  begin
    WriteLn('');
    WriteLn('');
    WriteLn(Format(LoadResString(@SExitCode_D), [ExitCode]));
    WriteLn('');
    Write(LoadResString(@SPressAnyKey));
    ReadLn;
    WriteLn('');
  end;
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
end.
