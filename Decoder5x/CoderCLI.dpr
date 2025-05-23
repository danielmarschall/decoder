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

resourcestring
  SFileSNotFound = 'File %s not found';
  SDirectorySNotFound = 'Directory %s not found';
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

procedure OnProgressProc(Size, Pos: Int64; const Task: string; State: TDcProgressState);
begin
  case State of
    TDcProgressState.Started:    Write(  #13 + Format('%6.2f',[0.00])+'% ... ' + Task);
    TDcProgressState.Processing: Write(  #13 + Format('%6.2f',[Pos/Size*100])+'% ... ' + Task);
    TDcProgressState.Finished:   WriteLn(#13 + Format('%6.2f',[100.00])+'% ... ' + Task + ' = Done');
  end;
end;

procedure CheckFileExists(const AFileName: string);
begin
  if not FileExists(AFileName) then
    raise Exception.CreateResFmt(@SFileSNotFound, [AFileName]);
end;

procedure CheckDirectoryExists(const AFileName: string);
begin
  if not DirectoryExists(AFileName) then
    raise Exception.CreateResFmt(@SDirectorySNotFound, [AFileName]);
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
  Cmd_Debug_ListAlgos = 'Debug_ListAlgos';
  Cmd_Debug_EntropyTest = 'Debug_EntropyTest';
  {$ENDIF}

{$REGION 'Debug methods'}
{$IFDEF Debug}

procedure Debug_ListAlgos;
var
  sl: TStringList;
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
    else if SameText(ParamStr(1), Cmd_Debug_ListAlgos) and (ParamCount = 1) then
    begin
      Debug_ListAlgos;
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
      WriteLn(Format('%s %-13s <InFile> <OutFile> <Password>  -- %s', [OwnName, Cmd_DC50_EnCrypt_WithInfo, Format(LoadResString(@SEncryptDecryptFilesAndFolders_2),[Cmd_DC50_EnCrypt_NoInfo])]));
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
      WriteLn(Format('%s %-13s          -- Run internal testcases from folder ..\TestData', [OwnName, Cmd_Debug_ListAlgos]));
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
