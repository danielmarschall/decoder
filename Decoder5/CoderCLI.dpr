program CoderCLI;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  DECTypes,
  StrUtils,
  DecoderEncDec in 'DecoderEncDec.pas',
  DecoderFuncs in 'DecoderFuncs.pas',
  DecoderOldCiphers in 'DecoderOldCiphers.pas';

procedure OnProgressProc(Size, Pos: Int64; State: TDECProgressState);
begin
  case State of
    Started:    Write(  #13 + Format('%6.2f',[0.00])+'% ...');
    Processing: Write(  #13 + Format('%6.2f',[Pos/Size*100])+'% ...');
    Finished:   WriteLn(#13 + Format('%6.2f',[100.00])+'% ... Done');
  end;
end;

procedure CheckFileExists(AFileName: string);
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File %s not found', [AFileName]);
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

var
  iKey: integer;

begin
  try
    {$REGION '(De)Coder 1.0'}
    if SameText(ParamStr(1), Cmd_DC10_EnCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder10_EncodeFile(ParamStr(2), ParamStr(3), false, OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC10_DeCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder10_DecodeFile(ParamStr(2), ParamStr(3), OnProgressProc);
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION '(De)Coder 2.0'}
    else if SameText(ParamStr(1), Cmd_DC20_EnCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder20_EncodeFile(ParamStr(2), ParamStr(3), OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC20_DeCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      DeCoder20_DecodeFile(ParamStr(2), ParamStr(3), OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC21_EnCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder21_EncodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC21_DeCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder21_DecodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC22_EnCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder22_EncodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      ExitCode := 0;
    end
    else if SameText(ParamStr(1), Cmd_DC22_DeCrypt) then
    begin
      CheckFileExists(ParamStr(2));
      if not TryStrToInt(ParamStr(4), iKey) then iKey := -1;
      DeCoder22_DecodeFile(ParamStr(2), ParamStr(3), iKey, OnProgressProc);
      ExitCode := 0;
    end
    {$ENDREGION}
    {$REGION 'Help page'}
    else
    begin
      WriteLn('ViaThinkSoft (De)Coder 5.0');
      WriteLn('Built ' + DateTimeToStr(GetOwnBuildTimestamp));
      WriteLn('');
      WriteLn('Parameters:');
      WriteLn(Format('- %s %s <InFile> <OutFile> -- Encrypts files using the (De)Coder 1.0 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC10_EnCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> -- Decrypts files using the (De)Coder 1.0 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC10_DeCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> -- Encrypts files using the (De)Coder 2.0 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC20_EnCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> -- Decrypts files using the (De)Coder 2.0 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC20_DeCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> <Key> -- Encrypts files using the (De)Coder 2.1 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC21_EnCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> <Key> -- Decrypts files using the (De)Coder 2.1 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC21_DeCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> <Key> -- Encrypts files using the (De)Coder 2.2 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC22_EnCrypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile> <Key> -- Decrypts files using the (De)Coder 2.2 format (INSECURE)', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC22_DeCrypt]));
      ExitCode := 0;
    end;
    {$ENDREGION}

    {$IFDEF MsWindows}
    {$WARN SYMBOL_PLATFORM OFF}
    if DebugHook <> 0 then
    begin
      WriteLn('');
      WriteLn('Press any key to continue...');
      ReadLn;
    end;
    {$WARN SYMBOL_PLATFORM ON}
    {$ENDIF}
  except
    on E: Exception do
    begin
      ExitCode := 1;
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
end.
