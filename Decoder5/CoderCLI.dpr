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
  Cmd_DC10_Crypt = 'DC10_Crypt';
  Cmd_DC10_DeCrypt = 'DC10_DeCrypt';

begin
  try
    if SameText(ParamStr(1), Cmd_DC10_Crypt) then
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
    else
    begin
      WriteLn('ViaThinkSoft (De)Coder 5.0');
      WriteLn('');
      WriteLn('Parameters:');
      // DC10_DeCrypt TestData\dc10_example_in.txt TestData\dc10_example_out.tmp
      WriteLn(Format('- %s %s <InFile> <OutFile>', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC10_Crypt]));
      WriteLn(Format('- %s %s <InFile> <OutFile>', [Uppercase(ExtractFileName(ParamStr(0))), Cmd_DC10_DeCrypt]));
      ExitCode := 0;
    end;

    WriteLn('Done');
    Readln;
    { TODO -oUser -cConsole Main : Code hier einfügen }
  except
    on E: Exception do
    begin
      ExitCode := 1;
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
end.
