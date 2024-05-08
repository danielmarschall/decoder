program Coder;

uses
  Vcl.Forms,
  DecoderMain in 'DecoderMain.pas' {FormMain},
  DecoderEncDec in 'DecoderEncDec.pas',
  DecoderOldCiphers in 'DecoderOldCiphers.pas',
  DecoderFuncs in 'DecoderFuncs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
