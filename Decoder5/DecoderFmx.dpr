program DecoderFmx;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Styles,
  DecoderFmxMain in 'DecoderFmxMain.pas' {Form3},
  DecoderEncDec in 'DecoderEncDec.pas',
  DecoderFuncs in 'DecoderFuncs.pas',
  DecoderOldCiphers in 'DecoderOldCiphers.pas';

{$R *.res}

begin
  // Styles here: c:\Program Files (x86)\Embarcadero\Studio\23.0\Redist\styles\Fmx\
  TStyleManager.SetStyleFromFile('Transparent.style');
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
