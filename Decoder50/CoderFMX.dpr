program CoderFMX;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Styles,
  System.SysUtils,
  System.IOUtils,
  DecoderFmxMain in 'DecoderFmxMain.pas' {DecoderMainForm},
  DecoderEncDec in 'DecoderEncDec.pas',
  DecoderFuncs in 'DecoderFuncs.pas',
  DecoderOldCiphers in 'DecoderOldCiphers.pas',
  DecoderSevenZipUtils in 'DecoderSevenZipUtils.pas',
  DecoderConst in 'DecoderConst.pas';

{$R *.res}

begin
  // Styles taken from: c:\Program Files (x86)\Embarcadero\Studio\23.0\Redist\styles\Fmx\Transparent.style
  // and included using "Project => Resources and images"
  TStyleManager.TrySetStyleFromResource('FMX_STYLE');
  Application.Initialize;
  Application.CreateForm(TDecoderMainForm, DecoderMainForm);
  Application.Run;
end.
