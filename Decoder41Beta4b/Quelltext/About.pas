unit About;

interface

uses
  Windows, Messages, SysUtils, Forms, StdCtrls, ExtCtrls, Inifiles, shellapi,
  DCConst, Controls, Classes;

type
  TAboutForm = class(TForm)
    lbl_name: TLabel;
    img_schloss: TImage;
    lbl_version: TLabel;
    lbl_leader: TLabel;
    lbl_copyright: TLabel;
    spe_leftshape: TShape;
    gbx_lang: TGroupBox;
    lbl_lang_value: TLabel;
    lbl_translator_value: TLabel;
    lbl_revision_value: TLabel;
    btn_close: TButton;
    lbl_url: TLabel;
    lbl_lang_name: TLabel;
    lbl_translator_name: TLabel;
    lbl_revision_name: TLabel;
    procedure form_create(Sender: TObject);
    procedure btn_closeClick(Sender: TObject);
    procedure lbl_urlClick(Sender: TObject);
  private
    LangArray, LangArraySig: array of TLanguageEntry;
    function GetLangEntry(name: string): string;
    function GetLangSig(name: string): string;
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

function TAboutForm.GetLangEntry(name: string): string;
var
  i: integer;
begin
  for i := 0 to high(LangArray) do
  begin
    if LangArray[i].name = name then
    begin
      result := LangArray[i].text;
      break;
    end;
  end;
end;

function TAboutForm.GetLangSig(name: string): string;
var
  i: integer;
begin
  for i := 0 to high(LangArraySig) do
  begin
    if LangArraySig[i].name = name then
    begin
      result := LangArraySig[i].text;
      break;
    end;
  end;
end;

procedure TAboutForm.lbl_urlClick(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.viathinksoft.de/', '', '', sw_normal);
end;

procedure TAboutForm.btn_closeClick(Sender: TObject);
begin
  close;
end;

procedure TAboutForm.form_create(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i: integer;
  Y, M, D: Word;
begin
  // Sprachdatei & Signatur auslesen

  ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'Language.ini');
  str := TStringList.Create();
  try
    ini.ReadSection(Name, str);
    for i := 0 to str.count-1 do
    begin
      setlength(LangArray, length(LangArray)+1);
      LangArray[length(LangArray)-1].name := str.strings[i];
      LangArray[length(LangArray)-1].text := ini.ReadString(Name, str.strings[i], '?');
      LangArray[length(LangArray)-1].text := StringReplace(LangArray[length(LangArray)-1].text, '###', #13#10, [rfReplaceAll]);
    end;

    ini.ReadSection('Signature', str);
    for i := 0 to str.count-1 do
    begin
      setlength(LangArraySig, length(LangArraySig)+1);
      LangArraySig[length(LangArraySig)-1].name := str.strings[i];
      LangArraySig[length(LangArraySig)-1].text := ini.ReadString('Signature', str.strings[i], '?');
      LangArraySig[length(LangArraySig)-1].text := StringReplace(LangArraySig[length(LangArraySig)-1].text, '###', #13#10, [rfReplaceAll]);
    end;
  finally
    ini.free;
    str.Free;
  end;

  // Bilder

  icon.LoadFromResourceID(hInstance, 104);
  img_schloss.Picture.Bitmap.LoadFromResourceName(hInstance, 'SCHLOSS');

  // Texte

  Caption := GetLangEntry('about');
  lbl_translator_value.Caption := GetLangSig('Translator');
  lbl_lang_value.Caption := GetLangSig('Language');
  lbl_revision_value.Caption := GetLangSig('Revision');
  gbx_lang.Caption := GetLangEntry('installedlang');
  lbl_leader.Caption := Format(GetLangEntry('leader'), ['Daniel Marschall']);
  lbl_version.Caption :=  Format(GetLangEntry('version'), [DC4Ver]);
  lbl_name.Caption := '(De)Coder';
  lbl_url.Caption := 'www.viathinksoft.de';
  DecodeDate(Now(), Y, M, D);
  lbl_copyright.Caption := Format(GetLangEntry('copyright'), [2001, Y, 'ViaThinkSoft']);
  lbl_lang_name.Caption := GetLangEntry('lang');
  lbl_translator_name.Caption := GetLangEntry('translator');
  lbl_revision_name.Caption := GetLangEntry('revision');
  btn_close.Caption := GetLangEntry('ok');

  // Horizontal

  spe_leftshape.Width := img_schloss.Width + 16;
  lbl_name.Left := spe_leftshape.Left + spe_leftshape.Width + 8;
  lbl_url.left := lbl_name.left;
  lbl_version.left := lbl_url.left;
  lbl_leader.left := lbl_version.left;
  lbl_copyright.left := lbl_leader.left;
  gbx_lang.Left := lbl_copyright.left;
  gbx_lang.Width := max_4(lbl_name.width, lbl_version.width, lbl_leader.width, lbl_copyright.width);
  btn_close.Left := gbx_lang.left + gbx_lang.width - btn_close.width;
  lbl_lang_value.Left := lbl_lang_name.left + max_3(lbl_lang_name.Width, lbl_translator_name.Width, lbl_revision_name.Width) + 8;
  lbl_translator_value.left := lbl_translator_name.left + max_3(lbl_lang_name.Width, lbl_translator_name.Width, lbl_revision_name.Width) + 8;
  lbl_revision_value.left := lbl_revision_name.left + max_3(lbl_lang_name.Width, lbl_translator_name.Width, lbl_revision_name.Width) + 8;

  // Vertikal

  spe_leftshape.Height := btn_close.Top + btn_close.Height + 8;
  clientheight := spe_leftshape.top + spe_leftshape.Height;
  clientwidth := btn_close.Left + btn_close.Width + 8;
end;

end.
