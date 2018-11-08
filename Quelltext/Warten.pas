unit Warten;

interface

uses
  Windows, Messages, SysUtils, Forms, IniFiles, Controls, Classes,
  ComCtrls, StdCtrls, DCConst, ExtCtrls,
  dialogs;

type
  TWartenForm = class(TForm)
    pbr_progress: TProgressBar;
    lbl_wait: TLabel;
    lbl_info1: TLabel;
    lbl_info2: TLabel;
    btn_escape: TButton;
    procedure btn_escapeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    LangArray: array of TLanguageEntry;
    function GetLangEntry(name: string): string;
  end;

var
  WartenForm: TWartenForm;

implementation

uses Main;

{$R *.dfm}

function TWartenForm.GetLangEntry(name: string): string;
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

procedure TWartenForm.btn_escapeClick(Sender: TObject);
begin
  mainform.close;
end;

procedure TWartenForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i: integer;
begin
  // Sprachdatei auslesen

  ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'Language.ini');
  str := TStringList.Create();
  try
    ini.ReadSection(Name, str);
    for i := 0 to str.count-1 do
    begin
      setlength(LangArray, length(LangArray)+1);
      LangArray[length(LangArray)-1].name := str.strings[i];
      LangArray[length(LangArray)-1].text := ini.ReadString(name, str.strings[i], '?');
      LangArray[length(LangArray)-1].text := StringReplace(LangArray[length(LangArray)-1].text, '###', #13#10, [rfReplaceAll]);
    end;
  finally
    ini.free;
    str.Free;
  end;

  // Formular vorbereiten

  lbl_wait.Caption := GetLangEntry('wait');
  btn_escape.Caption := GetLangEntry('exit');

  lbl_wait.Left := clientwidth div 2 - lbl_wait.Width div 2;
end;

end.
