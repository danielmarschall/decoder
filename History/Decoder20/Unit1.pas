unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    load_button: TButton;
    crypt_button: TButton;
    beenden_button: TButton;
    FileName_Edit: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Label3: TLabel;
    procedure beenden_buttonClick(Sender: TObject);
    procedure load_buttonClick(Sender: TObject);
    procedure crypt_buttonClick(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var
  st: Array[0..255] of char;

procedure TForm1.beenden_buttonClick(Sender: TObject);
begin
  Form1.close;
end;

procedure TForm1.load_buttonClick(Sender: TObject);
begin
  if Opendialog1.Execute then
  begin
    FileName_Edit.text := ExtractFileName(OpenDialog1.FileName);
    crypt_button.Enabled := true;
  end;
end;

procedure TForm1.crypt_buttonClick(Sender: TObject);
var
  mem: TMemoryStream;
  i: integer;
  a: byte;
  key: integer;
begin
  if not fileexists(ExtractFilePath(OpenDialog1.FileName) + ExtractFileName(OpenDialog1.FileName)) then
  begin
    showmessage('Datei nicht vorhanden!');
    exit;
  end;
  mem := TMemoryStream.Create;
  mem.LoadFromfile(OpenDialog1.FileName);
  key := a;
  for i := 0 to mem.Size - 1 do
  begin
    mem.Position := i;
    mem.Read(a, 1);
    a := a xor key;
    inc(key);
    mem.Position := i;
    mem.write(a, 1);
  end;
  mem.SaveToFile(ExtractFilePath(OpenDialog1.FileName) + ExtractFileName(OpenDialog1.FileName));
  mem.Free;
  ShowMessage('Datei wurde geändert!');
end;

procedure TForm1.StatusBar1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', StrPCopy(st, 'http://www.d-m-home.de'), nil, nil, SW_SHOW);
end;

end.

