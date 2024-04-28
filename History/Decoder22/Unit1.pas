unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ShellAPI, jpeg, Gauges;

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
    Panel1: TPanel;
    Label4: TLabel;
    vk: TTrackBar;
    Label5: TLabel;
    close_button: TButton;
    SaveDialog1: TSaveDialog;
    Gauge1: TGauge;
    procedure beenden_buttonClick(Sender: TObject);
    procedure load_buttonClick(Sender: TObject);
    procedure crypt_buttonClick(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure vkChange(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

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
    vk.Enabled:=true;
    label4.Enabled:=true;
    label5.Enabled:=true;
    close_button.enabled := true;
    SaveDialog1.filename := Opendialog1.FileName;
  end;
end;

procedure TForm1.crypt_buttonClick(Sender: TObject);
var
  mem: TMemoryStream;
  i: integer;
  a: byte;
  key: integer;
begin
  if Savedialog1.Execute then
  begin
    if not fileexists(OpenDialog1.FileName) then
    begin
      MessageDLG('Datei nicht gefunden!', mtWarning, [mbOK], 0);
      exit;
    end;
    mem := TMemoryStream.Create;
    mem.LoadFromfile(OpenDialog1.FileName);
    gauge1.MaxValue := mem.size;
    key := vk.position;
    for i := 0 to mem.Size - 1 do
    begin
      mem.Position := i;
      mem.Read(a, 1);
      a := a xor key;
      inc(key);
      mem.Position := i;
      mem.write(a, 1);
      gauge1.Progress := gauge1.Progress + 1;
    end;
    mem.SaveToFile(SaveDialog1.FileName);
    mem.Free;
    MessageDLG('Datei wurde erfolgreich Verschlüsselt / Entschlüsselt!', mtInformation, [mbOK], 0);
    gauge1.Progress := gauge1.MinValue;
  end;
end;

procedure TForm1.StatusBar1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.d-m-home.de', '', '', 1);
end;

procedure TForm1.vkChange(Sender: TObject);
begin
  label5.Caption:=inttostr(vk.Position);
end;

procedure TForm1.close_buttonClick(Sender: TObject);
begin
  opendialog1.FileName := '';
  filename_edit.text := '';
  crypt_button.Enabled := false;
  vk.Enabled := false;
  label4.Enabled := false;
  label5.Enabled := false;
  close_button.enabled := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  label1.caption := application.title;
  form1.caption := application.title;
end;

end.

