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
    Ueberschrift2: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Label3: TLabel;
    Panel1: TPanel;
    Label4: TLabel;
    close_button: TButton;
    SaveDialog1: TSaveDialog;
    Gauge1: TGauge;
    Edit1: TEdit;
    Ueberschrift1: TLabel;
    Timer1: TTimer;
    Label1: TLabel;
    Status: TLabel;
    Bevel1: TBevel;
    procedure beenden_buttonClick(Sender: TObject);
    procedure load_buttonClick(Sender: TObject);
    procedure crypt_buttonClick(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var
  mem: TMemoryStream;
  a, b: char;
  j: integer;

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
    Gauge1.enabled := false;
    Edit1.Enabled:=true;
    label4.Enabled:=true;
    close_button.enabled := true;
    SaveDialog1.filename := Opendialog1.FileName;
  end;
end;

procedure TForm1.crypt_buttonClick(Sender: TObject);
begin
  if not fileexists(opendialog1.filename) then
  begin
    MessageDLG('Datei ist gelöscht worden. Bitte öffnen Sie eine neue Datei!', mtInformation, [mbOK], 0);
  end
  else
  begin
    if Edit1.Text = '' then
    begin
      MessageDLG('Sie müssen ein Passwort zum Verschlüsseln / Entschlüsseln eingeben!', mtInformation, [mbOK], 0);
    end
    else
    begin
      if Savedialog1.Execute then
      begin
        Status.caption := 'Programm arbeitet...';
        Timer1.enabled := true;
      end;
    end;
  end;
end;

procedure TForm1.StatusBar1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.d-m-home.de', '', '', 1);
end;

procedure TForm1.close_buttonClick(Sender: TObject);
begin
  filename_edit.text := '';
  crypt_button.Enabled := false;
  Edit1.Enabled := false;
  label4.Enabled := false;
  Gauge1.enabled := false;
  close_button.enabled := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Ueberschrift1.caption := application.title;
  Ueberschrift2.caption := application.title;
  form1.caption := application.title;
  Status.caption := 'Programm bereit!';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i, temp: integer;
begin
  Timer1.enabled := false;
  mem := TMemoryStream.Create;
  mem.LoadFromfile(OpenDialog1.FileName);
  gauge1.MaxValue := mem.size;
  for i := 0 to mem.Size - 1 do
  begin
    mem.Position := i;
    mem.Read(a, 1);
    j := j + 1;
    if j > length(edit1.text) then j := 1;
    for temp := 0 to 255 do
    begin
      if copy(edit1.text, j, 1) = chr(temp) then b := chr(temp);
    end;
    a := CHR(byte(b) xor byte(a));
    mem.Position := i;
    mem.write(a, 1);
    gauge1.Progress := gauge1.Progress + 1;
  end;
  j := 0;
  mem.SaveToFile(SaveDialog1.FileName);
  mem.Free;
  MessageDLG('Datei wurde erfolgreich Verschlüsselt / Entschlüsselt!', mtInformation, [mbOK], 0);
  gauge1.Progress := gauge1.MinValue;
  Status.caption := 'Programm bereit!';
end;

end.

