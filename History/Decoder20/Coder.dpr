program Coder;

{$Description '(De)Coder V2.0 VLL'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '(De)Coder V2.0';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
