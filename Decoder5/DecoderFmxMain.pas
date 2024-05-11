unit DecoderFmxMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, System.IOUtils, DecoderEncDec;

type
  TForm3 = class(TForm)
    DropTarget1: TDropTarget;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Memo1: TMemo;
    Edit1: TEdit;
    Button2: TButton;
    Label4: TLabel;
    Label5: TLabel;
    SaveDialog1: TSaveDialog;
    CheckBox1: TCheckBox;
    procedure DropTarget1Dropped(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure DropTarget1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
    ChosenFile: string;
    DC4FileInfo: TDC4FileInfo;
    procedure OpenFile(const AFileName: string);
  public
    { Public-Deklarationen }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses
  DecoderFuncs, DECTypes;

const
  TAG_DC10_DECRYPT = 1;
  TAG_DC4X_DECRYPT = 2;
  TAG_DC50_ENCRYPT = 3;

procedure OnProgressProc(Size, Pos: Int64; State: TDECProgressState);
begin
  Form3.ProgressBar1.Min := 0;
  Form3.ProgressBar1.Max := Size;

  if (State = Finished) then
    Form3.ProgressBar1.Value := Form3.ProgressBar1.Max
  else
    Form3.ProgressBar1.Value := Pos;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  AOutput: string;
  fp: TDC4Parameters;
begin
  case TButton(Sender).Tag of
    {$REGION '(De)Coder 1.0 decrypt'}
    TAG_DC10_DECRYPT:
    begin
      if SaveDialog1.Execute then
      begin
        AOutput := SaveDialog1.FileName;
        DeCoder10_DecodeFile(ChosenFile, AOutput, OnProgressProc);
        ExplorerNavigateToFile(AOutput);
      end;
    end;
    {$ENDREGION}
    {$REGION '(De)Coder 4.x/5.0 decrypt'}
    TAG_DC4X_DECRYPT:
    begin
      SaveDialog1.FileName := DC4FileInfo.OrigFileName;
      if SaveDialog1.Execute then
      begin
        AOutput := SaveDialog1.FileName;
        DeCoder4X_DecodeFile(ChosenFile, AOutput, Edit1.Text, OnProgressProc);
        ExplorerNavigateToFile(AOutput);
      end;
    end;
    {$ENDREGION}
    {$REGION 'Encrypt'}
    TAG_DC50_ENCRYPT:
    begin
      // TODO: Let user repeat the password!
      if SaveDialog1.Execute then
      begin
        AOutput := SaveDialog1.FileName;
        fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
        if CheckBox1.IsChecked then
        begin
          fp.ContainFileOrigName := fpExpose;
          fp.ContainFileOrigSize := true;
          fp.ContainFileOrigDate := true;
        end
        else
        begin
          fp.ContainFileOrigName := fpHide;
          fp.ContainFileOrigSize := false;
          fp.ContainFileOrigDate := false;
        end;
        // TODO: UTF-8 passwords!
        DeCoder4X_EncodeFile(ChosenFile, AOutput, Edit1.Text, fp, OnProgressProc);
        ExplorerNavigateToFile(AOutput);
      end;
    end;
    {$ENDREGION}
  end;
  Edit1.Text := '';
end;

procedure TForm3.DropTarget1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TForm3.DropTarget1Dropped(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  if Length(Data.Files) > 1 then raise Exception.Create('Please only choose one file!');
  OpenFile(Data.Files[0]);
end;

procedure TForm3.OpenFile(const AFileName: string);
begin
  Memo1.Lines.Clear;

  ChosenFile := AFileName;

  {$REGION '(De)Coder 1.0 decrypt'}
  Label1.Text := ExtractFileName(OpenDialog1.FileName);
  if DeCoder10_DetectFile(AFileName) then
  begin
    Label2.Text := 'This file was encrypted using (De)Coder 1.0';
    Button2.Tag := TAG_DC10_DECRYPT;
    Button2.Text := 'Decrypt';
    Exit;
  end;
  {$ENDREGION}

  {$REGION '(De)Coder 4.x/5.0 decrypt'}
  try
    DC4FileInfo := DeCoder4X_FileInfo(AFileName);
    if DC4FileInfo.Parameters.Dc4FormatVersion >= fvDc50 then
      Label2.Text := 'This file was encrypted using (De)Coder 5.0'
    else if DC4FileInfo.Parameters.Dc4FormatVersion >= fvDc41Beta then
      Label2.Text := 'This file was encrypted using (De)Coder 4.1 Beta'
    else
      Label2.Text := 'This file was encrypted using (De)Coder 4.0';
    DeCoder4X_PrintFileInfo(DC4FileInfo, Memo1.Lines);
    Button2.Tag := TAG_DC4X_DECRYPT;
    Button2.Text := 'Decrypt';
    Exit;
  except
    on E: Exception do
    begin
      if AFileName.EndsWith('.dc4', true) or AFileName.EndsWith('.dc5', true) then
      begin
        Label2.Text := 'This is not a valid (De)Coder 4.0/5.0 file!' + #13#10 + E.Message;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Encrypt'}
  Label2.Text := 'This file is not encrypted using (De)Coder 1.x/4.x/5.x.' + #13#10 + 'Do you want to encrypt it now?';
  Button2.Tag := TAG_DC50_ENCRYPT;
  Button2.Text := 'Encrypt';
  Memo1.Lines.Add('File name: ' + ExtractFileName(AFileName));
  Memo1.Lines.Add('Location: ' + ExtractFilePath(AFileName));
  Memo1.Lines.Add('File type: ' + GetFileTypename(AFileName));
  Memo1.Lines.Add('File size: ' + FileSizeHumanReadable(TFile.GetSize(AFileName)));
  Memo1.Lines.Add('Modification time: ' + DateTimeToStr(TFile.GetLastWriteTime(AFileName)));
  // TODO: Add note about (De)Coder 2.x and 3.x
  {$ENDREGION}
end;

end.
