unit DecoderFmxMain;

// TODO: Should we also offer SecureDelete in this GUI?
// TODO: Make app multi-lingual!

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, System.IOUtils, DecoderEncDec;

type
  TDcGuiElement = (gePassword, geStartButton, geInfos, geMetadataCheckbox);
  TDcGuiElements = set of TDcGuiElement;

type
  TDecoderMainForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
  private
    FChosenFile: string;
    FDC4FileInfo: TDC4FileInfo;
    procedure OpenFile(const AFileName: string);
    procedure GuiShowElements(AElements: TDcGuiElements);
  end;

var
  DecoderMainForm: TDecoderMainForm;

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
  DecoderMainForm.ProgressBar1.Min := 0;
  DecoderMainForm.ProgressBar1.Max := Size;

  if (State = Finished) then
    DecoderMainForm.ProgressBar1.Value := DecoderMainForm.ProgressBar1.Max
  else
    DecoderMainForm.ProgressBar1.Value := Pos;

  DecoderMainForm.ProgressBar1.Visible := State = Processing;

  Application.ProcessMessages;
  if Application.Terminated then
    Abort;
end;

procedure TDecoderMainForm.Button2Click(Sender: TObject);
var
  AOutput: string;
  fp: TDC4Parameters;
  RepeatedPassword: string;
begin
  try
    case TButton(Sender).Tag of
      {$REGION '(De)Coder 1.0 decrypt'}
      TAG_DC10_DECRYPT:
      begin
        SaveDialog1.FileName := ChangeFileExt(FChosenFile, '_decoded.txt');
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          DeCoder10_DecodeFile(FChosenFile, AOutput, OnProgressProc);
          ExplorerNavigateToFile(AOutput);
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 4.x/5.0 decrypt'}
      TAG_DC4X_DECRYPT:
      begin
        SaveDialog1.FileName := FDC4FileInfo.OrigFileName;
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          DeCoder4X_DecodeFile(FChosenFile, AOutput, Edit1.Text, OnProgressProc);
          ExplorerNavigateToFile(AOutput);
        end;
      end;
      {$ENDREGION}
      {$REGION 'Encrypt'}
      TAG_DC50_ENCRYPT:
      begin
        while true do
        begin
          // #0 means that the password char '*' is used
          if not InputQuery(Caption, #0 + 'Please repeat the password for encryption', RepeatedPassword) then
            Abort;
          if RepeatedPassword <> Edit1.Text then
            ShowMessage('Password mismatch!')  // TODO: can this box have style and icon?
          else
            break;
        end;
        SaveDialog1.FileName := ChangeFileExt(FChosenFile, '.dc5');
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
          DeCoder4X_EncodeFile(FChosenFile, AOutput, Edit1.Text, fp, OnProgressProc);
          ExplorerNavigateToFile(AOutput);
        end;
      end;
      {$ENDREGION}
    end;
    Edit1.Text := '';
  except
    ProgressBar1.Visible := false;
  end;
end;

procedure TDecoderMainForm.DropTarget1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TDecoderMainForm.DropTarget1Dropped(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  if Length(Data.Files) > 1 then raise Exception.Create('Please only choose one file!');
  OpenFile(Data.Files[0]);
end;

procedure TDecoderMainForm.GuiShowElements(AElements: TDcGuiElements);
begin
  Label4.Visible := gePassword in AElements;
  Edit1.Visible := gePassword in AElements;
  Label5.Visible := geInfos in AElements;
  Memo1.Visible := geInfos in AElements;
  CheckBox1.Visible := geMetadataCheckbox in AElements;
  Button2.Visible := geStartButton in AElements;
  if gePassword in AElements then Edit1.SetFocus;

  if (geStartButton in AElements) and not (gePassword in AElements) then
    Button2.Position.X := Edit1.Position.X
  else
    Button2.Position.X := Edit1.Position.X + Edit1.Width + 8;

  if (geInfos in AElements) and not (geMetadataCheckbox in AElements) then
    Memo1.Height := CheckBox1.Position.Y + CheckBox1.Height - Memo1.Position.Y
  else
    Memo1.Height := CheckBox1.Position.Y - (DecoderMainForm.ClientHeight - (CheckBox1.Position.Y + CheckBox1.Height)) - Memo1.Position.Y;
end;

procedure TDecoderMainForm.FormCreate(Sender: TObject);
begin
  Label2.Text :=
    'Built ' + DateTimeToStr(GetOwnBuildTimestamp) + #13#10 +
    'Developed by Daniel Marschall - www.daniel-marschall.de' + #13#10 +
    'FREEWARE - Licensed under the terms of the Apache 2.0 License';
  ProgressBar1.Visible := false; // will be automatically shown and hidden by OnProgressProc
  GuiShowElements([]);
  Application.Title := Caption; // because of Message dialog captions
end;

procedure TDecoderMainForm.OpenFile(const AFileName: string);
var
  fp: TDC4Parameters;
begin
  Memo1.Lines.Clear;

  FChosenFile := AFileName;

  {$REGION '(De)Coder 1.0 decrypt'}
  Label1.Text := ExtractFileName(OpenDialog1.FileName);
  if DeCoder10_DetectFile(AFileName) then
  begin
    Label2.Text := 'This file was encrypted using (De)Coder 1.0' + #13#10 + 'Do you want to decrypt it now?';
    Button2.Tag := TAG_DC10_DECRYPT;
    Button2.Text := 'Decrypt';
    GuiShowElements([geStartButton]);
    Exit;
  end;
  {$ENDREGION}

  {$REGION '(De)Coder 4.x/5.0 decrypt'}
  try
    FDC4FileInfo := DeCoder4X_FileInfo(AFileName);
    if FDC4FileInfo.Parameters.Dc4FormatVersion >= fvDc50 then
      Label2.Text := 'This file was encrypted using (De)Coder 5.0'
    else if FDC4FileInfo.Parameters.Dc4FormatVersion >= fvDc41Beta then
      Label2.Text := 'This file was encrypted using (De)Coder 4.1 Beta'
    else
      Label2.Text := 'This file was encrypted using (De)Coder 4.0';
    Label2.Text := Label2.Text + #13#10 + 'Do you want to decrypt it now?';
    DeCoder4X_PrintFileInfo(FDC4FileInfo, Memo1.Lines);
    Button2.Tag := TAG_DC4X_DECRYPT;
    Button2.Text := 'Decrypt';
    GuiShowElements([gePassword, geStartButton, geInfos]);
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
  fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
  CheckBox1.IsChecked := (fp.ContainFileOrigName=fpExpose) and fp.ContainFileOrigSize and fp.ContainFileOrigDate;
  // TODO: Add note about (De)Coder 2.x and 3.x
  GuiShowElements([gePassword, geStartButton, geInfos, geMetadataCheckbox]);
  {$ENDREGION}
end;

end.
