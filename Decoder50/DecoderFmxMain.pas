unit DecoderFmxMain;

// TODO: Make CLI and FMX-App multi-lingual!

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
    OpenedFileLabel: TLabel;
    OpenDialog1: TOpenDialog;
    ShortInfoLabel: TLabel;
    MoreInfoMemo: TMemo;
    PasswordEdit: TEdit;
    EncryptDecryptButton: TButton;
    PasswordEditLabel: TLabel;
    MoreInfoLabel: TLabel;
    SaveDialog1: TSaveDialog;
    MetadataCheckbox: TCheckBox;
    ProgressStepLabel: TLabel;
    procedure DropTarget1Dropped(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure DropTarget1Click(Sender: TObject);
    procedure EncryptDecryptButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FChosenFile: string;
    FDC4FileInfo: TDC4FileInfo;
    InitOpenedFileLabelSize: Single;
    InitOpenedFileLabelWidth: Single;
    procedure ChangeOpenedFileLabelText(const AText: string);
    procedure OpenFile(const AFileName: string);
    procedure GuiShowElements(AElements: TDcGuiElements);
    procedure GuiShowChosenFile;
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

procedure OnProgressProc(Size, Pos: Int64; const Task: string; State: TDCProgressState);
begin
  DecoderMainForm.ProgressBar1.Min := 0;
  DecoderMainForm.ProgressBar1.Max := Size;

  if (State = TDcProgressState.Finished) then
    DecoderMainForm.ProgressBar1.Value := DecoderMainForm.ProgressBar1.Max
  else
    DecoderMainForm.ProgressBar1.Value := Pos;

  DecoderMainForm.ProgressBar1.Visible := State = TDcProgressState.Processing;

  DecoderMainForm.ProgressStepLabel.Text := Task;
  DecoderMainForm.ProgressStepLabel.Visible := State = TDcProgressState.Processing;

  Application.ProcessMessages;
  if Application.Terminated then
    Abort;
end;

procedure TDecoderMainForm.EncryptDecryptButtonClick(Sender: TObject);
var
  AOutput: string;
  fp: TDC4Parameters;
  RepeatedPassword: string;
resourcestring
  STextFiles = 'Text files';
  SAllFiles = 'All files';
  SEncryptedFiles = 'DC4/5 Encrypted files';
  SPleaseRepeatPassword = 'Please repeat the password for encryption';
  SPasswordsDoNotMatch = 'Passwords do not match!';
begin
  try
    case TButton(Sender).Tag of
      {$REGION '(De)Coder 1.0 decrypt'}
      TAG_DC10_DECRYPT:
      begin
        TButton(Sender).Tag := -TButton(Sender).Tag; // disable "double clicking"
        SaveDialog1.Filter := STextFiles+' (*.txt)|*.txt|'+SAllFiles+' (*.*)|*.*';
        SaveDialog1.FileName := ChangeFileExt(FChosenFile, '_decoded.txt');
        SaveDialog1.DefaultExt := 'txt';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          DeCoder10_DecodeFile(FChosenFile, AOutput, OnProgressProc);
          ExplorerNavigateToFile(AOutput);
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 4.x/5.0 decrypt'}
      TAG_DC4X_DECRYPT:
      begin
        TButton(Sender).Tag := -TButton(Sender).Tag; // disable "double clicking"
        if PasswordEdit.Text = '' then exit;
        SaveDialog1.Filter := SAllFiles+' (*.*)|*.*';
        SaveDialog1.FileName := FDC4FileInfo.OrigFileName;
        SaveDialog1.DefaultExt := ExtractFileExt(FDC4FileInfo.OrigFileName);
        SaveDialog1.DefaultExt := Copy(SaveDialog1.DefaultExt, 2, Length(SaveDialog1.DefaultExt)-1);
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          DeCoder4X_DecodeFile(FChosenFile, AOutput, PasswordEdit.Text, OnProgressProc);
          ExplorerNavigateToFile(AOutput);
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 5.0 encrypt'}
      TAG_DC50_ENCRYPT:
      begin
        TButton(Sender).Tag := -TButton(Sender).Tag; // disable "double clicking"
        if PasswordEdit.Text = '' then exit;
        while true do
        begin
          // #0 means that the password char '*' is used
          RepeatedPassword := '';
          Application.ProcessMessages; // Otherwise, the text "Please repeat the password for encryption" vanishes if the user has entered the password wrong once
          if not InputQuery(Caption, #0 + SPleaseRepeatPassword, RepeatedPassword) then
            Abort;
          if RepeatedPassword <> PasswordEdit.Text then
            MessageDlg(SPasswordsDoNotMatch, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOk], 0)
          else
            break;
        end;
        SaveDialog1.Filter := SEncryptedFiles+' (*.dc4;*.dc5)|*.dc4;*.dc5|'+SAllFiles+' (*.*)|*.*';
        SaveDialog1.FileName := ChangeFileExt(FChosenFile, '.dc5');
        SaveDialog1.DefaultExt := 'dc5';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
          if MetadataCheckbox.IsChecked then
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
          DeCoder4X_EncodeFile(FChosenFile, AOutput, PasswordEdit.Text, fp, OnProgressProc);
          ExplorerNavigateToFile(AOutput);
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
    end;
  finally
    ProgressBar1.Visible := false;
    if TButton(Sender).Tag < 0 then
      TButton(Sender).Tag := -TButton(Sender).Tag; // allow clicking again
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
resourcestring
  SOnlyOneFileAllowed = 'Please only choose one file!';
begin
  if Length(Data.Files) > 1 then raise Exception.Create(SOnlyOneFileAllowed);
  try
    OpenFile(Data.Files[0]);
  except
    on E: Exception do
    begin
      // We need to do this, because for some reason an Exception will be swallowed in this event
      MessageDlg(e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    end;
  end;
end;

procedure TDecoderMainForm.ChangeOpenedFileLabelText(const AText: string);
begin
  If OpenedFileLabel.Tag = 0 then
  begin
    OpenedFileLabel.Tag := 1;
    InitOpenedFileLabelSize := OpenedFileLabel.TextSettings.Font.Size;
    InitOpenedFileLabelWidth := OpenedFileLabel.Width;
    OpenedFileLabel.AutoSize := true;
    OpenedFileLabel.TextSettings.WordWrap := false;
  end;
  OpenedFileLabel.Text := AText;
  OpenedFileLabel.TextSettings.Font.Size := InitOpenedFileLabelSize;
  while (OpenedFileLabel.Width > InitOpenedFileLabelWidth) and (OpenedFileLabel.TextSettings.Font.Size >= 1) do
    OpenedFileLabel.TextSettings.Font.Size := OpenedFileLabel.TextSettings.Font.Size - 0.1;
end;

procedure TDecoderMainForm.GuiShowChosenFile;
begin
  ChangeOpenedFileLabelText(ExtractFileName(FChosenFile));
end;

procedure TDecoderMainForm.GuiShowElements(AElements: TDcGuiElements);
begin
  PasswordEditLabel.Visible := gePassword in AElements;
  PasswordEdit.Visible := gePassword in AElements;
  MoreInfoLabel.Visible := geInfos in AElements;
  MoreInfoMemo.Visible := geInfos in AElements;
  MetadataCheckbox.Visible := geMetadataCheckbox in AElements;
  EncryptDecryptButton.Visible := geStartButton in AElements;
  if gePassword in AElements then PasswordEdit.SetFocus;

  if (geStartButton in AElements) and not (gePassword in AElements) then
    EncryptDecryptButton.Position.X := PasswordEdit.Position.X
  else
    EncryptDecryptButton.Position.X := PasswordEdit.Position.X + PasswordEdit.Width + 8;

  if (geInfos in AElements) and not (geMetadataCheckbox in AElements) then
    MoreInfoMemo.Height := MetadataCheckbox.Position.Y + MetadataCheckbox.Height - MoreInfoMemo.Position.Y
  else
    MoreInfoMemo.Height := MetadataCheckbox.Position.Y - (DecoderMainForm.ClientHeight - (MetadataCheckbox.Position.Y + MetadataCheckbox.Height)) - MoreInfoMemo.Position.Y;
end;

procedure TDecoderMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := Caption; // because of Message dialog captions
end;

procedure TDecoderMainForm.FormShow(Sender: TObject);
resourcestring
  SPleaseChooseOnlyOneFile = 'Please only choose one file!';
  S_IntroLine_0_S = 'Built %s';
  S_IntroLine_1 = 'Developed by Daniel Marschall - www.daniel-marschall.de';
  S_IntroLine_2 = 'FREEWARE - Licensed under the terms of the Apache 2.0 License';
  S_IntroLine_3 = '';
  S_IntroLine_4 = '';
  S_IntroLine_5 = 'Please note that (De)Coder also comes with a command-line tool,';
  S_IntroLine_6 = 'which can additionally decrypt old (De)Coder 2.x and 3.x files,';
  S_IntroLine_7 = 'and it can also wipe files and folders in a secure way.';
  S_IntroLine_8 = '';
  S_IntroLine_9 = '';
  S_IntroLine_10 = 'Please use this tool at your own risk! Files can only be decrypted with the';
  S_IntroLine_11 = 'correct passwords, and they must be 100% free of any damage!';
begin
  ShortInfoLabel.Text :=
    Format(S_IntroLine_0_S, [DateTimeToStr(GetOwnBuildTimestamp)]) + #13#10 +
    S_IntroLine_1 + #13#10 +
    S_IntroLine_2 + #13#10 +
    S_IntroLine_3 + #13#10 +
    S_IntroLine_4 + #13#10 +
    S_IntroLine_5 + #13#10 +
    S_IntroLine_6 + #13#10 +
    S_IntroLine_7 + #13#10 +
    S_IntroLine_8 + #13#10 +
    S_IntroLine_9 + #13#10 +
    S_IntroLine_10 + #13#10 +
    S_IntroLine_11;
  ProgressBar1.Visible := false; // will be automatically shown and hidden by OnProgressProc
  ProgressStepLabel.Visible := false;
  GuiShowElements([]);
  try
    if ParamCount > 1 then
      MessageDlg(SPleaseChooseOnlyOneFile, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
    else if ParamCount = 1 then
      OpenFile(ParamStr(1));
  except
    on E: Exception do
    begin
      MessageDlg(e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    end;
  end;
end;

procedure TDecoderMainForm.OpenFile(const AFileName: string);
var
  fp: TDC4Parameters;
resourcestring
  SFileOrFolderNotExisting = 'File or folder %s does not exist!';
  SFileName_S = 'File name: %s';
  SFolderName_S = 'Folder name: %s';
  SLocation_S = 'Location: %s';
  SEncryptedDc10 = 'This file was encrypted using (De)Coder 1.0';
  SDoYouWantDecrypt = 'Do you want to decrypt it now?';
  SDecrypt = 'Decrypt';
  SEncryptedDc50 = 'This file was encrypted using (De)Coder 5.0';
  SEncryptedDc41Beta = 'This file was encrypted using (De)Coder 4.1 Beta';
  SEncryptedDc40 = 'This file was encrypted using (De)Coder 4.0';
  SNoValidDc45File = 'This is not a valid (De)Coder 4.0/5.0 file!';
  SThisIsAFolder = 'This is a folder which you can pack and encrypt using (De)Coder.';
  SThisIsAnUnencryptedFile = 'This file is not encrypted using (De)Coder 1.x/4.x/5.x.';
  SDoYouWantPackAndEncrypt = 'Do you want to pack + encrypt it now?';
  SDoYouWantEncrypt = 'Do you want to encrypt it now?';
  SEncrypt = 'Encrypt';
  SFileType_S = 'File type: %s';
  SFileSize_S = 'File size: %s';
  SModTime_S = 'Modification time: %s';
begin
  if not FileExists(AFileName) and not DirectoryExists(AFileName) then
    raise Exception.CreateFmt(SFileOrFolderNotExisting, [AFileName]);

  MoreInfoMemo.Lines.Clear;
  if FileExists(AFileName) then
    MoreInfoMemo.Lines.Add(Format(SFileName_S, [ExtractFileName(AFileName)]))
  else
    MoreInfoMemo.Lines.Add(Format(SFolderName_S, [ExtractFileName(AFileName)]));
  MoreInfoMemo.Lines.Add(Format(SLocation_S, [ExtractFilePath(AFileName)]));

  if FileExists(AFileName) then
  begin
    {$REGION '(De)Coder 1.0 decrypt'}
    if DeCoder10_DetectFile(AFileName) then
    begin
      ShortInfoLabel.Text := SEncryptedDc10 + #13#10 + SDoYouWantDecrypt;
      EncryptDecryptButton.Tag := TAG_DC10_DECRYPT;
      EncryptDecryptButton.Text := SDecrypt;
      GuiShowElements([geStartButton]);
      FChosenFile := AFileName;
      GuiShowChosenFile;
      Exit;
    end;
    {$ENDREGION}

    {$REGION '(De)Coder 4.x/5.0 decrypt'}
    try
      FDC4FileInfo := DeCoder4X_FileInfo(AFileName);
      if FDC4FileInfo.Parameters.Dc4FormatVersion >= fvDc50 then
        ShortInfoLabel.Text := SEncryptedDc50
      else if FDC4FileInfo.Parameters.Dc4FormatVersion >= fvDc41Beta then
        ShortInfoLabel.Text := SEncryptedDc41Beta
      else
        ShortInfoLabel.Text := SEncryptedDc40;
      ShortInfoLabel.Text := ShortInfoLabel.Text + #13#10 + SDoYouWantDecrypt;
      DeCoder4X_PrintFileInfo(FDC4FileInfo, MoreInfoMemo.Lines);
      EncryptDecryptButton.Tag := TAG_DC4X_DECRYPT;
      EncryptDecryptButton.Text := SDecrypt;
      GuiShowElements([gePassword, geStartButton, geInfos]);
      FChosenFile := AFileName;
      GuiShowChosenFile;
      Exit;
    except
      on E: Exception do
      begin
        if AFileName.EndsWith('.dc4', true) or AFileName.EndsWith('.dc5', true) then
        begin
          raise Exception.Create(SNoValidDc45File + #13#10 + E.Message);
        end;
      end;
    end;
    {$ENDREGION}
  end;

  {$REGION '(De)Coder 5.0 encrypt'}
  if DirectoryExists(AFileName) then
    ShortInfoLabel.Text := SThisIsAFolder + #13#10 + SDoYouWantPackAndEncrypt
  else
    ShortInfoLabel.Text := SThisIsAnUnencryptedFile + #13#10 + SDoYouWantEncrypt;
  EncryptDecryptButton.Tag := TAG_DC50_ENCRYPT;
  EncryptDecryptButton.Text := SEncrypt;
  if FileExists(AFileName) then
  begin
    MoreInfoMemo.Lines.Add(Format(SFileType_S, [GetFileTypename(AFileName)]));
    MoreInfoMemo.Lines.Add(Format(SFileSize_S, [FileSizeHumanReadable(TFile.GetSize(AFileName))]));
    MoreInfoMemo.Lines.Add(Format(SModTime_S, [DateTimeToStr(TFile.GetLastWriteTime(AFileName))]));
  end;
  fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
  MetadataCheckbox.IsChecked := (fp.ContainFileOrigName=fpExpose) and fp.ContainFileOrigSize and fp.ContainFileOrigDate;
  GuiShowElements([gePassword, geStartButton, geInfos, geMetadataCheckbox]);
  FChosenFile := AFileName;
  GuiShowChosenFile;
  {$ENDREGION}
end;

end.
