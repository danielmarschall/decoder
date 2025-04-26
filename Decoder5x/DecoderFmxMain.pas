unit DecoderFmxMain;

// TODO: Make CLI and FMX-App multi-lingual!

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, System.IOUtils, DecoderEncDec, FMX.ListBox;

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
    ComboBox1: TComboBox;
    procedure DropTarget1Dropped(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure DropTarget1Click(Sender: TObject);
    procedure EncryptDecryptButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    FChosenFile: string;
    FDC4FileInfo: TDC4FileInfo;
    InitOpenedFileLabelSize: Single;
    InitOpenedFileLabelWidth: Single;
    procedure ChangeOpenedFileLabelText(const AText: string);
    procedure OpenFile(const AFileName: string);
    procedure GuiShowElements(AElements: TDcGuiElements);
    procedure GuiShowChosenFile;
    procedure InitView;
  end;

var
  DecoderMainForm: TDecoderMainForm;

implementation

{$R *.fmx}

uses
  DecoderFuncs, DECTypes, System.Rtti, System.IniFiles, System.TypInfo, System.SysConst;

resourcestring
  S_IntroLine_0_S = 'Version %s';
  S_IntroLine_1 = 'Developed by Daniel Marschall - www.daniel-marschall.de';
  S_IntroLine_2 = 'FREEWARE - Licensed under the terms of the Apache 2.0 License';
  S_IntroLine_3 = '';
  S_IntroLine_4 = 'Source code and latest version available at';
  S_IntroLine_5 = 'www.github.com/danielmarschall/decoder';
  S_IntroLine_6 = '';
  S_IntroLine_7 = '';
  S_IntroLine_8 = 'Please use this tool at your own risk! Files can only be decrypted with the';
  S_IntroLine_9 = 'correct passwords, and they must be 100% free of any damage!';
  S_IntroLine_10 = '';
  S_IntroLine_11 = '';
  SAllFiles = 'All files';
  SEncryptedFiles = 'DC4/5 Encrypted files';
  SFileOrFolderSNotFound = 'File or folder %s not found!';
  STextFiles = 'Text files';
  SPleaseRepeatPassword = 'Please repeat the password for encryption';
  SPasswordsDoNotMatch = 'Passwords do not match!';
  SDestroyComplete = 'Successfully shredded!';
  SInfoLegacyDecrypt = 'Decryption process done. Please CHECK if the output file is what you expect. (With this legacy file format version, there is no possibility for (De)Coder to check if algorithm or password was okay.)';
  SDecryptDone = 'Decryption process done';
  SEncryptDone = 'Encryption process done';
  STryAgain = 'Please try again.';
  SPleaseChooseOnlyOneFile = 'Please choose only one file!';
  SProductTitle = 'ViaThinkSoft (De)Coder 5.1';
  SFileName_S = 'File name: %s';
  SFolderName_S = 'Folder name: %s';
  SLocation_S = 'Location: %s';
  SEncryptedDc10 = 'This file was encrypted using (De)Coder 1.0';
  SDoYouWantDecrypt = 'Do you want to decrypt it now?';
  SDecrypt = 'Decrypt';
  SEncryptedDc50 = 'This file was encrypted using (De)Coder 5.x';
  SEncryptedDc41Beta = 'This file was encrypted using (De)Coder 4.1 Beta';
  SEncryptedDc40 = 'This file was encrypted using (De)Coder 4.0';
  SNoValidDc45File = 'This is not a valid (De)Coder 4.x/5.x file! (%s)';
  SThisIsAFolder = 'This is a folder which you can pack and encrypt using (De)Coder.';
  SThisIsAnUnencryptedFile = 'This file is not encrypted using (De)Coder 1.x/4.x/5.x.';
  SDoYouWantPackAndEncrypt = 'Do you want to pack + encrypt it now?';
  SDoYouWantEncrypt = 'Do you want to encrypt it now?';
  SEncrypt = 'Encrypt';
  SFileType_S = 'File type: %s';
  SFileSize_S = 'File size: %s';
  SModTime_S = 'Modification time: %s';
  SInfoLegacyDC_S = 'Here you can decrypt a file that was encrypted with (De)Coder %s';
  SShredderInfo = 'THIS FILE OR FOLDER WILL BE DESTROYED WITHOUT POSSIBILITY OF RECOVERY!';
  SShredderButton = 'DESTROY';
  SWarnLegacyDecrypt = 'Note that there is no detection if the password is correct; so check the output file if the decrypted contents are valid.';

const
  // ComboBox1.ItemIndex
  CB1_IDX_DC45 = 0;
  CB1_IDX_DC32 = 1;
  CB1_IDX_DC30 = 2;
  CB1_IDX_DC22 = 3;
  CB1_IDX_DC21 = 4;
  CB1_IDX_DC20 = 5;
  CB1_IDX_SHRED = 6;

const
  // EncryptDecryptButton.Tag
  TAG_DC10_DECRYPT = 1;
  TAG_DC4X_DECRYPT = 2;
  TAG_DC50_ENCRYPT = 3;
  TAG_DC32_DECRYPT = 4;
  TAG_DC30_DECRYPT = 5;
  TAG_DC22_DECRYPT = 6;
  TAG_DC21_DECRYPT = 7;
  TAG_DC20_DECRYPT = 8;
  TAG_SHRED = 9;

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

procedure TextCallback(const Text: string);
begin
  DecoderMainForm.MoreInfoMemo.Lines.Add(Text);
end;

procedure TDecoderMainForm.EncryptDecryptButtonClick(Sender: TObject);
var
  AOutput: string;
  fp: TDC4Parameters;
  RepeatedPassword: string;
  iKey: integer;
  ButtonTag: integer;
begin
  if FChosenFile = '' then exit;
  try
    ButtonTag := TButton(Sender).Tag;
    TButton(Sender).Tag := -TButton(Sender).Tag; // disable "double clicking"
    case ButtonTag of
      {$REGION '(De)Coder 1.0 decrypt'}
      TAG_DC10_DECRYPT:
      begin
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@STextFiles)+' (*.txt)|*.txt|'+LoadResString(@SAllFiles)+' (*.*)|*.*';
        SaveDialog1.FileName := ChangeFileExt(FChosenFile, '_decoded.txt');
        {$ELSE}
        SaveDialog1.FileName := ExtractFileName(ChangeFileExt(FChosenFile, '_decoded.txt'));
        {$ENDIF}
        SaveDialog1.DefaultExt := 'txt';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          DeCoder10_DecodeFile(FChosenFile, AOutput, OnProgressProc);
          MessageDlg(LoadResString(@SInfoLegacyDecrypt), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 4.x/5.x decrypt'}
      TAG_DC4X_DECRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*';
        SaveDialog1.FileName := FDC4FileInfo.OrigFileName;
        {$ELSE}
        SaveDialog1.FileName := ExtractFileName(FDC4FileInfo.OrigFileName);
        {$ENDIF}
        SaveDialog1.DefaultExt := ExtractFileExt(FDC4FileInfo.OrigFileName);
        SaveDialog1.DefaultExt := Copy(SaveDialog1.DefaultExt, 2); // remove dot from the beginning
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          RepeatedPassword := PasswordEdit.Text;
          while true do
          begin
            try
              DeCoder4X_DecodeFile(FChosenFile, AOutput, RepeatedPassword, OnProgressProc);
              break;
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                // #0 means that the password char '*' is used
                if not InputQuery(Caption, #0 + LoadResString(@STryAgain) + ' ' + E.Message, RepeatedPassword) then
                  Abort;
              end;
            end;
          end;
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ELSE}
          MessageDlg(LoadResString(@SDecryptDone), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 5.x encrypt'}
      TAG_DC50_ENCRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        while true do
        begin
          // #0 means that the password char '*' is used
          RepeatedPassword := '';
          Application.ProcessMessages; // Otherwise, the text "Please repeat the password for encryption" vanishes if the user has entered the password wrong once
          if not InputQuery(Caption, #0 + LoadResString(@SPleaseRepeatPassword), RepeatedPassword) then
            Abort;
          if RepeatedPassword <> PasswordEdit.Text then
            MessageDlg(LoadResString(@SPasswordsDoNotMatch), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOk], 0)
          else
            break;
        end;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SEncryptedFiles)+' (*.dc4;*.dc5)|*.dc4;*.dc5|'+LoadResString(@SAllFiles)+' (*.*)|*.*';
        SaveDialog1.FileName := ChangeFileExt(FChosenFile, '.dc5');
        {$ELSE}
        SaveDialog1.FileName := ExtractFileName(ChangeFileExt(FChosenFile, '.dc5'));
        {$ENDIF}
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
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ELSE}
          MessageDlg(LoadResString(@SEncryptDone), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 3.2 decrypt'}
      TAG_DC32_DECRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*';
        {$ENDIF}
        SaveDialog1.FileName := '';
        SaveDialog1.DefaultExt := '';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          RepeatedPassword := PasswordEdit.Text;
          while true do
          begin
            try
              DeCoder32_DecodeFile(FChosenFile, AOutput, RepeatedPassword, OnProgressProc);
              break;
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                // #0 means that the password char '*' is used
                if not InputQuery(Caption, #0 + LoadResString(@STryAgain) + ' ' + E.Message, RepeatedPassword) then
                  Abort;
              end;
            end;
          end;
          MessageDlg(LoadResString(@SInfoLegacyDecrypt), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 3.0 decrypt'}
      TAG_DC30_DECRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*';
        {$ENDIF}
        SaveDialog1.FileName := '';
        SaveDialog1.DefaultExt := '';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          RepeatedPassword := PasswordEdit.Text;
          while true do
          begin
            try
              DeCoder30_DecodeFile(FChosenFile, AOutput, RepeatedPassword, OnProgressProc);
              break;
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                // #0 means that the password char '*' is used
                if not InputQuery(Caption, #0 + LoadResString(@STryAgain) + ' ' + E.Message, RepeatedPassword) then
                  Abort;
              end;
            end;
          end;
          MessageDlg(LoadResString(@SInfoLegacyDecrypt), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 2.2 decrypt'}
      TAG_DC22_DECRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*';
        {$ENDIF}
        SaveDialog1.FileName := '';
        SaveDialog1.DefaultExt := '';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          RepeatedPassword := PasswordEdit.Text;
          while true do
          begin
            try
              if TryStrToInt(RepeatedPassword, iKey) then iKey := -1;
              DeCoder22_DecodeFile(FChosenFile, AOutput, iKey, OnProgressProc);
              break;
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                // #0 means that the password char '*' is used
                if not InputQuery(Caption, #0 + LoadResString(@STryAgain) + ' ' + E.Message, RepeatedPassword) then
                  Abort;
              end;
            end;
          end;
          MessageDlg(LoadResString(@SInfoLegacyDecrypt), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 2.1 decrypt'}
      TAG_DC21_DECRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*';
        {$ENDIF}
        SaveDialog1.FileName := '';
        SaveDialog1.DefaultExt := '';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          RepeatedPassword := PasswordEdit.Text;
          while true do
          begin
            try
              if TryStrToInt(RepeatedPassword, iKey) then iKey := -1;
              DeCoder21_DecodeFile(FChosenFile, AOutput, iKey, OnProgressProc);
              break;
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                // #0 means that the password char '*' is used
                if not InputQuery(Caption, #0 + LoadResString(@STryAgain) + ' ' + E.Message, RepeatedPassword) then
                  Abort;
              end;
            end;
          end;
          MessageDlg(LoadResString(@SInfoLegacyDecrypt), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ENDIF}
          PasswordEdit.Text := '';
        end;
      end;
      {$ENDREGION}
      {$REGION '(De)Coder 2.0 decrypt'}
      TAG_DC20_DECRYPT:
      begin
        if PasswordEdit.Text = '' then exit;
        {$IFDEF MsWindows}
        SaveDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*';
        {$ENDIF}
        SaveDialog1.FileName := '';
        SaveDialog1.DefaultExt := '';
        if SaveDialog1.Execute then
        begin
          AOutput := SaveDialog1.FileName;
          DeCoder20_DecodeFile(FChosenFile, AOutput, OnProgressProc);
          MessageDlg(LoadResString(@SInfoLegacyDecrypt), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
          {$IFDEF MsWindows}
          ExplorerNavigateToFile(AOutput);
          {$ENDIF}
        end;
      end;
      {$ENDREGION}
      {$REGION 'File shredder'}
      TAG_SHRED:
      begin
        MoreInfoMemo.Text := ''; // will be filled with progress text from SecureDelete*()
        if FileExists(FChosenFile) then
          SecureDeleteFile(FChosenFile, TextCallback)
        else if DirectoryExists(FChosenFile) then
          SecureDeleteFolder(FChosenFile, TextCallback)
        else
          raise Exception.CreateResFmt(@SFileOrFolderSNotFound, [FChosenFile]);
        FChosenFile := '';
        GuiShowChosenFile;
        OpenedFileLabel.Text := LoadResString(@SDestroyComplete);
        PlayEmptyRecycleBinSound;
      end;
      {$ENDREGION}
    end;
  finally
    ProgressBar1.Visible := false;
    if TButton(Sender).Tag < 0 then
      TButton(Sender).Tag := -TButton(Sender).Tag; // allow clicking again
  end;
end;

procedure TDecoderMainForm.ComboBox1Change(Sender: TObject);
begin
  if FChosenFile <> '' then
    OpenFile(FChosenFile)
  else
    InitView;
end;

procedure TDecoderMainForm.DropTarget1Click(Sender: TObject);
begin
  {$IFDEF MsWindows}
  OpenDialog1.Filter := LoadResString(@SAllFiles)+' (*.*)|*.*|'+LoadResString(@SEncryptedFiles)+' (*.dc4;*.dc5)|*.dc4;*.dc5';
  {$ENDIF}
  OpenDialog1.FileName := '';
  OpenDialog1.DefaultExt := '';
  if OpenDialog1.Execute then
  begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TDecoderMainForm.DropTarget1Dropped(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  if Length(Data.Files) > 1 then raise Exception.CreateRes(@SPleaseChooseOnlyOneFile);
  try
    OpenFile(Data.Files[0]);
  except
    on E: EAbort do
    begin
      Abort;
    end;
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

procedure TDecoderMainForm.InitView;
begin
  Caption := LoadResString(@SProductTitle);
  OpenedFileLabel.Text := LoadResString(@SProductTitle);
  ShortInfoLabel.Text :=
    Format(LoadResString(@S_IntroLine_0_S), [GetDecoderVersion]) + #13#10 +
    LoadResString(@S_IntroLine_1) + #13#10 +
    LoadResString(@S_IntroLine_2) + #13#10 +
    LoadResString(@S_IntroLine_3) + #13#10 +
    LoadResString(@S_IntroLine_4) + #13#10 +
    LoadResString(@S_IntroLine_5) + #13#10 +
    LoadResString(@S_IntroLine_6) + #13#10 +
    LoadResString(@S_IntroLine_7) + #13#10 +
    LoadResString(@S_IntroLine_8) + #13#10 +
    LoadResString(@S_IntroLine_9) + #13#10 +
    LoadResString(@S_IntroLine_10) + #13#10 +
    LoadResString(@S_IntroLine_11);
  ProgressBar1.Visible := false; // will be automatically shown and hidden by OnProgressProc
  ProgressStepLabel.Visible := false;
  GuiShowElements([]);
end;

procedure TDecoderMainForm.FormShow(Sender: TObject);
begin
  ComboBox1.ItemIndex := CB1_IDX_DC45;
  try
    if ParamCount > 1 then
      MessageDlg(LoadResString(@SPleaseChooseOnlyOneFile), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
    else if ParamCount = 1 then
      OpenFile(ParamStr(1));
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      MessageDlg(e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    end;
  end;
end;

procedure TDecoderMainForm.OpenFile(const AFileName: string);
var
  fp: TDC4Parameters;
begin
  if not FileExists(AFileName) and not DirectoryExists(AFileName) then
    raise Exception.CreateResFmt(@SFileOrFolderSNotFound, [AFileName]);

  EncryptDecryptButton.Tag := 0;

  MoreInfoMemo.Lines.Clear;
  if FileExists(AFileName) then
    MoreInfoMemo.Lines.Add(Format(LoadResString(@SFileName_S), [ExtractFileName(AFileName)]))
  else
    MoreInfoMemo.Lines.Add(Format(LoadResString(@SFolderName_S), [ExtractFileName(AFileName)]));
  MoreInfoMemo.Lines.Add(Format(LoadResString(@SLocation_S), [ExtractFilePath(AFileName)]));

  if ComboBox1.ItemIndex = CB1_IDX_DC45 then
  begin
    if FileExists(AFileName) then
    begin
      {$REGION '(De)Coder 1.0 decrypt'}
      if DeCoder10_DetectFile(AFileName) then
      begin
        ShortInfoLabel.Text := LoadResString(@SEncryptedDc10) + #13#10 + LoadResString(@SDoYouWantDecrypt) + #13#10#13#10 + LoadResString(@SWarnLegacyDecrypt);
        EncryptDecryptButton.Tag := TAG_DC10_DECRYPT;
        EncryptDecryptButton.Text := LoadResString(@SDecrypt);
        GuiShowElements([geStartButton]);
        FChosenFile := AFileName;
        GuiShowChosenFile;
        Exit;
      end;
      {$ENDREGION}

      {$REGION '(De)Coder 4.x/5.x decrypt'}
      try
        FDC4FileInfo := DeCoder4X_FileInfo(AFileName);
        if FDC4FileInfo.Parameters.Dc4FormatVersion >= fvDc50 then
          ShortInfoLabel.Text := LoadResString(@SEncryptedDc50)
        else if FDC4FileInfo.Parameters.Dc4FormatVersion >= fvDc41Beta then
          ShortInfoLabel.Text := LoadResString(@SEncryptedDc41Beta)
        else
          ShortInfoLabel.Text := LoadResString(@SEncryptedDc40);
        ShortInfoLabel.Text := ShortInfoLabel.Text + #13#10 + LoadResString(@SDoYouWantDecrypt);
        DeCoder4X_PrintFileInfo(FDC4FileInfo, MoreInfoMemo.Lines);
        EncryptDecryptButton.Tag := TAG_DC4X_DECRYPT;
        EncryptDecryptButton.Text := LoadResString(@SDecrypt);
        GuiShowElements([gePassword, geStartButton, geInfos]);
        FChosenFile := AFileName;
        GuiShowChosenFile;
        Exit;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          if AFileName.EndsWith('.dc4', true) or AFileName.EndsWith('.dc5', true) then
          begin
            raise Exception.CreateResFmt(@SNoValidDc45File, [E.Message]);
          end;
        end;
      end;
      {$ENDREGION}
    end;

    {$REGION '(De)Coder 5.x encrypt'}
    if DirectoryExists(AFileName) then
      ShortInfoLabel.Text := LoadResString(@SThisIsAFolder) + #13#10 + LoadResString(@SDoYouWantPackAndEncrypt)
    else
      ShortInfoLabel.Text := LoadResString(@SThisIsAnUnencryptedFile) + #13#10 + LoadResString(@SDoYouWantEncrypt);
    EncryptDecryptButton.Tag := TAG_DC50_ENCRYPT;
    EncryptDecryptButton.Text := LoadResString(@SEncrypt);
    if FileExists(AFileName) then
    begin
      MoreInfoMemo.Lines.Add(Format(LoadResString(@SFileType_S), [GetFileTypename(AFileName)]));
      MoreInfoMemo.Lines.Add(Format(LoadResString(@SFileSize_S), [FileSizeHumanReadable(TFile.GetSize(AFileName))]));
      MoreInfoMemo.Lines.Add(Format(LoadResString(@SModTime_S), [DateTimeToStr(TFile.GetLastWriteTime(AFileName))]));
    end;
    fp := DeCoder4X_GetDefaultParameters(High(TDc4FormatVersion));
    MetadataCheckbox.IsChecked := (fp.ContainFileOrigName=fpExpose) and fp.ContainFileOrigSize and fp.ContainFileOrigDate;
    GuiShowElements([gePassword, geStartButton, geInfos, geMetadataCheckbox]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end
  else if ComboBox1.ItemIndex = CB1_IDX_DC32 then
  begin
    {$REGION '(De)Coder 3.2 decrypt'}
    EncryptDecryptButton.Tag := TAG_DC32_DECRYPT;
    EncryptDecryptButton.Text := LoadResString(@SDecrypt);
    ShortInfoLabel.Text := Format(LoadResString(@SInfoLegacyDC_S), ['3.2']) + #13#10#13#10 + LoadResString(@SWarnLegacyDecrypt);
    GuiShowElements([gePassword, geStartButton, geInfos]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end
  else if ComboBox1.ItemIndex = CB1_IDX_DC30 then
  begin
    {$REGION '(De)Coder 3.0 decrypt'}
    EncryptDecryptButton.Tag := TAG_DC30_DECRYPT;
    EncryptDecryptButton.Text := LoadResString(@SDecrypt);
    ShortInfoLabel.Text := Format(LoadResString(@SInfoLegacyDC_S), ['3.0']) + #13#10#13#10 + LoadResString(@SWarnLegacyDecrypt);
    GuiShowElements([gePassword, geStartButton, geInfos]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end
  else if ComboBox1.ItemIndex = CB1_IDX_DC22 then
  begin
    {$REGION '(De)Coder 2.2 decrypt'}
    EncryptDecryptButton.Tag := TAG_DC22_DECRYPT;
    EncryptDecryptButton.Text := LoadResString(@SDecrypt);
    ShortInfoLabel.Text := Format(LoadResString(@SInfoLegacyDC_S), ['2.2']) + #13#10#13#10 + LoadResString(@SWarnLegacyDecrypt);
    GuiShowElements([gePassword, geStartButton, geInfos]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end
  else if ComboBox1.ItemIndex = CB1_IDX_DC21 then
  begin
    {$REGION '(De)Coder 2.1 decrypt'}
    EncryptDecryptButton.Tag := TAG_DC21_DECRYPT;
    EncryptDecryptButton.Text := LoadResString(@SDecrypt);
    ShortInfoLabel.Text := Format(LoadResString(@SInfoLegacyDC_S), ['2.1']) + #13#10#13#10 + LoadResString(@SWarnLegacyDecrypt);
    GuiShowElements([gePassword, geStartButton, geInfos]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end
  else if ComboBox1.ItemIndex = CB1_IDX_DC20 then
  begin
    {$REGION '(De)Coder 2.0 decrypt'}
    EncryptDecryptButton.Tag := TAG_DC20_DECRYPT;
    EncryptDecryptButton.Text := LoadResString(@SDecrypt);
    ShortInfoLabel.Text := Format(LoadResString(@SInfoLegacyDC_S), ['2.0']) + #13#10#13#10 + LoadResString(@SWarnLegacyDecrypt);
    GuiShowElements([geStartButton, geInfos]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end
  else if ComboBox1.ItemIndex = CB1_IDX_SHRED then
  begin
    {$REGION 'File shredder'}
    EncryptDecryptButton.Tag := TAG_SHRED;
    EncryptDecryptButton.Text := LoadResString(@SShredderButton);
    ShortInfoLabel.Text := #13#10#13#10 + LoadResString(@SShredderInfo);
    GuiShowElements([geStartButton, geInfos]);
    FChosenFile := AFileName;
    GuiShowChosenFile;
    {$ENDREGION}
  end;
end;

end.
