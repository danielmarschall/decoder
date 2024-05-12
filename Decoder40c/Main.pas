unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, DECUtil,
  DECHash, DECCipher, registry, KAZip, shlobj, DropTarget, DropSource, Dialogs,
  StdCtrls, Grids, ValEdit, math, shellapi, Buttons, XPMan, ComCtrls, ExtCtrls,
  inifiles;

type
  TCase = (tsBroken, tcEncrypted, tcDecrypted);

  TLanguageEntry = record
    name: string;
    text: string;
  end;

  TProgress = class(TInterfacedObject, IDECProgress)
    procedure Process(const Min,Max,Pos: Int64); stdcall;
    constructor Create;
    destructor Destroy; override;
  end;

  TMainForm = class(TForm)
    dlg_open: TOpenDialog;
    dlg_save_dec: TSaveDialog;
    dlg_save_enc: TSaveDialog;
    vle: TValueListEditor;
    btn_config: TBitBtn;
    btn_open: TBitBtn;
    btn_folder: TBitBtn;
    btn_help: TBitBtn;
    btn_dec: TBitBtn;
    btn_enc: TBitBtn;
    grp_dec: TGroupBox;
    lbl_dec_label1: TLabel;
    edt_dec_password: TEdit;
    grp_enc: TGroupBox;
    edt_enc_password: TEdit;
    edt_enc_password2: TEdit;
    chk_enc_securedelete: TCheckBox;
    lbl_enc_label1: TLabel;
    lbl_enc_label2: TLabel;
    lbl_enc_entropy: TLabel;
    lbl_enc_equal: TLabel;
    lbl_file: TLabel;
    lbl_label1: TLabel;
    xp: TXPManifest;
    tmr_wait: TTimer;
    img_enc_warning: TImage;
    lbl_enc_warning: TLabel;
    statusbar: TStatusBar;
    chk_dec_securedelete: TCheckBox;
    img_dec_warning: TImage;
    lbl_dec_warning: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure kazip_actions(Sender: TObject; Current, Total: Integer);
    procedure kazip_add(Sender: TObject; ItemName: string);
    procedure tmr_wait_timer(Sender: TObject);
    procedure btn_open_click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_dec_click(Sender: TObject);
    procedure edt_dec_password_keypress(Sender: TObject; var Key: Char);
    procedure btn_config_click(Sender: TObject);
    procedure btn_help_click(Sender: TObject);
    procedure btn_enc_click(Sender: TObject);
    procedure btn_folder_click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure dropfileDrop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure edt_enc_password_change(Sender: TObject);
    procedure edt_enc_password2_change(Sender: TObject);
    procedure edt_enc_password_keypress(Sender: TObject; var Key: Char);
    procedure edt_enc_password2_keypress(Sender: TObject; var Key: Char);
  public
    // VCL-Ersatz start
    DropFile: TDropFileTarget;
    KaZip: TKaZip;
    // VCL-Ersatz ende
    fileopen: string; // geöffnete datei
    ersatzname: string; // wenn ein ordner verschlüsselt wird, ordnernamen statt zip-dateinamen angeben
    ausgabegesetzt: string; // befehlszeile, wenn gesetzt
    errorlevel: integer; // fehlerlevel für befehlszeilenanwendung
    flag: byte; // ordner oder datei verschlüsselt?
    mode: TCase; // status der geöffneten datei?
    procedure OpenFile(fil: string; alternativer_name: string = '');
    procedure OpenFolder(fol: string);
    function Unzip(ZipFile, Verzeichnis: string): boolean;
    procedure zipfolder(verzeichnis, zipname: string);
    procedure FensterInit();
  private
    LangArray: Array of TLanguageEntry;
    tempstream: TStream;
    progress_pos: integer;
    lg_StartFolder: String;
    temp_unique_number: string;
    procedure steuerelementesperren(sperren: boolean);
    function GetLangEntry(name: string): string;
    procedure progress_position(pos: integer);
    procedure progress_text(text: string);
  end;

var
  MainForm: TMainForm;

implementation

uses Config;

{$R *.dfm}

const
  SeedSize = 16;
  formanfangsgroesse = 275;
  dc_version = $01;
  progress_min = 0;
  progress_max = 100; // wenn auf eine progressbar migriert werden sollte

function TMainForm.GetLangEntry(name: string): string;
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

function IsCapsLockOn: boolean;
begin
  Result := 0 <>
    (GetKeyState(VK_CAPITAL) and $01);
end;

procedure TMainForm.progress_position(pos: integer);
begin
  progress_pos := pos;
  if mainform.statusbar.Panels.Items[1].Text <> inttostr(pos)+'%' then
    mainform.statusbar.Panels.Items[1].Text := inttostr(pos)+'%';
  application.ProcessMessages;
end;

procedure TMainForm.progress_text(text: string);
begin
  statusbar.Panels.Items[0].Text := text;
  application.processmessages;
end;

procedure dc_deletefile(Filename: string);
const
  buf = 1024;
var
  S: TStream;
  size: int64;
  abbruch: boolean;
begin
  if fileexists(filename) then
  begin
    S := TFileStream.Create(Filename, fmOpenWrite or fmShareExclusive);
    try
      size := S.Size;
      mainform.progress_position(progress_min);
      mainform.progress_text(mainform.GetLangEntry('deletefile'));
      abbruch := false;
      while not abbruch do
      begin
        size := size - buf;
        if size > 0 then
        begin
          ProtectStream(S, buf);
          mainform.progress_position(floor((s.size-size) / s.Size * progress_max));
        end
        else
        begin
          if size < 0 then
            ProtectStream(S, s.size-s.Position); // wenn nicht size = 0

          mainform.progress_position(progress_Min);
          mainform.progress_text(mainform.GetLangEntry('ready'));

          abbruch := true;
        end;
      end;
    finally
      S.Free;
    end;
    deletefile(filename);
  end;
end;

procedure dc_deletedir(const Directory: String);
var
  SR: TSearchRec;
  zus: string;
begin
  if copy(Directory, length(Directory), 1) <> '\' then
    zus := '\'
  else
    zus := '';

  if FindFirst(Directory + zus + '*.*', faAnyFile - faDirectory, SR) = 0 then
  try
    repeat
      try
        SetFileAttributes(pchar(Directory + zus + SR.Name), FILE_ATTRIBUTE_NORMAL);
        dc_deletefile(Directory + zus + SR.Name)
      except
        // z.B. durch Benutzung oder durch Schreibschutz verursacht
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  if FindFirst(Directory + zus + '*.*', faAnyFile, SR) = 0 then
  try
    repeat
      if ((SR.attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        SetFileAttributes(pchar(Directory + zus + SR.Name), FILE_ATTRIBUTE_NORMAL);
        dc_deletedir(Directory + zus + SR.Name + '\');
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  removedirectory(pchar(Directory));
end;

// http://www.swissdelphicenter.ch/torry/showcode.php?id=144
function GetTempDir: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  Result := StrPas(Buffer);
end;

function HexAnsicht(inp: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to length(inp) do
    result := result + inttohex(ord(inp[i]), 2) + ' ';
end;

// http://www.delphipraxis.net/post50248.html 
  function PassphraseQuality(const Password: String): Extended;
// returns computed Quality in range 0.0 to 1.0
// source extracted from Delphi Encryption Compendium, DEC 

  function Entropy(P: PByteArray; L: Integer): Extended; 
  var 
    Freq: Extended; 
    I: Integer; 
    Accu: array[Byte] of LongWord; 
  begin
    Result := 0.0; 
    if L <= 0 then Exit; 
    FillChar(Accu, SizeOf(Accu), 0); 
    for I := 0 to L-1 do Inc(Accu[P[I]]); 
    for I := 0 to 255 do 
      if Accu[I] <> 0 then 
      begin
        Freq := Accu[I] / L;
        Result := Result - Freq * (Ln(Freq) / Ln(2)); 
      end; 
  end; 

  function Differency: Extended; 
  var 
    S: String; 
    L,I: Integer; 
  begin 
    Result := 0.0; 
    L := Length(Password); 
    if L <= 1 then Exit; 
    SetLength(S, L-1);
    for I := 2 to L do
      Byte(S[I-1]) := Byte(Password[I-1]) - Byte(Password[I]); 
    Result := Entropy(Pointer(S), Length(S)); 
  end; 

  function KeyDiff: Extended; 
  const 
    Table = '^1234567890ß´qwertzuiopü+asdfghjklöä#<yxcvbnm,.-°!"§$%&/()=?`QWERTZUIOPÜ*ASDFGHJKLÖÄ''>YXCVBNM;:_'; 
  var 
    S: String; 
    L,I,J: Integer; 
  begin 
    Result := 0.0; 
    L := Length(Password); 
    if L <= 1 then Exit; 
    S := Password; 
    UniqueString(S); 
    for I := 1 to L do 
    begin
      J := Pos(S[I], Table);
      if J > 0 then S[I] := Char(J); 
    end; 
    for I := 2 to L do 
      Byte(S[I-1]) := Byte(S[I-1]) - Byte(S[I]); 
    Result := Entropy(Pointer(S), L-1); 
  end; 

const 
  GoodLength = 10.0; // good length of Passphrases 
var 
  L: Extended; 
begin 
  Result := Entropy(Pointer(Password), Length(Password)); 
  if Result <> 0 then 
  begin
    Result := Result * (Ln(Length(Password)) / Ln(GoodLength));
    L := KeyDiff + Differency; 
    if L <> 0 then L := L / 64; 
    Result := Result * L; 
    if Result < 0 then Result := -Result; 
    if Result > 1 then Result := 1; 
  end; 
end;

///////////////////////////////////////////////////////////////////
// Call back function used to set the initial browse directory.
///////////////////////////////////////////////////////////////////
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
        lParam, lpData: LPARAM): Integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage(Wnd,BFFM_SETSELECTION,1,Integer(@mainform.lg_StartFolder[1]));
  result := 0;
end;

///////////////////////////////////////////////////////////////////
// This function allows the user to browse for a folder
//
// Arguments:-
//    browseTitle : The title to display on the browse dialog.
//  NewFolder : Allow to create a new folder
//  initialFolder : Optional argument. Use to specify the folder
//                  initially selected when the dialog opens.
//
// Returns: The empty string if no folder was selected (i.e. if the
//          user clicked cancel), otherwise the full folder path.
///////////////////////////////////////////////////////////////////
function BrowseForFolder(const browseTitle: String; const NewFolder: boolean = false;
        const initialFolder: String =''): String;
var
  browse_info: TBrowseInfo;
  folder: array[0..MAX_PATH] of char;
  find_context: PItemIDList;
const
  BIF_NEWDIALOGSTYLE=$40;
begin
  FillChar(browse_info,SizeOf(browse_info),#0);
  mainform.lg_StartFolder := initialFolder;
  browse_info.pszDisplayName := @folder[0];
  browse_info.lpszTitle := PChar(browseTitle);
  if NewFolder then
    browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE
  else
    browse_info.ulFlags := BIF_RETURNONLYFSDIRS;
  browse_info.hwndOwner := Application.Handle;
  if initialFolder <> '' then
    browse_info.lpfn := BrowseForFolderCallBack;
  find_context := SHBrowseForFolder(browse_info);
  if Assigned(find_context) then
  begin
    if SHGetPathFromIDList(find_context,folder) then
      result := folder
    else
      result := '';
    GlobalFreePtr(find_context);
  end
  else
    result := '';
end;

// Entnommen von FileCtrl
procedure CutFirstDirectory(var S: TFileName);
var
  Root: Boolean;
  P: Integer;
begin
  if S = '\' then
    S := ''
  else
  begin
    if S[1] = '\' then
    begin
      Root := True;
      Delete(S, 1, 1);
    end
    else
      Root := False;
    if S[1] = '.' then
      Delete(S, 1, 4);
    P := AnsiPos('\',S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
    if Root then
      S := '\' + S;
  end;
end;

// Entnommen von FileCtrl
function MinimizeName(const Filename: TFileName; Canvas: TCanvas;
  MaxLen: Integer): TFileName;
var
  Drive: TFileName;
  Dir: TFileName;
  Name: TFileName;
begin
  Result := FileName;
  Dir := ExtractFilePath(Result);
  Name := ExtractFileName(Result);

  if (Length(Dir) >= 2) and (Dir[2] = ':') then
  begin
    Drive := Copy(Dir, 1, 2);
    Delete(Dir, 1, 2);
  end
  else
    Drive := '';
  while ((Dir <> '') or (Drive <> '')) and (Canvas.TextWidth(Result) > MaxLen) do
  begin
    if Dir = '\...\' then
    begin
      Drive := '';
      Dir := '...\';
    end
    else if Dir = '' then
      Drive := ''
    else
      CutFirstDirectory(Dir);
    Result := Drive + Dir + Name;
  end;
end;

// Entnommen von http://www.delphipraxis.net/post429178.html
function IsFileName(FileName: String): Boolean; 
const ForbiddenChars = ['"', '<', '>', '|', '*', '/', '\', '?']; // verbotene Zeichen 

const ForbiddenNames: Array[0..22] of String[6] = ('AUX', 'NUL', 'PRN' ,'CON', 'CLOCK$',  // verbotene Namen 
'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9', 
'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9');

var i: Integer;
var p: PChar;
var FileNameU: String;
begin 
Result := False; 

  if FileName <> '' then // Name darf nicht leer sein 
  begin
    i := Length(FileName);

    if FileName[i] <> '.' then // letze Zeichen darf kein Punkt sein 
    begin 
      p := Pointer(FileName); 

      repeat if p^ in ForbiddenChars then 
        Exit; 
        inc(p); 
      until p^ = #0; 

      if (i < 7) and (i > 2) then 
      begin 
        FileNameU := UpperCase(FileName);
        for i := 0 to High(ForbiddenNames) do
        begin 
          if CompareStr(ForbiddenNames[i], FileNameU) = 0 then 
          Exit; 
        end; 
      end; 

    Result := True; 
  end; 
  end; 
end;

  procedure EncodeFile(const AFileName, AOutput: String; const APassword: Binary;
                       ACipher: TDECCipherClass = nil; AMode: TCipherMode = cmCTSx;
                       AHash: TDECHashClass = nil);

    procedure Write(const Value; Size: Integer);
    begin
      mainform.tempstream.WriteBuffer(Value, Size);
    end;

    procedure WriteByte(Value: Byte);
    begin
      Write(Value, SizeOf(Value));
    end;

    procedure WriteLong(Value: LongWord);
    begin
      Value := SwapLong(Value);
      Write(Value, SizeOf(Value));
    end;

    procedure WriteBinary(const Value: Binary);
    begin
      WriteByte(Length(Value));
      Write(Value[1], Length(Value));
    end;

    procedure WriteRaw(const Value: Binary);
    begin
      Write(Value[1], Length(Value));
    end;

  var
    Source: TStream;
    Seed: Binary;
    P: IDECProgress;
  begin
    mainform.progress_position(progress_min);
    mainform.progress_text(mainform.GetLangEntry('encodedata'));

    ACipher := ValidCipher(ACipher);
    AHash := ValidHash(AHash);

    Seed := RandomBinary(SeedSize);

    Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        mainform.tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
        try
          with ACipher.Create do
          try
            try
              Mode := AMode;

              Init(AHash.KDFx(APassword, Seed, Context.KeySize));

              WriteByte(mainform.flag);
              WriteByte(dc_version);
              if mainform.ersatzname <> '' then
                WriteRaw(ExtractFileName(mainform.ersatzname)+'?')
              else
                WriteRaw(ExtractFileName(AFileName)+'?');
              WriteRaw(Seed);

              P := TProgress.Create;
              try
                EncodeStream(Source, mainform.tempstream, Source.Size, P);
              except
                // Kann auftreten, wenn das Programm beim Verschlüsseln geschlossen wird
                exit;
              end;
              source.Position := 0;
              WriteRaw(ahash.CalcStream(source, source.size));
            finally
              Free;
            end;
          except
            raise;
          end;
        finally
          try
            mainform.tempstream.Free;
          except
            // kann passieren, wenn der Stream bereits durch FormClose geschlossen wurde
          end;
        end;
      except
        // Ist eine tempoäre Datei und wird bei Programmbeendigung gelöscht
        // dc_deletefile(aoutput);
        mainform.steuerelementesperren(false);
        raise; // programmablauf endet hier
      end;
    finally
      Source.Free;
    end;
  end;

  procedure DecodeFile(const AFileName, AOutput: String; const APassword: Binary);
  var
    Source: TStream;
    OrigName: string;
    ahash: TDECHashClass;

    procedure Read(var Value; Size: Integer);
    begin
      Source.ReadBuffer(Value, Size);
    end;

    function ReadByte: Byte;
    begin
      Read(Result, SizeOf(Result));
    end;

    function ReadLong: LongWord;
    begin
      Read(Result, SizeOf(Result));
      Result := SwapLong(Result);
    end;

    function ReadBinary: Binary;
    begin
      SetLength(Result, ReadByte);
      Read(Result[1], Length(Result));
    end;

    function ReadRaw(leng: integer): Binary;
    begin
      SetLength(Result, leng);
      Read(Result[1], Length(Result));
    end;

  var
    ch: string;
    P: IDECProgress;
  begin
    mainform.progress_position(progress_min);
    mainform.progress_text(mainform.GetLangEntry('decodedata'));

    Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        ReadByte; // F
        ReadByte; // V

        OrigName := '';
        ch := readraw(1);
        while ch <> '?' do
        begin
          OrigName := OrigName + ch;
          ch := readraw(1);
        end;

        mainform.tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
        try
          try
            with ValidCipher(nil).Create do
            try
              Mode := TCipherMode(0);
              ahash := ValidHash(nil);

              Init(ahash.KDFx(APassword, ReadRaw(SeedSize), Context.KeySize));

              P := TProgress.Create;
              try
                DecodeStream(Source, mainform.tempstream, source.size-source.Position-ahash.DigestSize , P);
              except
                // Kann auftreten, wenn das Programm beim verschlüsseln geschlossen wird
                exit;
              end;
              mainform.tempstream.position := 0;
              if readraw(ahash.DigestSize) <> ahash.CalcStream(mainform.tempstream, mainform.tempstream.size) then
                raise EDECException.Create(mainform.GetLangEntry('hasherror'));
            finally
              Free;
            end;
          except
            // ProtectStream wird in dc_deletefile durchgeführt
            // ProtectStream(Dest);
            raise;
          end;
        finally
          try
            mainform.tempstream.Free;
          except
            // kann passieren, wenn der Stream bereits durch FormClose geschlossen wurde
          end;
        end;
      except
        // Ist eine tempoäre Datei und wird bei Programmbeendigung gelöscht
        // dc_deletefile(aoutput);
        mainform.steuerelementesperren(false);
        raise; // programmablauf endet hier
      end;
    finally
      Source.Free;
    end;
  end;

function FindeDateityp(filename: string): string;
var
  Reg: TRegistry;
  temp: string;
begin
  {* Dateityp herausfinden *}
  if ExtractFileExt(filename) <> '' then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKey(ExtractFileExt(filename), false) then
      begin
        temp := Reg.ReadString('');
        Reg.CloseKey();
        if temp <> '' then
        begin
          Reg.OpenKey(temp, false);
          temp := Reg.ReadString('');
          Reg.CloseKey();
          if temp <> '' then
            result := temp
          else
            result := ''; {* Weiterleitung hat keinen Namen für den Dateityp *}
        end
        else
          result := ''; {* Konnte keine Weiterleitung in der Registry finden *}
      end
      else
        result := ''; {* Keinen Eintrag der Erweiterung in der Registry gefunden *}
    finally
      Reg.free;
    end;
  end
  else
    result := ''; {* Keine Erweiterung *}
end;

procedure TMainForm.FensterInit();
begin
  grp_dec.Visible := mode = tcDecrypted;
  btn_dec.visible := mode = tcDecrypted;
  grp_enc.Visible := mode = tcEncrypted;
  btn_enc.visible := mode = tcEncrypted;

  if mode = tcEncrypted then
  begin
    top := top + ((ClientHeight - (formanfangsgroesse + grp_enc.Height + 8)) div 2);
    ClientHeight := formanfangsgroesse + grp_enc.Height + 8;
    btn_enc.Top := grp_enc.Top + grp_enc.Height - btn_enc.Height;
  end;

  if mode = tcDecrypted then
  begin
    top := top + ((ClientHeight - (formanfangsgroesse + grp_dec.Height + 8)) div 2);
    ClientHeight := formanfangsgroesse + grp_dec.Height + 8;
    btn_dec.Top := grp_dec.Top + grp_dec.Height - btn_dec.Height;
  end;

  if top < 0 then top := 0;
  if top > screen.Height then top := screen.Height - GetSystemMetrics(SM_CYCAPTION);
end;

procedure TMainForm.OpenFile(fil: string; alternativer_name: string = '');
var
  Source: TStream;
  O, T: string;
  F, V: byte;
  ch: string;
  hash: TDECHashClass;
  cipher: TDECCipherClass;

  procedure Read(var Value; Size: Integer);
  begin
    Source.ReadBuffer(Value, Size);
  end;

  function ReadByte: Byte;
  begin
    Read(Result, SizeOf(Result));
  end;

  function ReadLong: LongWord;
  begin
    Read(Result, SizeOf(Result));
    Result := SwapLong(Result);
  end;

  function ReadBinary: Binary;
  begin
    SetLength(Result, ReadByte);
    Read(Result[1], Length(Result));
  end;

  function ReadRaw(leng: integer): Binary;
  begin
    SetLength(Result, leng);
    Read(Result[1], Length(Result));
  end;

  procedure Dateibeschaedigt();
  begin
    ClientHeight := formanfangsgroesse;
    vle.strings.Clear;
    vle.InsertRow(GetLangEntry('status'), GetLangEntry('broken'), true);
    if ausgabegesetzt <> '' then
      errorlevel := 1 // DC4 Datei beschädigt
    else
      Application.MessageBox(pchar(GetLangEntry('errorreading')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
  end;

begin
  cipher := validcipher(nil);
  hash := validhash(nil);

  steuerelementesperren(true);

  fileopen := fil;

  if alternativer_name <> '' then
  begin
    lbl_file.Caption := MinimizeName(alternativer_name, lbl_file.canvas, lbl_file.width);
    lbl_file.Hint := alternativer_name;
    caption := GetLangEntry('caption')+' [' + extractfilename(alternativer_name) + ']';
  end
  else
  begin
    lbl_file.Caption := MinimizeName(fil, lbl_file.canvas, lbl_file.width);
    lbl_file.Hint := fil;
    caption := GetLangEntry('caption')+' [' + extractfilename(fil) + ']';
  end;

  vle.Strings.Clear;

  edt_enc_password.Text := '';
  edt_enc_password2.Text := '';
  edt_dec_password.text := '';
  edt_enc_password_change(edt_enc_password);
  chk_enc_securedelete.Checked := (paramstr(2) = '/x');
  chk_dec_securedelete.Checked := (paramstr(2) = '/x');
  // wird durch edt_enc_password_change automatisch aufgerufen
  // edt_enc_password2_change(edt_enc_password2);

  Source := TFileStream.Create(fil, fmOpenRead or fmShareDenyNone);

  try
    try
      if extractfileext(fil) = '.dc4' then
      begin
        vle.InsertRow(GetLangEntry('status'), GetLangEntry('encrypted'), true);

        F := ReadByte;
        V := ReadByte;

        if ((F <> $00) and (F <> $01)) or (V <> dc_version)then
        begin
          Dateibeschaedigt;
          steuerelementesperren(false);
          exit;
        end
        else if F = $00 then grp_dec.Caption := GetLangEntry('filedecryption')
        else if F = $01 then grp_dec.Caption := GetLangEntry('folderdecryption');

        // damit später bei DecodeFile entschieden werden kann, ob entzippt werden muss
        flag := F;

        O := '';
        ch := readraw(1);
        while ch <> '?' do
        begin
          O := O + ch;
          ch := readraw(1);
        end;

        if IsFileName(O) then
        begin
          dlg_save_dec.FileName := O;
          dlg_save_dec.DefaultExt := copy(extractfileext(O), 2, length(extractfileext(O))-1);
        end
        else
          O := GetLangEntry('unknown');
        vle.InsertRow(GetLangEntry('originalname'), O, true);

        if F = $01 then
        begin
          T := GetLangEntry('folder');
          ersatzname := O; // damit nach decodefile der richtige ordnername verwendet wird
        end
        else
        begin
          T := FindeDateityp(O);
          ersatzname := '';
        end;

        if (T = '') then T := GetLangEntry('unknown');
        vle.InsertRow(GetLangEntry('filetype'), T, true);

        // vle.InsertRow(l_seed, HexAnsicht(ReadRaw(SeedSize)), true);

        if F = $00 then
          vle.InsertRow(GetLangEntry('filesize'), FloatToStrF((source.size - hash.DigestSize)-source.Position,ffNumber,14,0) + ' '+GetLangEntry('byte'), true);

        if F = $01 then
          vle.InsertRow(GetLangEntry('zipsize'), FloatToStrF((source.size - hash.DigestSize)-source.Position,ffNumber,14,0) + ' '+GetLangEntry('byte'), true);

        source.Position := source.size - cipher.context.BufferSize - hash.DigestSize;
        // vle.InsertRow(l_calcmac, HexAnsicht(readraw(cipher.context.BufferSize)), true);
        // vle.InsertRow(l_filehash, HexAnsicht(readraw(hash.DigestSize)), true);

        mode := tcDecrypted;

        FensterInit();

      end
      else
      begin

        // damit später in EncodeFile der richtige Flag geschrieben wird
        if alternativer_name <> '' then
        begin
          flag := $01;
          grp_enc.Caption := GetLangEntry('folderencryption');
        end
        else
        begin
          flag := $00;
          grp_enc.Caption := GetLangEntry('fileencryption');
        end;

        // damit später in EncodeFile nicht der Name der DeCoder.zip geschrieben wird, sondern der Verzeichnisname
        ersatzname := alternativer_name;

        vle.InsertRow(GetLangEntry('status'), GetLangEntry('unencrypted'), true);

        if alternativer_name <> '' then
          T := GetLangEntry('folder')
        else
        begin
          T := FindeDateityp(fil);
          if (T = '') then T := GetLangEntry('unknown');
        end;
        vle.InsertRow(GetLangEntry('filetype'), T, true);

        if alternativer_name = '' then
          vle.InsertRow(GetLangEntry('filesize'), FloatToStrF(source.size,ffNumber,14,0) + ' '+GetLangEntry('bytes'), true)
        else
          vle.InsertRow(GetLangEntry('zipsize'), FloatToStrF(source.size,ffNumber,14,0) + ' '+GetLangEntry('bytes'), true);

        mode := tcEncrypted;

        FensterInit();

      end;

    finally
      source.free;
    end;
  except
    dateibeschaedigt;
  end;

  steuerelementesperren(false);

  if (mode = tcEncrypted) and (edt_enc_password.Showing) then edt_enc_password.SetFocus;
  if (mode = tcDecrypted) and (edt_dec_password.Showing) then edt_dec_password.SetFocus;
end;

procedure TMainForm.btn_open_click(Sender: TObject);
begin
  if dlg_open.execute then
    openfile(dlg_open.FileName);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i: integer;
  arbeitsverzeichnis: string;
begin
  // VCL-Ersatz

  DropFile := TDropFileTarget.Create(self);
  DropFile.Dragtypes := [dtCopy];

  KaZip := TKaZip.Create(self);
  KaZip.OverwriteAction := oaOverwriteAll;

  // Anwendung vorbereiten

  SetDefaultCipherClass(TCipher_Rijndael);
  SetDefaultHashClass(THash_SHA512);

  RegisterDECClasses([TCipher_Rijndael, THash_SHA512]);

  IdentityBase := $59178954;

  RandomSeed;

  Randomize;
  temp_unique_number := inttostr(random(2147483647));

  // Sprachdatei auslesen

  if fileexists(ExtractFilePath(Application.ExeName)+'Language.ini') then
  begin
    ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'Language.ini');
    str := TStringList.Create();
    try
      ini.ReadSection(Name, str);
      for i := 0 to str.count-1 do
      begin
        setlength(LangArray, length(LangArray)+1);
        LangArray[length(LangArray)-1].name := str.strings[i];
        LangArray[length(LangArray)-1].text := ini.ReadString(name, str.strings[i], '');
        LangArray[length(LangArray)-1].text := StringReplace(LangArray[length(LangArray)-1].text, '###', #13#10, [rfReplaceAll]);
      end;
    finally
      ini.free;
      str.Free;
    end;
  end;

  // Formular vorbereiten

  clientheight := formanfangsgroesse;

  btn_open.Glyph.LoadFromResourceName(hInstance, 'OPENFILE');
  btn_folder.Glyph.LoadFromResourceName(hInstance, 'OPENFOLDER');
  btn_config.Glyph.LoadFromResourceName(hInstance, 'CONFIG');
  btn_help.Glyph.LoadFromResourceName(hInstance, 'HELP');
  btn_dec.Glyph.LoadFromResourceName(hInstance, 'CHECK');
  btn_enc.Glyph.LoadFromResourceName(hInstance, 'CHECK');
  img_enc_warning.Picture.Bitmap.LoadFromResourceName(hInstance, 'WARNING');
  img_dec_warning.Picture.Bitmap.LoadFromResourceName(hInstance, 'WARNING');

  caption := GetLangEntry('caption');
  btn_open.Hint := GetLangEntry('openhint');
  btn_folder.hint := GetLangEntry('folderhint');
  btn_config.hint := GetLangEntry('confighint');
  btn_help.Hint := GetLangEntry('helphint');
  btn_dec.Hint := GetLangEntry('dechint');
  btn_enc.hint := GetLangEntry('enchint');
  lbl_enc_label1.caption := GetLangEntry('enc_password');
  lbl_enc_label2.caption := GetLangEntry('enc_repassword');
  lbl_dec_label1.caption := GetLangEntry('dec_password');
  lbl_enc_equal.Hint := GetLangEntry('equal');
  lbl_enc_entropy.Hint := GetLangEntry('entropy');
  chk_enc_securedelete.Caption := GetLangEntry('securedelete');
  chk_dec_securedelete.Caption := GetLangEntry('securedelete');
  img_enc_warning.Hint := GetLangEntry('capswarning');
  lbl_enc_warning.caption := GetLangEntry('capslock');
  lbl_enc_warning.Hint := GetLangEntry('capswarning');
  img_dec_warning.Hint := GetLangEntry('capswarning');
  lbl_dec_warning.caption := GetLangEntry('capslock');
  lbl_dec_warning.Hint := GetLangEntry('capswarning');
  progress_text(GetLangEntry('ready'));
  dlg_open.filter := GetLangEntry('filter_all')+'|*.*|'+GetLangEntry('filter_encrypted')+'|*.dc4';
  dlg_save_dec.filter := GetLangEntry('filter_all')+'|*.*';
  dlg_save_enc.filter := GetLangEntry('filter_encrypted')+'|*.dc4';
  progress_position(progress_min);

  lbl_label1.Caption := GetLangEntry('opened');
  lbl_file.caption := GetLangEntry('nofile');
  vle.TitleCaptions.Strings[0] := GetLangEntry('property');
  vle.TitleCaptions.Strings[1] := GetLangEntry('value');

  DropFile.Register(mainform);

  // Verarbeitung der Befehlszeile

  errorlevel := 0; // Keine Fehler
  if (paramcount > 1) then
  begin
    if (paramstr(2) <> '/c') and (paramstr(2) <> '/x') then
      Application.MessageBox(pchar(GetLangEntry('onlyonefileparam')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
    else
    begin
      if (fileexists(paramstr(1))) and (paramcount = 4) then
      begin
        GetDir(0, arbeitsverzeichnis);
        ausgabegesetzt := arbeitsverzeichnis+'\'+paramstr(4);
        openfile(paramstr(1));

        if mode = tcEncrypted then
        begin
          // Datei verschlüsseln
          if extractfileext(ausgabegesetzt) <> '.dc4' then
            ausgabegesetzt := ausgabegesetzt + '.dc4'; // Ausgabe muss .dc4 haben!
          edt_enc_password.Text := paramstr(3);
          edt_enc_password2.Text := paramstr(3);
          try
            btn_enc.click;
          except
            // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
            // bleiben, deswegen der Try-Except-Block.
            errorlevel := 2; // Fehler bei Ver/Entschlüsselung
          end;
        end;

        if mode = tcDecrypted then
        begin
          // Datei oder Ordner entschlüsseln
          if extractfileext(ausgabegesetzt) = '.dc4' then
            ausgabegesetzt := copy(ausgabegesetzt, 0, length(ausgabegesetzt)-length('.dc4')); // Ausgabe darf nicht .dc4 haben!
          edt_dec_password.Text := paramstr(3);
          try
            btn_dec.click;
          except
            // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
            // bleiben, deswegen der Try-Except-Block.
            errorlevel := 2; // Fehler bei Ver/Entschlüsselung
          end;
        end;
      end
      else if (directoryexists(paramstr(1))) and (paramcount = 4) then
      begin
        // Ordner verschlüsseln
        openfolder(paramstr(1));
        edt_enc_password.Text := paramstr(3);
        edt_enc_password2.Text := paramstr(3);
        ausgabegesetzt := paramstr(4);
        if extractfileext(ausgabegesetzt) <> '.dc4' then
        ausgabegesetzt := ausgabegesetzt + '.dc4'; // Ausgabe muss .dc4 haben!
        try
          btn_enc.click;
        except
          // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
          // bleiben, deswegen der Try-Except-Block.
          errorlevel := 2; // Fehler bei Ver/Entschlüsselung
        end;
      end
      else
        errorlevel := 3; // Falsche Syntax oder Datei/Verzeichnis nicht gefunden
      close;
    end;
  end
  else
  begin
    show;

    if fileexists(ParamStr(1)) then
      openfile(ParamStr(1));

    if directoryexists(ParamStr(1)) then
      openfolder(ParamStr(1));
  end;
end;

function FindeFreienOrdnernamen(verzeichnis: string): string;
var
  i: integer;
  act: string;
begin
  act := verzeichnis;
  i := 1;
  while directoryexists(act) or fileexists(act) do
  begin
    inc(i);
    act := verzeichnis+' ('+inttostr(i)+')';
  end;
  result := act;
end;

procedure TMainForm.btn_dec_click(Sender: TObject);
var
  temp, fol, neu: string;
  error: boolean;
begin
  steuerelementesperren(true);

  // Datei
  if flag = $00 then
  begin
    temp := GetTempDir+'DeCoder_'+temp_unique_number+'.tmp';

    // durch fmcreate in decodefile wird der dateiinhalt sowieso gelöscht
    // dc_deletefile(temp);
    error := false;
    try
      DecodeFile(fileopen, temp, edt_dec_password.Text);
    except
      // Kann auftreten, wenn das Programm beim verschlüsseln geschlossen wird
      on EDECException do
      begin
        edt_dec_password.Text := '';
        edt_dec_password.SetFocus; // wirkt durch Exception nicht -.-
        error := true;
        if bordericons <> [] then raise;
      end;
    end;

    // Wenn es zu keinem Fehler kam,
    // dann den User fragen, wohin endgültig speichern
    if not error then
    begin
      if ausgabegesetzt <> '' then
      begin
        // Ausgabeverzeichnis ist durch Befehlszeileneingabe bereits gesetzt
        dc_deletefile(ausgabegesetzt);
        if movefile(pchar(temp), pchar(ausgabegesetzt)) then
        begin
          if chk_dec_securedelete.Checked then
          begin
            dc_deletefile(fileopen);
            if fileexists(fileopen) then
              errorlevel := 6;
          end;
        end
        else
          errorlevel := 5;
      end
      else
      begin
        if dlg_save_dec.Execute then
        begin
          dc_deletefile(dlg_save_dec.FileName);
          if movefile(pchar(temp), pchar(dlg_save_dec.FileName)) then
          begin
            if chk_dec_securedelete.Checked then
            begin
              dc_deletefile(fileopen);
              if fileexists(fileopen) then
                Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION);
            end;
          end
          else
            Application.MessageBox(pchar(GetLangEntry('moveerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
        end
      end;
    end;
  end;

  // Ordner
  if flag = $01 then
  begin
    temp := GetTempDir+'DeCoder_'+temp_unique_number+'.zip';

    // durch fmcreate in decodefile wird der dateiinhalt sowieso gelöscht
    // dc_deletefile(temp);

    error := false;
    try
      DecodeFile(fileopen, temp, edt_dec_password.text);
    except
      // Kann auftreten, wenn das Programm beim verschlüsseln geschlossen wird
      edt_dec_password.Text := '';
      edt_dec_password.SetFocus; // wirkt durch Exception nicht -.-
      error := true;
      if bordericons <> [] then raise;
    end;

    // Wenn es zu keinem Fehler kam,
    // dann den User fragen, wohin extrahieren (unzippen)
    if not error then
    begin
      if (ausgabegesetzt <> '') then
      begin
        if copy(ausgabegesetzt, length(ausgabegesetzt), 1) = '\' then
          neu := FindeFreienOrdnernamen(ausgabegesetzt+ersatzname)
        else
          neu := FindeFreienOrdnernamen(ausgabegesetzt+'\'+ersatzname);
        if not ForceDirectories(neu) then
          errorlevel := 4 // Ausgabeverzeichnis konnte nicht erstellt werden
        else
        begin
          if Unzip(temp, neu) then
          begin
            if chk_dec_securedelete.Checked then
            begin
              dc_deletefile(fileopen);
              if fileexists(fileopen) then
                errorlevel := 6;
            end;
          end
          else
            errorlevel := 7;
        end;
      end
      else
      begin
        fol := BrowseForFolder(GetLangEntry('foldermessagesave'), true);
        if fol <> '' then
        begin
          neu := FindeFreienOrdnernamen(fol+'\'+ersatzname);
          if not ForceDirectories(neu) then
            Application.MessageBox(pchar(GetLangEntry('mkdirerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
          else
          begin
            if Unzip(temp, neu) then
            begin
              if chk_dec_securedelete.Checked then
              begin
                dc_deletefile(fileopen);
                if fileexists(fileopen) then
                  Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // Wird sowieso beim Programmende ausgeführt
  // dc_deletefile(temp);

  steuerelementesperren(false);
end;

procedure TMainForm.btn_config_click(Sender: TObject);
begin
  ConfigForm.showmodal;
end;

procedure TMainForm.btn_help_click(Sender: TObject);
begin
  if fileexists(extractfilepath(application.exename)+GetLangEntry('helpfile')) then
    ShellExecute(Application.Handle, 'open', PChar(extractfilepath(application.exename)+GetLangEntry('helpfile')), '', '', SC_DEFAULT)
  else
    Application.MessageBox(pchar(GetLangEntry('helpnotfound')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
end;

procedure TMainForm.btn_enc_click(Sender: TObject);
var
  temp: string;
  securedelete_lock, error: boolean;
begin
  if edt_enc_password.text <> edt_enc_password2.text then
    Application.MessageBox(pchar(GetLangEntry('passwordsdifferent')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
  else
  begin
    steuerelementesperren(true);
    securedelete_lock := false;

    temp := GetTempDir+'DeCoder_'+temp_unique_number+'.tmp';
    error := false;
    try
      EncodeFile(fileopen, temp, edt_enc_password.text);
    except
      // Kann auftreten, wenn das Programm beim verschlüsseln geschlossen wird
      on EDECException do
      begin
        error := true;
        if bordericons <> [] then raise;
      end;
    end;

    // Wenn es zu keinem Fehler kam,
    // dann den User fragen, wohin endgültig speichern
    if not error then
    begin
      if ausgabegesetzt <> '' then
      begin
        // Ausgabeverzeichnis ist durch Befehlszeileneingabe bereits gesetzt
        dc_deletefile(ausgabegesetzt);
        if (extractfilepath(ausgabegesetzt) <> '') and not ForceDirectories(extractfilepath(ausgabegesetzt)) then
          errorlevel := 4
        else
        begin
          if movefile(pchar(temp), pchar(ausgabegesetzt)) then
          begin
            if chk_enc_securedelete.Checked then
            begin
              if ersatzname <> '' then
              begin
                try
                  dc_deletedir(ersatzname);
                except
                  // Fehler abfangen, sodas er nicht durchgeleitet wird und EL=2 verursacht
                end;
                if directoryexists(ersatzname) then
                  errorlevel := 6;
              end
              else
              begin
                dc_deletefile(fileopen);
                if fileexists(fileopen) then
                  errorlevel := 6;
              end;
            end;
          end
          else
            errorlevel := 5;
        end;
      end
      else
      begin
        if dlg_save_enc.Execute then
        begin
          dc_deletefile(dlg_save_enc.FileName);
          // ForceDirectories hier nicht nötig,
          // da dlg_save_enc.options[ofPathMustExists] = True
          if movefile(pchar(temp), pchar(dlg_save_enc.FileName)) then
          begin
            if chk_enc_securedelete.Checked then
            begin
              if ersatzname <> '' then
              begin
                dc_deletedir(ersatzname);
                if directoryexists(ersatzname) then
                  Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION)
                else
                  securedelete_lock := true;
              end
              else
              begin
                dc_deletefile(fileopen);
                if fileexists(fileopen) then
                  Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION)
                else
                  securedelete_lock := true;
              end;
            end;
          end
          else
            Application.MessageBox(pchar(GetLangEntry('moveerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
        end
      end;
    end;

    // Wird bei Programmbeendigung ausgeführt
    // dc_deletefile(temp);

    steuerelementesperren(false);
    if securedelete_lock then
      chk_enc_securedelete.Enabled := false;
  end;
end;

procedure TMainForm.btn_folder_click(Sender: TObject);
var
  fol: string;
begin
  fol := BrowseForFolder(GetLangEntry('foldermessageopen'), true);
  if fol <> '' then
    OpenFolder(fol);
end;

procedure TMainForm.openfolder(fol: string);
var
  tz: string;
begin
  steuerelementesperren(true);

  // Tempname
  tz := GetTempDir+'DeCoder_'+temp_unique_number+'.zip';

  // durch fmcreate in zipfolder wird der dateiinhalt sowieso gelöscht
  // dc_deletefile(tz);
  zipfolder(fol, tz);
  if fileexists(tz) then
    openfile(tz, fol); //

  // wird am ende von openfile bereits ausgeführt
  // steuerelementesperren(false);
end;

procedure TMainForm.zipfolder(verzeichnis, zipname: string);
var
  FS: TStream;
begin
  KAZip.Close;
  progress_position(progress_min);
  progress_text(GetLangEntry('zip_folder'));
  FS := TFileStream.Create(zipname, fmOpenReadWrite or fmCreate);
  Try
    KAZip.CreateZip(FS);
  Finally
    FS.Free;
  End;
  KAZip.Open(zipname);
  try
    kazip.AddFolder(verzeichnis, verzeichnis, '*', true);
    kazip.close;
  except
    // Kann auftreten, wenn Anwendung beim Zippen geschlossen wurde
  end;
  progress_position(progress_min);
  progress_text(GetLangEntry('ready'));
end;

procedure SecureDeleteWhenUnlocked(Filename: string);
var
  exc: boolean;
  chk: TStream;
  versuche: integer;
begin
  if fileexists(Filename) then
  begin
    // Wenn die Datei nicht innerhalb von 5 Sekunden freigegeben, es einfach lassen
    versuche := 0;
    while versuche <= 500 do
    begin
      inc(versuche);
      try
        exc := false;
        mainform.progress_text(mainform.GetLangEntry('shutdownwait'));
        mainform.progress_position(progress_min);
        application.ProcessMessages;
        chk := TFileStream.Create(Filename, fmOpenWrite or fmShareExclusive);
        sleep(10);
        chk.free;
      except
        exc := true; // immer noch gesperrt
      end;
      if not exc then break; // datei ist wieder schreibbar und somit löschbar
    end;
    dc_deletefile(Filename);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  steuerelementesperren(true);
  bordericons := [];

  kazip.Close;

  // ich kenne keine andere möglichkeit, den laufenden cipher-prozess zu beenden
  // ich gebe einfach den stream frei und fange die exceptions ab
  try
    tempstream.Free;
  except
    // kann passieren, wenn bereits freigegeben wurde (z.B. nach decodefile/encodefile)
  end;

  // Weitermachen, bis die Dateien nicht mehr gesperrt ist
  SecureDeleteWhenUnlocked(GetTempDir+'DeCoder_'+temp_unique_number+'.tmp');
  SecureDeleteWhenUnlocked(GetTempDir+'DeCoder_'+temp_unique_number+'.zip');

  DropFile.unregister;
  DropFile.Free;

  KaZip.Free;

  application.Terminate; // Wenn über Befehlszeile verschlüsselt wird,
                         // dann ist diese Zeile notwendig, ansonsten endet
                         // die Anwendung nie
end;

function TMainForm.Unzip(ZipFile, Verzeichnis: string): boolean;
begin
  result := true;
  steuerelementesperren(true);
  progress_position(progress_min);
  progress_text(GetLangEntry('unzip_folder'));
  kazip.Open(ZipFile);
  if (not kazip.IsZipFile) and (not kazip.HasBadEntries) then
  begin
    removedir(Verzeichnis);
    if ausgabegesetzt = '' then
      Application.MessageBox(pchar(GetLangEntry('archivecorrupt')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
    result := false;
  end
  else
  begin
    try
      kazip.ExtractAll(Verzeichnis);
    except

    end;
  end;
  kazip.Close;
  progress_position(progress_min);
  progress_text(GetLangEntry('ready'));
  steuerelementesperren(false);
end;

procedure TMainForm.dropfileDrop(Sender: TObject; ShiftState: TShiftState;
  Point: TPoint; var Effect: Integer);
begin
  if DropFile.Files.Count > 1 then
    Application.MessageBox(pchar(GetLangEntry('onlyonefiledrop')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
  else
  begin
    if fileexists(dropfile.Files.strings[0]) then
      openfile(dropfile.Files.strings[0]);

    if directoryexists(dropfile.Files.strings[0]) then
      openfolder(dropfile.Files.strings[0]);
  end;
end;

procedure TMainForm.steuerelementesperren(sperren: boolean);
begin
  btn_config.enabled := not sperren;
  btn_dec.enabled := not sperren;
  btn_enc.enabled := not sperren;
  btn_folder.enabled := not sperren;
  btn_help.enabled := not sperren;
  btn_open.enabled := not sperren;
  grp_dec.enabled := not sperren;
    edt_dec_password.enabled := not sperren;
    lbl_dec_label1.enabled := not sperren;
    chk_dec_securedelete.enabled := not sperren;
    img_dec_warning.enabled := not sperren;
    lbl_dec_warning.enabled := not sperren;
  grp_enc.enabled := not sperren;
    edt_enc_password.enabled := not sperren;
    edt_enc_password2.enabled := not sperren;
    lbl_enc_label1.enabled := not sperren;
    lbl_enc_label2.enabled := not sperren;
    lbl_enc_entropy.Enabled := not sperren;
    lbl_enc_equal.enabled := not sperren;
    chk_enc_securedelete.enabled := not sperren;
    img_enc_warning.enabled := not sperren;
    lbl_enc_warning.enabled := not sperren;
  lbl_file.enabled := not sperren;
  lbl_label1.enabled := not sperren;
  vle.enabled := not sperren;

  application.ProcessMessages;
end;

procedure TMainForm.edt_enc_password_change(Sender: TObject);
var
  qu: extended;
  r, g: integer;
const
  helligkeit: byte = 128;
begin
  qu := PassphraseQuality(edt_enc_password.Text);

  if qu <= 0.5 then
  begin
    r := helligkeit;
    g := floor(qu*helligkeit*2);
  end
  else
  begin
    r := helligkeit-floor((qu-0.5)*helligkeit*2);
    g := helligkeit;
  end;

  lbl_enc_entropy.Caption := inttostr(round(qu*100)) + '%';
  lbl_enc_entropy.font.color := rgb(r, g, 0);

  edt_enc_password2_change(sender);
end;

procedure TMainForm.edt_enc_password2_change(Sender: TObject);
begin
  if edt_enc_password.Text = edt_enc_password2.text then
  begin
    lbl_enc_equal.Caption := GetLangEntry('ok');
    lbl_enc_equal.font.color := clGreen;
  end
  else
  begin
    lbl_enc_equal.Caption := GetLangEntry('error');
    lbl_enc_equal.font.color := clMaroon;
  end;
end;

procedure TMainForm.edt_dec_password_keypress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
  begin
    key := #0;
    btn_dec.click;
  end;
end;

procedure TMainForm.edt_enc_password_keypress(Sender: TObject;
  var Key: Char);
begin
  if key = #13 then
  begin
    key := #0;
    if not GetKeyState(VK_Shift) and $8000 <> 0 then
      PostMessage(Handle, WM_NEXTDLGCTL, 0, 0); //edt_enc_password2.SetFocus;
  end;
end;

procedure TMainForm.edt_enc_password2_keypress(Sender: TObject;
  var Key: Char);
begin
  if key = #13 then
  begin
    Key := #0;
    // Ist Shift zusätzlich gedrückt?
    if GetKeyState(VK_Shift) and $8000 <> 0 then
      PostMessage(Handle, WM_NEXTDLGCTL, 1, 0) //edt_enc_password.SetFocus;
    else
      btn_enc.Click;
  end;
end;

procedure TMainForm.tmr_wait_timer(Sender: TObject);
begin
  img_enc_warning.visible := IsCapsLockOn;
  lbl_enc_warning.visible := IsCapsLockOn;
  img_dec_warning.visible := IsCapsLockOn;
  lbl_dec_warning.visible := IsCapsLockOn;
end;

procedure TMainForm.kazip_add(Sender: TObject; ItemName: string);
begin
  application.processmessages;
end;

procedure TProgress.Process(const Min,Max,Pos: Int64);
var
  P: Integer;
begin
  P := floor((Pos - Min) / (Max - Min) * progress_max);
  mainform.progress_position(P);
  application.ProcessMessages;
end;

constructor TProgress.Create;
begin
  inherited Create;
  mainform.progress_position(progress_min);
end;

destructor TProgress.Destroy;
begin
  inherited Destroy;
  mainform.progress_position(progress_min);
  mainform.progress_text(mainform.GetLangEntry('ready'));
end;

procedure TMainForm.kazip_actions(Sender: TObject; Current,
  Total: Integer);
begin
  progress_position(floor(Current / Total * progress_Max));
  Application.ProcessMessages;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := BorderIcons <> []; 
end;

end.
