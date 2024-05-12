unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, DECUtil,
  DECHash, DECCipher, registry, KAZip, shlobj, StdCtrls, math,
  shellapi, Buttons, ComCtrls, ExtCtrls, inifiles, DragDropFile, DragDrop,
  ImgList, base64, ZLib, wininet, OneInst, DCConst, Dialogs,
  Menus, System.ImageList{, XPMenu};

type
  TCase = (tcUnknown, tcEncrypted, tcDecrypted);

  TProgress = class(TInterfacedObject, IDECProgress)
    procedure Process(const Min, Max, Pos: Int64); stdcall;
    constructor Create;
    destructor Destroy; override;
  end;

  TMainForm = class(TForm)
    dlg_open: TOpenDialog;
    dlg_save_dec: TSaveDialog;
    dlg_save_enc: TSaveDialog;
    tmr_capslock: TTimer;
    img_type: TImage;
    lbl_vle1: TLabel;
    edt_vle1: TEdit;
    edt_vle2: TEdit;
    lbl_vle2: TLabel;
    lbl_vle3: TLabel;
    edt_vle3: TEdit;
    lbl_vle4: TLabel;
    edt_vle4: TEdit;
    lbl_vle5: TLabel;
    edt_vle5: TEdit;
    lbl_vle6: TLabel;
    edt_vle6: TEdit;
    lbl_vle7: TLabel;
    edt_vle7: TEdit;
    lbl_passwort: TLabel;
    edt_passwort: TEdit;
    lbl_passwort2: TLabel;
    edt_passwort2: TEdit;
    img_warning: TImage;
    lbl_capswarning: TLabel;
    chk_securedelete: TCheckBox;
    lbl_entropy: TLabel;
    lbl_equal: TLabel;
    imagelist: TImageList;
    img_error: TImage;
    lbl_readerror: TLabel;
    b_encrypt: TBitBtn;
    b_decrypt: TBitBtn;
    b_direct: TBitBtn;
    b_open: TBitBtn;
    b_folder: TBitBtn;
    chk_compress: TCheckBox;
    tmr_refresh: TTimer;
    mainmenu: TMainMenu;
    mm_file: TMenuItem;
    m_open: TMenuItem;
    m_folder: TMenuItem;
    seperator1: TMenuItem;
    m_config: TMenuItem;
    seperator2: TMenuItem;
    m_close: TMenuItem;
    m_exit: TMenuItem;
    mm_actions: TMenuItem;
    mm_help: TMenuItem;
    m_help: TMenuItem;
    m_web_update: TMenuItem;
    m_info: TMenuItem;
    m_encrypt: TMenuItem;
    m_decrypt: TMenuItem;
    m_direct: TMenuItem;
    seperator5: TMenuItem;
    m_delete: TMenuItem;
    m_web: TMenuItem;
    seperator6: TMenuItem;
    m_web_dm: TMenuItem;
    m_web_vts: TMenuItem;
    m_web_project: TMenuItem;
    m_web_keytransmitter: TMenuItem;
    m_web_email: TMenuItem;
    mm_dateiensammlung: TMenuItem;
    m_dateiensammlung_add_file: TMenuItem;
    m_dateiensammlung_show: TMenuItem;
    m_dateiensammlung_neu: TMenuItem;
    dlg_open_element: TOpenDialog;
    seperator3: TMenuItem;
    seperator4: TMenuItem;
    m_dateiensammlung_add_folder: TMenuItem;
    tmp_progressdurchlauf: TTimer;
    m_web_forum: TMenuItem;
    m_web_infopages: TMenuItem;
    procedure form_closequery(Sender: TObject; var CanClose: Boolean);
    procedure kazip_actions_compress(Sender: TObject; Current, Total: Integer);
    procedure kazip_actions_decompress(Sender: TObject; Current, Total: Integer);
    procedure kazip_add(Sender: TObject; ItemName: string);
    procedure kazip_onchange(Sender: TObject; ChangeType: Integer);
    procedure kazip_overwrite(Sender:TObject; Var FileName : String; Var Action : TOverwriteAction);
    procedure tmr_capslock_timer(Sender: TObject);
    procedure form_create(Sender: TObject);
    procedure edt_dec_password_keypress(Sender: TObject; var Key: Char);
    procedure form_close(Sender: TObject; var Action: TCloseAction);
    procedure dropfile_drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure edt_password2_change(Sender: TObject);
    procedure edt_enc_password_keypress(Sender: TObject; var Key: Char);
    procedure edt_passwort_change(Sender: TObject);
    procedure edt_passwort2_keypress(Sender: TObject; var Key: Char);
    procedure m_open_execute(Sender: TObject);
    procedure m_folder_execute(Sender: TObject);
    procedure m_config_execute(Sender: TObject);
    procedure m_exit_execute(Sender: TObject);
    procedure m_close_execute(Sender: TObject);
    procedure m_delete_execute(Sender: TObject);
    procedure m_direct_execute(Sender: TObject);
    procedure m_encrypt_execute(Sender: TObject);
    procedure m_decrypt_execute(Sender: TObject);
    procedure mm_actions_execute(Sender: TObject);
    procedure mm_file_execute(Sender: TObject);
    procedure mm_help_execute(Sender: TObject);
    procedure m_help_execute(Sender: TObject);
    procedure m_info_execute(Sender: TObject);
    procedure edt_passwort_keypress(Sender: TObject; var Key: Char);
    procedure edt_passwort_enter(Sender: TObject);
    procedure m_web_update_execute(Sender: TObject);
    procedure form_show(Sender: TObject);
    procedure tmr_refresh_timer(Sender: TObject);
    procedure m_web_dm_click(Sender: TObject);
    procedure m_web_vts_click(Sender: TObject);
    procedure m_web_project_click(Sender: TObject);
    procedure m_web_keytransmitter_click(Sender: TObject);
    procedure m_web_email_click(Sender: TObject);
    procedure m_dateiensammlung_neu_click(Sender: TObject);
    procedure m_dateiensammlung_show_click(Sender: TObject);
    procedure m_dateiensammlung_add_file_click(Sender: TObject);
    procedure m_dateiensammlung_add_folder_click(Sender: TObject);
    procedure tmp_progressdurchlauf_timer(Sender: TObject);
    procedure m_web_forum_click(Sender: TObject);
    procedure m_web_infopages_click(Sender: TObject);
  private
    steuerelementegesperrt: boolean;
    LangArray: Array of TLanguageEntry;
    lg_StartFolder: String;
    temp_unique_number: string;
    DateienCounter, DateienImOrdner: integer;
    // Freigegebene Streams, die bei der spontanen Programmbeendigung
    // unter Abfang der Exceptions freigegeben werden
    BestimmeDateiGroesseSource: TStream;
    tempstream: TStream;
    CompressInputStream, CompressOutputStream: TFileStream;
    CompressionStream: ZLib.TCompressionStream;
    DeCompressionStream: ZLib.TDeCompressionStream;
    ZippingStream: TStream;
    // Ende
    procedure progress_position(pos: integer);
    procedure progress_text(text, item: string);
    procedure Compress(InputFileName, OutputFileName: string);
    procedure Decompress(InputFileName, OutputFileName: string);
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  public
    // VCL-Ersatz start
    DropFile: TDropFileTarget;
    KaZip: TKaZip;
    //xpmenu: TXPMenu;
    // VCL-Ersatz ende
    fileopen: string; // geöffnete datei
    ersatzname: string; // wenn ein ordner verschlüsselt wird, ordnernamen statt zip-dateinamen angeben
    ausgabegesetzt: string; // befehlszeile, wenn gesetzt
    errorlevel: integer; // fehlerlevel für befehlszeilenanwendung
    flag: byte; // ordner oder datei verschlüsselt?
    mode: TCase; // status der geöffneten datei?
    fMenuBrush: TBrush;
    procedure steuerelementesperren(sperren: boolean);
    procedure zeige_wartenform(dothis: boolean);
    function GetLangEntry(name: string): string;
    procedure OpenFile(fil: string; alternativer_name: string = ''; dateiensammlung: boolean = false);
    procedure OpenFolder(fol: string);
    procedure SchliesseDatei();
    function Unzip(ZipFile, Verzeichnis: string): boolean;
    procedure zipfolder(verzeichnis, zipname: string);
    procedure addtozip(fileorfolder, zipfile: string);
    procedure CreateZipFile(zipfile: string);
    procedure freestreams();
    procedure DefaultHandler(var Message); override;
    procedure ProcessCommandline(lpData: Pointer);
    procedure Start;
    function BestimmeDateiGroesse(filename: string): string;
  end;

var
  MainForm: TMainForm;

implementation

uses Config, About, Warten, Elemente;

{$R *.dfm}

{$R WindowsXP.res}

const
  // Konstanten
  Terminus: String = 'DCTERMINUS';
  FileNameTerminus: Char = '?';
  DateiTag: Byte = $00;
  OrdnerTag: Byte = $01;
  DateiCompressTag: Byte = $02;
  OrdnerCompressTag: Byte = $03;
  TempPre = 'DeCoder_';
  TempDirect = 'DeCoder_Sensitiv';
  TempExtTmp = '.tmp';
  TempExtCmp = '.cmp';
  TempExtZip = '.zip';
  ExtDC4 = '.dc4';
  updateid = 'decoder';

  // Standardeinstellungen

  StZIPType: TZipCompressionType = ctNone;
  ZLibCompressFile: Boolean = True;
  ZLibCompressFolder: Boolean = True;

  // (De)Coder 4.1 Standardeinstellungen
  VTag: Byte = $02;
  IDBase: LongInt = $84671842;
  SeedSize: Byte = 32;
  StMode: TCipherMode = cmCTSx;
  StCipher: String = 'TCipher_Rijndael';
  StHash: String = 'THash_SHA512';

  // (De)Coder 4.0 Kompatibilitätseinstellungen
  DC40VTag: Byte = $01;
  DC40IDBase: LongInt = $59178954;
  DC40SeedSize: Byte = 16;
  DC40StMode: TCipherMode = cmCTSx;
  DC40StCipher: String = 'TCipher_Rijndael';
  DC40StHash: String = 'THash_SHA512';

  // Konstanten von DEC
  cyprefix: String = 'TCipher_';
  haprefix: String = 'THash_';
  CipherModeStr: array[0..8] of string = ('CTSx', 'CBCx', 'CFB8', 'CFBx', 'OFB8', 'OFBx', 'CFS8', 'CFSx', 'ECBx');

{ Eine zweite Instanz hat uns ihre Kommandozeilenparameter geschickt }
// von OneInst
procedure TMainForm.WMCopyData(var Msg: TWMCopyData);
var
  i: integer;
  temp: string;
  tpara: string;
begin
  if (Msg.CopyDataStruct.dwData = SecondInstMsgId) and (SecondInstMsgId <> 0) then
  begin
    if (steuerelementegesperrt or wartenform.Visible or
       (not mainform.Visible)) and (paramstr_firstposition('/c') <> -1) and
       (paramstr_firstposition('/x') <> -1) and (paramstr_firstposition('/e') <> -1) and
       (paramstr_firstposition('/?') <> -1) and (paramstr_firstposition('/clean') <> -1) then
    begin
      temp := ParamBlobToStr(Msg.CopyDataStruct.lpData);
      tpara := '';
      for i := 0 to ZaehleLinien(temp)-1 do
        tpara := tpara + '"'+GebeLinieaus(temp, i+1) + '" ';

      shellexecute(application.Handle, 'open', pchar(application.exename), pchar(tpara+'/newinstance'), pchar(extractfiledir(application.exename)), sw_normal);
    end
    else
      ProcessCommandline(Msg.CopyDataStruct.lpData);
  end
  else
    { Tcha wohl doch nicht ID - stimmte nicht }
    inherited;
end;

{----------------------------------------------------------------------------}
{ Wir überschreiben den DefaultHandler, der alle Nachrichten zuerst bekommt, }
{ damit wir auf die Nachricht mit der ID SecondInstMsgId reagieren können.   }
{ (Dies ist notwendig, da wir jetzt noch nicht wissen welchen Wert           }
{  die ID haben wird, weswegen wir keine statische Message-Prozedure,        }
{  so wie bei WM_COPYDATA, schreiben können.)                                }
{----------------------------------------------------------------------------}
// von OneInst
procedure TMainForm.DefaultHandler(var Message);
begin
  if TMessage(Message).Msg = SecondInstMsgId then
    { Eine zweite Instanz hat uns nach dem Handle gefragt }
    { Es wird in die Message-Queue des Threads gepostet.  }
    PostThreadMessage(TMessage(Message).WParam, SecondInstMsgId, Handle, 0)
  else
    { Ansonsten rufen wir die geerbte Methode auf }
    inherited;
end;

procedure Schlafen(nDelay: Cardinal);
var
  nStart: Cardinal;
begin
  nStart := GetTickCount;
  while GetTickCount-nStart < nDelay do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

procedure TMainForm.Compress(InputFileName, OutputFileName: string);
begin
  try
    CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead);
    try
      CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
      try
        CompressionStream:=TCompressionStream.Create(clMax, CompressOutputStream);
        try
          CompressionStream.CopyFrom(CompressInputStream, CompressInputStream.Size);
        finally
          CompressionStream.Free;
        end;
      finally
        CompressOutputStream.Free;
      end;
    finally
      CompressInputStream.Free;
    end;
  except
    // z.B. bei Programmabbruch
  end;
end;

procedure TMainForm.m_web_dm_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.daniel-marschall.de/', '', '', sw_normal);
end;

procedure TMainForm.m_web_project_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.viathinksoft.de/index.php?page=projektanzeige&seite=projekt-18', '', '', sw_normal);
end;

procedure TMainForm.Decompress(InputFileName, OutputFileName: string);
var
  Buf: array[0..4095] of Byte;
  Count: Integer;
begin
  try
    CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead);
    try
      CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
      try
        DecompressionStream := TDecompressionStream.Create(CompressInputStream);
        try
          while true do
          begin
            Count := DecompressionStream.Read(Buf[0], SizeOf(Buf));
            if Count = 0 then
              break
            else
              CompressOutputStream.Write(Buf[0], Count);
          end;
        finally
          DecompressionStream.Free;
        end;
      finally
        CompressOutputStream.Free;
      end;
    finally
      CompressInputStream.Free;
    end;
  except
    // z.B. bei Programmabbruch
  end;
end;

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
  wartenform.pbr_progress.Position := pos;
  if ((wartenform.pbr_progress.Position <> wartenform.pbr_progress.Min) and (GebeLinieaus(ParamZeile, 2) <> '/silent')) and (not wartenform.visible) then
  begin
    wartenform.Visible := true;
    // tmr_openwartenform.Enabled := true;
  end;

  application.ProcessMessages;
end;

procedure TMainForm.zeige_wartenform(dothis: boolean);
begin
  if dothis then
  begin
    if (GebeLinieaus(ParamZeile, 2) <> '/silent') and (not wartenform.visible) then
    begin
      progress_text('', '');
      wartenform.Visible := true;
      // tmr_openwartenform.Enabled := true;
    end;
  end
  else
  begin
    progress_text('', '');
    wartenform.Visible := false;
    // wartenform.tmr_closetimer.enabled := true;
  end;
end;

procedure TMainForm.progress_text(text, item: string);
begin
  wartenform.lbl_info1.caption := text;
  wartenform.lbl_info2.caption := item;
  application.processmessages;
end;

function FormatiereZahlToStr(inp: extended): string;
begin
  result := FloatToStrF(inp, ffNumber, 14, 2);
end;

function lang_getdirectoryname(): string;
var
  myreg: tregistry;
begin
  result := '';
  myreg := TRegistry.Create;
  try
    myreg.RootKey := HKEY_CLASSES_ROOT;
    if myreg.OpenKey('Directory', false) then
    begin
      result := myreg.ReadString('');
    end;
  finally
    myreg.free;
  end;
end;

function kazip_numfiles(ka: tkazip): integer;
var
  i, a: Integer;
begin
  a := 0;
  for i := 0 to ka.FileNames.count - 1 do
  begin
    if copy(ka.FileNames.Strings[i], length(ka.FileNames.Strings[i]), 1) <> '\' then
      inc(a);
  end;
  result := a;
end;

{ function kazip_numfolders(ka: tkazip): integer;
var
  i, a: Integer;
begin
  a := 0;
  for i := 0 to ka.FileNames.count - 1 do
  begin
    if copy(ka.FileNames.Strings[i], length(ka.FileNames.Strings[i]), 1) = '\' then
      inc(a);
  end;
  result := a;
end; }

function IntelligenteDateigroesse(ibytes: int64): string;
begin
  if ibytes > power(1024, 5) then
    result := FormatiereZahlToStr(ibytes / power(1024, 4))+' EB'
  else if ibytes > power(1024, 4) then
    result := FormatiereZahlToStr(ibytes / power(1024, 4))+' TB'
  else if ibytes > power(1024, 3) then
    result := FormatiereZahlToStr(ibytes / power(1024, 3))+' GB'
  else if ibytes > power(1024, 2) then
    result := FormatiereZahlToStr(ibytes / power(1024, 2))+' MB'
  else if ibytes > power(1024, 1) then
    result := FormatiereZahlToStr(ibytes / power(1024, 1))+' KB'
  else
  begin
    if ibytes = 1 then
      result := FormatiereZahlToStr(ibytes)+' Byte'
    else
      result := FormatiereZahlToStr(ibytes)+' Bytes';
  end;
end;

procedure dc_deletefile(Filename: string);
const
  buf = 1024;
var
  S: TStream;
  size: int64;
  abbruch: boolean;
  lAttributes: integer;
begin
  if fileexists(filename) then
  begin
    try
      lAttributes := FileGetAttr(Filename);
      if lAttributes and SysUtils.faReadOnly <> 0 then
      begin
        lAttributes := lAttributes - SysUtils.faReadOnly;
        FileSetAttr(Filename, lAttributes);
      end;
      S := TFileStream.Create(Filename, fmOpenWrite or fmShareExclusive);
      try
        size := S.Size;
        mainform.progress_position(wartenform.pbr_progress.min);
        mainform.progress_text(mainform.GetLangEntry('deletefile'), filename);
        abbruch := false;
        while not abbruch do
        begin
          size := size - buf;
          if size > 0 then
          begin
            ProtectStream(S, buf);
            mainform.progress_position(floor((s.size-size) / s.Size * wartenform.pbr_progress.max));
          end
          else
          begin
            if size < 0 then
              ProtectStream(S, s.size-s.Position); // wenn nicht size = 0

            mainform.progress_position(wartenform.pbr_progress.min);

            abbruch := true;
          end;
        end;
      finally
        S.Free;
      end;
      deletefile(pchar(filename));
    except

    end;
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
        dc_deletefile(Directory + zus + SR.Name)
      except
        // z.B. durch Benutzung oder durch Schreibschutz verursacht
      end;
    until FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;

  if FindFirst(Directory + zus + '*.*', faAnyFile, SR) = 0 then
  try
    repeat
      if ((SR.attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
        dc_deletedir(Directory + zus + SR.Name + '\');
    until FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;

  removedirectory(pchar(Directory));
end;

// http://www.swissdelphicenter.ch/torry/showcode.php?id=144
function GetTempDir: string;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
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
    S: AnsiString;
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
    S: AnsiString;
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
      if J > 0 then S[I] := AnsiChar(J);
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

  // Original von Codebeispiel von Hagen Reddmann
  procedure EncodeFile(const AFileName, AOutput: String; const APassword: Binary);

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
    ACipher: TDECCipherClass;
    AHash: TDECHashClass;
    ACFileName: String;
  begin
    mainform.progress_position(wartenform.pbr_progress.min);
    mainform.progress_text(mainform.GetLangEntry('encodedata'), afilename);

    ACipher := CipherByName(StCipher);
    AHash := HashByName(StHash);

    ACipher := ValidCipher(ACipher);
    AHash := ValidHash(AHash);

    Seed := RandomBinary(SeedSize);

    ACFileName := GetTempDir+TempPre+mainform.temp_unique_number+TempExtCmp;

    // Ordner (-> ZIP-Dateien) werden nicht nocheinmal komprimiert ... oder doch?
    // if (ZLibCompressFile and (mainform.ersatzname = '')) or (ZLibCompressFolder and (mainform.ersatzname <> '')) then
    if mainform.chk_compress.checked then
      mainform.Compress(AFileName, ACFileName)
    else
      ACFileName := AFileName;

    Source := TFileStream.Create(ACFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        mainform.tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
        try
          with ACipher.Create do
          try
            try
              Mode := TCipherMode(StMode);

              Init(AHash.KDFx(APassword, Seed, Context.KeySize));

              if mainform.ersatzname <> '' then
              begin
                if mainform.chk_compress.Checked then
                  writebyte(OrdnerCompressTag)
                else
                  writebyte(OrdnerTag);
              end
              else
              begin
                if mainform.chk_compress.Checked then
                  writebyte(DateiCompressTag)
                else
                  writebyte(DateiTag);
              end;

              WriteByte(VTag);
              {if mainform.ersatzname <> '' then
                WriteRaw(Base64Encode(ExtractFileName(mainform.ersatzname)))
              else
                WriteRaw(Base64Encode(ExtractFileName(AFileName)));}
              WriteRaw(Base64Encode(mainform.edt_vle1.text));
              WriteRaw(FileNameTerminus);
              WriteLong(IDBase);
              WriteLong(acipher.Identity);
              WriteByte(Byte(Mode));
              WriteLong(ahash.Identity);
              WriteByte(SeedSize);
              WriteRaw(Seed);

              P := TProgress.Create;
              try
                EncodeStream(Source, mainform.tempstream, Source.Size, P);
              except
                // Kann auftreten, wenn das Programm beim Verschlüsseln geschlossen wird
                exit;
              end;
              source.Position := 0;
              WriteRaw(ahash.CalcBinary(ahash.CalcStream(source, source.size)+Seed+APassword));
              WriteRaw(Base64Encode(Terminus));
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
        // Ist eine temporäre Datei und wird bei Programmbeendigung gelöscht
        // dc_deletefile(aoutput);
        mainform.steuerelementesperren(false);
        raise; // programmablauf endet hier
      end;
    finally
      Source.Free;
    end;
  end;

  // Original von Codebeispiel von Hagen Reddmann
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
    {F, }V: Byte;
    P: IDECProgress;
    Cipher: TDECCipher;
    Seed: String;
  begin
    mainform.progress_position(wartenform.pbr_progress.min);
    mainform.progress_text(mainform.GetLangEntry('decodedata'), afilename);

    Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        {F := }ReadByte;
        V := ReadByte;

        OrigName := '';
        ch := readraw(1);
        while ch <> FileNameTerminus do
        begin
          OrigName := OrigName + ch;
          ch := readraw(1);
        end;

        if V = VTag then
        begin
          OrigName := Base64Decode(OrigName);
          ReadLong; // IdendityBase wird bereits bei FileOpen gesetzt
        end;

        mainform.tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
        try
          try
            Cipher := nil;
            if V = DC40VTag then Cipher := ValidCipher(CipherByName(DC40StCipher)).Create;
            if V = VTag then Cipher := ValidCipher(CipherByIdentity(ReadLong)).Create;
            try
              AHash := nil;
              if V = DC40VTag then
              begin
                Cipher.Mode := TCipherMode(DC40StMode);
                AHash := HashByName(DC40StHash);
              end;
              if V = VTag then
              begin
                Cipher.Mode := TCipherMode(ReadByte);
                AHash := HashByIdentity(ReadLong);
              end;
              AHash := ValidHash(AHash);

              if V = DC40VTag then
                Seed := ReadRaw(DC40SeedSize);
              if V = VTag then
                Seed := ReadRaw(ReadByte);

              Cipher.Init(ahash.KDFx(APassword, Seed, Cipher.Context.KeySize));

              P := TProgress.Create;
              try
                if V = DC40VTag then
                  Cipher.DecodeStream(Source, mainform.tempstream, source.size-source.Position-ahash.DigestSize, P);
                if V = VTag then
                  Cipher.DecodeStream(Source, mainform.tempstream, source.size-source.Position-ahash.DigestSize-Length(Base64Encode(Terminus)), P);
              except
                // Kann auftreten, wenn das Programm beim Verschlüsseln geschlossen wird
                exit;
              end;
              mainform.tempstream.position := 0;
              if V = DC40VTag then
              begin
                if readraw(ahash.DigestSize) <> ahash.CalcStream(mainform.tempstream, mainform.tempstream.size) then
                  raise EDECException.Create(mainform.GetLangEntry('hasherror'));
              end;
              if V = VTag then
              begin
                if readraw(ahash.DigestSize) <> ahash.CalcBinary(ahash.CalcStream(mainform.tempstream, mainform.tempstream.size)+Seed+APassword) then
                  raise EDECException.Create(mainform.GetLangEntry('hasherror'));
              end;
            finally
              Cipher.Free;
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
        // Ist eine temporäre Datei und wird bei Programmbeendigung gelöscht
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

{ function SecureDeleteWhenUnlocked(Filename: string): boolean;
var
  exc: boolean;
  chk: TStream;
  versuche: integer;
begin
  result := true;
  if fileexists(Filename) then
  begin
    // Wenn die Datei nicht innerhalb von 5 Sekunden freigegeben, es einfach lassen
    versuche := 0;
    while versuche <= 500 do
    begin
      inc(versuche);
      try
        exc := false;
        // DE: shutdownwait=Warte, bis temporäre Dateien freigegeben sind.
        // EN: shutdownwait=Wait, till temporary files are unlocked...
        // mainform.progress_text(mainform.GetLangEntry('shutdownwait'));
        // mainform.progress_position(progress_min);
        application.ProcessMessages;
        chk := TFileStream.Create(Filename, fmOpenWrite or fmShareExclusive);
        Schlafen(10);
        chk.free;
      except
        exc := true; // immer noch gesperrt
      end;
      if not exc then break; // datei ist wieder schreibbar und somit löschbar
    end;
    dc_deletefile(Filename);
    if fileexists(Filename) then result := false; // Fehlgeschlagen
  end;
end; }

// http://www.swissdelphicenter.ch/torry/showcode.php?id=2413
function DirectoryIsEmpty(Directory: string): Boolean;
var
  SR: TSearchRec;
  i: Integer;
begin
  Result := False;
  FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SR);
  for i := 1 to 2 do
    if (SR.Name = '.') or (SR.Name = '..') then
      Result := FindNext(SR) <> 0;
  FindClose(SR);
end;

procedure DeleteTempFiles();
var
  searchResult: TSearchRec;
  allesok, nichtsda: boolean;
  uqnr: string;
begin
  mainform.zeige_wartenform(true);
  mainform.steuerelementesperren(true);
  try

  allesok := true;
  nichtsda := true;

  if (copy(lowercase(GebeLinieaus(ParamZeile, 2)), 0, 6) = '/only=') or (copy(lowercase(gebelinieaus(ParamZeile, 3)), 0, 6) = '/only=') then
  begin
    // Nur Sitzungsdaten von einer Sitzung löscchen

    if copy(lowercase(GebeLinieaus(ParamZeile, 2)), 0, 6) = '/only=' then
      uqnr := copy(GebeLinieaus(ParamZeile, 2), 7, length(GebeLinieaus(ParamZeile, 2))-6);
    if copy(lowercase(GebeLinieaus(ParamZeile, 3)), 0, 6) = '/only=' then
      uqnr := copy(GebeLinieaus(ParamZeile, 3), 7, length(GebeLinieaus(ParamZeile, 3))-6);

    if FindFirst(GetTempDir+'*.*', faAnyFile, searchResult) = 0 then
    begin
      repeat
        if (lowercase(copy(extractfilename(searchResult.Name), 0, length(extractfilename(searchResult.Name))-length(extractfileext(searchResult.Name)))) = lowercase(TempPre+uqnr)) and fileexists(GetTempDir+searchResult.Name) then
        begin
          nichtsda := false;
          dc_deletefile(GetTempDir+extractfilename(searchResult.Name));
          if fileexists(GetTempDir+extractfilename(searchResult.Name)) then
            allesok := false;
        end;
      until FindNext(searchResult) <> 0;
      FindClose(searchResult);
    end;

    if (directoryexists(GetTempDir+TempDirect+'\'+uqnr) and not DirectoryIsEmpty(GetTempDir+TempDirect+'\'+uqnr)) then
    begin
      nichtsda := false;
      dc_deletedir(GetTempDir+TempDirect+'\'+uqnr);
      if directoryexists(GetTempDir+TempDirect+'\'+uqnr) then
        allesok := false;
    end;

  end
  else
  begin
    // Alle Sitzungsdaten löschen

    if FindFirst(GetTempDir+'*.*', faAnyFile, searchResult) = 0 then
    begin
      repeat
        if (lowercase(copy(searchResult.Name, 0, length(TempPre))) = lowercase(TempPre)) and fileexists(GetTempDir+searchResult.Name) then
        begin
          nichtsda := false;
          dc_deletefile(GetTempDir+searchResult.Name);
          if fileexists(GetTempDir+searchResult.Name) then
            allesok := false;
        end;
      until FindNext(searchResult) <> 0;
      FindClose(searchResult);
    end;

    if directoryexists(GetTempDir+TempDirect) then
    begin
      nichtsda := false;
      dc_deletedir(GetTempDir+TempDirect);
      if directoryexists(GetTempDir+TempDirect) then
        allesok := false;
    end;
  end;

  if (lowercase(GebeLinieaus(ParamZeile, 2)) <> '/silent') then
  begin
    if nichtsda then
      Application.MessageBox(pchar(MainForm.GetLangEntry('nothingtoclean')), pchar(MainForm.GetLangEntry('information')), MB_OK + MB_ICONASTERISK)
    else
    begin
      if not allesok then
        Application.MessageBox(pchar(MainForm.GetLangEntry('cleaningerror')), pchar(MainForm.GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION)
      else
        Application.MessageBox(pchar(MainForm.GetLangEntry('cleaningok')), pchar(MainForm.GetLangEntry('information')), MB_OK + MB_ICONASTERISK);
    end;
  end
  else
  begin
    if not allesok then
      MainForm.errorlevel := 8; // Datei oder Ordner konnte nicht oder nur teilweise entfernt werden.
  end;

  finally
  mainform.steuerelementesperren(false);
  mainform.zeige_wartenform(false);
  end;
end;

{ function HexAnsicht(inp: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to length(inp) do
    result := result + inttohex(ord(inp[i]), 2) + ' ';
end; }

procedure TMainForm.OpenFile(fil: string; alternativer_name: string = ''; dateiensammlung: boolean = false);
var
  Source: TStream;
  O, T: string;
  F, V: byte;
  Dateigroesse: int64;
  ch: string;
  hash: TDECHashClass;
  cipher: TDECCipherClass;
  InfoSeedSize: byte;
  InfoMode: byte;
  FileInfo: SHFILEINFO;

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
    // ClientHeight := formanfangsgroesse;
    edt_vle3.Text := '-';
    edt_vle4.Text := '-';
    edt_vle5.Text := GetLangEntry('broken');
    edt_vle6.Text := '-';
    edt_vle7.Text := '-';

    lbl_passwort.Visible := false;
    lbl_passwort2.Visible := false;
    edt_passwort.Visible := false;
    edt_passwort2.Visible := false;
    lbl_entropy.Visible := false;
    lbl_equal.Visible := false;
    chk_securedelete.Visible := false;
    chk_compress.Visible := false;
    tmr_capslock.Enabled := false;
    lbl_capswarning.Visible := false;
    img_warning.Visible := false;

    b_decrypt.Visible := false;
    b_encrypt.Visible := false;
    b_direct.Visible := false;
    b_open.Visible := true;
    b_folder.Visible := true;

    if ausgabegesetzt <> '' then
      errorlevel := 1 // DC4 Datei beschädigt
    else
    begin
      // Application.MessageBox(pchar(GetLangEntry('errorreading')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
      img_error.visible := true;
      lbl_readerror.visible := true;
    end;
  end;

begin
  steuerelementesperren(true);
  zeige_wartenform(true);
  try

  m_close.enabled := true;
  fileopen := fil;

  if (alternativer_name <> '') and (fileexists(alternativer_name) or directoryexists(alternativer_name)) then
    SHGetFileInfo(PChar(alternativer_name), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_LARGEICON)
  else
    SHGetFileInfo(PChar(fil), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_LARGEICON);

  img_type.Picture.Icon.Handle := FileInfo.hIcon;

  if alternativer_name <> '' then
  begin
    edt_vle1.Text := ExtractFileName(alternativer_name);
    if fileexists(alternativer_name) or directoryexists(alternativer_name) then
      edt_vle2.Text := ExtractFilePath(alternativer_name)
    else
      edt_vle2.Text := ExtractFilePath(fil);
    caption := '(De)Coder '+DC4Ver+' [' + extractfilename(alternativer_name) + ']';
  end
  else
  begin
    edt_vle1.Text := ExtractFileName(fil);
    edt_vle2.Text := ExtractFilePath(fil);
    caption := '(De)Coder '+DC4Ver+' [' + extractfilename(fil) + ']';
  end;

  edt_vle3.Text := '';
  edt_vle4.Text := '';
  edt_vle5.Text := '';
  edt_vle6.Text := '';
  edt_vle7.Text := '';

  edt_passwort.Text := '';
  edt_passwort2.Text := '';
  edt_passwort_change(edt_passwort);
  chk_securedelete.Checked := paramzeile_firstposition('/x') <> -1;
  chk_compress.Checked := true;
  // wird durch edt_enc_password_change automatisch aufgerufen
  // edt_enc_password2_change(edt_enc_password2);

  dlg_save_dec.filter := GetLangEntry('filter_all')+' (*.*)|*.*';
  dlg_save_dec.filename := '';
  dlg_save_dec.defaultext := '';
  dlg_save_enc.filename := '';

  mode := tcUnknown;

  Source := TFileStream.Create(fil, fmOpenRead or fmShareDenyNone);

  try
    try
      if lowercase(extractfileext(fil)) = lowercase(ExtDC4) then
      begin
        edt_vle1.Color := clBtnFace;
        edt_vle1.ReadOnly := true;
        edt_vle1.TabStop := false;

        F := ReadByte; // ID-Flag (Datei/Ordner)
        V := ReadByte; // Versions-Flag

        if V = DC40VTag then edt_vle5.Text := Format(GetLangEntry('encrypted'), ['(De)Coder 4.0']);
        if V = VTag then edt_vle5.Text := Format(GetLangEntry('encrypted'), ['(De)Coder 4.1']);

        if ((F <> DateiTag) and (F <> DateiCompressTag) and (F <> OrdnerTag) and (F <> OrdnerCompressTag)) or ((V <> DC40VTag) and (V <> VTag)) then
        begin
          Dateibeschaedigt;
          steuerelementesperren(false);
          exit;
        end;

        cipher := nil;
        hash := nil;

        if V = DC40VTag then
        begin
          cipher := CipherByName(DC40StCipher);
          hash := HashByName(DC40StHash);
        end;

        if V = VTag then
        begin
          cipher := CipherByName(StCipher);
          hash := HashByName(StHash);
        end;

        cipher := validcipher(cipher);
        hash := validhash(hash);

        // damit später bei DecodeFile entschieden werden kann, ob entzippt werden muss
        flag := F;

        O := '';
        ch := readraw(1);
        while ch <> FileNameTerminus do
        begin
          O := O + ch;
          ch := readraw(1);
        end;
        if V = VTag then O := Base64Decode(O);

        // Idendity Base
        if (V = VTag) then
          IdentityBase := ReadLong
        else
          IdentityBase := DC40IDBase;

        InfoSeedSize := DC40SeedSize;
        InfoMode := Byte(DC40StMode);
        if (V = VTag) then
        begin
          cipher := CipherByIdentity(ReadLong);
          InfoMode := ReadByte;
          hash := HashByIdentity(ReadLong);

          cipher := ValidCipher(cipher);
          hash := ValidHash(hash);

          InfoSeedSize := ReadByte;
        end;

        if IsFileName(O) then
        begin
          dlg_save_dec.FileName := O;
          dlg_save_dec.DefaultExt := copy(extractfileext(O), 1, length(extractfileext(O)));
        end
        else
        begin
          dateibeschaedigt;
          steuerelementesperren(false);
          exit;
        end;

        edt_vle6.Text := O;

        if (F = OrdnerTag) or (F = OrdnerCompressTag) then
        begin
          T := lang_getdirectoryname;
          ersatzname := O; // damit nach decodefile der richtige ordnername verwendet wird
        end;
        if (F = DateiTag) or (F = DateiCompressTag) then
        begin
          T := FindeDateityp(O);
          ersatzname := '';
        end;

        if (T = '') then T := Format(GetLangEntry('x-file'), [uppercase(copy(extractfileext(O), 2, length(extractfileext(O))-1))]);
        edt_vle3.Text := T;

        dlg_save_dec.Filter := T+' (*'+lowercase(extractfileext(O))+')|*'+lowercase(extractfileext(O))+'|'+GetLangEntry('filter_all')+' (*.*)|*.*';

        Dateigroesse := source.size - hash.DigestSize - source.Position - InfoSeedSize;
        If V = VTag then Dateigroesse := Dateigroesse - length(Base64Encode(Terminus));

        if Dateigroesse < 0 then
        begin
          dateibeschaedigt;
          steuerelementesperren(false);
          exit;
        end
        else
        begin
          if (F = DateiCompressTag) or (F = OrdnerTag) or (F = OrdnerCompressTag) then edt_vle4.Text := GetLangEntry('compressed')+': ';
          edt_vle4.Text := edt_vle4.Text + IntelligenteDateigroesse(Dateigroesse);
        end;

        if V = VTag then
        begin
          source.Position := source.Size - length(Base64Encode(terminus));
          if ReadRaw(length(Base64Encode(terminus))) <> Base64Encode(terminus) then
          begin
            dateibeschaedigt;
            steuerelementesperren(false);
            exit;
          end;
        end;

        edt_vle7.Text := copy(cipher.ClassName, length(cyprefix)+1, length(cipher.ClassName)-length(cyprefix)) +
                         ' ('+CipherModeStr[InfoMode]+'), ' +
                         copy(hash.ClassName, length(haprefix)+1, length(hash.ClassName)-length(haprefix)) +
                         ' (' + Format(GetLangEntry('x-byte-seed'), [InfoSeedSize]) + ')';

        // source.Position := source.size - cipher.context.BufferSize - hash.DigestSize;
        // l_calcmac, HexAnsicht(readraw(cipher.context.BufferSize)
        // l_filehash, HexAnsicht(readraw(hash.DigestSize)

        b_decrypt.Visible := true;
        b_encrypt.Visible := false;
        b_direct.Visible := true;
        b_open.Visible := false;
        b_folder.Visible := false;

        chk_compress.Visible := false;

        mode := tcEncrypted;
      end
      else
      begin
        edt_vle1.Color := clWindow;
        edt_vle1.ReadOnly := false;
        edt_vle1.TabStop := true;

        IdentityBase := IDBase;

        // damit später in EncodeFile nicht der Name der DeCoder.zip geschrieben wird, sondern der Verzeichnisname
        ersatzname := alternativer_name;

        edt_vle5.Text := GetLangEntry('unencrypted');

        edt_vle6.Text := '-';

        if alternativer_name <> '' then
        begin
          if alternativer_name = getlangentry('dateiensammlung') then
            T := getlangentry('dateiensammlung')
          else
            T := lang_getdirectoryname;
        end
        else
        begin
          T := FindeDateityp(fil);
          if (T = '') then T := Format(GetLangEntry('x-file'), [uppercase(copy(extractfileext(fil), 2, length(extractfileext(fil))-1))]);
        end;
        edt_vle3.Text := T;

        edt_vle4.Text := IntelligenteDateigroesse(source.size);

        edt_vle7.Text := '-';

        b_decrypt.Visible := false;
        b_encrypt.Visible := true;
        b_direct.Visible := false;
        b_open.Visible := false;
        b_folder.Visible := false;

        chk_compress.Visible := true;
        if alternativer_name = '' then
          chk_compress.Checked := ZLibCompressFile
        else
          chk_compress.Checked := ZLibCompressFolder;

        mode := tcDecrypted;
      end;

    finally
      source.free;
    end;
  except
    dateibeschaedigt;
    steuerelementesperren(false);
    exit;
  end;

  finally
  steuerelementesperren(false);
  zeige_wartenform(false);
  end;

  lbl_passwort.Visible := true;
  lbl_passwort2.Visible := mode <> tcEncrypted;
  edt_passwort.Visible := true;
  edt_passwort2.Visible := mode <> tcEncrypted;
  lbl_entropy.Visible := mode <> tcEncrypted;
  lbl_equal.Visible := mode <> tcEncrypted;
  chk_securedelete.Visible := true;
  lbl_readerror.Visible := false;
  img_error.Visible := false;
  tmr_capslock.Enabled := true;

  if ((mode = tcDecrypted) or (mode = tcEncrypted)) and (edt_passwort.Showing) then edt_passwort.SetFocus;
  mm_actions.visible := mode <> tcUnknown;
  m_encrypt.Enabled := mode = tcDecrypted;
  m_decrypt.Enabled := mode = tcEncrypted;
  m_direct.Enabled := mode = tcEncrypted;

  if (alternativer_name = getlangentry('dateiensammlung')) and (mode = tcDecrypted) then
  begin
    m_dateiensammlung_add_file.Enabled := true;
    m_dateiensammlung_add_folder.Enabled := true;
    m_dateiensammlung_show.Enabled := true;
  end
  else
  begin
    m_dateiensammlung_add_file.Enabled := false;
    m_dateiensammlung_add_folder.Enabled := false;
    m_dateiensammlung_show.Enabled := false;
  end;

  application.Restore;
  application.BringToFront;

  tmr_refresh.Enabled := true;
end;

procedure TMainForm.kazip_onchange(Sender: TObject; ChangeType: Integer);
begin
  //
end;

procedure TMainForm.kazip_overwrite(Sender:TObject; Var FileName : String; Var Action : TOverwriteAction);
begin
  //
end;

procedure TMainForm.form_create(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i: integer;
begin
  ClientHeight := 297; // Bug in Delphi verändert manchmal Formhöhe

  errorlevel := 0; // Keine Fehler
                             
  // Sprachdatei einlesen
  if fileexists(ExtractFilePath(Application.ExeName)+'Language.ini') then
  begin
    ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'Language.ini');

    // Version prüfen
    if (ini.ReadString('Signature', 'Version', '') <> DC4Ver) then
    begin
      // paramstr() wird verwendet, weil ParamZeile erst bei Start() gesetzt wird
      if not ((paramstr_firstposition('/clean') <> -1) and (paramstr_firstposition('/silent') <> -1)) then
      begin
        Application.MessageBox('Language.ini', 'Signature?', MB_OK + MB_ICONERROR);
        errorlevel := 11;
        close;
        exit;
      end;
    end;

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
  end
  else
  begin
    Application.MessageBox('Language.ini', 'Error', MB_OK + MB_ICONERROR);
    errorlevel := 11;
    close;
    exit;
  end;

  // Jetzt gehts los...

  // VCL-Ersatz

  DropFile := TDropFileTarget.Create(self);
  DropFile.Dragtypes := [dtCopy];
  DropFile.OnDrop := dropfile_drop;

  KaZip := TKaZip.Create(self);
  KaZip.UseTempFiles := false;
  kazip.CompressionType := StZIPType;
  KaZip.OverwriteAction := oaOverwriteAll;
  KaZip.OnDecompressFile := kazip_actions_decompress;
  KaZip.OnCompressFile := kazip_actions_compress;
  KaZip.OnZipChange := kazip_onchange;
  //KaZip.OnZipOpen := kazip_actions;
  KaZip.OnAddItem := kazip_add;
  //KaZip.OnRebuildZip := kazip_actions;
  //KaZip.OnRemoveItems := kazip_actions;
  KaZip.OnOverwriteFile := kazip_overwrite;

  {xpmenu := TXPMenu.Create(self);
  xpmenu.XPControls := [xcMainMenu];
  xpmenu.Gradient := true;
  xpmenu.Active := true;}

  // Anwendung vorbereiten

  RegisterDECClasses([TCipher_Blowfish, TCipher_Twofish, TCipher_IDEA,
    TCipher_Cast256, TCipher_Mars, TCipher_RC4, TCipher_RC6, TCipher_Rijndael,
    TCipher_Square, TCipher_SCOP, TCipher_Sapphire, TCipher_1DES, TCipher_2DES,
    TCipher_3DES, TCipher_2DDES, TCipher_3DDES, TCipher_3TDES, TCipher_3Way,
    TCipher_Cast128, TCipher_Gost, TCipher_Misty, TCipher_NewDES, TCipher_Q128,
    TCipher_RC2, TCipher_RC5, TCipher_SAFER, TCipher_Shark, TCipher_Skipjack,
    TCipher_TEA, TCipher_TEAN]);

  RegisterDECClasses([THash_MD2, THash_MD4, THash_MD5, THash_SHA, THash_SHA1,
    THash_SHA256, THash_SHA384, THash_SHA512, THash_Sapphire, THash_Panama,
    THash_Tiger, THash_RipeMD128, THash_RipeMD160, THash_RipeMD256,
    THash_RipeMD320, THash_Haval128, THash_Haval160, THash_Haval192,
    THash_Haval224, THash_Haval256, THash_Whirlpool, THash_Whirlpool1,
    THash_Square, THash_Snefru128, THash_Snefru256]);

  SetDefaultCipherClass(CipherByName(StCipher));
  SetDefaultHashClass(HashByName(StHash));

  RandomSeed;

  Randomize;
  temp_unique_number := inttostr(random(2147483647));

  // Formular vorbereiten
  img_warning.Picture.Bitmap.LoadFromResourceName(hInstance, 'WARNING');
  img_error.Picture.Bitmap.LoadFromResourceName(hInstance, 'ERROR');
  img_type.Picture.Bitmap.LoadFromResourceName(hInstance, 'NOOPEN');

  mm_file.Caption := GetLangEntry('file');
  m_open.Caption := GetLangEntry('openhint');
  m_folder.Caption := GetLangEntry('folderhint');
  m_config.Caption := GetLangEntry('confighint');
  m_close.Caption := GetLangEntry('closefile');
  m_exit.Caption := GetLangEntry('exit');

  mm_dateiensammlung.caption := GetLangEntry('dateiensammlung');
  m_dateiensammlung_neu.caption := GetLangEntry('dateiensammlung_neu');
  m_dateiensammlung_add_file.caption := GetLangEntry('dateiensammlung_add_file');
  m_dateiensammlung_add_folder.caption := GetLangEntry('dateiensammlung_add_folder');
  m_dateiensammlung_show.caption := GetLangEntry('dateiensammlung_show');

  mm_actions.Caption := GetLangEntry('actions');
  m_encrypt.Caption := GetLangEntry('enchint');
  m_decrypt.Caption := GetLangEntry('dechint');
  m_direct.Caption := GetLangEntry('direct');
  m_delete.Caption := GetLangEntry('erase');

  mm_help.Caption := GetLangEntry('help');
  m_help.Caption := GetLangEntry('helphint');
  m_web_update.Caption := GetLangEntry('updates');
  m_web_email.Caption := GetLangEntry('web_email');
  m_web_keytransmitter.Caption := GetLangEntry('web_keytransmitter');
  m_info.Caption := GetLangEntry('about');
  m_web.Caption := GetLangEntry('web_ressources');
  m_web_dm.Caption := GetLangEntry('web_dm');
  m_web_vts.Caption := GetLangEntry('web_vts');
  m_web_project.Caption := GetLangEntry('web_project');
  m_web_infopages.Caption := GetLangEntry('web_infopages');
  m_web_forum.Caption := GetLangEntry('web_forum');

  b_open.Caption := GetLangEntry('openhint');
  b_folder.Caption := GetLangEntry('folderhint');
  b_encrypt.Caption := GetLangEntry('enchint');
  b_decrypt.Caption := GetLangEntry('dechint');
  b_direct.Caption := GetLangEntry('direct');

  caption := '(De)Coder '+DC4Ver;
  lbl_vle1.Caption := GetLangEntry('opened');
  lbl_vle2.Caption := GetLangEntry('location');
  lbl_vle3.Caption := GetLangEntry('filetype');
  lbl_vle4.Caption := GetLangEntry('filesize');
  lbl_vle5.Caption := GetLangEntry('status');
  lbl_vle6.Caption := GetLangEntry('originalname');
  lbl_vle7.Caption := GetLangEntry('cryptmode');
  lbl_passwort.caption := GetLangEntry('password');
  lbl_passwort2.caption := GetLangEntry('repassword');
  lbl_equal.Hint := GetLangEntry('equalhint');
  lbl_equal.caption := '';
  lbl_entropy.Hint := GetLangEntry('entropy');
  chk_securedelete.Caption := GetLangEntry('securedelete');
  chk_compress.Caption := GetLangEntry('compress');
  img_warning.Hint := GetLangEntry('capswarning');
  lbl_capswarning.caption := GetLangEntry('capslock');
  lbl_capswarning.Hint := GetLangEntry('capswarning');
  dlg_open.filter := GetLangEntry('filter_all')+' (*.*)|*.*|'+GetLangEntry('filter_encrypted')+' (*.dc4)|*.dc4';
  dlg_open_element.filter := GetLangEntry('filter_all')+' (*.*)|*.*';
  dlg_save_dec.filter := GetLangEntry('filter_all')+' (*.*)|*.*';
  dlg_save_enc.filter := GetLangEntry('filter_encrypted')+' (*.dc4)|*.dc4';
  lbl_readerror.caption := GetLangEntry('errorreading');
  //progress_position(wartenform.ProgressBar1.min);

  edt_vle1.text := GetLangEntry('nofile');
  lbl_entropy.Caption := '';

  DropFile.Register(mainform);

  // Dynamische Größenanpassung der Elemente aufgrund verschiedener Sprachen

  edt_vle1.left := lbl_vle1.Left + 8 + max_7(lbl_vle1.Width, lbl_vle2.Width, lbl_vle3.Width, lbl_vle4.Width, lbl_vle5.Width, lbl_vle6.Width, lbl_vle7.Width);
  edt_vle2.left := edt_vle1.left;
  edt_vle3.left := edt_vle1.left;
  edt_vle4.left := edt_vle1.left;
  edt_vle5.left := edt_vle1.left;
  edt_vle6.left := edt_vle1.left;
  edt_vle7.left := edt_vle1.left;

  edt_vle1.Width := clientwidth - edt_vle1.Left - 8;
  edt_vle2.Width := edt_vle1.Width;
  edt_vle3.Width := edt_vle1.Width;
  edt_vle4.Width := edt_vle1.Width;
  edt_vle5.Width := edt_vle1.Width;
  edt_vle6.Width := edt_vle1.Width;
  edt_vle7.Width := edt_vle1.Width;

  edt_passwort.left := lbl_passwort.left + max_2(lbl_passwort.width, lbl_passwort2.width) + 8;
  edt_passwort2.left := edt_passwort.left;
  lbl_entropy.left := edt_passwort.left + edt_passwort.width + 8;
  lbl_equal.left := lbl_entropy.left;
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

function NumberOfFiles(RootFolder: string): integer;
var
  SR: TSearchRec;
  Num: integer;
begin
  Num := 0;
  result := 0;

  if RootFolder = '' then
    Exit;
  if AnsiLastChar(RootFolder)^ <> '\' then
    RootFolder := RootFolder + '\';

  RootFolder := IncludeTrailingPathDelimiter(RootFolder);

  if FindFirst(RootFolder + '*.*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
          // --> ein Verzeichnis wurde gefunden
          if (SR.Name <> '.') and (SR.Name <> '..') then
            Num := Num + NumberOfFiles(RootFolder + SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  if FindFirst(RootFolder + '*.*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory <> faDirectory then
        begin
          // --> eine Datei wurde gefunden
          inc(Num);
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;

  result := Num;
end;

procedure TMainForm.openfolder(fol: string);
var
  tz: string;
begin
  steuerelementesperren(true);
  zeige_wartenform(true);
  try

  // Vorrausberechnung für ZIP-Vorgang
  DateienImOrdner := NumberOfFiles(fol);
  DateienCounter := 0;

  // Tempname
  tz := GetTempDir+TempPre+temp_unique_number+TempExtZip;

  dc_deletefile(tz);
  zipfolder(fol, tz);

  if fileexists(tz) then
  begin
    // Oha! Ein ganzes Laufwerk wurde verschlüsselt (da hat jemand zu viel Zeit...)
    if copy(fol, length(fol)-1, 2) = ':\' then
      fol := GetLangEntry('drive')+' '+copy(fol, 0, 1);
    openfile(tz, fol, false);
  end;

  finally
  steuerelementesperren(false);
  zeige_wartenform(false);
  end;
end;

procedure TMainForm.zipfolder(verzeichnis, zipname: string);
begin
  try
    KAZip.Close;
  except

  end;

  progress_position(wartenform.pbr_progress.min);
  progress_text(GetLangEntry('zip_folder'), verzeichnis);

  try
    CreateZipFile(zipname);

    KAZip.Open(zipname);
    kazip.AddFolder(verzeichnis, verzeichnis, '*', true);
    kazip.close;
  except
    // Kann auftreten, wenn Anwendung beim Zippen geschlossen wurde
  end;

  progress_position(wartenform.pbr_progress.min);
end;

procedure TMainForm.freestreams();
begin
  // ich kenne keine andere möglichkeit, den laufenden cipher-prozess zu beenden
  // ich gebe einfach den stream frei und fange die exceptions ab

  try
    kazip.Close;
  except
  end;

  try
    tempstream.Free;
  except
  end;

  try
    CompressInputStream.free;
  except
  end;

  try
    CompressOutputStream.free;
  except
  end;

  try
    CompressionStream.free;
  except
  end;

  try
    DeCompressionStream.Free;
  except
  end;

  try
    ZippingStream.Free;
  except
  end;

  try
    DropFile.unregister;
  except
  end;

  try
    DropFile.Free;
  except
  end;

  try
    KaZip.Free;
  except
  end;

  try
    BestimmeDateiGroesseSource.Free;
  except
  end;

  {try
    XPMenu.Free;
  except
  end;}
end;

procedure TMainForm.form_close(Sender: TObject; var Action: TCloseAction);
begin
  steuerelementesperren(true);
  bordericons := [];

  // Weitermachen, bis die Dateien nicht mehr gesperrt ist
  // wird nun von eigenen Prozess übernommen
  // SecureDeleteWhenUnlocked(GetTempDir+TempPre+temp_unique_number+TempExtTmp);
  // SecureDeleteWhenUnlocked(GetTempDir+TempPre+temp_unique_number+TempExtZip);

  // Beim Beenden, sollen nur die temporären Dateien der aktuellen Sitzung entfernt werden,
  // daher macht diese Zeile wenig Sinn!
  // if paramzeile_firstposition('/clean') = -1 then
  //   ShellExecute(application.Handle, 'open', pchar(application.exename), pchar('/clean /silent'), pchar(extractfilepath(application.exename)), SW_HIDE);

  // Information: Da 10 Sekunden bei /clean /silent gewartet wird, kann bereits hier gestartet werden

  if (paramzeile_firstposition('/clean') = -1) and (errorlevel <> 11) then
    ShellExecute(application.Handle, 'open', pchar(application.exename), pchar('/clean /silent /only='+temp_unique_number), pchar(extractfilepath(application.exename)), SW_HIDE);

  try
    freestreams;
  except

  end;

  halt(mainform.errorlevel);

  //application.Terminate; // Wenn über Befehlszeile verschlüsselt wird,
                           // dann ist diese Zeile notwendig, ansonsten endet
                           // die Anwendung nie
end;

function TMainForm.Unzip(ZipFile, Verzeichnis: string): boolean;
begin
  result := true;
  steuerelementesperren(true);
  zeige_wartenform(true);
  try
  progress_position(wartenform.pbr_progress.min);
  progress_text(GetLangEntry('unzip_folder'), zipfile);
  kazip.Open(ZipFile);
  DateienImOrdner := kazip_numfiles(kazip);
  DateienCounter := 0;
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
  progress_position(wartenform.pbr_progress.min);
  finally
  steuerelementesperren(false);
  zeige_wartenform(false);
  end;
end;

procedure TMainForm.m_web_vts_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.viathinksoft.de/', '', '', sw_normal);
end;

procedure TMainForm.mm_actions_execute(Sender: TObject);
begin
  // platzhalter, damit enabled
end;

procedure TMainForm.mm_file_execute(Sender: TObject);
begin
  // platzhalter, damit enabled
end;

procedure TMainForm.mm_help_execute(Sender: TObject);
begin
  // platzhalter, damit enabled
end;

procedure TMainForm.m_web_infopages_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.viathinksoft.de/info/decoder/', '', '', sw_normal);
end;

procedure TMainForm.m_web_forum_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.viathinksoft.de/devboard/viewforum.php?f=32', '', '', sw_normal);
end;

procedure TMainForm.m_help_execute(Sender: TObject);
begin
  if fileexists(extractfilepath(application.exename)+'help.html') then
    ShellExecute(Application.Handle, 'open', PChar(extractfilepath(application.exename)+'help.html'), '', '', SC_DEFAULT)
  else
    Application.MessageBox(pchar(GetLangEntry('helpnotfound')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
end;

procedure TMainForm.dropfile_drop(Sender: TObject; ShiftState: TShiftState;
  Point: TPoint; var Effect: Integer);
var
  i: integer;
begin
  effect := DROPEFFECT_NONE; // damit stürzt Windows 95 nicht ab

  steuerelementesperren(true);
  zeige_wartenform(true);
  try

  if DropFile.Files.Count > 1 then
  begin
    try
      kazip.close;
    except

    end;

    if fileexists(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip) then
      deletefile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
    CreateZipFile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
    for i := 0 to dropfile.Files.Count - 1 do
    begin
      addtozip(dropfile.Files.strings[i], GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip)
    end;

    openfile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip, GetLangEntry('dateiensammlung'), true);
  end
  else
  begin
    if fileexists(dropfile.Files.strings[0]) then
      openfile(dropfile.Files.strings[0], '', false);

    if directoryexists(dropfile.Files.strings[0]) then
      openfolder(dropfile.Files.strings[0]);
  end;

  finally
  steuerelementesperren(false);
  zeige_wartenform(false);
  end;
end;

procedure TMainForm.edt_passwort_change(Sender: TObject);
var
  qu: extended;
  r, g: integer;
const
  helligkeit: byte = 128;
begin
  qu := PassphraseQuality(edt_passwort.Text);

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

  lbl_entropy.Caption := inttostr(round(qu*100)) + '%';
  lbl_entropy.font.color := rgb(r, g, 0);

  edt_password2_change(sender);
end;

procedure TMainForm.m_exit_execute(Sender: TObject);
begin
  close;
end;

// von Krypto Anarkist
procedure TMainForm.edt_passwort2_keypress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
  begin
    Key := #0;
    // Ist Shift zusätzlich gedrückt?
    if GetKeyState(VK_Shift) and $8000 <> 0 then
      PostMessage(Handle, WM_NEXTDLGCTL, 1, 0) //edt_enc_password.SetFocus;
    else
      if m_encrypt.Enabled then m_encrypt.Click;
  end;

  edt_passwort_keypress(sender, key);
end;

// von Krypto Anarkist
procedure SimulateKeyDown(Key : byte);
begin
  keybd_event(Key, 0, 0, 0);
end;

// von Krypto Anarkist
procedure SimulateKeyUp(Key : byte);
begin
  keybd_event(Key, 0, KEYEVENTF_KEYUP, 0);
end;

// von Krypto Anarkist
procedure SimulateKeystroke(Key : byte; extra : DWORD);
begin
  keybd_event(Key,
              extra,
              0,
              0);
  keybd_event(Key,
              extra,
              KEYEVENTF_KEYUP,
              0);
end;

// von Krypto Anarkist
procedure SendKeys(s : string);
var
  i : integer;
  flag : bool;
  w : word;
begin
 {Get the state of the caps lock key}
  flag := not GetKeyState(VK_CAPITAL) and 1 = 0;
 {If the caps lock key is on then turn it off}
  if flag then
    SimulateKeystroke(VK_CAPITAL, 0);
  for i := 1 to Length(s) do begin
    w := VkKeyScan(s[i]);
   {If there is not an error in the key translation}
    if ((HiByte(w) <> $FF) and
        (LoByte(w) <> $FF)) then begin
     {If the key requires the shift key down - hold it down}
      if HiByte(w) and 1 = 1 then
        SimulateKeyDown(VK_SHIFT);
     {Send the VK_KEY}
      SimulateKeystroke(LoByte(w), 0);
     {If the key required the shift key down - release it}
      if HiByte(w) and 1 = 1 then
        SimulateKeyUp(VK_SHIFT);
    end;
  end;
 {if the caps lock key was on at start, turn it back on}
  if flag then
    SimulateKeystroke(VK_CAPITAL, 0);
end;

// von Krypto Anarkist
function genGarbageString(chars: integer):string;
var
   i: integer;
begin
   result := '';
   for i := 1 to chars do
      result := result + chr(random(95)+32);
end;

// http://www.dsdt.info/tipps/?id=28
function KeyPressed(Key: Integer): Boolean;
begin
  KeyPressed := (GetAsyncKeyState(Key) and $8000 <> 0);
end;

procedure TMainForm.edt_passwort_enter(Sender: TObject);
begin
  if not KeyPressed(vk_Menu) and not KeyPressed(vk_Control) then
  begin
    tedit(sender).Tag := random(configform.upd_garbarge.Position);
    sendKeys(genGarbageString(tedit(sender).Tag));
  end;
end;

// idee von Krypto Anarkist... hat in seiner komponente keypress verwendet, was bugs bei Strg-Kombinationen verursacht hat
procedure TMainForm.edt_passwort_keypress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if m_encrypt.Enabled then m_encrypt.Click;
    if m_decrypt.Enabled then m_decrypt.Click;
  end
  else
  begin
    if tedit(sender).Tag > 0 then
    begin
      tedit(sender).Tag := tedit(sender).Tag - 1;
      Key := chr(0);
    end
    else
    begin
      if not KeyPressed(vk_Menu) and not KeyPressed(vk_Control) then
      begin
        tedit(sender).Tag := random(configform.upd_garbarge.Position);
        sendKeys(genGarbageString(tedit(sender).Tag));
      end;
    end;
  end;
end;

procedure TMainForm.form_show(Sender: TObject);
begin
  application.Restore;
  application.BringToFront;
end;

procedure TMainForm.steuerelementesperren(sperren: boolean);
begin
  steuerelementegesperrt := sperren;

  chk_securedelete.enabled := not sperren;
  chk_compress.Enabled := not sperren;
  img_type.enabled := not sperren;
  img_warning.enabled := not sperren;
  lbl_capswarning.enabled := not sperren;
  img_error.enabled := not sperren;
  lbl_readerror.enabled := not sperren;
  lbl_passwort.enabled := not sperren;
  lbl_passwort2.enabled := not sperren;
  lbl_entropy.Enabled := not sperren;
  lbl_equal.enabled := not sperren;
  lbl_vle1.enabled := not sperren;
  lbl_vle2.enabled := not sperren;
  lbl_vle3.enabled := not sperren;
  lbl_vle4.enabled := not sperren;
  lbl_vle5.enabled := not sperren;
  lbl_vle6.enabled := not sperren;
  lbl_vle7.enabled := not sperren;
  edt_vle1.enabled := not sperren;
  edt_vle2.enabled := not sperren;
  edt_vle3.enabled := not sperren;
  edt_vle4.enabled := not sperren;
  edt_vle5.enabled := not sperren;
  edt_vle6.enabled := not sperren;
  edt_vle7.enabled := not sperren;
  mm_file.enabled := not sperren;
  mm_dateiensammlung.enabled := not sperren;
  mm_actions.enabled := not sperren;
  mm_help.enabled := not sperren;
  b_encrypt.enabled := not sperren;
  b_decrypt.enabled := not sperren;
  b_direct.enabled := not sperren;
  b_open.enabled := not sperren;
  b_folder.enabled := not sperren;
  edt_passwort.enabled := not sperren;
  edt_passwort2.enabled := not sperren;

  if sperren then
    screen.Cursor := crHourGlass
  else
    screen.Cursor := crDefault;

  application.ProcessMessages;
end;

procedure TMainForm.tmr_refresh_timer(Sender: TObject);
begin
  // Bei einem Test mit meinem Windows 98 System kam es zu Fehlern bei den vle-Editboxen
  // nach dem Drag&Drop. Daher werden sie her neu gezeichnet nach OpenFile.

  tmr_refresh.Enabled := false;

  mainform.edt_vle1.Repaint;
  mainform.edt_vle2.Repaint;
  mainform.edt_vle3.Repaint;
  mainform.edt_vle4.Repaint;
  mainform.edt_vle5.Repaint;
  mainform.edt_vle6.Repaint;
  mainform.edt_vle7.Repaint;

  lbl_vle1.Repaint;
  lbl_vle2.Repaint;
  lbl_vle3.Repaint;
  lbl_vle4.Repaint;
  lbl_vle5.Repaint;
  lbl_vle6.Repaint;
  lbl_vle7.Repaint;

  img_error.Repaint;
  lbl_readerror.Repaint;
  lbl_entropy.Repaint;
  lbl_equal.Repaint;
  lbl_passwort.Repaint;
  lbl_passwort2.Repaint;
  edt_passwort.Repaint;
  edt_passwort2.Repaint;
  chk_securedelete.Repaint;
  chk_compress.Repaint;
  lbl_capswarning.Repaint;
  img_warning.Repaint;
  img_type.Repaint;

  b_direct.Repaint;
  b_encrypt.Repaint;
  b_decrypt.Repaint;
  b_open.Repaint;
  b_folder.Repaint;

  mainform.Repaint;

  application.ProcessMessages;
end;

procedure TMainForm.edt_password2_change(Sender: TObject);
begin
  if edt_passwort.Text = edt_passwort2.text then
  begin
    lbl_equal.caption := GetLangEntry('equal');
    lbl_equal.Font.Color := clGreen;
  end
  else
  begin
    lbl_equal.caption := GetLangEntry('notequal');
    lbl_equal.Font.Color := clMaroon;
  end;
end;

procedure TMainForm.edt_dec_password_keypress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
  begin
    key := #0;
    m_decrypt.Click;
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

procedure TMainForm.tmp_progressdurchlauf_timer(Sender: TObject);
begin
  if wartenform.pbr_progress.Position + (wartenform.pbr_progress.max div ttimer(self).tag) >= wartenform.pbr_progress.max then
    progress_position(wartenform.pbr_progress.min)
  else
    progress_position(wartenform.pbr_progress.position + wartenform.pbr_progress.max div ttimer(self).tag);
end;

procedure TMainForm.tmr_capslock_timer(Sender: TObject);
begin
  img_warning.visible := IsCapsLockOn;
  lbl_capswarning.visible := IsCapsLockOn;
end;

procedure TMainForm.kazip_add(Sender: TObject; ItemName: string);
begin
  progress_text(GetLangEntry('zip_folder'), ItemName);
  inc(DateienCounter);
  application.processmessages;
end;

procedure TMainForm.m_delete_execute(Sender: TObject);
var
  res: integer;
  old_ersatzname: string;
begin
  res := Application.MessageBox(pchar(GetLangEntry('erasewarning')), pchar(GetLangEntry('warning')), MB_YESNOCANCEL + MB_ICONEXCLAMATION);

  if res = ID_YES then
  begin
    zeige_wartenform(true);
    steuerelementesperren(true);
    try

    // Ersatzname bei geöffneten Ordner:          X:\BlaBlaBla\xxxx\
    // Ersatzname bei DC4-Datei mit Ordner-Flag:  xxxx
    if (copy(ersatzname, 2, 2) = ':\') then
    begin
      old_ersatzname := ersatzname;
      schliessedatei;
      dc_deletedir(old_ersatzname);
      if directoryexists(old_ersatzname) then
        Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
      else
        Application.MessageBox(pchar(GetLangEntry('erasesuccessful')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION);
    end
    else
    begin
      dc_deletefile(fileopen);
      if fileexists(fileopen) then
      begin
        Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
      end
      else
      begin
        schliessedatei;
        Application.MessageBox(pchar(GetLangEntry('erasesuccessful')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION);
      end;
    end;

    finally
    steuerelementesperren(false);
    zeige_wartenform(false);
    end;
  end;
end;

procedure TMainForm.m_info_execute(Sender: TObject);
begin
  AboutForm.PopupParent := Screen.ActiveForm; // http://www.delphipraxis.net/topic75743,0,asc,0.html
  AboutForm.showmodal;
end;

procedure TMainForm.m_open_execute(Sender: TObject);
var
  i: integer;
begin
  if dlg_open.execute then
  begin
    if dlg_open.Files.Count > 1 then
    begin
      steuerelementesperren(true);
      zeige_wartenform(true);
      try

      try
        kazip.close;
      except

      end;

      if fileexists(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip) then
        deletefile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
      CreateZipFile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
      for i := 0 to dlg_open.Files.Count - 1 do
      begin
        addtozip(dlg_open.Files.strings[i], GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip)
      end;

      openfile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip, GetLangEntry('dateiensammlung'), true);

      finally
      steuerelementesperren(false);
      zeige_wartenform(false);
      end;
    end
    else
      openfile(dlg_open.FileName, '', false);
  end;
end;

// https://www.delphipraxis.net/post43515.html , fixed , works for Delphi 12 Athens
function GetHTML(AUrl: string): RawByteString;
var
  databuffer : array[0..4095] of ansichar; // SIC! ansichar!
  ResStr : ansistring; // SIC! ansistring
  hSession, hfile: hInternet;
  dwindex,dwcodelen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of char;
  res    : pchar;
  Str    : pansichar; // SIC! pansichar
begin
  ResStr:='';
  if (system.pos('http://',lowercase(AUrl))=0) and
     (system.pos('https://',lowercase(AUrl))=0) then
     AUrl:='http://'+AUrl;

  // Hinzugefügt
  if Assigned(Application) then Application.ProcessMessages;

  hSession:=InternetOpen('InetURL:/1.0',
                         INTERNET_OPEN_TYPE_PRECONFIG,
                         nil,
                         nil,
                         0);
  if assigned(hsession) then
  begin
    // Hinzugefügt
    if Assigned(Application) then application.ProcessMessages;

    hfile:=InternetOpenUrl(
           hsession,
           pchar(AUrl),
           nil,
           0,
           INTERNET_FLAG_RELOAD,
           0);
    dwIndex  := 0;
    dwCodeLen := 10;

    // Hinzugefügt
    if Assigned(Application) then application.ProcessMessages;

    HttpQueryInfo(hfile,
                  HTTP_QUERY_STATUS_CODE,
                  @dwcode,
                  dwcodeLen,
                  dwIndex);
    res := pchar(@dwcode);
    dwNumber := sizeof(databuffer)-1;
    if (res ='200') or (res = '302') then
    begin
      while (InternetReadfile(hfile,
                              @databuffer,
                              dwNumber,
                              DwRead)) do
      begin

        // Hinzugefügt
        if Assigned(Application) then application.ProcessMessages;

        if dwRead =0 then
          break;
        databuffer[dwread]:=#0;
        Str := pansichar(@databuffer);
        resStr := resStr + Str;
      end;
    end
    else
      ResStr := 'Status:'+AnsiString(res);
    if assigned(hfile) then
      InternetCloseHandle(hfile);
  end;

  // Hinzugefügt
  if Assigned(Application) then application.ProcessMessages;

  InternetCloseHandle(hsession);
  Result := resStr;
end;

procedure TMainForm.m_web_update_execute(Sender: TObject);
var
  temp: string;
begin
  zeige_wartenform(true);
  steuerelementesperren(true);

  progress_position(wartenform.pbr_progress.Min);
  progress_text(getlangentry('wait_internet'), '');

  ttimer(self).tag := 2;
  tmp_progressdurchlauf.Enabled := true;

  temp := GetHTML('https://www.viathinksoft.de/update/?id='+updateid);
  if copy(temp, 0, 7) = 'Status:' then
  begin
    tmp_progressdurchlauf.Enabled := false;
    zeige_wartenform(false);
    steuerelementesperren(false);

    Application.MessageBox(pchar(MainForm.GetLangEntry('update_error')), pchar(MainForm.GetLangEntry('error')), MB_OK + MB_ICONERROR)
  end
  else
  begin
    if GetHTML('https://www.viathinksoft.de/update/?id='+updateid) <> DC4Ver then
    begin
      tmp_progressdurchlauf.Enabled := false;
      zeige_wartenform(false);
      steuerelementesperren(false);

      if Application.MessageBox(pchar(MainForm.GetLangEntry('update_yes')), pchar(MainForm.GetLangEntry('information')), MB_YESNO + MB_ICONASTERISK) = ID_YES then
        shellexecute(application.handle, 'open', pchar('https://www.viathinksoft.de/update/?id=@'+updateid), '', '', sw_normal);
    end
    else
    begin
      tmp_progressdurchlauf.Enabled := false;
      zeige_wartenform(false);
      steuerelementesperren(false);

      Application.MessageBox(pchar(MainForm.GetLangEntry('update_no')), pchar(MainForm.GetLangEntry('information')), MB_OK + MB_ICONASTERISK);
    end;
  end;
end;

procedure TMainForm.m_web_email_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', pchar('mailto:info@daniel-marschall.de?subject=(De)Coder '+DC4Ver), '', '', sw_normal);
end;

procedure TMainForm.m_config_execute(Sender: TObject);
begin
  ConfigForm.PopupParent := Screen.ActiveForm; // http://www.delphipraxis.net/topic75743,0,asc,0.html
  ConfigForm.showmodal;
end;

procedure TProgress.Process(const Min,Max,Pos: Int64);
var
  P: Integer;
begin
  P := floor((Pos - Min) / (Max - Min) * wartenform.pbr_progress.max);
  mainform.progress_position(P);
  application.ProcessMessages;
end;

constructor TProgress.Create;
begin
  inherited Create;
  mainform.progress_position(wartenform.pbr_progress.min);
end;

destructor TProgress.Destroy;
begin
  inherited Destroy;
  mainform.progress_position(wartenform.pbr_progress.min);
end;

procedure TMainForm.kazip_actions_compress(Sender: TObject; Current,
  Total: Integer);
begin
  progress_position(floor(wartenform.pbr_progress.max / DateienImOrdner * (DateienCounter + (Current / Total))));
  Application.ProcessMessages;
end;

procedure TMainForm.kazip_actions_decompress(Sender: TObject; Current,
  Total: Integer);
begin
  // Jede einzelne Datei:
  // progress_position(floor(Current / Total * wartenform.ProgressBar1.max));

  inc(DateienCounter);
  progress_position(floor(wartenform.pbr_progress.max / DateienImOrdner * DateienCounter));
  Application.ProcessMessages;
end;

procedure TMainForm.form_closequery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := BorderIcons <> [];
end;

procedure TMainForm.m_folder_execute(Sender: TObject);
var
  fol: string;
begin
  fol := BrowseForFolder(GetLangEntry('foldermessageopen'), true);
  if fol <> '' then
    OpenFolder(fol);
end;

// http://www.delphipraxis.net/post478066.html#478066
procedure ExecuteProgramm(const PFileName: string);
var 
  SEInfo: TShellExecuteInfo; 
  ExitCode: DWORD; 
  ExecuteFile: string; 
begin 
  ExecuteFile := '"' + PFileName + '"';
  FillChar(SEInfo, SizeOf(SEInfo), 0); 
  SEInfo.cbSize := SizeOf(TShellExecuteInfo); 

  with SEInfo do 
  begin 
    fMask := SEE_MASK_NOCLOSEPROCESS; 
    Wnd := Application.Handle; 
    lpFile := PChar(ExecuteFile); 
    nShow := SW_SHOWNORMAL; 
  end; 

  if ShellExecuteEx(@SEInfo) then 
  begin 
    repeat 
      Application.ProcessMessages; 
      GetExitCodeProcess(SEInfo.hProcess, ExitCode); 
    until (ExitCode <> STILL_ACTIVE) or 
      Application.Terminated; 
  end else 
  begin 
    // Application.MessageBox('Fehler beim Starten des Programms', 'Hinweis', MB_OK + MB_ICONERROR);
  end; 
end;

procedure TMainForm.m_direct_execute(Sender: TObject);
var
  ctemp, temp: string;
  error: boolean;
begin
  zeige_wartenform(true);
  steuerelementesperren(true);
  try

  forcedirectories(GetTempDir+TempDirect+'\'+temp_unique_number+'\');

  // Datei
  if (flag = DateiTag) or (flag = DateiCompressTag) then
  begin
    temp := GetTempDir+TempDirect+'\'+temp_unique_number+'\'+edt_vle6.text;
    ctemp := GetTempDir+TempPre+temp_unique_number+TempExtCmp;

    dc_deletefile(temp);
    error := false;
    try
      if (flag = DateiTag) then
        DecodeFile(fileopen, temp, edt_passwort.Text);
      if (flag = DateiCompressTag) then
      begin
        DecodeFile(fileopen, ctemp, edt_passwort.Text);
        DeCompress(ctemp, temp);
      end;
    except
      // Kann auftreten, wenn das Programm beim entschlüsseln geschlossen wird
      on EDECException do
      begin
        edt_passwort.Text := '';
        edt_passwort.SetFocus; // wirkt durch Exception nicht -.-
        error := true;
        if bordericons <> [] then raise;
      end;
    else
      error := true;
    end;

    if not error then
      ExecuteProgramm(temp);
  end;

  // Ordner
  if (flag = OrdnerTag) or (flag = OrdnerCompressTag) then
  begin
    temp := GetTempDir+TempPre+temp_unique_number+TempExtZip;
    ctemp := GetTempDir+TempPre+temp_unique_number+TempExtCmp;

    dc_deletefile(temp);
    error := false;
    try
      if (flag = OrdnerTag) then
        DecodeFile(fileopen, temp, edt_passwort.text);
      if (flag = OrdnerCompressTag) then
      begin
        DecodeFile(fileopen, ctemp, edt_passwort.Text);
        DeCompress(ctemp, temp);
      end;
    except
      // Kann auftreten, wenn das Programm beim entschlüsseln geschlossen wird
      edt_passwort.Text := '';
      edt_passwort.SetFocus; // wirkt durch Exception nicht -.-
      error := true;
      if bordericons <> [] then raise;
    end;

    if not error then
    begin
      if directoryexists(GetTempDir+TempDirect+'\'+temp_unique_number+'\'+edt_vle6.text) then
        dc_deletedir(GetTempDir+TempDirect+'\'+temp_unique_number+'\'+edt_vle6.text);
      mkdir(GetTempDir+TempDirect+'\'+temp_unique_number+'\'+edt_vle6.text);
      if Unzip(temp, GetTempDir+TempDirect+'\'+temp_unique_number+'\'+edt_vle6.text) then
      begin
        ExecuteProgramm(GetTempDir+TempDirect+'\'+temp_unique_number+'\'+edt_vle6.text);
      end;
    end;
  end;

  finally
  steuerelementesperren(false);
  mainform.zeige_wartenform(false);
  end;
end;

procedure TMainForm.m_dateiensammlung_add_file_click(Sender: TObject);
var
  i: integer;
begin
  if dlg_open_element.Execute then
  begin
    steuerelementesperren(true);
    zeige_wartenform(true);
    try

    if dlg_open_element.Files.Count > 1 then
    begin
      for i := 0 to dlg_open_element.Files.Count - 1 do
      begin
        addtozip(dlg_open_element.Files.Strings[i], fileopen);
      end;
    end
    else
    begin
      addtozip(dlg_open_element.filename, fileopen);
    end;

    edt_vle4.Text := BestimmeDateiGroesse(fileopen);

    finally
    steuerelementesperren(false);
    zeige_wartenform(false);
    end;
  end;
end;

function TMainForm.BestimmeDateiGroesse(filename: string): string;
begin
  BestimmeDateiGroesseSource := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    try
      result := IntelligenteDateigroesse(BestimmeDateiGroesseSource.size);
    except
      result := IntelligenteDateigroesse(0);
    end;
  finally
    BestimmeDateiGroesseSource.free;
  end;
end;

procedure TMainForm.m_dateiensammlung_add_folder_click(Sender: TObject);
var
  fol: string;
begin
  fol := BrowseForFolder(getlangentry('dateisammlung_folder_add_message'), true);
  if fol <> '' then
  begin
    steuerelementesperren(true);
    zeige_wartenform(true);
    try

    addtozip(fol, fileopen);
    edt_vle4.Text := BestimmeDateiGroesse(fileopen);

    finally
    steuerelementesperren(false);
    zeige_wartenform(false);
    end;
  end;
end;

procedure TMainForm.m_dateiensammlung_neu_click(Sender: TObject);
begin
  steuerelementesperren(true);
  zeige_wartenform(true);
  try

    try
      kazip.close;
    except

    end;

    if fileexists(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip) then
      deletefile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
    CreateZipFile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);

    openfile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip, GetLangEntry('dateiensammlung'), true);

  finally
  steuerelementesperren(false);
  zeige_wartenform(false);
  end;
end;

procedure TMainForm.m_dateiensammlung_show_click(Sender: TObject);
begin
  elementeform.PopupParent := Screen.ActiveForm; // http://www.delphipraxis.net/topic75743,0,asc,0.html
  elementeform.showmodal;
end;

procedure TMainForm.m_decrypt_execute(Sender: TObject);
var
  temp, ctemp, fol, neu: string;
  error: boolean;
begin
  zeige_wartenform(true);
  steuerelementesperren(true);
  try

  // Datei
  if (flag = DateiTag) or (flag = DateiCompressTag) then
  begin
    temp := GetTempDir+TempPre+temp_unique_number+TempExtTmp;
    ctemp := GetTempDir+TempPre+temp_unique_number+TempExtCmp;

    dc_deletefile(temp);
    error := false;
    try
      DecodeFile(fileopen, temp, edt_passwort.Text);
      if flag = DateiCompressTag then
        DeCompress(temp, ctemp)
      else
        ctemp := temp;
    except
      // Kann auftreten, wenn das Programm beim entschlüsseln geschlossen wird
      on EDECException do
      begin
        edt_passwort.Text := '';
        edt_passwort.SetFocus; // wirkt durch Exception nicht -.-
        error := true;
        if bordericons <> [] then raise;
      end;
    else
      error := true;
    end;

    // Wenn es zu keinem Fehler kam,
    // dann den User fragen, wohin endgültig speichern
    if not error then
    begin
      if ausgabegesetzt <> '' then
      begin
        // Ausgabeverzeichnis ist durch Befehlszeileneingabe bereits gesetzt
        dc_deletefile(ausgabegesetzt);
        if movefile(pchar(ctemp), pchar(ausgabegesetzt)) then
        begin
          if chk_securedelete.Checked then
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
          forcedirectories(extractfilepath(dlg_save_dec.filename));
          if movefile(pchar(ctemp), pchar(dlg_save_dec.FileName)) then
          begin
            if chk_securedelete.Checked then
            begin
              dc_deletefile(fileopen);
              if fileexists(fileopen) then
                Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION)
              else
                schliessedatei;
            end;
          end
          else
            Application.MessageBox(pchar(GetLangEntry('moveerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP);
        end
      end;
    end;
  end;

  // Ordner
  if (flag = OrdnerTag) or (flag = OrdnerCompressTag) then
  begin
    temp := GetTempDir+TempPre+temp_unique_number+TempExtZip;
    ctemp := GetTempDir+TempPre+temp_unique_number+TempExtCmp;

    dc_deletefile(temp);
    error := false;
    try
      DecodeFile(fileopen, temp, edt_passwort.text);
      if flag = OrdnerCompressTag then
      begin
        DeCompress(temp, ctemp);
        dc_deletefile(temp);
        movefile(pchar(ctemp), pchar(temp)); // KaZIP kann nur ZIP-Dateien öffnen, keine CMP
      end;
    except
      // Kann auftreten, wenn das Programm beim entschlüsseln geschlossen wird
      edt_passwort.Text := '';
      edt_passwort.SetFocus; // wirkt durch Exception nicht -.-
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
            if chk_securedelete.Checked then
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
              if chk_securedelete.Checked then
              begin
                dc_deletefile(fileopen);
                if fileexists(fileopen) then
                  Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
                else
                  schliessedatei;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // Wird sowieso beim Programmende ausgeführt
  // dc_deletefile(temp);

  finally
  steuerelementesperren(false);
  zeige_wartenform(false);
  end;
end;

procedure TMainForm.m_encrypt_execute(Sender: TObject);
var
  temp: string;
  error: boolean;
begin
  if edt_passwort.text <> edt_passwort2.text then
    Application.MessageBox(pchar(GetLangEntry('passwordsdifferent')), pchar(GetLangEntry('error')), MB_OK + MB_ICONSTOP)
  else
  begin
    steuerelementesperren(true);
    zeige_wartenform(true);
    try

    temp := GetTempDir+TempPre+temp_unique_number+TempExtTmp;

    dc_deletefile(temp);
    error := false;
    try
      EncodeFile(fileopen, temp, edt_passwort.text);
    except
      // Kann auftreten, wenn das Programm beim verschlüsseln geschlossen wird
      on EDECException do
      begin
        error := true;
        if bordericons <> [] then raise;
      end;
    else
      error := true;
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
            if chk_securedelete.Checked then
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
          forcedirectories(extractfilepath(dlg_save_enc.filename));
          if movefile(pchar(temp), pchar(dlg_save_enc.FileName)) then
          begin
            if chk_securedelete.Checked then
            begin
              if ersatzname <> '' then
              begin
                dc_deletedir(ersatzname);
                if directoryexists(ersatzname) then
                  Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION)
                else
                  schliessedatei;
              end
              else
              begin
                dc_deletefile(fileopen);
                if fileexists(fileopen) then
                  Application.MessageBox(pchar(GetLangEntry('notdeleted')), pchar(GetLangEntry('warning')), MB_OK + MB_ICONEXCLAMATION)
                else
                  schliessedatei;
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

    finally
    steuerelementesperren(false);
    zeige_wartenform(false);
    end;
  end;
end;

procedure TMainForm.m_close_execute(Sender: TObject);
begin
  SchliesseDatei;
end;

procedure TMainForm.schliessedatei();
begin
  m_close.Enabled := false;
  mm_actions.visible := false;
  m_dateiensammlung_add_file.Enabled := false;
  m_dateiensammlung_add_folder.Enabled := false;
  m_dateiensammlung_show.Enabled := false;

  fileopen := '';
  ersatzname := '';
  ausgabegesetzt := '';
  flag := 0;
  mode := tcUnknown;
  kazip.Close;

  edt_passwort.Text := '';
  edt_passwort2.Text := '';

  chk_securedelete.Checked := false;
  caption := '(De)Coder '+DC4Ver;
  progress_position(wartenform.pbr_progress.min);

  edt_vle1.Text := GetLangEntry('nofile');
  edt_vle2.text := '-';
  edt_vle3.Text := '-';
  edt_vle4.Text := '-';
  edt_vle5.Text := '-';
  edt_vle6.Text := '-';
  edt_vle7.Text := '-';

  edt_vle1.Color := clBtnFace;
  edt_vle1.ReadOnly := true;
  edt_vle1.TabStop := false;

  b_decrypt.Visible := false;
  b_encrypt.Visible := false;
  b_direct.Visible := false;
  b_open.Visible := true;
  b_folder.Visible := true;

  lbl_passwort.Visible := false;
  lbl_passwort2.Visible := false;
  edt_passwort.Visible := false;
  edt_passwort2.Visible := false;
  lbl_entropy.Visible := false;
  lbl_equal.Visible := false;
  chk_securedelete.Visible := false;
  lbl_readerror.Visible := false;
  img_error.Visible := false;
  tmr_capslock.Enabled := false;
  lbl_capswarning.Visible := false;
  img_warning.Visible := false;
  chk_compress.Visible := false;

  img_type.Picture.Bitmap.LoadFromResourceName(hInstance, 'NOOPEN');
end;

procedure TMainForm.m_web_keytransmitter_click(Sender: TObject);
begin
  shellexecute(application.Handle, 'open', 'https://www.viathinksoft.de/keytransmitter/', '', '', sw_normal);
end;

{ Allgemeine Funktion zum Verarbeiten der Kommandozeilenparameter }
// ursprünglich von OneInst
procedure TMainForm.ProcessCommandline(lpData: Pointer);
var
  arbeitsverzeichnis: string;
  i: integer;
  SR: TSearchRec;
  ts: tstringlist;
  fehler: boolean;
begin
  ParamZeile := ParamBlobToStr(lpData);

  // Syntaxprüfung der Befehlszeile

  if ((paramzeile_firstposition('/c') <> -1) and (ZaehleLinien(ParamZeile) > paramzeile_firstposition('/c')+2)) or
     ((paramzeile_firstposition('/x') <> -1) and (ZaehleLinien(ParamZeile) > paramzeile_firstposition('/x')+2)) or
     ((paramzeile_firstposition('/e') <> -1) and (ZaehleLinien(ParamZeile) > paramzeile_firstposition('/e')+1)) or
     ((paramzeile_firstposition('/?') <> -1) and (ZaehleLinien(ParamZeile) > paramzeile_firstposition('/?')+0)) or
     ((paramzeile_firstposition('/clean') <> -1) and (ZaehleLinien(ParamZeile) > paramzeile_firstposition('/clean')+2)) then
  begin
    errorlevel := 3; // Falsche Syntax
  end;

  // Anwendung dient nur als Hilfeaufruf

  if GebeLinieaus(ParamZeile, 1) = '/?' then
  begin
    m_help.Click;
    close;
    exit;
  end;

  // Anwendung dient nur als Cleaner

  if GebeLinieaus(ParamZeile, 1) = '/clean' then
  begin
    // 10 Sekunden warten, bis ggf. die temporären Dateien freigegeben wurden
    if (lowercase(GebeLinieaus(ParamZeile, 2)) = '/silent') then Schlafen(10000);
    DeleteTempFiles;
    close;
    exit;
  end;

  // Verarbeitung der Befehlszeile

  if (ZaehleLinien(ParamZeile) > 1) then
  begin
    if (paramzeile_firstposition('/c') <> -1) then
    begin
      GetDir(0, arbeitsverzeichnis);
      ausgabegesetzt := arbeitsverzeichnis+'\'+GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+2);
      if paramzeile_firstposition('/c') = 2 then
      begin
        // 1 Datei soll geöffnet werden
        if not fileexists(GebeLinieaus(ParamZeile, 1)) and not directoryexists(GebeLinieaus(ParamZeile, 1)) then
        begin
          errorlevel := 9; // Datei oder Ordner nicht gefunden.
        end
        else
        begin
          ts := tstringlist.Create;
          try
            if FindFirst(GebeLinieaus(ParamZeile, 1), faAnyFile, SR) = 0 then
            try
              repeat
                ts.Add(sr.Name);
              until FindNext(SR) <> 0;
            finally
              SysUtils.FindClose(SR);
            end;

            if ts.count = 1 then
            begin
              if fileexists(GebeLinieaus(ParamZeile, 1)) then
              begin
                openfile(GebeLinieaus(ParamZeile, 1), '', true);

                if mode = tcDecrypted then
                begin
                  // Datei verschlüsseln
                  if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
                    ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
                  edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
                  edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
                  try
                    m_encrypt.Click;
                  except
                    // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                    // bleiben, deswegen der Try-Except-Block.
                    errorlevel := 2; // Fehler bei Ver/Entschlüsselung
                  end;
                end;

                if mode = tcEncrypted then
                begin
                  // Datei oder Ordner entschlüsseln
                  if lowercase(extractfileext(ausgabegesetzt)) = lowercase(ExtDC4) then
                    ausgabegesetzt := copy(ausgabegesetzt, 0, length(ausgabegesetzt)-length(ExtDC4)); // Ausgabe darf nicht .dc4 haben!
                  edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
                  try
                    m_decrypt.Click;
                  except
                    // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                    // bleiben, deswegen der Try-Except-Block.
                    errorlevel := 2; // Fehler bei Ver/Entschlüsselung
                  end;
                end;
              end
              else if directoryexists(GebeLinieaus(ParamZeile, 1)) then
              begin
                // Ordner verschlüsseln
                openfolder(GebeLinieaus(ParamZeile, 1));
                edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
                edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
                if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
                  ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
                try
                  m_encrypt.Click;
                except
                  // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                  // bleiben, deswegen der Try-Except-Block.
                  errorlevel := 2; // Fehler bei Ver/Entschlüsselung
                end;
              end;
            end
            else
            begin
              // Dateiliste soll angelegt werden
              m_dateiensammlung_neu.Click;
              for i := 0 to ts.Count - 1 do
              begin
                addtozip(ts.Strings[i], fileopen);
              end;
              edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
              edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
              if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
                ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
              try
                m_encrypt.Click;
              except
                // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                // bleiben, deswegen der Try-Except-Block.
                errorlevel := 2; // Fehler bei Ver/Entschlüsselung
              end;
            end;
          finally
            ts.Free;
          end;
        end;
      end
      else
      begin
        // Dateiliste soll angelegt werden
        m_dateiensammlung_neu.Click;
        for i := 1 to paramzeile_firstposition('/c') - 1 do
        begin
          addtozip(dlg_open_element.filename, fileopen);
        end;
        edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
        edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/c')+1);
        if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
          ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
        try
          m_encrypt.Click;
        except
          // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
          // bleiben, deswegen der Try-Except-Block.
          errorlevel := 2; // Fehler bei Ver/Entschlüsselung
        end;
      end;
      close;
    end

    // Der Unterschied von /x zu /c ist, dass openfile() anders reagiert und die checkbox chk_securedelete anwählt
    else if (paramzeile_firstposition('/x') <> -1) then
    begin
      GetDir(0, arbeitsverzeichnis);
      ausgabegesetzt := arbeitsverzeichnis+'\'+GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+2);
      if paramzeile_firstposition('/x') = 2 then
      begin
        // 1 Datei soll geöffnet werden
        if not fileexists(GebeLinieaus(ParamZeile, 1)) and not directoryexists(GebeLinieaus(ParamZeile, 1)) then
        begin
          errorlevel := 9; // Datei oder Ordner nicht gefunden.
        end
        else
        begin
          ts := tstringlist.Create;
          try
            if FindFirst(GebeLinieaus(ParamZeile, 1), faAnyFile, SR) = 0 then
            try
              repeat
                ts.Add(sr.Name);
              until FindNext(SR) <> 0;
            finally
              SysUtils.FindClose(SR);
            end;

            if ts.count = 1 then
            begin
              if fileexists(GebeLinieaus(ParamZeile, 1)) then
              begin
                openfile(GebeLinieaus(ParamZeile, 1), '', true);

                if mode = tcDecrypted then
                begin
                  // Datei verschlüsseln
                  if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
                    ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
                  edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
                  edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
                  try
                    m_encrypt.Click;
                  except
                    // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                    // bleiben, deswegen der Try-Except-Block.
                    errorlevel := 2; // Fehler bei Ver/Entschlüsselung
                  end;
                end;

                if mode = tcEncrypted then
                begin
                  // Datei oder Ordner entschlüsseln
                  if lowercase(extractfileext(ausgabegesetzt)) = lowercase(ExtDC4) then
                    ausgabegesetzt := copy(ausgabegesetzt, 0, length(ausgabegesetzt)-length(ExtDC4)); // Ausgabe darf nicht .dc4 haben!
                  edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
                  try
                    m_decrypt.Click;
                  except
                    // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                    // bleiben, deswegen der Try-Except-Block.
                    errorlevel := 2; // Fehler bei Ver/Entschlüsselung
                  end;
                end;
              end
              else if directoryexists(GebeLinieaus(ParamZeile, 1)) then
              begin
                // Ordner verschlüsseln
                openfolder(GebeLinieaus(ParamZeile, 1));
                edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
                edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
                if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
                  ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
                try
                  m_encrypt.Click;
                except
                  // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                  // bleiben, deswegen der Try-Except-Block.
                  errorlevel := 2; // Fehler bei Ver/Entschlüsselung
                end;
              end;
            end
            else
            begin
              // Dateiliste soll angelegt werden
              m_dateiensammlung_neu.Click;
              for i := 0 to ts.Count - 1  do
              begin
                addtozip(ts.Strings[i], fileopen);
              end;
              edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
              edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
              if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
                ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
              try
                m_encrypt.Click;
              except
                // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
                // bleiben, deswegen der Try-Except-Block.
                errorlevel := 2; // Fehler bei Ver/Entschlüsselung
              end;
            end;
          finally
            ts.Free;
          end;
        end;
      end
      else
      begin
        // Dateiliste soll angelegt werden
        m_dateiensammlung_neu.Click;
        for i := 1 to paramzeile_firstposition('/x') - 1 do
        begin
          addtozip(dlg_open_element.filename, fileopen);
        end;
        edt_passwort.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
        edt_passwort2.Text := GebeLinieaus(ParamZeile, paramzeile_firstposition('/x')+1);
        if lowercase(extractfileext(ausgabegesetzt)) <> lowercase(ExtDC4) then
          ausgabegesetzt := ausgabegesetzt + ExtDC4; // Ausgabe muss .dc4 haben!
        try
          m_encrypt.Click;
        except
          // Falls ein Fehler auftreten sollte, soll der Ablauf hier nicht stehen
          // bleiben, deswegen der Try-Except-Block.
          errorlevel := 2; // Fehler bei Ver/Entschlüsselung
        end;
      end;
      close;
    end

    // Dateien löschen
    else if (lowercase(GebeLinieaus(ParamZeile, 2)) = '/e') then
    begin
      if (ZaehleLinien(ParamZeile) = 2) or ((ZaehleLinien(ParamZeile) = 3) and (GebeLinieaus(ParamZeile, 3) = '/notsilent')) then
      begin

        ts := tstringlist.Create;
        try
          if FindFirst(GebeLinieaus(ParamZeile, 1), faAnyFile, SR) = 0 then
          try
            repeat
               ts.add(sr.name);
            until FindNext(SR) <> 0;
          finally
            SysUtils.FindClose(SR);
          end;

          if ts.count = 1 then
          begin
            // 1 Datei
            if directoryexists(GebeLinieaus(ParamZeile, 1)) then
            begin
              dc_deletedir(GebeLinieaus(ParamZeile, 1));
              if directoryexists(GebeLinieaus(ParamZeile, 1)) then
              begin
                if (GebeLinieaus(ParamZeile, 3) = '/notsilent') then
                  Application.MessageBox(pchar(GetLangEntry('delerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR)
                else
                  errorlevel := 8; // Datei oder Ordner konnte nicht oder nur teilweise entfernt werden.
              end
              else if (GebeLinieaus(ParamZeile, 3) = '/notsilent') then
                Application.MessageBox(pchar(GetLangEntry('delok')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION)
            end
            else if fileexists(GebeLinieaus(ParamZeile, 1)) then
            begin
              dc_deletefile(GebeLinieaus(ParamZeile, 1));
              if fileexists(GebeLinieaus(ParamZeile, 1)) then
              begin
                if (GebeLinieaus(ParamZeile, 3) = '/notsilent') then
                  Application.MessageBox(pchar(GetLangEntry('delerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR)
                else
                  errorlevel := 8; // Datei oder Ordner konnte nicht oder nur teilweise entfernt werden.
              end
              else if (GebeLinieaus(ParamZeile, 3) = '/notsilent') then
                Application.MessageBox(pchar(GetLangEntry('delok')), pchar(GetLangEntry('information')), MB_OK + MB_ICONINFORMATION)
            end
            else
            begin
              if (GebeLinieaus(ParamZeile, 3) = '/notsilent') then
                Application.MessageBox(pchar(GetLangEntry('fileorfoldernotfound')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR)
              else
                errorlevel := 9; // Datei oder Ordner nicht gefunden.
            end;
          end
          else
          begin
            // Wildcard verwendet
            fehler := false;
            for i := 0 to ts.count - 1 do
            begin
              if fileexists(ts.strings[i]) then
                dc_deletefile(ts.strings[i]);
              if directoryexists(ts.strings[i]) then
                dc_deletedir(ts.strings[i]);

              if (fileexists(ts.strings[i]) or directoryexists(ts.strings[i])) and
                 (GebeLinieaus(ParamZeile, 3) = '/notsilent') then
              begin
                fehler := true;
              end;
            end;

            if fehler then
              Application.MessageBox(pchar(GetLangEntry('delerror')), pchar(GetLangEntry('error')), MB_OK + MB_ICONERROR)
          end;
        finally
          ts.free;
        end;

      end
      else
      begin
        errorlevel := 3; // Falsche Syntax
      end;
      close;
    end

    // Mehrere Dateien öffnen
    else
    begin
      show;

      steuerelementesperren(true);
      zeige_wartenform(true);
      try

      try
        kazip.close;
      except

      end;

      if fileexists(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip) then deletefile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
      CreateZipFile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip);
      for i := 1 to ZaehleLinien(ParamZeile) do
      begin
        addtozip(GebeLinieaus(ParamZeile, i), GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip)
      end;

      openfile(GetTempDir+TempPre+mainform.temp_unique_number+TempExtZip, GetLangEntry('dateiensammlung'), true);

      finally
      steuerelementesperren(false);
      zeige_wartenform(false);
      end;

      application.Restore;
      application.BringToFront;
    end;
  end
  else
  begin
    show;

    ts := tstringlist.Create;
    try
      if FindFirst(GebeLinieaus(ParamZeile, 1), faAnyFile, SR) = 0 then
      try
        repeat
          ts.Add(sr.Name);
        until FindNext(SR) <> 0;
      finally
        SysUtils.FindClose(SR);
      end;

      if ts.Count = 1 then
      begin
        // 1 Datei öffnen
        if fileexists(GebeLinieaus(ParamZeile, 1)) then
        begin
          steuerelementesperren(true);
          zeige_wartenform(true);
          try
            openfile(GebeLinieaus(ParamZeile, 1), '', false);
          finally
            steuerelementesperren(false);
            zeige_wartenform(false);
          end;
        end
        else if directoryexists(GebeLinieaus(ParamZeile, 1)) then
        begin
          steuerelementesperren(true);
          zeige_wartenform(true);
          try
            openfolder(GebeLinieaus(ParamZeile, 1));
          finally
            steuerelementesperren(false);
            zeige_wartenform(false);
          end;
        end;
      end
      else if ts.Count > 1 then
      begin
        // Dateiliste soll angelegt werden

        steuerelementesperren(true);
        zeige_wartenform(true);
        try

        m_dateiensammlung_neu.Click;
        for i := 0 to ts.count - 1 do
        begin
          addtozip(ts.Strings[i], fileopen);
        end;

        finally
        steuerelementesperren(false);
        zeige_wartenform(false);
        end;
      end;
    finally
      ts.free;
    end;

    application.Restore;
    application.BringToFront;
  end;
end;

procedure TMainForm.CreateZipFile(zipfile: string);
begin
  // kazip.CreateZip(zipfile);

  try
    ZippingStream := TFileStream.Create(zipfile, fmOpenReadWrite or fmCreate);
    try
      KAZip.CreateZip(ZippingStream);
    finally
      ZippingStream.Free;
    end;
  except

  end;
end;

procedure TMainForm.addtozip(fileorfolder, zipfile: string);
begin
  try
    kazip.close;
  except

  end;

  DateienImOrdner := 0;
  if directoryexists(fileorfolder) then
    DateienImOrdner := NumberOfFiles(fileorfolder);
  if fileexists(fileorfolder) then
    DateienImOrdner := 1;
    
  DateienCounter := 0;

  kazip.Open(zipfile);
  try
    try
      if fileexists(fileorfolder) then
        kazip.AddFile(fileorfolder, extractfilename(fileorfolder));
      if directoryexists(fileorfolder) then
        kazip.AddFolder(fileorfolder, extractfilepath(fileorfolder), '*', true);
    except

    end;
  finally
    kazip.Close;
  end;
end;

// von OneInst
procedure TMainForm.Start;
var
  lpData: Pointer;
  cbData: DWORD;
begin
  lpData := ParamStrToBlob(cbData);
  try
    ProcessCommandline(lpData);
  finally
    FreeMemory(lpData);
  end;
end;

end.
