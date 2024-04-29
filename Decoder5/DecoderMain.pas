unit DecoderMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, DECTypes, DECFormatBase;

type
  TFormMain = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UITypes, DECCiphers, DECCipherBase, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECCipherFormats, ZLib,
  EncdDecd, System.NetEncoding, DECCRC, DECBaseClass, Generics.Collections;

{$R *.dfm}

function SwapLong(Value: LongWord): LongWord;
{$IFDEF UseASM}
  {$IFDEF 486GE}
    {$DEFINE SwapLong_asm}
  {$ENDIF}
{$ENDIF}
{$IFDEF SwapLong_asm}
asm
       BSWAP  EAX
end;
{$ELSE}
begin
  Result := Value shl 24 or Value shr 24 or Value shl 8 and $00FF0000 or Value shr 8 and $0000FF00;
end;
{$ENDIF}

procedure OnProgressProc(Size, Pos: Int64; State: TDECProgressState);
begin
  FormMain.ProgressBar1.Min := 0;
  FormMain.ProgressBar1.Max := Size;

  if (State = Finished) then
    FormMain.ProgressBar1.Position := FormMain.ProgressBar1.Max
  else
    FormMain.ProgressBar1.Position := Pos;
end;

type
  TDcFormatVersion = (fvUnknown, fvDc40, fvDc41Beta, fvDc41FinalCancelled);

function DEC51_Identity(IdentityBase: Int64; ClassName: string): Int64;
var
  Signature: AnsiString;
begin
  Signature := AnsiString(StringOfChar(#$5A, 256 - Length(Classname)) + AnsiUpperCase(ClassName));
  Result := CRC32(IdentityBase, Signature[1], Length(Signature));
end;

function DEC51_HashById(IdentityBase, Identity: Int64): TDECHashClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
begin
  for p in TDECHash.ClassList do
  begin
    c := p.Value;
    cn := c.ClassName;
    if cn = 'THash_SHA0'{DEC6.0} then cn := 'THash_SHA'{DEC5.1};
    if cn = 'THash_Whirlpool0'{DEC6.0} then cn := 'THash_Whirlpool'{DEC5.1};
    if (c <> nil) and (Identity = DEC51_Identity(IdentityBase, cn)) then
    begin
      result := TDECHashClass(c);
      exit;
    end;
  end;
  raise Exception.CreateFmt('Hash ID %d with base %d not found', [Identity, IdentityBase]);
end;

function DEC51_CipherById(IdentityBase, Identity: Int64): TDECCipherClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
begin
  for p in TDECCipher.ClassList do
  begin
    c := p.Value;
    cn := c.ClassName;
    if cn = 'TCipher_AES'{DEC6.0} then cn := 'TCipher_Rijndael'{DEC5.1};
    if (c <> nil) and (Identity = DEC51_Identity(IdentityBase, cn)) then
    begin
      result := TDecCipherClass(c);
      exit;
    end;
  end;
  raise Exception.CreateFmt('Cipher ID %d with base %d not found', [Identity, IdentityBase]);
end;

procedure DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString);
var
  Source: TStream;
  OrigName: RawByteString;
  ahash: TDECHash;

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

  function ReadBinary: RawByteString;
  begin
    SetLength(Result, ReadByte);
    Read(Result[1], Length(Result));
  end;

  function ReadRaw(leng: integer): RawByteString;
  begin
    SetLength(Result, leng);
    Read(Result[1], Length(Result));
  end;

  function Convert(const Bytes: TBytes): RawByteString; inline;
  begin
    SetString(Result, PAnsiChar(pointer(Bytes)), length(Bytes));
  end;

const
  DC4_ID_BASES: array[0..3] of Int64 = (
    $84485225, // Hagen Reddmann Example (no .dc4 files)
    $59178954, // (De)Coder 4.0 (identities not used)
    $84671842, // (De)Coder 4.1 beta
    $19387612  // (De)Coder 4.1 final/cancelled
  );
var
  ch: RawByteString;
  F: byte;
  V: TDcFormatVersion;
  tmp: Long;
  Cipher: TDECCipher;
  Seed: RawByteString;
  tempstream: TFileStream;
  HashResult: TBytes;
  HashResult2: RawByteString;
  idBase: Int64;
  FileTerminus: RawByteString;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    // TODO: Make a version 4 file format, based on version 2 (not 3!)
    //       - Add a human readable magic checksum?
    //       - Add IV (enable it with the flags field)
    //       - hmac key
    //       - encrypt-then-hmac
    //       - No file terminus
    //       - Filler
    //       - Instead of identify base, use class names human readable?

    // 1. Flags
    // Bit 0:    [Ver1+] Is ZIP compressed folder (1) or a regular file (0)?
    // Bit 1:    [Ver2+] Additionally ZLip compressed (1) or not ZLib compressed (0)?
    // Bit 2:    Reserved
    // Bit 3:    Reserved
    // Bit 4:    Reserved
    // Bit 5:    Reserved
    // Bit 6:    Reserved
    // Bit 7:    Reserved
    F := ReadByte;

    // 2. Version
    // 01 = (De)Coder 4.0
    // 02 = (De)Coder 4.1 Beta
    // 03 = (De)Coder 4.1 Final Cancelled (never released)
    V := TDcFormatVersion(ReadByte); // if too big, it will automatically be set to 0
    if V = fvUnknown then raise Exception.Create('DC Invalid version');

    // We need this later
    if V = fvDc40 then
      FileTerminus := ''
    else if V = fvDc41Beta then
      FileTerminus := 'RENURVJNSU5VUw==' // (BASE64: "DCTERMINUS")
    else if V = fvDc41FinalCancelled then
      FileTerminus := RawByteString(#$63#$F3#$DF#$89#$B7#$27#$20#$EA)
    else
      Assert(False);
    tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);

    // 3. Filename
    // Ver1: Clear text filename, terminated with "?"
    // Ver2: Base64 encoded filename, terminated with "?"
    // Ver3: Encrypted filename
    OrigName := '';
    if (V = fvDc40) or (V = fvDc41Beta) then
    begin
      ch := ReadRaw(1);
      while ch <> '?' do
      begin
        OrigName := OrigName + ch;
        ch := ReadRaw(1);
      end;
      if V = fvDc41Beta then
      begin
        OrigName := Convert(DecodeBase64(OrigName));
      end;
    end
    else if V = fvDc41FinalCancelled then
    begin
      ReadByte; // Filename encrypted with user-password? (00=No, 01=Yes)
      tmp := ReadLong; // Size of the encrypted filename
      ReadRaw(tmp); // Filename encrypted with DEC 5.1c
                    // Encryption-Password = Hash->KDfx(5Eh D1h 6Bh 12h 7Dh B4h C4h 3Ch, Seed)
                    // if not encrypted with user-password, otherwise:
                    // Encryption-Password = Hash->KDfx(User-Password, Seed)
      OrigName := '(Not implemented)'; // TODO
    end
    else
      Assert(False);

    // 4. IdBase (only version 2+)
    if V = fvDc40 then
      idBase := DC4_ID_BASES[Ord(V)]
    else
      idBase := ReadLong;

    // 5. Cipher identity (only version 2+)
    if V = fvDc40 then
      Cipher := TCipher_Rijndael.Create
    else
      Cipher := DEC51_CipherById(idBase, ReadLong).Create;

    // 6. Cipher mode (only version 2+)
    if V = fvDc40 then
      Cipher.Mode := TCipherMode.cmCTSx
    else
      Cipher.Mode := TCipherMode(ReadByte);

    // 7. Hash identity (only version 2+)
    if V = fvDc40 then
      AHash := THash_SHA512.Create
    else
      AHash := DEC51_HashById(idBase, ReadLong).Create;

    // 8. Seed
    if V = fvDc40 then
      Seed := ReadRaw(16)
    else
      Seed := ReadRaw(ReadByte);

    // 9. Encrypted data
    (* TODO Not implemented for version 3 (actually, I don't understand this description anymore):
 					The "special-checksum" of a file can be used as the user password.
					The formula is:
					User-Password = Hash(File-Contents)
					Combined formula:
					Encryption-Password = Hash->KDfx(Hash(File-Contents), Seed)
      What I don't understand: How should the program know if the user password or the "hash" password is used??
    *)
    ahash.Init;
    Cipher.Init(TDECHashExtended(ahash).KDFx(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize), nil, $FF);
    ahash.Done;
    TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc);
    Cipher.Done;
    Cipher.Free;

    // 10. Checksum
    tempstream.position := 0;
    ahash.Init;
    TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
    if V = fvDc40 then
    begin
      HashResult2 := Convert(HashResult);
    end
    else if V = fvDc41Beta then
    begin
      HashResult2 := TDECHashExtended(ahash).CalcString(Convert(HashResult)+Seed+APassword, TFormat_Copy);
    end
    else if V = fvDc41FinalCancelled then
    begin
      HashResult2 := TDECHashExtended(ahash).CalcString(
        Convert(HashResult) + Seed +
            TDECHashExtended(ahash).CalcString(
              Seed+TDECHashExtended(ahash).CalcString(Seed+APassword, TFormat_Copy)
            , TFormat_Copy)
      , TFormat_Copy);
    end
    else
      Assert(False);
    ahash.Done;
    ahash.Free;
    if readraw(ahash.DigestSize) <> HashResult2 then
      raise Exception.Create('Hash mismatch');

    // 11. Terminus (only version 2+)
    if (FileTerminus <> '') and (ReadRaw(Length(FileTerminus)) <> FileTerminus) then
      raise Exception.Create('File terminus wrong');

    tempstream.Free;
  finally
    FreeAndNil(Source);
  end;
end;

procedure Decompress(InputFileName, OutputFileName: string);
var
  Buf: array[0..4095] of Byte;
  Count: Integer;
  CompressInputStream: TFileStream;
  CompressOutputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
begin
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
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  DecodeFile('c:\CORA2012\schloss.dc4', 'c:\CORA2012\schloss.dec', 'test');
  Decompress('c:\CORA2012\schloss.dec', 'c:\CORA2012\schloss_decoded.bmp');
  DeleteFile('c:\CORA2012\schloss.dec');
  ShowMessage('ok');
end;

end.
