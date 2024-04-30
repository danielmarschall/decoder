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

procedure OnProgressProc(Size, Pos: Int64; State: TDECProgressState);
begin
  FormMain.ProgressBar1.Min := 0;
  FormMain.ProgressBar1.Max := Size;

  if (State = Finished) then
    FormMain.ProgressBar1.Position := FormMain.ProgressBar1.Max
  else
    FormMain.ProgressBar1.Position := Pos;
end;

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

type
  TDECHashExtendedAuthentication = class helper for TDECHashAuthentication
    class function HMACStream(const Key: TBytes; const Stream: TStream; Size: Int64;
      const OnProgress:TDECProgressEvent): TBytes;
  end;

procedure DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString);
var
  Source: TStream;

  type
    TDcFormatVersion = (fvUnknown, fvDc40, fvDc41Beta, fvDc41FinalCancelled, fvDc50Wip);

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
    Result := Result shl 24 or Result shr 24 or Result shl 8 and $00FF0000 or Result shr 8 and $0000FF00;
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
  DC4_ID_BASES: array[0..4] of Int64 = (
    $84485225, // Hagen Reddmann Example (no .dc4 files)
    $59178954, // (De)Coder 4.0 (identities not used)
    $84671842, // (De)Coder 4.1 beta
    $19387612, // (De)Coder 4.1 final/cancelled
    $1259d82a  // (De)Coder 5.0 WIP
  );
var
  ch: RawByteString;
  F: byte;
  V: TDcFormatVersion;
  Cipher: TDECCipher;
  Seed: RawByteString;
  tempstream: TFileStream;
  HashResult: TBytes;
  HashResult2: RawByteString;
  idBase: Int64;
  FileTerminus: RawByteString;
  OrigName: RawByteString;
  ahash: TDECHash;
  Key: TBytes;
  FileNameUserPasswordEncrypted: boolean;
  FilenamePassword: RawByteString;
  KdfVersion: byte;
  HMacKey: RawByteString;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  tempstream := nil;
  cipher := nil;
  ahash := nil;
  try
    // TODO: Make a version 4 file format, based on version 2 (not 3!)
    //       - Add IV
    //       - Add Filler
    //  (OK) - Add version of KDF (KDFx, KDF1, KDF2, KDF3)
    //  (OK) - encrypt-then-hmac instead of hash of original data. Add hmac key
    //       - No file terminus, or a human readable magic sequence (OID)?

    // 1. Flags
    // Bit 0:    [Ver1+] Is ZIP compressed folder (1) or a regular file (0)?
    // Bit 1:    [Ver2+] Additionally ZLib compressed (1) or not ZLib compressed (0)?
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
    // 04 = (De)Coder 5.0 WorkInProgress
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
    FileNameUserPasswordEncrypted := false;
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
      FileNameUserPasswordEncrypted := ReadByte = $01; // Filename encrypted with user-password? (00=No, 01=Yes)
      // Filename encrypted with DEC 5.1c
      // Encryption-Password = Hash->KDfx(User-Password, Seed)
      // if not encrypted with user-password, otherwise:
      // Encryption-Password = Hash->KDfx(5Eh D1h 6Bh 12h 7Dh B4h C4h 3Ch, Seed)
      OrigName := ReadRaw(ReadLong); // will be decrypted below (after we initialized hash/cipher)
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

    // 7.5 KDF version (only version 4+)
    // 0=KDFx, 1=KDF1, 2=KDF2, 3=KDF3
    if V = fvDc50Wip then
      KdfVersion := ReadByte
    else
      KdfVersion := 0; // KDFx

    // 7.6 HMAC Key (only version 4+)
    if V = fvDc50Wip then
      HMacKey := ReadRaw(ReadLong);

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
    try
      if KDFVersion = 0 then
        Key := TDECHashExtended(ahash).KDFx(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = 1 then
        Key := TDECHashExtended(ahash).KDF1(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = 2 then
        Key := TDECHashExtended(ahash).KDF2(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = 3 then
        Key := TDECHashExtended(ahash).KDF3(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
      else
        raise Exception.Create('Invalid KDF version');
    finally
      ahash.Done;
    end;
    Cipher.Init(Key, nil, $FF);
    try
      TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc);
    finally
      Cipher.Done;
    end;

    // Decrypt filename (version 3 only)
    if V = fvDc41FinalCancelled then
    begin
      ahash.Init;
      try
        if FileNameUserPasswordEncrypted then
          FilenamePassword := APassword
        else
          FilenamePassword := RawByteString(#$5E#$D1#$6B#$12#$7D#$B4#$C4#$3C);
        if KDFVersion = 0 then
          Key := TDECHashExtended(ahash).KDFx(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = 1 then
          Key := TDECHashExtended(ahash).KDF1(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = 2 then
          Key := TDECHashExtended(ahash).KDF2(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = 3 then
          Key := TDECHashExtended(ahash).KDF3(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else
          Assert(False);
      finally
        ahash.Done;
      end;
      Cipher.Init(Key, nil, $FF);
      try
        OrigName := TDECFormattedCipher(Cipher).DecodeStringToString(OrigName);
      finally
        Cipher.Done;
      end;
    end;

    // 10. Checksum (version 1-3 hash, version 4+ hmac)
    tempstream.position := 0;
    ahash.Init;
    try
      if V = fvDc40 then
      begin
        TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
        HashResult2 := Convert(HashResult);
      end
      else if V = fvDc41Beta then
      begin
        TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
        HashResult2 := TDECHashExtended(ahash).CalcString(Convert(HashResult)+Seed+APassword, TFormat_Copy);
      end
      else if V = fvDc41FinalCancelled then
      begin
        TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
        HashResult2 := TDECHashExtended(ahash).CalcString(
          Convert(HashResult) + Seed +
              TDECHashExtended(ahash).CalcString(
                Seed+TDECHashExtended(ahash).CalcString(Seed+APassword, TFormat_Copy)
              , TFormat_Copy)
        , TFormat_Copy);
      end
      else if V = fvDc50Wip then
      begin
        HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(BytesOf(HMacKey), tempstream, tempstream.size, OnProgressProc));
      end
      else
        Assert(False);
    finally
      ahash.Done;
    end;
    if readraw(ahash.DigestSize) <> HashResult2 then
      raise Exception.Create('Hash mismatch');

    // 11. Terminus (only version 2+)
    if (FileTerminus <> '') and (ReadRaw(Length(FileTerminus)) <> FileTerminus) then
      raise Exception.Create('File terminus wrong');

  finally
    if Assigned(Source) then FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
    if Assigned(Cipher) then FreeAndNil(Cipher);
    if Assigned(ahash) then FreeAndNil(ahash);
  end;
end;

procedure DeCoder4X_Decompress(InputFileName, OutputFileName: string);
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
        FreeAndNil(DecompressionStream);
      end;
    finally
      FreeAndNil(CompressOutputStream);
    end;
  finally
    FreeAndNil(CompressInputStream);
  end;
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  DeCoder4X_DecodeFile('schloss.dc4', 'schloss.tmp', 'test');
  DeCoder4X_Decompress('schloss.tmp', 'schloss_decoded.bmp');
  DeleteFile('schloss.tmp');
  ShowMessage('ok');
end;

{ TDECHashExtendedAuthentication }

class function TDECHashExtendedAuthentication.HMACStream(const Key: TBytes;
  const Stream: TStream; Size: Int64;
  const OnProgress: TDECProgressEvent): TBytes;
const
  CONST_UINT_OF_0x36 = $3636363636363636;
  CONST_UINT_OF_0x5C = $5C5C5C5C5C5C5C5C;
var
  HashInstance: TDECHashAuthentication;
  InnerKeyPad, OuterKeyPad: array of Byte;
  I, KeyLength, BlockSize, DigestLength: Integer;
begin
  // Taken from TDECHashAuthentication.HMAC and changed HashInstance.Calc to HashInstance.CalcStream for the message
  HashInstance := TDECHashAuthenticationClass(self).Create;
  try
    BlockSize    := HashInstance.BlockSize; // 64 for sha1, ...
    DigestLength := HashInstance.DigestSize;
    KeyLength    := Length(Key);

    SetLength(InnerKeyPad, BlockSize);
    SetLength(OuterKeyPad, BlockSize);

    I := 0;

    if KeyLength > BlockSize then
    begin
      Result    := HashInstance.CalcBytes(Key);
      KeyLength := DigestLength;
    end
    else
      Result := Key;

    while I <= KeyLength - SizeOf(NativeUInt) do
    begin
      PNativeUInt(@InnerKeyPad[I])^ := PNativeUInt(@Result[I])^ xor NativeUInt(CONST_UINT_OF_0x36);
      PNativeUInt(@OuterKeyPad[I])^ := PNativeUInt(@Result[I])^ xor NativeUInt(CONST_UINT_OF_0x5C);
      Inc(I, SizeOf(NativeUInt));
    end;

    while I < KeyLength do
    begin
      InnerKeyPad[I] := Result[I] xor $36;
      OuterKeyPad[I] := Result[I] xor $5C;
      Inc(I);
    end;

    while I <= BlockSize - SizeOf(NativeUInt) do
    begin
      PNativeUInt(@InnerKeyPad[I])^ := NativeUInt(CONST_UINT_OF_0x36);
      PNativeUInt(@OuterKeyPad[I])^ := NativeUInt(CONST_UINT_OF_0x5C);
      Inc(I, SizeOf(NativeUInt));
    end;

    while I < BlockSize do
    begin
      InnerKeyPad[I] := $36;
      OuterKeyPad[I] := $5C;
      Inc(I);
    end;

    HashInstance.Init;
    HashInstance.Calc(InnerKeyPad[0], BlockSize);
    if (Stream.Size - Stream.Position) > 0 then
      TDECHashExtended(HashInstance).CalcStream(Stream, Stream.Size-Stream.Position, OnProgress, false);
    HashInstance.Done;
    Result := HashInstance.DigestAsBytes;

    HashInstance.Init;
    HashInstance.Calc(OuterKeyPad[0], BlockSize);
    HashInstance.Calc(Result[0], DigestLength);
    HashInstance.Done;

    Result := HashInstance.DigestAsBytes;
  finally
    HashInstance.Free;
  end;
end;

end.
