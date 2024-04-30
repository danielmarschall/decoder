unit DecoderMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, DECTypes, DECFormatBase;

type
  TFormMain = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UITypes, DECCiphers, DECCipherBase, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECCipherFormats, ZLib,
  EncdDecd, System.NetEncoding, DECCRC, DECBaseClass, Generics.Collections,
  DECRandom;

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

procedure SecureDeleteFile(AFileName: string);
begin
  // TODO: Implement
  DeleteFile(AFileName);
end;

function DEC51_Identity(IdentityBase: Int64; ClassName: string): Int64;
var
  Signature: AnsiString;
  cn: string;
begin
  cn := ClassName;
  if cn = 'THash_SHA0'{DEC6.0} then cn := 'THash_SHA'{DEC5.1};
  if cn = 'THash_Whirlpool0'{DEC6.0} then cn := 'THash_Whirlpool'{DEC5.1};
  if cn = 'TCipher_AES'{DEC6.0} then cn := 'TCipher_Rijndael'{DEC5.1};
  Signature := AnsiString(StringOfChar(#$5A, 256 - Length(cn)) + AnsiUpperCase(cn));
  Result := CRC32(IdentityBase, Signature[1], Length(Signature));
end;

function DEC51_HashById(IdentityBase, Identity: Int64): TDECHashClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
begin
  for p in TDECHash.ClassList do
  begin
    c := p.Value;
    if (c <> nil) and (Identity = DEC51_Identity(IdentityBase, c.ClassName)) then
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
begin
  for p in TDECCipher.ClassList do
  begin
    c := p.Value;
    if (c <> nil) and (Identity = DEC51_Identity(IdentityBase, c.ClassName)) then
    begin
      result := TDecCipherClass(c);
      exit;
    end;
  end;
  raise Exception.CreateFmt('Cipher ID %d with base %d not found', [Identity, IdentityBase]);
end;

type
  // https://github.com/MHumm/DelphiEncryptionCompendium/issues/62
  TDECHashExtendedAuthentication = class helper for TDECHashAuthentication
    class function HMACFile(const Key: TBytes; const FileName: string;
      const OnProgress:TDECProgressEvent = nil): TBytes;
    class function HMACStream(const Key: TBytes; const Stream: TStream; Size: Int64;
      const OnProgress:TDECProgressEvent): TBytes;
  end;

type
  TDcFormatVersion = (fvUnknown, fvDc40, fvDc41Beta, fvDc41FinalCancelled, fvDc50Wip);

const
  DC4_ID_BASES: array[0..4] of Int64 = (
    $84485225, // Hagen Reddmann Example (no .dc4 files)
    $59178954, // (De)Coder 4.0 (identities not used)
    $84671842, // (De)Coder 4.1 beta
    $19387612, // (De)Coder 4.1 final/cancelled
    $1259d82a  // (De)Coder 5.0 WIP
  );

  // This is the OID { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 37476 products(2) decoder(2) fileformat(1) dc4(4) }
  DC4_OID = '1.3.6.1.4.1.37476.2.2.1.4';

procedure DeCoder4X_Compress(InputFileName, OutputFileName: string);
var
  CompressInputStream: TFileStream;
  CompressOutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
  CompressInputStream:=TFileStream.Create(InputFileName, fmOpenRead);
  try
    CompressOutputStream:=TFileStream.Create(OutputFileName, fmCreate);
    try
      CompressionStream:=TCompressionStream.Create(clMax, CompressOutputStream);
      try
        CompressionStream.CopyFrom(CompressInputStream, CompressInputStream.Size);
      finally
        FreeAndNil(CompressionStream);
      end;
    finally
      FreeAndNil(CompressOutputStream);
    end;
  finally
    FreeAndNil(CompressInputStream);
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

procedure DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString);
var
  Source: TStream;

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

  function ReadRaw(leng: integer): RawByteString;
  begin
    SetLength(Result, leng);
    Read(Result[1], Length(Result));
  end;

  function Convert(const Bytes: TBytes): RawByteString; inline;
  begin
    SetString(Result, PAnsiChar(pointer(Bytes)), length(Bytes));
  end;

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
  MagicSeq, FileTerminus: RawByteString;
  OrigName: string;
  ahash: TDECHash;
  Key: TBytes;
  FileNameUserPasswordEncrypted: boolean;
  FilenamePassword: RawByteString;
  KdfVersion: byte;
  HMacKey: TBytes;
  IV: TBytes;
  Filler: Byte;
  CipherClass: TDECCipherClass;
  HashClass: TDECHashClass;
  bakTempStreamPosEncryptedData: Int64;
  IsCompressed: boolean;
  IsFolder: boolean;
  ATempFileName: string;
  KdfIterations: Long;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  tempstream := nil;
  cipher := nil;
  ahash := nil;
  IsCompressed := false;
  IsFolder := false;
  try
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
    IsFolder := (F and 1) <> 0;
    IsCompressed := (F and 2) <> 0;

    if IsCompressed then
    begin
      ATempFileName := ChangeFileExt(AFileName, '.dc5_tmp');
      tempstream := TFileStream.Create(ATempFileName, fmOpenReadWrite or fmCreate);
    end
    else
    begin
      tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
    end;
    tempstream.Size := 0;

    // 2. Version
    // 01 = (De)Coder 4.0
    // 02 = (De)Coder 4.1 Beta
    // 03 = (De)Coder 4.1 Final Cancelled (never released)
    // 04 = (De)Coder 5.0 WorkInProgress
    V := TDcFormatVersion(ReadByte); // if too big, it will automatically be set to 0
    if V = fvUnknown then raise Exception.Create('DC Unsupported version');

    // We need this later
    if V = fvDc40 then
    begin
      MagicSeq := '';
      FileTerminus := '';
    end
    else if V = fvDc41Beta then
    begin
      MagicSeq := '';
      FileTerminus := 'RENURVJNSU5VUw=='; // (BASE64: "DCTERMINUS")
    end
    else if V = fvDc41FinalCancelled then
    begin
      MagicSeq := '';
      FileTerminus := RawByteString(#$63#$F3#$DF#$89#$B7#$27#$20#$EA);
    end
    else
    begin
      MagicSeq := RawByteString(DC4_OID);
      FileTerminus := '';
    end;

    // 2.1 Magic Sequence (only version 4)
    if MagicSeq <> '' then
    begin
      if ReadRaw(Length(MagicSeq)) <> MagicSeq then
        raise Exception.Create('Invalid magic sequence');
    end;

    // 3. Filename
    // Ver1: Clear text filename, terminated with "?"
    // Ver2: Base64 encoded filename, terminated with "?"
    // Ver3: Encrypted filename
    // Ver4: Clear text filename, with length byte in front of it
    OrigName := '';
    FileNameUserPasswordEncrypted := false;
    if (V = fvDc40) or (V = fvDc41Beta) then
    begin
      ch := ReadRaw(1);
      while ch <> '?' do
      begin
        OrigName := OrigName + string(ch);
        ch := ReadRaw(1);
      end;
      if V = fvDc41Beta then
      begin
        OrigName := string(Convert(DecodeBase64(AnsiString(OrigName))));
      end;
    end
    else if V = fvDc41FinalCancelled then
    begin
      FileNameUserPasswordEncrypted := ReadByte = $01; // Filename encrypted with user-password? (00=No, 01=Yes)
      // Filename encrypted with DEC 5.1c
      // Encryption-Password = Hash->KDfx(User-Password, Seed)
      // if not encrypted with user-password, otherwise:
      // Encryption-Password = Hash->KDfx(5Eh D1h 6Bh 12h 7Dh B4h C4h 3Ch, Seed)
      // TODO: Unsure... will RawByteString correctly casted to WideString (UTF-16), or do we need to do the cast with pointers? (Probably the latter...)
      OrigName := WideString(ReadRaw(ReadLong*SizeOf(WideChar))); // will be decrypted below (after we initialized hash/cipher)
    end
    else if V = fvDc50Wip then
    begin
      // Possible values:
      // - Original name in its entirety (foobar.txt)
      // - Just its extension (*.txt)
      // - Redacted (empty string)
      OrigName := UTF8ToString(ReadRaw(ReadByte));
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
      CipherClass := TCipher_AES
    else
      CipherClass := DEC51_CipherById(idBase, ReadLong);
    if (V <> fvDc50Wip) and (CipherClass = TCipher_SCOP) then Cipherclass := TCipher_SCOP_DEC52; // unclear if it was faulty in DEC 5.2 or DEC 5.1c
    if (V <> fvDc50Wip) and (CipherClass = TCipher_XTEA) then Cipherclass := TCipher_XTEA_DEC52; // XTEA was not existing in DEC 5.1c, so it must be a DEC 5.2 problem only
    if (V <> fvDc50Wip) and (CipherClass = TCipher_Shark) then Cipherclass := TCipher_Shark_DEC52; // It didn't work in DEC 5.1c
    Cipher := CipherClass.Create;

    // 6. Cipher mode (only version 2+)
    if V = fvDc40 then
      Cipher.Mode := TCipherMode.cmCTSx
    else
      Cipher.Mode := TCipherMode(ReadByte);

    // 7. Hash identity (only version 2+)
    if V = fvDc40 then
      HashClass := THash_SHA512
    else
      HashClass := DEC51_HashById(idBase, ReadLong);
    AHash := HashClass.Create;

    // 7.5 IV (only version 4+)
    if V = fvDc50Wip then
      IV := BytesOf(ReadRaw(ReadByte));

    // 7.6 Last-Block-Filler (only version 4+)
    if V = fvDc50Wip then
      Filler := ReadByte
    else
      Filler := $FF;

    // 8. Seed
    if V = fvDc40 then
      Seed := ReadRaw(16)
    else
      Seed := ReadRaw(ReadByte);

    // 8.5 KDF version (only version 4+)
    // 0=KDFx, 1=KDF1, 2=KDF2, 3=KDF3, 4=PBKDF2
    // For PBKDF2, a DWORD with the iterations follows
    if V = fvDc50Wip then
      KdfVersion := ReadByte
    else
      KdfVersion := 0; // KDFx
    if KDFVersion = 4 then
      KdfIterations := ReadLong
    else
      KdfIterations := 0;

    // 9. Encrypted data
    (* TODO:
          Not implemented for version 3 (actually, I don't understand this description anymore):
                The "special-checksum" of a file can be used as the user password.
                The formula is:
                   User-Password = Hash(File-Contents)
                Combined formula:
                   Encryption-Password = Hash->KDfx(Hash(File-Contents), Seed)
          What I don't understand: How should the program know if the user password or the "hash" password is used??
    *)
    if KDFVersion = 0 then
      Key := TDECHashExtended(ahash).KDFx(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = 1 then
      Key := TDECHashExtended(ahash).KDF1(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = 2 then
      Key := TDECHashExtended(ahash).KDF2(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = 3 then
      Key := TDECHashExtended(ahash).KDF3(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = 4 then
      Key := TDECHashExtended(ahash).PBKDF2(BytesOf(APassword), BytesOf(Seed), KdfIterations, Cipher.Context.KeySize)
    else
      raise Exception.Create('Invalid KDF version');

    HMacKey := Key;

    // Verify HMAC before decrypting (the HMAC located below)
    if V = fvDc50Wip then
    begin
      bakTempStreamPosEncryptedData := Source.Position;
      HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(HMacKey, Source, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc));
      Source.Position := Source.Size - ahash.DigestSize - Length(FileTerminus);
      if ReadRaw(ahash.DigestSize) <> HashResult2 then
        raise Exception.Create('HMAC mismatch');
      Source.Position := bakTempStreamPosEncryptedData;
    end;

    Cipher.Init(Key, IV, Filler);
    try
      TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc);
    finally
      Cipher.Done;
    end;

    // Decrypt filename (version 3 only)
    if V = fvDc41FinalCancelled then
    begin
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
      else if KDFVersion = 4 then
        Key := TDECHashExtended(ahash).PBKDF2(BytesOf(FilenamePassword), BytesOf(Seed), KdfIterations, Cipher.Context.KeySize)
      else
        Assert(False);
      Cipher.Init(Key, IV, Filler);
      try
        OrigName := TDECFormattedCipher(Cipher).DecodeStringToString(OrigName);
      finally
        Cipher.Done;
      end;
    end;

    // 10. Checksum (version 1-3 hash on source, version 4+ hmac on ciphertext)
    // (For version 4, the HMAC was checked above, before encrypting)
    if V <> fvDc50Wip then
    begin
      tempstream.position := 0;
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
      else
        Assert(False);
      if ReadRaw(ahash.DigestSize) <> HashResult2 then
        raise Exception.Create('Hash mismatch');
    end;

    // 11. Terminus (only version 2 and 3)
    if (FileTerminus <> '') and (ReadRaw(Length(FileTerminus)) <> FileTerminus) then
      raise Exception.Create('File terminus wrong');

    if IsCompressed then
    begin
      FreeAndNil(tempstream);
      DeCoder4X_Decompress(ATempFileName, AOutput);
    end;

    if IsFolder then
    begin
      // TODO: Extract ZIP (ver1-3) or 7zip (ver4) to folder
      ShowMessage('Note: Decrypting of folders is not possible. The archive was decrypted, but you must unpack it with an external tool');
    end;

  finally
    if Assigned(Source) then FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
    if Assigned(Cipher) then FreeAndNil(Cipher);
    if Assigned(ahash) then FreeAndNil(ahash);
    if IsCompressed and (ATempFileName<>'') then
    begin
      SecureDeleteFile(ATempFileName);
    end;
  end;
end;

function IsCompressedFileType(AFileName: string): boolean;
begin
  result :=
    SameText(ExtractFileExt(AFileName), '.zip') or
    SameText(ExtractFileExt(AFileName), '.mp3') or
    SameText(ExtractFileExt(AFileName), '.png') or
    SameText(ExtractFileExt(AFileName), '.jpg') or
    SameText(ExtractFileExt(AFileName), '.jpeg');
end;

procedure DeCoder4X_EncodeFile_Ver4(const AFileName, AOutput: String; const APassword: RawByteString);
var
  tempstream: TStream;

  procedure Write(var Value; Size: Integer);
  begin
    tempstream.WriteBuffer(Value, Size);
  end;

  procedure WriteByte(b: Byte);
  begin
    Write(b, SizeOf(b));
  end;

  procedure WriteLong(lw: LongWord);
  begin
    lw := lw shl 24 or lw shr 24 or lw shl 8 and $00FF0000 or lw shr 8 and $0000FF00;
    Write(lw, SizeOf(lw));
  end;

  procedure WriteRaw(rb: RawByteString);
  begin
    Write(rb[1], Length(rb));
  end;

  function Convert(const Bytes: TBytes): RawByteString; inline;
  begin
    SetString(Result, PAnsiChar(pointer(Bytes)), length(Bytes));
  end;

var
  F: byte;
  Cipher: TDECCipher;
  Seed: RawByteString;
  Source: TFileStream;
  HashResult2: RawByteString;
  idBase: Int64;
  OrigName: RawByteString;
  ahash: TDECHash;
  Key: TBytes;
  KdfVersion: byte;
  HMacKey: TBytes;
  IV: TBytes;
  Filler: Byte;
  CipherClass: TDECCipherClass;
  HashClass: TDECHashClass;
  bakTempStreamPosEncryptedData: Int64;
  tmp64: Int64;
  IsCompressed: boolean;
  IsFolder: boolean;
  ATempFileName: string;
begin
  tempstream := nil;
  Source := nil;
  cipher := nil;
  ahash := nil;
  IsCompressed := false;
  IsFolder := false;
  try
    IsFolder := DirectoryExists(AFileName);
    if IsFolder then
    begin
      // TODO: Implement Zipping
      // For ver1-3: ZIP (attention: 4 GiB limitation!)
      // For ver4: 7zip
      raise Exception.Create('Encryption of folders is not supported. Please pack the file contents using an external tool.');
    end;

    IsCompressed := not IsCompressedFileType(AFileName);
    if IsCompressed then
    begin
      ATempFileName := ChangeFileExt(AFileName, '.dc5_tmp');
      DeCoder4X_Compress(AFileName, ATempFileName);
      Source := TFileStream.Create(ATempFileName, fmOpenRead or fmShareDenyNone);
    end
    else
    begin
      Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    end;

    tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
    tempstream.Size := 0;

    // 1. Flags
    // Bit 0:    [Ver1+] Is ZIP compressed folder (1) or a regular file (0)?
    // Bit 1:    [Ver2+] Additionally ZLib compressed (1) or not ZLib compressed (0)?
    // Bit 2:    Reserved
    // Bit 3:    Reserved
    // Bit 4:    Reserved
    // Bit 5:    Reserved
    // Bit 6:    Reserved
    // Bit 7:    Reserved
    F := 0;
    if IsFolder then F := F + 1;
    if IsCompressed then F := F + 2;
    WriteByte(F);

    // 2. Version
    // 04 = (De)Coder 5.0 WorkInProgress
    WriteByte(Ord(fvDc50Wip));

    // 2.1 Magic Sequence (only version 4)
    WriteRaw(DC4_OID);

    // 3. Filename
    // Ver4: Clear text filename, with length byte in front of it
    // Possible values:
    // - Original name in its entirety (foobar.txt)
    // - Just its extension (*.txt)
    // - Redacted (empty string)
    OrigName := UTF8Encode(ExtractFileName(AFileName));
    WriteByte(Length(OrigName));
    WriteRaw(OrigName);

    // 4. IdBase (only version 2+)
    idBase := DC4_ID_BASES[Ord(fvDc50Wip)];
    WriteLong(idBase);

    // 5. Cipher identity (only version 2+)
    CipherClass := TCipher_AES;
    WriteLong(DEC51_Identity(idBase, CipherClass.ClassName));
    Cipher := CipherClass.Create;

    // 6. Cipher mode (only version 2+)
    Cipher.Mode := TCipherMode.cmCTSx;
    WriteByte(Ord(Cipher.Mode));

    // 7. Hash identity (only version 2+)
    HashClass := THash_SHA3_512;
    WriteLong(DEC51_Identity(idBase, HashClass.ClassName));
    AHash := HashClass.Create;

    // 7.5 IV (only version 4+)
    WriteByte(16);
    IV := RandomBytes(16);
    WriteRaw(Convert(IV));

    // 7.6 Last-Block-Filler (only version 4+)
    Filler := $FF;
    WriteByte(Filler);

    // 8. Seed
    WriteByte(32);
    Seed := Convert(RandomBytes(32));
    WriteRaw(Seed);

    // 8.5 KDF version (only version 4+)
    // 0=KDFx, 1=KDF1, 2=KDF2, 3=KDF3
    KdfVersion := 0; // KDFx
    WriteByte(KdfVersion);

    // 9. Encrypted data
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
    HMacKey := Key;

    Cipher.Init(Key, IV, Filler);
    try
      Source.Position := 0;
      bakTempStreamPosEncryptedData := tempstream.Position;
      TDECFormattedCipher(Cipher).EncodeStream(Source, tempstream, source.size, OnProgressProc);
    finally
      Cipher.Done;
    end;

    // 10. Checksum (version 1-3 hash on source, version 4+ hmac on ciphertext)
    tmp64 := tempstream.Position;
    tempstream.Position := bakTempStreamPosEncryptedData;
    HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(HMacKey, tempstream, tempstream.size-tempstream.Position, OnProgressProc));
    tempstream.Position := tmp64;
    WriteRaw(HashResult2);

  finally
    if Assigned(Source) then FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
    if Assigned(Cipher) then FreeAndNil(Cipher);
    if Assigned(ahash) then FreeAndNil(ahash);
    if IsCompressed and (ATempFileName<>'') then
    begin
      SecureDeleteFile(ATempFileName);
    end;
  end;
end;



procedure TFormMain.Button1Click(Sender: TObject);
begin

  DeleteFile('schloss_decoded.bmp');

  DeCoder4X_DecodeFile('schloss.dc4', 'schloss_decoded.bmp', 'test');
  ShowMessage('ok');

  DeCoder4X_EncodeFile_Ver4('schloss_decoded.bmp', 'schloss.dc5', 'test');
  DeCoder4X_DecodeFile('schloss.dc5', 'schloss_decoded_dc5.bmp', 'test');
  ShowMessage('ok');

end;

{ TDECHashExtendedAuthentication }

class function TDECHashExtendedAuthentication.HMACFile(const Key: TBytes;
  const FileName: string; const OnProgress: TDECProgressEvent): TBytes;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    HMACStream(Key, fs, fs.Size, OnProgress);
  finally
    FreeAndNil(fs);
  end;
end;

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
    if Size > 0 then
      TDECHashExtended(HashInstance).CalcStream(Stream, Size, OnProgress, false);
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

procedure TFormMain.Button2Click(Sender: TObject);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
const
  IdentityBase = $1259D82A; // DC 5.0
begin
  Memo1.Clear;
  for p in TDECHash.ClassList do
  begin
    c := p.Value;
    cn := c.ClassName;
    Memo1.Lines.Add(
      '0x'+IntToHex(DEC51_Identity(IdentityBase, cn), 8) + #9 +
      cn +
      ' (DigestSize: '+IntToStr(TDECHashClass(c).DigestSize) +
      ', BlockSize: '+IntToStr(TDECHashClass(c).BlockSize) + ')'
    );
  end;
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
const
  IdentityBase = $1259D82A; // DC 5.0
begin
  Memo1.Clear;
  for p in TDECCipher.ClassList do
  begin
    c := p.Value;
    cn := c.ClassName;
    Memo1.Lines.Add(
      '0x'+IntToHex(DEC51_Identity(IdentityBase, cn), 8) + #9 +
      cn +
      ' (KeySize: '+IntToStr(TDECCipherClass(c).Context.KeySize) +
      ', BlockSize: '+IntToStr(TDECCipherClass(c).Context.BlockSize) +
      ', BufferSize: '+IntToStr(TDECCipherClass(c).Context.BufferSize) + ')'
    );
  end;
end;

end.
