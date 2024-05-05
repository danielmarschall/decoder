unit DecoderEncDec;

interface

uses
  Windows, Dialogs, SysUtils, Classes, DECFormatBase, DECTypes,
  System.UITypes, DECCiphers, DECCipherBase, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECCipherFormats, ZLib,
  EncdDecd, System.NetEncoding, DECCRC, DECBaseClass, Generics.Collections,
  DECRandom;

type
  TDcFormatVersion = (fvHagenReddmannExample, fvDc40, fvDc41Beta, fvDc41FinalCancelled, fvDc50Wip);

  TKdfVersion = (kvUnknown, kvKdf1, kvKdf2, kvKdf3, kvKdfx, kvPbkdf2);

  TDC4FileInfo = record
    Dc4FormatVersion: TDcFormatVersion;
    IsZLibCompressed: boolean;
    IsCompressedFolder: boolean;
    OrigFileName: string;
    KDF: TKdfVersion;
    KDF_Iterations: Integer;
    IVSize: integer;
    SeedSize: integer;
    HashClass: TDECHashClass;
    CipherClass: TDECCipherClass;
    CipherMode: TCipherMode;
    FillMode: TBlockFillMode;
  end;

const
  DC4_SUBFORMAT_VERSION: array[Low(TDcFormatVersion)..High(TDcFormatVersion)] of string = (
    'Hagen Reddmann Example File',
    '(De)Coder 4.0',
    '(De)Coder 4.1 Beta',
    '(De)Coder 4.1 Final (Cancelled)',
    '(De)Coder 5.0 WIP'
  );

  INTEGRITY_CHECK_INFO: array[Low(TDcFormatVersion)..High(TDcFormatVersion)] of string = (
    'CalcMac',
    'Hash of source data',
    'Nested Hash of source data with password',
    'Nested Hash of source data with password',
    'Encrypt-then-HMAC'
  );

  KDF_VERSION_NAMES: array[Low(TKdfVersion)..High(TKdfVersion)] of string = (
    'Unknown', 'KDF1', 'KDF2', 'KDF3', 'KDFx', 'PBKDF2'
  );

  CIPHER_MODE_NAMES: array[Low(TCipherMode)..High(TCipherMode)] of string = (
    'CTSx = double CBC, with CFS8 padding of truncated final block',
    'CBCx = Cipher Block Chaining, with CFB8 padding of truncated final block',
    'CFB8 = 8bit Cipher Feedback mode',
    'CFBx = CFB on Blocksize of Cipher',
    'OFB8 = 8bit Output Feedback mode',
    'OFBx = OFB on Blocksize bytes',
    'CFS8 = 8Bit CFS, double CFB',
    'CFSx = CFS on Blocksize bytes',
    'ECBx = Electronic Code Book',
    'GCM  = Galois Counter Mode'
  );

  CIPHER_FILLMODE_NAMES: array[Low(TBlockFillMode)..High(TBlockFillMode)] of string = (
    'Bytes'
  );

procedure DeCoder4X_EncodeFile_Ver4(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil);
function DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; const OnlyReadFileInfo: boolean=false; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

implementation

type
  // https://github.com/MHumm/DelphiEncryptionCompendium/issues/62
  TDECHashExtendedAuthentication = class helper for TDECHashAuthentication
    class function HMACFile(const Key: TBytes; const FileName: string;
      const OnProgress:TDECProgressEvent = nil): TBytes;
    class function HMACStream(const Key: TBytes; const Stream: TStream; Size: Int64;
      const OnProgress:TDECProgressEvent): TBytes;
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

function DEC51_HashById(IdentityBase, Identity: Int64; NoException: boolean=false): TDECHashClass;
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
  if NoException then
    result := nil
  else
    raise Exception.CreateFmt('Hash ID %d with base %d not found', [Identity, IdentityBase]);
end;

function DEC51_CipherById(IdentityBase, Identity: Int64; NoException: boolean=false): TDECCipherClass;
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
  if NoException then
    result := nil
  else
    raise Exception.CreateFmt('Cipher ID %d with base %d not found', [Identity, IdentityBase]);
end;

const
  DC4_ID_BASES: array[Low(TDcFormatVersion)..High(TDcFormatVersion)] of Int64 = (
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

function DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; const OnlyReadFileInfo: boolean=false; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
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
  KdfVersion: TKdfVersion;
  HMacKey: TBytes;
  IV: TBytes;
  Filler: Byte;
  CipherClass: TDECCipherClass;
  HashClass: TDECHashClass;
  bakSourcePosEncryptedData: Int64;
  IsCompressed: boolean;
  IsFolder: boolean;
  ATempFileName: string;
  KdfIterations: Long;
  OrigNameEncrypted: RawByteString;
  iBlockFillMode: Byte;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  tempstream := nil;
  cipher := nil;
  ahash := nil;
  IsCompressed := false;
  IsFolder := false;
  try
    try
      // Is it the Hagen Reddmann example file format?
      CipherClass := DEC51_CipherById(DC4_ID_BASES[fvHagenReddmannExample], ReadLong, true);
      if not Assigned(CipherClass) then
        Source.Position := 0;

      {$REGION '1. Flags (version 1+)'}
      if not Assigned(CipherClass) then
      begin
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
      end
      else
      begin
        IsCompressed := false;
        IsFolder := false;
      end;
      {$ENDREGION}

      {$REGION 'Create output stream'}
      if IsCompressed then
      begin
        ATempFileName := ChangeFileExt(AFileName, '.dc5_tmp');
        if not OnlyReadFileInfo then
          tempstream := TFileStream.Create(ATempFileName, fmOpenReadWrite or fmCreate);
      end
      else
      begin
        if not OnlyReadFileInfo then
          tempstream := TFileStream.Create(AOutput, fmOpenReadWrite or fmCreate);
      end;
      if not OnlyReadFileInfo then
        tempstream.Size := 0;
      {$ENDREGION}

      {$REGION '2. Version (version 1+)'}
      if not Assigned(CipherClass) then
      begin
        // 01 = (De)Coder 4.0
        // 02 = (De)Coder 4.1 Beta
        // 03 = (De)Coder 4.1 Final Cancelled (never released)
        // 04 = (De)Coder 5.0 WorkInProgress
        V := TDcFormatVersion(ReadByte); // if too big, it will automatically be set to fvHagenReddmannExample
        if V = fvHagenReddmannExample then raise Exception.Create('DC Unsupported version');
      end
      else
      begin
        V := fvHagenReddmannExample;
      end;
      {$ENDREGION}

      {$REGION 'Device about magic sequence / file terminus'}
      // Note in re Hagen Redmann example: Now, V can be used to check it
      if (V = fvHagenReddmannExample) or (V = fvDc40) then
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
      {$ENDREGION}

      {$REGION '2.1 Magic Sequence (only version 4)'}
      if MagicSeq <> '' then
      begin
        if ReadRaw(Length(MagicSeq)) <> MagicSeq then
          raise Exception.Create('Invalid magic sequence');
      end;
      {$ENDREGION}

      {$REGION '3. Filename (version 1+)'}
      FileNameUserPasswordEncrypted := false;
      if V <> fvHagenReddmannExample then
      begin
        // Ver1: Clear text filename, terminated with "?"
        // Ver2: Base64 encoded filename, terminated with "?"
        // Ver3: Encrypted filename
        // Ver4: Clear text filename, with length byte in front of it
        OrigName := '';
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
          OrigNameEncrypted := ReadRaw(ReadLong); // will be decrypted below (after we initialized hash/cipher)
        end
        else if V = fvDc50Wip then
        begin
          // Possible values:
          // - Original name in its entirety (example "foobar.txt")
          // - Just its extension (example "*.txt")
          // - Redacted (empty string "")
          OrigName := UTF8ToString(ReadRaw(ReadByte));
        end
        else
          Assert(False);
      end
      else
      begin
        OrigName := '';
      end;
      {$ENDREGION}

      {$REGION '4. IdBase (only version 2+)'}
      if (V = fvHagenReddmannExample) or (V = fvDc40) then
        idBase := DC4_ID_BASES[V] // hardcoded
      else
        idBase := ReadLong;
      {$ENDREGION}

      {$REGION '5. Cipher identity (only version 0 or 2+)'}
      if V <> fvHagenReddmannExample then // If V=fvHagenReddmannExample, then we already checked it and the stream position should be 4.
      begin
        if V = fvDc40 then
          CipherClass := TCipher_AES
        else
          CipherClass := DEC51_CipherById(idBase, ReadLong);
      end;
      if (V <> fvDc50Wip) and (CipherClass = TCipher_SCOP) then Cipherclass := TCipher_SCOP_DEC52; // unclear if it was faulty in DEC 5.2 or DEC 5.1c
      if (V <> fvDc50Wip) and (CipherClass = TCipher_XTEA) then Cipherclass := TCipher_XTEA_DEC52; // XTEA was not existing in DEC 5.1c, so it must be a DEC 5.2 problem only
      if (V <> fvDc50Wip) and (CipherClass = TCipher_Shark) then Cipherclass := TCipher_Shark_DEC52; // It didn't work in DEC 5.1c
      Cipher := CipherClass.Create;
      {$ENDREGION}

      {$REGION '6. Cipher mode (only version 0 or 2+)'}
      if V = fvDc40 then
        Cipher.Mode := TCipherMode.cmCTSx
      else
        Cipher.Mode := TCipherMode(ReadByte);
      {$ENDREGION}

      {$REGION '7. Hash identity (only version 0 or version 2+)'}
      if V = fvDc40 then
        HashClass := THash_SHA512
      else
        HashClass := DEC51_HashById(idBase, ReadLong);
      AHash := HashClass.Create;
      {$ENDREGION}

      {$REGION '7.5 IV (only version 4+)'}
      if V = fvDc50Wip then
        IV := BytesOf(ReadRaw(ReadByte))
      else
        SetLength(IV, 0);
      {$ENDREGION}

      {$REGION '7.6 Cipher block filling mode (only version 4+)'}
      if V = fvDc50Wip then
      begin
        iBlockFillMode := ReadByte;
        if integer(iBlockFillMode) > Ord(High(TBlockFillMode)) then
          raise Exception.Create('Invalid block filling mode');
        Cipher.FillMode := TBlockFillMode(iBlockFillMode);
      end
      else
      begin
        Cipher.FillMode := TBlockFillMode.fmByte;
      end;
      {$ENDREGION}

      {$REGION '7.7 Last-Block-Filler (only version 4+)'}
      if V = fvDc50Wip then
        Filler := ReadByte
      else
        Filler := $FF;
      {$ENDREGION}

      {$REGION '8. Seed (only version 0 or version 2+)'}
      if V = fvDc40 then
        Seed := ReadRaw(16)
      else
        Seed := ReadRaw(ReadByte);
      {$ENDREGION}

      {$REGION '8.5 KDF version (only version 4+)'}
      // 1=KDF1, 2=KDF2, 3=KDF3, 4=KDFx, 5=PBKDF2
      // For PBKDF2, a DWORD with the iterations follows
      if V = fvDc50Wip then
        KdfVersion := TKdfVersion(ReadByte)
      else
        KdfVersion := kvKdfx;
      if KDFVersion = kvUnknown then {this will also be set if the value is too big}
        raise Exception.Create('Invalid KDF version');
      {$ENDREGION}

      {$REGION '8.6 KDF Iterations (ONLY PRESENT for PBKDF2)'}
      if KDFVersion = kvPbkdf2 then
        KdfIterations := ReadLong
      else
        KdfIterations := 0;
      {$ENDREGION}

      {$REGION 'Generate key used by HMAC and Cipher'}
      if not OnlyReadFileInfo then
      begin
        (* TODO:
              Not implemented for version 3 (actually, I don't understand this description anymore):
                    The "special-checksum" of a file can be used as the user password.
                    The formula is:
                       User-Password = Hash(File-Contents)
                    Combined formula:
                       Encryption-Password = Hash->KDfx(Hash(File-Contents), Seed)
              What I don't understand: How should the program know if the user password or the "hash" password is used??
        *)
        if KDFVersion = kvKdfx then
          Key := TDECHashExtended(ahash).KDFx(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvKdf1 then
          Key := TDECHashExtended(ahash).KDF1(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvKdf2 then
          Key := TDECHashExtended(ahash).KDF2(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvKdf3 then
          Key := TDECHashExtended(ahash).KDF3(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvPbkdf2 then
          Key := TDECHashExtended(ahash).PBKDF2(BytesOf(APassword), BytesOf(Seed), KdfIterations, Cipher.Context.KeySize)
        else
          Assert(False);

        HMacKey := Key;
      end;
      {$ENDREGION}

      {$REGION 'Verify HMAC of whole file before decrypting (version 4+)'}
      if not OnlyReadFileInfo and (V = fvDc50Wip) then
      begin
        bakSourcePosEncryptedData := Source.Position;
        Source.Position := 0;
        HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(HMacKey, Source, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc));
        Source.Position := Source.Size - ahash.DigestSize - Length(FileTerminus);
        if ReadRaw(ahash.DigestSize) <> HashResult2 then
          raise Exception.Create('HMAC mismatch');
        Source.Position := bakSourcePosEncryptedData;
      end;
      {$ENDREGION}

      {$REGION '9. Encrypted data'}
      if not OnlyReadFileInfo then
      begin
        Cipher.Init(Key, IV, Filler);
        try
          if V = fvHagenReddmannExample then
            TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, ReadLong, OnProgressProc)
          else
            TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc);
        finally
          Cipher.Done;
        end;
      end;
      {$ENDREGION}

      {$REGION 'Decrypt filename (version 3 only)'}
      if V = fvDc41FinalCancelled then
      begin
        if not FileNameUserPasswordEncrypted then
        begin
          FilenamePassword := RawByteString(#$5E#$D1#$6B#$12#$7D#$B4#$C4#$3C);
          if KDFVersion = kvKdfx then
            Key := TDECHashExtended(ahash).KDFx(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvKdf1 then
            Key := TDECHashExtended(ahash).KDF1(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvKdf2 then
            Key := TDECHashExtended(ahash).KDF2(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvKdf3 then
            Key := TDECHashExtended(ahash).KDF3(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvPbkdf2 then
            Key := TDECHashExtended(ahash).PBKDF2(BytesOf(FilenamePassword), BytesOf(Seed), KdfIterations, Cipher.Context.KeySize)
          else
            Assert(False);
        end;
        Cipher.Init(Key, IV, Filler);
        try
          OrigNameEncrypted := Convert(TDECFormattedCipher(Cipher).DecodeBytes(BytesOf(OrigNameEncrypted)));
          if Length(OrigNameEncrypted) mod 2 <> 0 then OrigNameEncrypted := OrigNameEncrypted + #0; // should not happen, otherwise it is no valid UTF-16!
          OrigName := WideString(PWideString(Pointer(OrigNameEncrypted)));
        finally
          Cipher.Done; // TODO: I don't understand, if Done() processes the last byte/block, it won't affect our string since we already got the result from DecodeRawByteString(). Asked here https://github.com/MHumm/DelphiEncryptionCompendium/issues/63
        end;
      end;
      {$ENDREGION}

      {$REGION '10. Checksum (version 0 on cipher, 1-3 hash on source, version 4+ hmac on encrypted file)'}
      // (For version 4, the HMAC was checked above, before encrypting)
      if not OnlyReadFileInfo and (V <> fvDc50Wip) then
      begin
        if V = fvHagenReddmannExample then
        begin
          HashResult2 := Cipher.CalcMAC;
        end
        else if V = fvDc40 then
        begin
          tempstream.position := 0;
          TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
          HashResult2 := Convert(HashResult);
        end
        else if V = fvDc41Beta then
        begin
          tempstream.position := 0;
          TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
          HashResult2 := TDECHashExtended(ahash).CalcString(Convert(HashResult)+Seed+APassword, TFormat_Copy);
        end
        else if V = fvDc41FinalCancelled then
        begin
          tempstream.position := 0;
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

        if (V= fvHagenReddmannExample) and (ReadRaw(ReadByte)         <> HashResult2) or
           (V<>fvHagenReddmannExample) and (ReadRaw(ahash.DigestSize) <> HashResult2) then
          raise Exception.Create('Hash mismatch');
      end;
      {$ENDREGION}

      {$REGION '11. Terminus (only version 2 and 3)'}
      if FileTerminus <> '' then
      begin
        if OnlyReadFileInfo then Source.Position := Source.Size - Length(FileTerminus);
        if (ReadRaw(Length(FileTerminus)) <> FileTerminus) then
          raise Exception.Create('File terminus wrong');
      end;
      {$ENDREGION}

      {$REGION 'Decompress stuff'}
      if not OnlyReadFileInfo and IsCompressed then
      begin
        FreeAndNil(tempstream);
        DeCoder4X_Decompress(ATempFileName, AOutput);
      end;

      if not OnlyReadFileInfo and IsFolder then
      begin
        // TODO: Extract ZIP (ver1-3) or 7zip (ver4) to folder
        ShowMessage('Note: Decrypting of folders is not possible. The archive was decrypted, but you must unpack it with an external tool');
      end;
      {$ENDREGION}

      ZeroMemory(@result, Sizeof(result));
      result.Dc4FormatVersion := V;
      result.IsZLibCompressed := IsCompressed;
      result.IsCompressedFolder := IsFolder;
      result.OrigFileName := OrigName;
      result.KDF := KdfVersion;
      result.KDF_Iterations := KdfIterations;
      result.IVSize := Length(IV);
      result.SeedSize := Length(Seed);
      result.HashClass := HashClass;
      result.CipherClass := CipherClass;
      result.CipherMode := Cipher.Mode;
      result.FillMode := Cipher.FillMode;
    except
      if Assigned(tempstream) then ProtectStream(tempstream);
      raise;
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
    SameText(ExtractFileExt(AFileName), '.7z') or
    SameText(ExtractFileExt(AFileName), '.rar') or
    SameText(ExtractFileExt(AFileName), '.gz') or
    SameText(ExtractFileExt(AFileName), '.xz') or
    SameText(ExtractFileExt(AFileName), '.mp3') or
    SameText(ExtractFileExt(AFileName), '.mp4') or
    SameText(ExtractFileExt(AFileName), '.png') or
    SameText(ExtractFileExt(AFileName), '.gif') or
    SameText(ExtractFileExt(AFileName), '.jpg') or
    SameText(ExtractFileExt(AFileName), '.jpeg');
end;

procedure DeCoder4X_EncodeFile_Ver4(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil);
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
  KdfVersion: TKdfVersion;
  HMacKey: TBytes;
  IV: TBytes;
  Filler: Byte;
  CipherClass: TDECCipherClass;
  HashClass: TDECHashClass;
  tmp64: Int64;
  IsCompressed: boolean;
  IsFolder: boolean;
  ATempFileName: string;
  KdfIterations: long;
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
    // - Original name in its entirety (example "foobar.txt")
    // - Just its extension (example "*.txt")
    // - Redacted (empty string "")
    OrigName := UTF8Encode(ExtractFileName(AFileName));
    WriteByte(Length(OrigName));
    WriteRaw(OrigName);

    // 4. IdBase (only version 2+)
    idBase := DC4_ID_BASES[fvDc50Wip];
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

    // 7.6 Cipher block filling mode (only version 4+)
    Cipher.FillMode := TBlockFillMode.fmByte;
    WriteByte(Ord(Cipher.FillMode));

    // 7.7 Last-Block-Filler (only version 4+)
    Filler := $FF;
    WriteByte(Filler);

    // 8. Seed
    WriteByte(32);
    Seed := Convert(RandomBytes(32));
    WriteRaw(Seed);

    // 8.5 KDF version (only version 4+)
    // 1=KDF1, 2=KDF2, 3=KDF3, 4=KDFx, 5=PBKDF2
    KdfVersion := kvKdfx;
    WriteByte(Ord(KdfVersion));
    KdfIterations := 0; // only for KdfVersion=kvPbkdf2

    // 9. Encrypted data
    if KDFVersion = kvKdfx then
      Key := TDECHashExtended(ahash).KDFx(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvKdf1 then
      Key := TDECHashExtended(ahash).KDF1(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvKdf2 then
      Key := TDECHashExtended(ahash).KDF2(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvKdf3 then
      Key := TDECHashExtended(ahash).KDF3(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvPbkdf2 then
      Key := TDECHashExtended(ahash).PBKDF2(BytesOf(APassword), BytesOf(Seed), KdfIterations, Cipher.Context.KeySize)
    else
      Assert(False);
    HMacKey := Key;

    Cipher.Init(Key, IV, Filler);
    try
      Source.Position := 0;
      TDECFormattedCipher(Cipher).EncodeStream(Source, tempstream, source.size, OnProgressProc);
    finally
      Cipher.Done;
    end;

    // 10. Checksum (version 0 on cipher, 1-3 hash on source, version 4+ hmac on encrypted file)
    tmp64 := tempstream.Position;
    tempstream.Position := 0;
    HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(HMacKey, tempstream, tmp64, OnProgressProc));
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

function YesNo(b: boolean): string;
begin
  if b then exit('Yes') else exit('No');
end;

procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);
begin
  sl.Add('File Format: (De)Coder 4.x/5.x Encrypted File');
  sl.Add('Sub-Format: ' + DC4_SUBFORMAT_VERSION[fi.Dc4FormatVersion]);
  sl.Add('Is compressed folder: ' + YesNo(fi.IsCompressedFolder));
  sl.Add('Data additionally ZLib-compressed: ' + YesNo(fi.IsZLibCompressed));
  sl.Add('Original filename: ' + fi.OrigFileName);
  sl.Add('Key Derivation Algorithm: ' + KDF_VERSION_NAMES[fi.KDF]);
  if fi.KDF = kvPbkdf2 then
    sl.Add('PBKDF Iterations: ' + IntToStr(fi.KDF_Iterations));
  sl.Add('Hashing Algorithm: ' + StringReplace(fi.HashClass.ClassName, 'THash_', '', []));
  sl.Add('Hash Digest Size: ' + IntToStr(fi.HashClass.DigestSize));
  sl.Add('Hash Block Size: ' + IntToStr(fi.HashClass.BlockSize));
  sl.Add('Hash Seed Size: ' + IntToStr(fi.SeedSize));
  sl.Add('Encryption Algorithm: ' + StringReplace(fi.CipherClass.ClassName, 'TCipher_', '', []));
  sl.Add('Cipher Key Size: ' + IntToStr(fi.CipherClass.Context.KeySize));
  sl.Add('Cipher Block Size: ' + IntToStr(fi.CipherClass.Context.BlockSize));
  sl.Add('Cipher Buffer Size: ' + IntToStr(fi.CipherClass.Context.BufferSize));
  sl.Add('Cipher IV Size: ' + IntToStr(fi.IVSize));
  sl.Add('Cipher Mode: ' + CIPHER_MODE_NAMES[fi.CipherMode]);
  sl.Add('Cipher Block Filling Mode: ' + CIPHER_FILLMODE_NAMES[fi.FillMode]);
  sl.Add('Message Authentication: ' + INTEGRITY_CHECK_INFO[fi.Dc4FormatVersion]);
end;

initialization
  RandomSeed;
end.
