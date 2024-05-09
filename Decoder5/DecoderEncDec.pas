unit DecoderEncDec;

interface

uses
  Windows, Dialogs, SysUtils, Classes, DECFormatBase, DECTypes,
  System.UITypes, DECCiphers, DECCipherBase, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECCipherFormats, 
  EncdDecd, System.NetEncoding, DECCRC, DECBaseClass, Generics.Collections,
  DECRandom;

type
  TDcFormatVersion = (fvHagenReddmannExample, fvDc40, fvDc41Beta, fvDc41FinalCancelled, fvDc50Wip);

  TKdfVersion = (kvUnknown, kvKdf1, kvKdf2, kvKdf3, kvKdfx, kvPbkdf2);

  TYesNoAuto = (Yes, No, Auto);

  TDC4Parameters = record
    Dc4FormatVersion: TDcFormatVersion;
    ShouldBeZLibCompressed: TYesNoAuto;
    KDF: TKdfVersion;
    PBKDF_Iterations: Integer;
    IVSizeInBytes: integer;
    IVFillByte: byte;
    SeedSize: integer;
    HashClass: TDECHashClass;
    CipherClass: TDECCipherClass;
    CipherMode: TCipherMode;
    BlockFillMode: TBlockFillMode;
    FileNameUserPasswordEncrypted: boolean; // only used in version 3
    GCMAuthTagSizeInBytes: byte;
  end;

  TDC4FileInfo = record
    IsZLibCompressed: boolean;
    IsCompressedFolder: boolean;
    OrigFileName: string;
    Parameters: TDC4Parameters;
  end;

procedure DeCoder10_EncodeFile(const AFileName, AOutput: String; ForceUpperCase: boolean; OnProgressProc: TDECProgressEvent=nil);
procedure DeCoder10_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);

procedure DeCoder20_EncodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);
procedure DeCoder20_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);

procedure DeCoder21_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
procedure DeCoder21_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);

procedure DeCoder22_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
procedure DeCoder22_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);

procedure DeCoder30_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
procedure DeCoder30_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);

procedure DeCoder32_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
procedure DeCoder32_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);

function DeCoder4X_GetDefaultParameters(V: TDcFormatVersion): TDC4Parameters;
procedure DeCoder4X_ValidateParameterBlock(fi: TDC4Parameters);
procedure DeCoder4X_EncodeFile(const AFileName, AOutput: String; const APassword: RawByteString; AParameters: TDC4Parameters; OnProgressProc: TDECProgressEvent=nil);
// Note: If you just want to read the file information without decrypting, then let AOutput be blank
function DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

implementation

uses
  DecoderOldCiphers, DecoderFuncs;

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
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
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

function DEC51_Identity(IdentityBase: Int64; ClassName: string): Int64;
var
  Signature: AnsiString;
  cn: string;
begin
  cn := ClassName;
  if cn = 'THash_SHA0'{DEC6.0} then cn := 'THash_SHA'{DEC5.1};
  if cn = 'THash_Whirlpool0'{DEC6.0} then cn := 'THash_Whirlpool'{DEC5.1};
  if cn = 'TCipher_AES'{DEC6.0} then cn := 'TCipher_Rijndael'{DEC5.1};
  if cn = 'TCipher_Magma' then cn := 'TCipher_Gost'; // TCipher_Magma is an alias for TCipher_Gost
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
    raise Exception.CreateFmt('Hash ID %.8x with base %.8x not found', [Identity, IdentityBase]);
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
    raise Exception.CreateFmt('Cipher ID %.8x with base %.8x not found', [Identity, IdentityBase]);
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

function DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
var
  Source: TFileStream;
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
  IvFillByte: Byte;
  CipherClass: TDECCipherClass;
  HashClass: TDECHashClass;
  bakSourcePosEncryptedData: Int64;
  IsZLibCompressed: boolean;
  IsFolder: boolean;
  ATempFileName: string;
  PbkdfIterations: Long;
  OrigNameEncrypted: RawByteString;
  iBlockFillMode: Byte;
  cMac: RawByteString;
  iTmp: integer;
  OnlyReadFileInfo: boolean;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  tempstream := nil;
  cipher := nil;
  ahash := nil;
  IsZLibCompressed := false;
  IsFolder := false;
  OnlyReadFileInfo := AOutput = '';
  ATempFileName := '';
  try
    try
      // Is it the Hagen Reddmann example file format?
      CipherClass := DEC51_CipherById(DC4_ID_BASES[fvHagenReddmannExample], Source.ReadLong, true);
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
        F := Source.ReadByte;
        IsFolder := (F and 1) <> 0;
        IsZLibCompressed := (F and 2) <> 0;
      end;
      {$ENDREGION}

      {$REGION 'Create output stream'}
      if IsZLibCompressed then
      begin
        if not OnlyReadFileInfo then
        begin
          ATempFileName := ChangeFileExt(AFileName, '.dc5_tmp');
          tempstream := TFileStream.Create(ATempFileName, fmCreate);
        end;
      end
      else
      begin
        if not OnlyReadFileInfo then
          tempstream := TFileStream.Create(AOutput, fmCreate);
      end;
      {$ENDREGION}

      {$REGION '2. Version (version 1+)'}
      if not Assigned(CipherClass) then
      begin
        // 01 = (De)Coder 4.0
        // 02 = (De)Coder 4.1 Beta
        // 03 = (De)Coder 4.1 Final Cancelled (never released)
        // 04 = (De)Coder 5.0 WorkInProgress
        V := TDcFormatVersion(Source.ReadByte); // if too big, it will automatically be set to fvHagenReddmannExample
        if V = fvHagenReddmannExample then raise Exception.Create('DC Unsupported version');
      end
      else
      begin
        V := fvHagenReddmannExample;
      end;
      {$ENDREGION}

      {$REGION 'Decide about magic sequence / file terminus'}
      // Note in re Hagen Redmann example: Now, V can be used to check it
      if (V = fvHagenReddmannExample) or (V = fvDc40) then
      begin
        MagicSeq := '';
        FileTerminus := '';
      end
      else if V = fvDc41Beta then
      begin
        MagicSeq := '';
        FileTerminus := RawByteString(TNetEncoding.Base64.Encode('DCTERMINUS'));
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

      {$REGION '2.1 Magic Sequence (only version 4+)'}
      if MagicSeq <> '' then
      begin
        if Source.ReadRawByteString(Length(MagicSeq)) <> MagicSeq then
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
          ch := Source.ReadRawByteString(1);
          while ch <> '?' do
          begin
            OrigName := OrigName + string(ch);
            ch := Source.ReadRawByteString(1);
          end;
          if V = fvDc41Beta then
          begin
            OrigName := TNetEncoding.Base64.Decode(OrigName);
          end;
        end
        else if V = fvDc41FinalCancelled then
        begin
          FileNameUserPasswordEncrypted := Source.ReadByte = $01; // Filename encrypted with user-password? (00=No, 01=Yes)
          // Filename encrypted with DEC 5.1c
          // Encryption-Password = Hash->KDfx(User-Password, Seed)
          // if not encrypted with user-password, otherwise:
          // Encryption-Password = Hash->KDfx(5Eh D1h 6Bh 12h 7Dh B4h C4h 3Ch, Seed)
          OrigNameEncrypted := Source.ReadRawByteString(Source.ReadLong); // will be decrypted below (after we initialized hash/cipher)
        end
        else if V >= fvDc50Wip then
        begin
          // Possible values:
          // - Original name in its entirety (example "foobar.txt")
          // - Just its extension (example "*.txt")
          // - Redacted (empty string "")
          OrigName := UTF8ToString(Source.ReadRawByteString(Source.ReadByte));
        end;
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
        idBase := Source.ReadLong;
      {$ENDREGION}

      {$REGION '5. Cipher identity (only version 0 or 2+)'}
      if V <> fvHagenReddmannExample then // If V=fvHagenReddmannExample, then we already checked it and the stream position should be 4.
      begin
        if V = fvDc40 then
          CipherClass := TCipher_AES
        else
          CipherClass := DEC51_CipherById(idBase, Source.ReadLong);
      end;
      if (V < fvDc50Wip) and (CipherClass = TCipher_SCOP) then Cipherclass := TCipher_SCOP_DEC52; // unclear if it was faulty in DEC 5.2 or DEC 5.1c
      if (V < fvDc50Wip) and (CipherClass = TCipher_XTEA) then Cipherclass := TCipher_XTEA_DEC52; // XTEA was not existing in DEC 5.1c, so it must be a DEC 5.2 problem only
      if (V < fvDc50Wip) and (CipherClass = TCipher_Shark) then Cipherclass := TCipher_Shark_DEC52; // It didn't work in DEC 5.1c
      Cipher := CipherClass.Create;
      {$ENDREGION}

      {$REGION '6. Cipher mode (only version 0 or 2+)'}
      if V = fvDc40 then
        Cipher.Mode := TCipherMode.cmCTSx
      else
        Cipher.Mode := TCipherMode(Source.ReadByte);
      {$ENDREGION}

      {$REGION '7. Hash identity (only version 0 or version 2+)'}
      if V = fvDc40 then
        HashClass := THash_SHA512
      else
        HashClass := DEC51_HashById(idBase, Source.ReadLong);
      AHash := HashClass.Create;
      {$ENDREGION}

      {$REGION '7.5 IV (only version 4+)'}
      if V >= fvDc50Wip then
        IV := Source.ReadRawBytes(Source.ReadByte)
      else
        SetLength(IV, 0);
      {$ENDREGION}

      {$REGION '7.6 IV Fill Byte (only version 4+)'}
      if V >= fvDc50Wip then
        IvFillByte := Source.ReadByte
      else
        IvFillByte := $FF;
      {$ENDREGION}

      {$REGION '7.7 Cipher block filling mode (only version 4+; currently unused by DEC)'}
      if V >= fvDc50Wip then
      begin
        iBlockFillMode := Source.ReadByte;
        if integer(iBlockFillMode) > Ord(High(TBlockFillMode)) then
          raise Exception.Create('Invalid block filling mode');
        Cipher.FillMode := TBlockFillMode(iBlockFillMode);
      end
      else
      begin
        Cipher.FillMode := TBlockFillMode.fmByte;
      end;
      {$ENDREGION}

      {$REGION '8. Seed (only version 0 or version 2+)'}
      if V = fvDc40 then
        Seed := Source.ReadRawByteString(16)
      else
        Seed := Source.ReadRawByteString(Source.ReadByte);
      {$ENDREGION}

      {$REGION '8.5 KDF version (only version 4+)'}
      // 1=KDF1, 2=KDF2, 3=KDF3, 4=KDFx, 5=PBKDF2
      // For PBKDF2, a DWORD with the iterations follows
      if V >= fvDc50Wip then
        KdfVersion := TKdfVersion(Source.ReadByte)
      else
        KdfVersion := kvKdfx;
      if KDFVersion = kvUnknown then {this will also be set if the value is too big}
        raise Exception.Create('Invalid KDF version');
      {$ENDREGION}

      {$REGION '8.6 KDF Iterations (ONLY PRESENT for PBKDF2)'}
      if KDFVersion = kvPbkdf2 then
        PbkdfIterations := Source.ReadLong
      else
        PbkdfIterations := 0;
      {$ENDREGION}

      {$REGION 'Generate key used by HMAC and Cipher'}
      if not OnlyReadFileInfo then
      begin
        (* TODO: Not implemented for version 3 (actually, I don't understand this description anymore):
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
          Key := TDECHashExtended(ahash).PBKDF2(BytesOf(APassword), BytesOf(Seed), PbkdfIterations, Cipher.Context.KeySize)
        else
          Assert(False);

        HMacKey := Key;
      end;
      {$ENDREGION}

      {$REGION 'Verify HMAC of whole file before decrypting (version 4+)'}
      if not OnlyReadFileInfo and (V >= fvDc50Wip) then
      begin
        bakSourcePosEncryptedData := Source.Position;
        Source.Position := 0;
        HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(HMacKey, Source, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc));
        Source.Position := Source.Size - ahash.DigestSize - Length(FileTerminus);
        if Source.ReadRawByteString(ahash.DigestSize) <> HashResult2 then
          raise Exception.Create('HMAC mismatch');
        Source.Position := bakSourcePosEncryptedData;
      end;
      {$ENDREGION}

      {$REGION '8.7 GCM Tag Length (only version 4+)'}
      if not OnlyReadFileInfo and (V>=fvDc50Wip) and (Cipher.Mode = cmGCM) then
      begin
        TDECFormattedCipher(Cipher).AuthenticationResultBitLength := Source.ReadByte * 8;
      end;
      {$ENDREGION}

      {$REGION '9. Encrypted data (version 0 with length prefix, version 1+ without)'}
      if not OnlyReadFileInfo then
      begin
        Cipher.Init(Key, IV, IvFillByte);
        try
          if V = fvHagenReddmannExample then
          begin
            TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, Source.ReadLong, OnProgressProc);
          end
          else
          begin
            iTmp := source.size - source.Position;
            if ((V=fvHagenReddmannExample) or (V>=fvDc50Wip)) and (Cipher.Mode <> cmECBx) then Dec(iTmp, Cipher.Context.BlockSize{CalcMac}); // field 9.1
            if (V>=fvDc50Wip) and (Cipher.Mode = cmGCM) then Dec(iTmp, TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3); // field 9.2
            Dec(iTmp, ahash.DigestSize{Hash/HMAC}); // field 10
            Dec(iTmp, Length(FileTerminus)); // field 11
            TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, iTmp, OnProgressProc);
          end;
        finally
          Cipher.Done;
        end;
      end;
      {$ENDREGION}

      {$REGION '9.1 DEC CalcMAC (version 0 with length prefix, version 4+ without)'}
      if not OnlyReadFileInfo and ((V=fvHagenReddmannExample) or (V>=fvDc50Wip)) and (Cipher.Mode <> cmECBx) then
      begin
        if V=fvHagenReddmannExample then
          cMac := Source.ReadRawByteString(Source.ReadByte)
        else
          cMac := Source.ReadRawByteString(Cipher.Context.BlockSize);
        if cMac <> Cipher.CalcMAC then
          raise Exception.Create('DEC CalcMAC mismatch');
      end;
      {$ENDREGION}

      {$REGION '9.2 GCM Tag (only version 4+)'}
      if not OnlyReadFileInfo and (V>=fvDc50Wip) and (Cipher.Mode = cmGCM) then
      begin
        if Convert(TDECFormattedCipher(Cipher).CalculatedAuthenticationResult) <> Source.ReadRawByteString(TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3) then
          raise Exception.Create('GCM Auth Tag mismatch');
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
            Key := TDECHashExtended(ahash).PBKDF2(BytesOf(FilenamePassword), BytesOf(Seed), PbkdfIterations, Cipher.Context.KeySize)
          else
            Assert(False);
        end;
        if OnlyReadFileInfo and (Length(Key)=0) then
        begin
          OrigName := '';
        end
        else
        begin
          Cipher.Init(Key, IV, IvFillByte);
          try
            OrigNameEncrypted := Convert(TDECFormattedCipher(Cipher).DecodeBytes(BytesOf(OrigNameEncrypted)));
            if Length(OrigNameEncrypted) mod 2 <> 0 then OrigNameEncrypted := OrigNameEncrypted + #0; // should not happen, otherwise it is no valid UTF-16!
            OrigName := WideString(PWideString(Pointer(OrigNameEncrypted)));
          finally
            Cipher.Done;
          end;
        end;
      end;
      {$ENDREGION}

      {$REGION '10. Hash/HMAC (version 1-3 hash on source, version 4+ hmac on encrypted file)'}
      // (For version 4, the HMAC was checked above, before encrypting, so we exclude the check here)
      if not OnlyReadFileInfo and (V >= fvDc40) and (V < fvDc50Wip) then
      begin
        if V = fvDc40 then
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

        if Source.ReadRawByteString(ahash.DigestSize) <> HashResult2 then
        begin
          if V >= fvDc50Wip then
            raise Exception.Create('HMAC mismatch')
          else
            raise Exception.Create('Hash mismatch');
        end;
      end;
      {$ENDREGION}

      {$REGION '11. Terminus (only version 2 and 3)'}
      if FileTerminus <> '' then
      begin
        if OnlyReadFileInfo then Source.Position := Source.Size - Length(FileTerminus);
        if (Source.ReadRawByteString(Length(FileTerminus)) <> FileTerminus) then
          raise Exception.Create('File terminus wrong');
      end;
      {$ENDREGION}

      {$REGION 'Decompress stuff'}
      if not OnlyReadFileInfo and IsZLibCompressed then
      begin
        FreeAndNil(tempstream);
        ZLib_Decompress(ATempFileName, AOutput, OnProgressProc);
      end;

      if not OnlyReadFileInfo and IsFolder then
      begin
        // TODO: Implement unzipping (ver1-3) or 7zip (ver4) to folder
        ShowMessage('Note: Decrypting of folders is not possible. The archive was decrypted, but you must unpack it with an external tool');
      end;
      {$ENDREGION}

      ZeroMemory(@result, Sizeof(result));
      result.IsZLibCompressed := IsZLibCompressed;
      result.IsCompressedFolder := IsFolder;
      result.OrigFileName := OrigName;
      result.Parameters.Dc4FormatVersion := V;
      if V < fvDc41Beta then
        result.Parameters.ShouldBeZLibCompressed := No
      else
        result.Parameters.ShouldBeZLibCompressed := Auto;
      result.Parameters.KDF := KdfVersion;
      result.Parameters.PBKDF_Iterations := PbkdfIterations;
      result.Parameters.IVSizeInBytes := Length(IV);
      result.Parameters.IVFillByte := IvFillByte;
      result.Parameters.SeedSize := Length(Seed);
      result.Parameters.HashClass := HashClass;
      result.Parameters.CipherClass := CipherClass;
      result.Parameters.CipherMode := Cipher.Mode;
      result.Parameters.BlockFillMode := Cipher.FillMode;
      result.Parameters.FileNameUserPasswordEncrypted := FileNameUserPasswordEncrypted;
      if (V >= fvDc50Wip) and (result.Parameters.CipherMode = cmGCM) then
        result.Parameters.GCMAuthTagSizeInBytes := TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3
      else
        result.Parameters.GCMAuthTagSizeInBytes := 0;
      DeCoder4X_ValidateParameterBlock(result.Parameters); // Should always pass
    except
      if Assigned(tempstream) then ProtectStream(tempstream);
      raise;
    end;
  finally
    if Assigned(Source) then FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
    if Assigned(Cipher) then FreeAndNil(Cipher);
    if Assigned(ahash) then FreeAndNil(ahash);
    if IsZLibCompressed and (ATempFileName<>'') and FileExists(ATempFileName) then
    begin
      SecureDeleteFile(ATempFileName);
    end;
  end;
end;

function DeCoder4X_GetDefaultParameters(V: TDcFormatVersion): TDC4Parameters;
begin
  result.Dc4FormatVersion := V;
  if V < fvDc41Beta then
    result.ShouldBeZLibCompressed := No
  else if V >= fvDc41Beta then
    result.ShouldBeZLibCompressed := Yes
  else
    result.ShouldBeZLibCompressed := Auto;
  result.KDF := kvKdfx;
  result.PBKDF_Iterations := 0; // only for KdfVersion=kvPbkdf2
  if V >= fvDc50Wip then
    result.IVSizeInBytes := 16
  else
    result.IVSizeInBytes := 0;
  result.IVFillByte := $FF;
  if V >= fvDc41Beta then
    result.SeedSize := 32
  else
    result.SeedSize := 16;
  if V = fvHagenReddmannExample then
    result.HashClass := THash_SHA1
  else if V<fvDc50Wip then
    result.HashClass := THash_SHA512
  else
    result.HashClass := THash_SHA3_512;
  result.CipherClass := TCipher_AES;
  result.CipherMode := TCipherMode.cmCTSx;
  result.BlockFillMode := TBlockFillMode.fmByte;
  result.FileNameUserPasswordEncrypted := V = fvDc41FinalCancelled; // only used in version 3
  if (V >= fvDc50Wip) and (result.CipherMode = cmGCM) then
    result.GCMAuthTagSizeInBytes := 128 shr 3
  else
    result.GCMAuthTagSizeInBytes := 0;
end;

procedure DeCoder4X_ValidateParameterBlock(fi: TDC4Parameters);
begin
  //if (fi.Dc4FormatVersion < fvDc40) and (fi.IsCompressedFolder) then
  //  raise Exception.Create('ZIP folder requires DC41beta+ version');

  if (fi.Dc4FormatVersion = fvDc40) and (fi.HashClass <> THash_SHA512) then
    raise Exception.Create('Hash not accepted in DC40 version');
  if (fi.Dc4FormatVersion = fvDc40) and (fi.CipherClass <> TCipher_Rijndael) and (fi.CipherClass <> TCipher_AES) then
     raise Exception.Create('Cipher not accepted in DC40 version');
  if (fi.Dc4FormatVersion = fvDc40) and (fi.KDF <> kvKdfx) then
    raise Exception.Create('KDF version not accepted in DC40 version');
  if (fi.Dc4FormatVersion = fvDc40) and (fi.SeedSize <> 16) then
    raise Exception.Create('Seed size not accepted in DC40 version');
  if (fi.Dc4FormatVersion = fvDc40) and (fi.CipherMode <> cmCTSx) then
    raise Exception.Create('CiperMode not accepted in DC40 version');

  if (fi.Dc4FormatVersion < fvDc41Beta) and (fi.ShouldBeZLibCompressed <> No) then
    raise Exception.Create('ZLib requires DC41beta+ version');

  if (fi.Dc4FormatVersion <> fvDc41FinalCancelled) and fi.FileNameUserPasswordEncrypted then
    raise Exception.Create('EncPwd only accepted in DC41Final version');

  if (fi.Dc4FormatVersion < fvDc50Wip) and (fi.IvFillByte <> $FF) then
    raise Exception.Create('Indiv IvFillByte only accepted format version 4+');
  if (fi.Dc4FormatVersion < fvDc50Wip) and (fi.IVSizeInBytes <> 0) then
    raise Exception.Create('Indiv IV only accepted format version 4+');
  if (fi.Dc4FormatVersion < fvDc50Wip) and (fi.KDF <> kvKdfx) then
    raise Exception.Create('Indiv KDF only accepted format version 4+');

  if (fi.CipherMode <> cmGCM) and (fi.GCMAuthTagSizeInBytes <> 0) then
    raise Exception.Create('GCM Auth Tag only allowed for cipher mode GCM');
  if (fi.CipherMode = cmGCM) and (fi.GCMAuthTagSizeInBytes = 0) then
    raise Exception.Create('GCM Auth Tag required for cipher mode GCM');
  if (fi.CipherMode = cmGCM) and (fi.GCMAuthTagSizeInBytes < 4) then
    raise Exception.Create('GCM Auth Tag too small (choose at least 32 bits = 4 bytes)');
  if (fi.CipherMode = cmGCM) and (fi.GCMAuthTagSizeInBytes > 16) then
    raise Exception.Create('GCM Auth Tag too large (max 128 bits = 16 bytes)'); // This is the output size of CalcGaloisHash

  if (fi.KDF <> kvPbkdf2) and (fi.PBKDF_Iterations <> 0) then
    raise Exception.Create('PBKDF Iterations must be >0');
  if (fi.KDF = kvPbkdf2) and (fi.PBKDF_Iterations = 0) then
    raise Exception.Create('PBKDF Iterations must be =0');
end;

procedure DeCoder4X_EncodeFile(const AFileName, AOutput: String; const APassword: RawByteString; AParameters: TDC4Parameters; OnProgressProc: TDECProgressEvent=nil);
var
  tempstream: TFileStream;
  FileNameKey: TBytes;
  FilenamePassword: RawByteString;
  OrigNameEncrypted: RawByteString;
  HashResult: TBytes;
  FileNameUserPasswordEncrypted: Boolean;
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
  IvFillByte: Byte;
  CipherClass: TDECCipherClass;
  HashClass: TDECHashClass;
  tmp64: Int64;
  IsZLibCompressed: boolean;
  IsFolder: boolean;
  ATempFileName: string;
  PbkdfIterations: long;
  V: TDcFormatVersion;
  GCMAuthTagSizeInBytes: byte;
  tmpWS: WideString;
begin
  DeCoder4X_ValidateParameterBlock(AParameters);

  tempstream := nil;
  Source := nil;
  cipher := nil;
  ahash := nil;
  IsZLibCompressed := false;
  IsFolder := DirectoryExists(AFileName);

  try
    if IsFolder then
    begin
      // TODO: Implement Zipping (ver1-3 zip, ver4: 7zip)
      // For ZIP, do not continue if the file size is 4GB+!!!
      raise Exception.Create('Encryption of folders is not supported. Please pack the file contents using an external tool.');
    end;

    {$REGION 'Transfer parameters from AParameters to the working variables'}
    V := AParameters.Dc4FormatVersion;
    case AParameters.ShouldBeZLibCompressed of
      Yes:   IsZLibCompressed := true;
      No:    IsZLibCompressed := false;

      // Measurement of 21367	files
      // ShanEntropy  AvgComprRatioZLibMax
      // 0.00�0.99		0.99908
      // 1.00�1.99		0.21721
      // 2.00�2.99		0.23161
      // 3.00�3.99		0.20000
      // 4.00�4.99		0.21339
      // 5.00�5.99		0.35107
      // 6.00�6.99		0.55397
      // 7.00�7.49		0.73129
      // 7.50�7.99		0.87920
      Auto:  IsZLibCompressed := not IsCompressedFileType(AFileName)
                                  or (ShannonEntropy(AFileName, OnProgressProc) < 7.5);
    end;
    // AParameters.IsCompressedFolder    (this is just an output and will be ignored as input)
    // AParameters.OrigFileName          (this is just an output and will be ignored as input)
    KdfVersion := AParameters.KDF;
    PbkdfIterations := AParameters.PBKDF_Iterations;
    if AParameters.IVSizeInBytes > 0 then
      IV := RandomBytes(AParameters.IVSizeInBytes)
    else
      SetLength(IV, 0);
    IvFillByte := AParameters.IvFillByte;
    Seed := Convert(RandomBytes(AParameters.SeedSize));

    HashClass := AParameters.HashClass;
    AHash := HashClass.Create;

    CipherClass := AParameters.CipherClass;
    Cipher := CipherClass.Create;
    Cipher.Mode := AParameters.CipherMode;
    Cipher.FillMode := AParameters.BlockFillMode;
    GCMAuthTagSizeInBytes := AParameters.GCMAuthTagSizeInBytes;

    FileNameUserPasswordEncrypted := AParameters.FileNameUserPasswordEncrypted;
    {$ENDREGION}

    {$REGION 'Compress stream if the file type is not already compressed'}
    if IsZLibCompressed and (V>=fvDc40) then
    begin
      ATempFileName := ChangeFileExt(AFileName, '.dc5_tmp');
      ZLib_Compress(AFileName, ATempFileName, OnProgressProc);
      Source := TFileStream.Create(ATempFileName, fmOpenRead or fmShareDenyWrite);
    end
    else
    begin
      Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    end;
    {$ENDREGION}

    tempstream := TFileStream.Create(AOutput, fmCreate);

    {$REGION 'Generate key used by HMAC and Cipher'}
    if KDFVersion = kvKdfx then
      Key := TDECHashExtended(ahash).KDFx(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvKdf1 then
      Key := TDECHashExtended(ahash).KDF1(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvKdf2 then
      Key := TDECHashExtended(ahash).KDF2(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvKdf3 then
      Key := TDECHashExtended(ahash).KDF3(BytesOf(APassword), BytesOf(Seed), Cipher.Context.KeySize)
    else if KDFVersion = kvPbkdf2 then
      Key := TDECHashExtended(ahash).PBKDF2(BytesOf(APassword), BytesOf(Seed), PbkdfIterations, Cipher.Context.KeySize)
    else
      Assert(False);
    HMacKey := Key;
    {$ENDREGION}

    {$REGION '1. Flags (version 1+)'}
    if V >= fvDc40 then
    begin
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
      if IsZLibCompressed then F := F + 2;
      tempstream.WriteByte(F);
    end;
    {$ENDREGION}

    {$REGION '2. Version (version 1+)'}
    if V>=fvDc40 then
      tempstream.WriteByte(Ord(V));
    {$ENDREGION}

    {$REGION '2.1 Magic Sequence (only version 4+)'}
    if (V>=fvDc50Wip) then
      tempstream.WriteRawByteString(DC4_OID);
    {$ENDREGION}

    {$REGION '3. Filename (version 1+)'}
    if V = fvDc40 then
    begin
      // Ver1: Clear text filename, terminated with "?"
      OrigName := RawByteString(ExtractFileName(AFileName)); // ANSI
      tempstream.WriteRawByteString(OrigName + '?');
    end
    else if V = fvDc41Beta then
    begin
      // Ver2: Base64 encoded filename, terminated with "?"
      OrigName := RawByteString(TNetEncoding.Base64.Encode(ExtractFileName(AFileName))); // ANSI
      tempstream.WriteRawByteString(OrigName + '?');
    end
    else if V = fvDc41FinalCancelled then
    begin
      // Ver3: Encrypted filename
      // Filename encrypted with DEC
      // Encryption-Password = Hash->KDfx(User-Password, Seed)
      // if not encrypted with user-password, otherwise:
      // Encryption-Password = Hash->KDfx(5Eh D1h 6Bh 12h 7Dh B4h C4h 3Ch, Seed)
      if FileNameUserPasswordEncrypted then tempstream.WriteByte($01) else tempstream.WriteByte($00);
      tmpWS := WideString(ExtractFileName(AFileName));
      SetLength(OrigName, Length(tmpWS)*SizeOf(WideChar));
      Move(tmpWS[Low(tmpWS)], OrigName[Low(OrigName)], Length(tmpWS)*SizeOf(WideChar));
      {$REGION 'Decide about filename key'}
      if FileNameUserPasswordEncrypted then
      begin
        FileNameKey := Key;
      end
      else
      begin
        FilenamePassword := RawByteString(#$5E#$D1#$6B#$12#$7D#$B4#$C4#$3C);
        if KDFVersion = kvKdfx then
          FileNameKey := TDECHashExtended(ahash).KDFx(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvKdf1 then
          FileNameKey := TDECHashExtended(ahash).KDF1(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvKdf2 then
          FileNameKey := TDECHashExtended(ahash).KDF2(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvKdf3 then
          FileNameKey := TDECHashExtended(ahash).KDF3(BytesOf(FilenamePassword), BytesOf(Seed), Cipher.Context.KeySize)
        else if KDFVersion = kvPbkdf2 then
          FileNameKey := TDECHashExtended(ahash).PBKDF2(BytesOf(FilenamePassword), BytesOf(Seed), PbkdfIterations, Cipher.Context.KeySize)
        else
          Assert(False);
      end;
      {$ENDREGION}
      Cipher.Init(FileNameKey, IV, IvFillByte);
      try
        if Length(OrigName) mod 2 <> 0 then OrigName := OrigName + #0; // should not happen, otherwise it is no valid UTF-16!
        OrigNameEncrypted := Convert(TDECFormattedCipher(Cipher).EncodeBytes(BytesOf(OrigName)));
      finally
        Cipher.Done;
      end;
      tempstream.WriteLong(Length(OrigNameEncrypted));
      tempstream.WriteRawByteString(OrigNameEncrypted);
    end
    else if V >= fvDc50Wip then
    begin
      // Ver4: Clear text filename, with length byte in front of it
      // Possible values:
      // - Original name in its entirety (example "foobar.txt")    <-- we choose this
      // - Just its extension (example "*.txt")
      // - Redacted (empty string "")
      OrigName := UTF8Encode(ExtractFileName(AFileName));
      tempstream.WriteByte(Length(OrigName));
      tempstream.WriteRawByteString(OrigName);
    end;
    {$ENDREGION}

    {$REGION '4. IdBase (only version 2+)'}
    idBase := DC4_ID_BASES[V];
    if V >= fvDc41Beta then tempstream.WriteLong(idBase);
    {$ENDREGION}

    {$REGION '5. Cipher identity (version 0 and 2+)'}
    if V <> fvDc40 then tempstream.WriteLong(DEC51_Identity(idBase, CipherClass.ClassName));
    {$ENDREGION}

    {$REGION '6. Cipher mode (version 0 and 2+)'}
    if V <> fvDc40 then tempstream.WriteByte(Ord(Cipher.Mode));
    {$ENDREGION}

    {$REGION '7. Hash identity (version 0 and 2+)'}
    if V <> fvDc40 then
    begin
      tempstream.WriteLong(DEC51_Identity(idBase, HashClass.ClassName));
    end;
    {$ENDREGION}

    {$REGION '7.5 IV (only version 4+)'}
    if V >= fvDc50Wip then
    begin
      tempstream.WriteByte(Length(IV));
      tempstream.WriteRawBytes(IV);
    end;
    {$ENDREGION}

    {$REGION '7.6 IV Fill Byte (only version 4+)'}
    if V >= fvDc50Wip then
    begin
      tempstream.WriteByte(IvFillByte);
    end;
    {$ENDREGION}

    {$REGION '7.7 Cipher block filling mode (only version 4+; currently unused by DEC)'}
    if V >= fvDc50Wip then
    begin
      tempstream.WriteByte(Ord(Cipher.FillMode));
    end;
    {$ENDREGION}

    {$REGION '8. Seed (only version 0 or version 2+)'}
    if V <> fvDc40 then tempstream.WriteByte(Length(Seed));
    tempstream.WriteRawByteString(Seed);
    {$ENDREGION}

    {$REGION '8.5 KDF version (only version 4+)'}
    if V >= fvDc50Wip then
    begin
      // 1=KDF1, 2=KDF2, 3=KDF3, 4=KDFx, 5=PBKDF2
      tempstream.WriteByte(Ord(KdfVersion));
    end;
    {$ENDREGION}

    {$REGION '8.6 PBKDF Iterations (only for KdfVersion=kvPbkdf2)'}
    if KdfVersion = kvPbkdf2 then tempstream.WriteByte(PbkdfIterations);
    {$ENDREGION}

    {$REGION '8.7 GCM Tag length (only if GCM mode)'}
    if (V>=fvDc50Wip) and (Cipher.Mode=cmGCM) then
    begin
      if GCMAuthTagSizeInBytes > 16 then GCMAuthTagSizeInBytes := 16; // 128 bits this is the size of CalcGaloisHash()
      TDECFormattedCipher(Cipher).AuthenticationResultBitLength := GCMAuthTagSizeInBytes * 8;
      tempstream.WriteByte(GCMAuthTagSizeInBytes);
    end;
    {$ENDREGION}

    {$REGION '9. Encrypted data (version 0 with length prefix, version 1+ without)'}
    Cipher.Init(Key, IV, IvFillByte);
    try
      Source.Position := 0;
      if V = fvHagenReddmannExample then
        tempstream.WriteLong(Source.Size);
      TDECFormattedCipher(Cipher).EncodeStream(Source, tempstream, source.size, OnProgressProc);
    finally
      Cipher.Done;
    end;
    {$ENDREGION}

    {$REGION '9.1 DEC CalcMAC (not if ECB mode)'}
    if ((V=fvHagenReddmannExample) or (V>=fvDc50Wip)) and (Cipher.Mode<>cmECBx) then
    begin
      HashResult2 := Cipher.CalcMAC;
      if V=fvHagenReddmannExample then
        tempstream.WriteByte(Length(HashResult2))
      else
        Assert(Length(HashResult2) = Cipher.Context.BlockSize);
      tempstream.WriteRawByteString(HashResult2);
    end;
    {$ENDREGION}

    {$REGION '9.2 GCM Tag (only if GCM mode)'}
    if (V>=fvDc50Wip) and (Cipher.Mode=cmGCM) then
    begin
      tempstream.WriteRawBytes(TDECFormattedCipher(Cipher).CalculatedAuthenticationResult);
    end;
    {$ENDREGION}

    {$REGION '10. Hash/HMAC (version 1-3 hash on source, version 4+ hmac on encrypted file)'}
    if V = fvDc40 then
    begin
      Source.Position := 0;
      TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, OnProgressProc);
      tempstream.WriteRawBytes(HashResult);
    end
    else if V = fvDc41Beta then
    begin
      Source.position := 0;
      TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, OnProgressProc);
      HashResult2 := TDECHashExtended(ahash).CalcString(Convert(HashResult)+Seed+APassword, TFormat_Copy);
      tempstream.WriteRawByteString(HashResult2);
    end
    else if V = fvDc41FinalCancelled then
    begin
      Source.position := 0;
      TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, OnProgressProc);
      HashResult2 := TDECHashExtended(ahash).CalcString(
        Convert(HashResult) + Seed +
            TDECHashExtended(ahash).CalcString(
              Seed+TDECHashExtended(ahash).CalcString(Seed+APassword, TFormat_Copy)
            , TFormat_Copy)
      , TFormat_Copy);
      tempstream.WriteRawByteString(HashResult2);
    end
    else if V >= fvDc50Wip then
    begin
      tmp64 := tempstream.Position;
      tempstream.Position := 0;
      HashResult2 := Convert(TDECHashAuthentication(ahash).HMACStream(HMacKey, tempstream, tmp64, OnProgressProc));
      tempstream.Position := tmp64;
      tempstream.WriteRawByteString(HashResult2);
    end;
    {$ENDREGION}

    {$REGION '11. File Terminus (readonly version 2 and 3)'}
    if V = fvDc41Beta then
    begin
      tempstream.WriteRawByteString(RawByteString(TNetEncoding.Base64.Encode('DCTERMINUS')));
    end
    else if V = fvDc41FinalCancelled then
    begin
      tempstream.WriteRawByteString(RawByteString(#$63#$F3#$DF#$89#$B7#$27#$20#$EA));
    end;
    {$ENDREGION}

  finally
    if Assigned(Source) then FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
    if Assigned(Cipher) then FreeAndNil(Cipher);
    if Assigned(ahash) then FreeAndNil(ahash);
    if IsZLibCompressed and (ATempFileName<>'') then
    begin
      SecureDeleteFile(ATempFileName);
    end;
  end;
end;

procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

  const
    DC4_SUBFORMAT_VERSION: array[Low(TDcFormatVersion)..High(TDcFormatVersion)] of string = (
      'Hagen Reddmann Example File',
      '(De)Coder 4.0',
      '(De)Coder 4.1 Beta',
      '(De)Coder 4.1 Final (Cancelled)',
      '(De)Coder 5.0 WIP'
    );

    INTEGRITY_CHECK_INFO: array[Low(TDcFormatVersion)..High(TDcFormatVersion)] of string = (
      'DEC CalcMac',
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
      'GCM = Galois Counter Mode'
    );

    CIPHER_FILLMODE_NAMES: array[Low(TBlockFillMode)..High(TBlockFillMode)] of string = (
      'Bytes'
    );

  function YesNo(b: boolean): string; overload;
  begin
    if b then exit('Yes') else exit('No');
  end;

  function YesNo(b: TYesNoAuto): string; overload;
  begin
    case b of
      Yes:   exit('Yes');
      No:    exit('No');
      Auto:  exit('Auto');
    end;
  end;

begin
  sl.Add('File Format: (De)Coder 4.x/5.x Encrypted File');
  sl.Add('Sub-Format: ' + DC4_SUBFORMAT_VERSION[fi.Parameters.Dc4FormatVersion]);
  sl.Add('Is compressed folder: ' + YesNo(fi.IsCompressedFolder));
  sl.Add('Data additionally ZLib-compressed: ' + YesNo(fi.IsZLibCompressed));
  sl.Add('Original filename: ' + fi.OrigFileName);
  sl.Add('Encrypted File Password: ' + YesNo(fi.Parameters.FileNameUserPasswordEncrypted));
  sl.Add('Key Derivation Algorithm: ' + KDF_VERSION_NAMES[fi.Parameters.KDF]);
  if fi.Parameters.KDF = kvPbkdf2 then
    sl.Add('PBKDF Iterations: ' + IntToStr(fi.Parameters.PBKDF_Iterations));
  sl.Add('Hashing Algorithm: ' + StringReplace(fi.Parameters.HashClass.ClassName, 'THash_', '', []));
  sl.Add('Hash Digest Size: ' + IntToStr(fi.Parameters.HashClass.DigestSize));
  sl.Add('Hash Block Size: ' + IntToStr(fi.Parameters.HashClass.BlockSize));
  sl.Add('Hash Seed Size: ' + IntToStr(fi.Parameters.SeedSize));
  sl.Add('Encryption Algorithm: ' + StringReplace(fi.Parameters.CipherClass.ClassName, 'TCipher_', '', []));
  sl.Add('Cipher Key Size: ' + IntToStr(fi.Parameters.CipherClass.Context.KeySize));
  sl.Add('Cipher Block Size: ' + IntToStr(fi.Parameters.CipherClass.Context.BlockSize));
  sl.Add('Cipher Buffer Size: ' + IntToStr(fi.Parameters.CipherClass.Context.BufferSize));
  sl.Add('Cipher IV Size: ' + IntToStr(fi.Parameters.IVSizeInBytes));
  sl.Add('Cipher IV Fill Byte: 0x'+IntToHex(fi.Parameters.IvFillByte,2));
  sl.Add('Cipher Mode: ' + CIPHER_MODE_NAMES[fi.Parameters.CipherMode]);
  // Commented out, because DEC currently doesn't use it
  //sl.Add('Cipher Block Filling Mode: ' + CIPHER_FILLMODE_NAMES[fi.FillMode]);
  if fi.Parameters.CipherMode = cmGCM then
    sl.Add('GCM Auth Tag Size: ' + IntToStr(fi.Parameters.GCMAuthTagSizeInBytes*8) + ' bits');
  sl.Add('Message Authentication: ' + INTEGRITY_CHECK_INFO[fi.Parameters.Dc4FormatVersion]);
end;

procedure DeCoder10_EncodeFile(const AFileName, AOutput: String; ForceUpperCase: boolean; OnProgressProc: TDECProgressEvent=nil);
var
  Source: TFileStream;
  tempstream: TFileStream;
  rbs: RawByteString;
  let: array[1..27] of AnsiChar;
  ch: AnsiChar;
  I: Integer;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  tempstream := nil;
  try
    tempstream := TFileStream.Create(AOutput, fmCreate);
    tempstream.WriteRawByteString('COD'+#1#1);
    for I := 1 to 27 do
      let[I] := AnsiChar(Chr(199+I));
    while Source.Position < Source.Size do
    begin
      rbs := Source.ReadRawByteString(1);
      ch := rbs[Low(rbs)];
      if ForceUpperCase then ch := UpCase(ch);
      if ch in ['A'..'Z'] then
        tempstream.WriteRawByteString(AnsiChar(#36) + let[Ord(ch)-Ord('A')+1] + AnsiChar(#16));
      let[27] := let[1];
      for i := 1 to 26 do
        let[i] := let[i+1];
      if not (ch in ['A'..'Z']) then
        tempstream.WriteRawByteString(AnsiChar(#36) + let[27] + ch);
    end;
    tempstream.WriteRawByteString(#1#1#1);
  finally
    FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
  end;
end;

procedure DeCoder10_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);
var
  Source: TFileStream;
  tempstream: TFileStream;
  rbs: RawByteString;
  b: TBytes;
  wrongFormat: boolean;
  let: array[1..27] of AnsiChar;
  inl: array[1..27] of AnsiChar;
  ch: AnsiChar;
  I: Integer;
  foundchar: boolean;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  tempstream := nil;
  try
    tempstream := TFileStream.Create(AOutput, fmCreate);

    if (Source.Size < 5+3) or ((Source.Size-5) mod 3 <> 0) then
      wrongFormat := true
    else
    begin
      rbs := Source.ReadRawByteString(5);
      Source.Position := Source.Size - 3;
      wrongFormat := (rbs <> 'COD'+#1#1) or (Source.ReadRawByteString(3) <> #1#1#1);
    end;
    if wrongFormat then
      raise Exception.Create('This file was not encrypted with (De)Coder 1.0');
    Source.Position := 5;
    for ch := 'A' to 'Z' do
      let[Ord(ch)-Ord('A')+1] := ch;
    for I := 1 to 27 do
      inl[I] := AnsiChar(Chr(199+I));
    while Source.Position < Source.Size-3 do
    begin
      foundchar := false;    
      b := Source.ReadRawBytes(3);
      for I := 1 to 26 do
      begin
        if (b[0]=36) and (b[1]=Ord(inl[i])) and (b[2]=16) then
        begin
          tempstream.WriteByte(Ord(let[i]));
          foundchar := true;
          break;
        end;
      end;      
      if not foundchar then
      begin
        for I := 1 to 27 do
        begin
          if (b[0]=36) and (b[1]=Ord(inl[i])) then
          begin
            tempstream.WriteByte(b[2]);
            break;
          end;
        end;
      end;
      for i := 27 downto 2 do
        let[i] := let[i-1];
      let[1] := let[27];
    end;
  finally
    FreeAndNil(Source);
    if Assigned(tempstream) then FreeAndNil(tempstream);
  end;
end;

procedure DeCoder20_EncodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder20.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(''), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder20_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder20.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(''), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder21_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder21.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder21_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder21.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder22_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder22.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder22_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder22.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder30_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder30.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder30_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder30.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder32_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder32.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

procedure DeCoder32_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ssOut := TFileStream.Create(AOutput, fmCreate);
    try
      Cipher := TCipher_VtsDeCoder32.Create;
      Cipher.Mode := cmECBx;
      Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
      TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, OnProgressProc);
      Cipher.Done;
      Cipher.Free;
    finally
      ssOut.Free;
    end;
  finally
    ssIn.Free;
  end;
end;

initialization
  RandomSeed;
end.
