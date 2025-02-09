unit DecoderEncDec;

interface

uses
  Windows, SysUtils, Classes, DECFormatBase, DECTypes,
  System.UITypes, DECCiphers, DECCipherBase, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECCipherFormats, 
  EncdDecd, System.NetEncoding, DECCRC, DECBaseClass, Generics.Collections,
  DECRandom, System.IOUtils, DecoderFuncs;

type
  TDc4FormatVersion = (fvHagenReddmannExample, fvDc40, fvDc41Beta, fvDc41FinalCancelled, fvDc50);

  TKdfVersion = (kvUnknown, kvKdf1, kvKdf2, kvKdf3, kvKdfx, kvPbkdf2);

  TYesNoAuto = (Yes, No, Auto);

  TDc4FileNamePolicy = (fpExpose, fpEncryptWithUserKey, fpHide);

  TDC4Parameters = record
    Dc4FormatVersion: TDc4FormatVersion;
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
    GCMAuthTagSizeInBytes: byte;
    ContainFileOrigName: TDc4FileNamePolicy;
    ContainFileOrigSize: boolean;
    ContainFileOrigDate: boolean;
  end;

  UnixTimestamp = Int64;

  TDC4FileInfo = record
    IsZLibCompressed: boolean;
    IsCompressedFolder: boolean;
    Parameters: TDC4Parameters;
    OrigFileName: string; // or '' if hidden
    OrigFileSize: Int64; // or =-1 if hidden
    OrigFileDate: UnixTimestamp; // or =-1 if hidden
  end;

procedure DeCoder10_EncodeFile(const AFileName, AOutput: String; ForceUpperCase: boolean; OnProgressProc: TDcProgressEvent=nil);
procedure DeCoder10_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDcProgressEvent=nil);
function DeCoder10_DetectFile(const AFileName: string): boolean;

procedure DeCoder20_EncodeFile(const AFileName, AOutput: String; OnProgressProc: TDcProgressEvent=nil);
procedure DeCoder20_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDcProgressEvent=nil);
//function DeCoder20_DetectFile(const AFileName: string): boolean;

procedure DeCoder21_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
procedure DeCoder21_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
//function DeCoder21_DetectFile(const AFileName: string): boolean;

procedure DeCoder22_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
procedure DeCoder22_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
//function DeCoder22_DetectFile(const AFileName: string): boolean;

procedure DeCoder30_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
procedure DeCoder30_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
//function DeCoder30_DetectFile(const AFileName: string): boolean;

procedure DeCoder32_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
procedure DeCoder32_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
//function DeCoder32_DetectFile(const AFileName: string): boolean;

function DeCoder4X_GetDefaultParameters(V: TDc4FormatVersion): TDC4Parameters;
procedure DeCoder4X_ValidateParameterBlock(AParameters: TDC4Parameters);

procedure DeCoder4X_EncodeFile(const AFileName, AOutput: String; const APassword: string; AParameters: TDC4Parameters; OnProgressProc: TDcProgressEvent=nil);
function DeCoder4X_DecodeFile(const AFileName: string; var AOutput: String; const APassword: string; OnProgressProc: TDcProgressEvent=nil): TDC4FileInfo;
function DeCoder4X_DetectFile(const AFileName: string): boolean;

// Note: A password is only required for reading an user-key-encrypted filename (feature was only available in format version 3)
function DeCoder4X_FileInfo(const AFileName: String; const APassword: string=''; OnProgressProc: TDcProgressEvent=nil): TDC4FileInfo;
procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

{$IFDEF Debug}
procedure Debug_ListHashAlgos(Lines: TStrings; V: TDc4FormatVersion);
procedure Debug_ListCipherAlgos(Lines: TStrings; V: TDc4FormatVersion);
{$ENDIF}

implementation

uses
  DecoderOldCiphers, DecoderSevenZipUtils, DateUtils, StrUtils, Math;

const
  DC4_ID_BASES: array[Low(TDc4FormatVersion)..High(TDc4FormatVersion)] of Int64 = (
    $84485225, // Hagen Reddmann Example (no .dc4 files)
    $59178954, // (De)Coder 4.0
    $84671842, // (De)Coder 4.1 beta*
    $19387612, // (De)Coder 4.1 final (cancelled)*
    $1259d82a  // (De)Coder 5.0
    // * = can be changed in the file format
  );

function DeCoder4X_GetOID(V: TDc4FormatVersion): string;
begin
  // This is the OID { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 37476 products(2) decoder(2) fileformat(1) dc4(4) stdX(X) }
  result := '1.3.6.1.4.1.37476.2.2.1.4.'+IntToStr(Ord(V));
end;

function DC_DEC_ClassExistedInDEC51(const cn: string): boolean;
begin
  result := (cn = 'TCipher_1DES') or
            (cn = 'TCipher_2DDES') or
            (cn = 'TCipher_2DES') or
            (cn = 'TCipher_3DDES') or
            (cn = 'TCipher_3DES') or
            (cn = 'TCipher_3TDES') or
            (cn = 'TCipher_3Way') or
            (cn = 'TCipher_Blowfish') or
            (cn = 'TCipher_Cast128') or
            (cn = 'TCipher_Cast256') or
            (cn = 'TCipher_Gost') or
            (cn = 'TCipher_IDEA') or
            (cn = 'TCipher_Mars') or
            (cn = 'TCipher_Misty') or
            (cn = 'TCipher_NewDES') or
            (cn = 'TCipher_Q128') or
            (cn = 'TCipher_RC2') or
            (cn = 'TCipher_RC4') or
            (cn = 'TCipher_RC5') or
            (cn = 'TCipher_RC6') or
            (cn = 'TCipher_Rijndael') or
            (cn = 'TCipher_SAFER') or
            (cn = 'TCipher_SCOP') or
            (cn = 'TCipher_Sapphire') or
            (cn = 'TCipher_Shark') or
            (cn = 'TCipher_Skipjack') or
            (cn = 'TCipher_Square') or
            (cn = 'TCipher_TEA') or
            (cn = 'TCipher_TEAN') or
            (cn = 'TCipher_Twofish') or
            (cn = 'THash_Haval128') or
            (cn = 'THash_Haval160') or
            (cn = 'THash_Haval192') or
            (cn = 'THash_Haval224') or
            (cn = 'THash_Haval256') or
            (cn = 'THash_MD2') or
            (cn = 'THash_MD4') or
            (cn = 'THash_MD5') or
            (cn = 'THash_Panama') or
            (cn = 'THash_RipeMD128') or
            (cn = 'THash_RipeMD160') or
            (cn = 'THash_RipeMD256') or
            (cn = 'THash_RipeMD320') or
            (cn = 'THash_SHA') or
            (cn = 'THash_SHA1') or
            (cn = 'THash_SHA256') or
            (cn = 'THash_SHA384') or
            (cn = 'THash_SHA512') or
            (cn = 'THash_Sapphire') or
            (cn = 'THash_Snefru128') or
            (cn = 'THash_Snefru256') or
            (cn = 'THash_Square') or
            (cn = 'THash_Tiger') or
            (cn = 'THash_Whirlpool') or
            (cn = 'THash_Whirlpool1');
end;

function DC_DEC_Identity(IdentityBase: Int64; const ClassName: string; dc51compat: boolean): Int64;
var
  Signature: AnsiString;
  cn: string;
begin
  cn := ClassName;
  if dc51compat then
  begin
    // Just different names in DEC 6.0 vs. DEC5.1 which we auto-correct to get the correct identity...
    if cn = 'THash_SHA0'{DEC6.0} then cn := 'THash_SHA'{DEC5.1};
    if cn = 'THash_Whirlpool0'{DEC6.0} then cn := 'THash_Whirlpool'{DEC5.1};
    if cn = 'TCipher_AES'{DEC6.0} then cn := 'TCipher_Rijndael'{DEC5.1};
    if cn = 'TCipher_Magma' then cn := 'TCipher_Gost'; // TCipher_Magma is an alias for TCipher_Gost

    // These ciphers/hashes were not available in older DEC, so we don't accept them
    if not DC_DEC_ClassExistedInDEC51(cn) then
    begin
      Exit(-1); // raise Exception.Create('This DEC class did not exist in old DEC 5.1c');
    end;

    Signature := AnsiString(StringOfChar(#$5A, 256 - Length(cn)) + AnsiUpperCase(cn));
    Result := CRC32(IdentityBase, Signature[1], Length(Signature));
  end
  else
  begin
    // Code copied from TDECObject.Identity
    // we cannot use TDECClassList.ClassByIdentity, because it does not allow switching of IdentityBase
    Signature := RawByteString(StringOfChar(#$5A, 256 - Length(cn)) + UpperCase(cn));
    Result := CRC32(IdentityBase, Signature[Low(Signature)], Length(Signature) * SizeOf(Signature[Low(Signature)]));
  end;
end;

function DC_DEC_HashById(IdentityBase, Identity: Int64; dc51compat: boolean; NoException: boolean=false): TDECHashClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
resourcestring
  SHashId_DD_NotFound = 'Hash ID 0x%.8x with base 0x%.8x not found';
begin
  for p in TDECHash.ClassList do
  begin
    c := p.Value;
    if (c <> nil) and
       (Identity = DC_DEC_Identity(IdentityBase, c.ClassName, dc51compat)) then
    begin
      result := TDECHashClass(c);
      exit;
    end;
  end;
  if NoException then
    result := nil
  else
    raise Exception.CreateFmt(SHashId_DD_NotFound, [Identity, IdentityBase]);
end;

function DC_DEC_CipherById(IdentityBase, Identity: Int64; dc51compat: boolean; NoException: boolean=false): TDECCipherClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
resourcestring
  SCipherId_DD_NotFound = 'Cipher ID 0x%.8x with base 0x%.8x not found';
begin
  for p in TDECCipher.ClassList do
  begin
    c := p.Value;
    if (c <> nil) and
       not c.InheritsFrom(TCipher_VtsDeCoderOldCipher) and
       (Identity = DC_DEC_Identity(IdentityBase, c.ClassName, dc51compat)) then
    begin
      result := TDecCipherClass(c);
      exit;
    end;
  end;
  if NoException then
    result := nil
  else
    raise Exception.CreateFmt(SCipherId_DD_NotFound, [Identity, IdentityBase]);
end;

{$IFDEF Debug}
procedure Debug_ListHashAlgos(Lines: TStrings; V: TDc4FormatVersion);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
  sl: TStringList;
  addinfo: string;
begin
  // No resourcestrings/Translation, because this method is used to generate the specification file which is only English
  sl := TStringList.Create;
  try
    for p in TDECHash.ClassList do
    begin
      c := p.Value;
      cn := c.ClassName;
      if (V<fvDc50) and (cn='THash_Whirlpool0') then cn := 'THash_Whirlpool';
      if (V<fvDc50) and (cn='THash_SHA0') then cn := 'THash_SHA';
      if (V<fvDc50) and not DC_DEC_ClassExistedInDEC51(cn) then continue;
      addinfo := '';
      if (V>=fvDc50) and (cn='THash_SHA3_512') then addinfo := ', default'
      else if (V>=fvDc40) and (V<fvDc50) and (cn='THash_SHA512') then addinfo := ', default'
      else if (V=fvHagenReddmannExample) and (cn='THash_SHA1') then addinfo := ', default';
      sl.Add(
        '0x'+IntToHex(DC_DEC_Identity(DC4_ID_BASES[V], cn, V<fvDc50), 8) + #9 +
        cn +
        ' (DigestSize: '+IntToStr(TDECHashClass(c).DigestSize) +
        ', BlockSize: '+IntToStr(TDECHashClass(c).BlockSize) +
        addinfo +
        ')'
      );
    end;
    sl.Sorted := true;
    Lines.AddStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;
{$ENDIF}

{$IFDEF Debug}
procedure Debug_ListCipherAlgos(Lines: TStrings; V: TDc4FormatVersion);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
  sl: TStringList;
  addinfo: string;
begin
  // No resourcestrings/Translation, because this method is used to generate the specification file which is only English
  sl := TStringList.Create;
  try
    for p in TDECCipher.ClassList do
    begin
      c := p.Value;
      cn := c.ClassName;
      if c.InheritsFrom(TCipher_VtsDeCoderOldCipher) then continue;
      if (V<fvDc50) and (cn='TCipher_AES') then cn := 'TCipher_Rijndael';  // TCipher_Rijndael (DEC5.x) was renamed to TCipher_AES (DEC6.x)
      if (V<fvDc50) and not DC_DEC_ClassExistedInDEC51(cn) then continue;
      addinfo := '';
      if (V<fvDc50) and (cn='TCipher_Shark') then addinfo := ', faulty implementation';
      if (V<fvDc50) and (cn='TCipher_SCOP') then addinfo := ', faulty implementation?';
      if (V<fvDc50) and (cn='TCipher_Rijndael') then addinfo := ', default';
      if (V>=fvDc50) and (cn='TCipher_AES') then addinfo := ', default'; // TCipher_Rijndael was renamed to TCipher_AES
      sl.Add(
        '0x'+IntToHex(DC_DEC_Identity(DC4_ID_BASES[V], cn, V<fvDc50), 8) + #9 +
        cn +
        ' (KeySize: '+IntToStr(TDECCipherClass(c).Context.KeySize) +
        ', BlockSize: '+IntToStr(TDECCipherClass(c).Context.BlockSize) +
        ', BufferSize: '+IntToStr(TDECCipherClass(c).Context.BufferSize) +
        addinfo +
        ')'
      );
    end;
    sl.Sorted := true;
    Lines.AddStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;
{$ENDIF}

const
  DC10_HEAD = 'COD'+#1#1;
  DC10_FOOT = #1#1#1;

procedure DeCoder10_EncodeFile(const AFileName, AOutput: String; ForceUpperCase: boolean; OnProgressProc: TDcProgressEvent=nil);
var
  Source: TFileStream;
  tempstream: TFileStream;
  let: array[1..27] of AnsiChar;
  ch: AnsiChar;
  I, J: Integer;
  rbsIn: RawByteString;
  rbsOut: RawByteString;
  outFileDidExist: boolean;
const
  chunksize = 4096; // bigger = faster
resourcestring
  SDC1Encode = '(De)Coder 1.0 encoding';
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SOnlyAsciiFilesAllowed = 'Only ASCII text files can be encrypted with (De)Coder 1.0';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  tempstream := nil;
  try
    try
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, 0, SDC1Encode, TDcProgressState.Started);
      tempstream := TFileStream.Create(AOutput, fmCreate);
      tempstream.WriteRawByteString(DC10_HEAD);
      for I := 1 to 27 do
        let[I] := AnsiChar(Chr(199+I));
      while Source.Position < Source.Size do
      begin
        if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Position, SDC1Encode, TDcProgressState.Processing);
        rbsIn := Source.ReadRawByteString(Min(chunksize,Source.Size-Source.Position));
        rbsOut := '';
        for j := Low(rbsIn) to High(rbsIn) do
        begin
          ch := rbsIn[j];
          if (Ord(ch)<32) or (Ord(ch)>126) then
            raise Exception.Create(SOnlyAsciiFilesAllowed);
          if ForceUpperCase then ch := UpCase(ch);
          if ch in ['A'..'Z'] then
            rbsOut := rbsOut + AnsiChar(#36) + let[Ord(ch)-Ord('A')+1] + AnsiChar(#16);
          let[27] := let[1];
          for i := 1 to 26 do
            let[i] := let[i+1];
          if not (ch in ['A'..'Z']) then
            rbsOut := rbsOut + AnsiChar(#36) + let[27] + ch;
        end;
        tempstream.WriteRawByteString(rbsOut);
      end;
      tempstream.WriteRawByteString(DC10_FOOT);
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Size, SDC1Encode, TDcProgressState.Finished);
    finally
      FreeAndNil(Source);
      if Assigned(tempstream) then FreeAndNil(tempstream);
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder10_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDcProgressEvent=nil);
var
  Source: TFileStream;
  tempstream: TFileStream;
  rbs: RawByteString;
  b: TBytes;
  wrongFormat: boolean;
  let: array[1..27] of AnsiChar;
  inl: array[1..27] of AnsiChar;
  ch: AnsiChar;
  foundchar: boolean;
  I, J: Integer;
  rbsIn: RawByteString;
  rbsOut: RawByteString;
  outFileDidExist: boolean;
const
  chunksize = 4096*3; // bigger = faster. Must be multiple of 3
resourcestring
  SDC1Decode = '(De)Coder 1.0 decoding';
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SNoDc10File = 'This file was not encrypted with (De)Coder 1.0';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  tempstream := nil;
  try
    try
      if (Source.Size < Length(DC10_HEAD)+Length(DC10_FOOT)) or
         ((Source.Size-Length(DC10_HEAD)-Length(DC10_FOOT)) mod 3 <> 0) then
        wrongFormat := true
      else
      begin
        rbs := Source.ReadRawByteString(Length(DC10_HEAD));
        Source.Position := Source.Size - Length(DC10_FOOT);
        wrongFormat := (rbs <> DC10_HEAD) or (Source.ReadRawByteString(Length(DC10_FOOT)) <> DC10_FOOT);
      end;
      if wrongFormat then
        raise Exception.Create(SNoDc10File);
      Source.Position := Length(DC10_HEAD);

      tempstream := TFileStream.Create(AOutput, fmCreate);
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, 0, SDC1Decode, TDcProgressState.Started);
      for ch := 'A' to 'Z' do
        let[Ord(ch)-Ord('A')+1] := ch;
      for I := 1 to 27 do
        inl[I] := AnsiChar(Chr(199+I));
      SetLength(b,3);
      while Source.Position < Source.Size-Length(DC10_FOOT) do
      begin
        if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Position, SDC1Decode, TDCProgressState.Processing);
        rbsIn := Source.ReadRawByteString(Min(chunksize,Source.Size-Source.Position));
        rbsOut := '';
        for j := 0 to (Length(rbsIn)-1) div 3 do
        begin
          b[0] := Ord(rbsIn[Low(rbsIn)+3*j]);
          b[1] := Ord(rbsIn[Low(rbsIn)+3*j+1]);
          b[2] := Ord(rbsIn[Low(rbsIn)+3*j+2]);
          foundchar := false;
          for I := 1 to 26 do
          begin
            if (b[0]=36) and (b[1]=Ord(inl[i])) and (b[2]=16) then
            begin
              rbsOut := rbsOut + let[i];
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
                rbsOut := rbsOut + AnsiChar(Chr(b[2]));
                break;
              end;
            end;
          end;
          for i := 27 downto 2 do
            let[i] := let[i-1];
          let[1] := let[27];
        end;
        tempstream.WriteRawByteString(rbsOut);
      end;
      SetLength(b,0);
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Size, SDC1Decode, TDcProgressState.Finished);
    finally
      FreeAndNil(Source);
      if Assigned(tempstream) then FreeAndNil(tempstream);
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

function DeCoder10_DetectFile(const AFileName: string): boolean;
var
  Source: TFileStream;
  rbs: RawByteString;
  wrongformat: boolean;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if (Source.Size < 5+3) or ((Source.Size-5) mod 3 <> 0) then
      wrongFormat := true
    else
    begin
      rbs := Source.ReadRawByteString(5);
      Source.Position := Source.Size - 3;
      wrongFormat := (rbs <> 'COD'+#1#1) or (Source.ReadRawByteString(3) <> #1#1#1);
    end;
    result := not wrongFormat;
  finally
    FreeAndNil(Source);
  end;
end;

procedure DeCoder20_EncodeFile(const AFileName, AOutput: String; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SEncodeStream = 'Encode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder20.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(''), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SEncodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder20_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SDecodeStream = 'Decode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder20.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(''), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder21_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SEncodeStream = 'Encode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder21.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SEncodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder21_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SDecodeStream = 'Decode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder21.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder22_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SEncodeStream = 'Encode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder22.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SEncodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder22_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SDecodeStream = 'Decode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder22.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(IntToStr(Key)), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder30_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SEncodeStream = 'Encode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder30.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SEncodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder30_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SDecodeStream = 'Decode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder30.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder32_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SEncodeStream = 'Encode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder32.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SEncodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder32_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDcProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SDecodeStream = 'Decode stream';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
    try
      ssOut := TFileStream.Create(AOutput, fmCreate);
      try
        Cipher := TCipher_VtsDeCoder32.Create;
        Cipher.Mode := cmECBx;
        Cipher.Init(AnsiString(Key), AnsiString(''), $FF);
        TDECFormattedCipher(Cipher).DecodeStream(ssIn, ssOut, ssIn.Size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
          end);
        Cipher.Done;
        Cipher.Free;
      finally
        ssOut.Free;
      end;
    finally
      ssIn.Free;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

function DeCoder4X_GetDefaultParameters(V: TDc4FormatVersion): TDC4Parameters;
begin
  result.Dc4FormatVersion := V;
  if V >= fvDc50 then
    result.ShouldBeZLibCompressed := Auto
  else if V >= fvDc41Beta then
    result.ShouldBeZLibCompressed := Yes
  else
    result.ShouldBeZLibCompressed := No;
  result.KDF := kvKdfx;
  result.PBKDF_Iterations := 0; // only for KdfVersion=kvPbkdf2
  if V >= fvDc50 then
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
  else if V < fvDc50 then
    result.HashClass := THash_SHA512
  else
    result.HashClass := THash_SHA3_512;
  result.CipherClass := TCipher_AES;
  result.CipherMode := TCipherMode.cmCTSx;
  result.BlockFillMode := TBlockFillMode.fmByte;
  if V = fvDc41FinalCancelled then
    Result.ContainFileOrigName := fpEncryptWithUserKey // only available in version 3
  else if V < fvDc50 then
    Result.ContainFileOrigName := fpExpose
  else
    Result.ContainFileOrigName := fpHide; // default disabled for privacy
  if (V >= fvDc50) and (result.CipherMode = cmGCM) then
    result.GCMAuthTagSizeInBytes := 128 shr 3
  else
    result.GCMAuthTagSizeInBytes := 0;
  result.ContainFileOrigSize := false; // available since ver4, but disabled by default
  result.ContainFileOrigDate := false; // available since ver4, but disabled by default
end;

procedure DeCoder4X_ValidateParameterBlock(AParameters: TDC4Parameters);
resourcestring
  SZipRequiresDc40 = 'ZIP folder requires DC40';
  SDc40HashNotAccepted = 'Hash not accepted in DC40 version';
  SDc40CipherNotAccepted = 'Cipher not accepted in DC40 version';
  SDc40KdfVersionNotAccepted = 'KDF version not accepted in DC40 version';
  SDc40SeedSizeNotAccepted = 'Seed size not accepted in DC40 version';
  SDc40CipherModeNotAccepted = 'CiperMode not accepted in DC40 version';
  SZlibRequiresDc41beta = 'ZLib requires DC41beta+ version';
  SIndivFillByteOnlyVer4plus = 'Indiv IvFillByte only accepted format version 4+';
  SIndivIvOnlyVer4plus = 'Indiv IV only accepted format version 4+';
  SIndivKdfOnlyVer4plus = 'Indiv KDF only accepted format version 4+';
  SGcmAuthTagOnlyForGcm = 'GCM Auth Tag only allowed for cipher mode GCM';
  SGcmAuthTagRequired = 'GCM Auth Tag required for cipher mode GCM';
  SGcmAuthTagTooSmall = 'GCM Auth Tag too small (choose at least 32 bits = 4 bytes)';
  SGcmAuthTagTooLarge = 'GCM Auth Tag too large (max 128 bits = 16 bytes)';
  SPbkdfIterationsMayNotBe0 = 'PBKDF Iterations must be >0';
  SPbkdfIterationsMustBe0 = 'PBKDF Iterations must be =0';
  SEncFileNameOnlyVerDc41Final = 'Encrypted Filename only accepted in DC41Final version';
  SOrigFileNameOnlyHiddenInVer4 = 'Orig File Name can only be hidden in format version 4';
  SOrigFileSizeOnlyAvailableInVer4 = 'Orig FileSize only available in format version 4';
  SOrigFileDateOnlyAvailableInVer4 = 'Orig FileDate only available in format version 4';
begin
  // Uncommented, because IsCompressedFolder is part of TDC4FileInfo, not of TDC4Parameters
  //if (AParameters.Dc4FormatVersion < fvDc40) and (AParameters.IsCompressedFolder) then
  //  raise Exception.Create(SZipRequiresDc40);

  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.HashClass <> THash_SHA512) then
    raise Exception.Create(SDc40HashNotAccepted);
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.CipherClass <> TCipher_Rijndael) and (AParameters.CipherClass <> TCipher_AES) then
     raise Exception.Create(SDc40CipherNotAccepted);
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.KDF <> kvKdfx) then
    raise Exception.Create(SDc40KdfVersionNotAccepted);
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.SeedSize <> 16) then
    raise Exception.Create(SDc40SeedSizeNotAccepted);
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.CipherMode <> cmCTSx) then
    raise Exception.Create(SDc40CipherModeNotAccepted);

  if (AParameters.Dc4FormatVersion < fvDc41Beta) and (AParameters.ShouldBeZLibCompressed <> No) then
    raise Exception.Create(SZlibRequiresDc41beta);

  if (AParameters.Dc4FormatVersion < fvDc50) and (AParameters.IvFillByte <> $FF) then
    raise Exception.Create(SIndivFillByteOnlyVer4plus);
  if (AParameters.Dc4FormatVersion < fvDc50) and (AParameters.IVSizeInBytes <> 0) then
    raise Exception.Create(SIndivIvOnlyVer4plus);
  if (AParameters.Dc4FormatVersion < fvDc50) and (AParameters.KDF <> kvKdfx) then
    raise Exception.Create(SIndivKdfOnlyVer4plus);

  if (AParameters.CipherMode <> cmGCM) and (AParameters.GCMAuthTagSizeInBytes <> 0) then
    raise Exception.Create(SGcmAuthTagOnlyForGcm);
  if (AParameters.CipherMode = cmGCM) and (AParameters.GCMAuthTagSizeInBytes = 0) then
    raise Exception.Create(SGcmAuthTagRequired);
  if (AParameters.CipherMode = cmGCM) and (AParameters.GCMAuthTagSizeInBytes < 4) then
    raise Exception.Create(SGcmAuthTagTooSmall);
  if (AParameters.CipherMode = cmGCM) and (AParameters.GCMAuthTagSizeInBytes > 16) then
    raise Exception.Create(SGcmAuthTagTooLarge); // This is the output size of CalcGaloisHash

  if (AParameters.KDF <> kvPbkdf2) and (AParameters.PBKDF_Iterations <> 0) then
    raise Exception.Create(SPbkdfIterationsMayNotBe0);
  if (AParameters.KDF = kvPbkdf2) and (AParameters.PBKDF_Iterations = 0) then
    raise Exception.Create(SPbkdfIterationsMustBe0);

  if (AParameters.Dc4FormatVersion <> fvDc41FinalCancelled) and (AParameters.ContainFileOrigName=fpEncryptWithUserKey) then
    raise Exception.Create(SEncFileNameOnlyVerDc41Final);
  if (AParameters.Dc4FormatVersion < fvDc50) and (AParameters.ContainFileOrigName=fpHide) then
    raise Exception.Create(SOrigFileNameOnlyHiddenInVer4);
  if (AParameters.Dc4FormatVersion < fvDc50) and AParameters.ContainFileOrigSize then
    raise Exception.Create(SOrigFileSizeOnlyAvailableInVer4);
  if (AParameters.Dc4FormatVersion < fvDc50) and AParameters.ContainFileOrigDate then
    raise Exception.Create(SOrigFileDateOnlyAvailableInVer4);
end;

procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

  resourcestring
    DC4_SUBFORMAT_VERSION_0 = 'Hagen Reddmann Example File';
    DC4_SUBFORMAT_VERSION_1 = '(De)Coder 4.0';
    DC4_SUBFORMAT_VERSION_2 = '(De)Coder 4.1 Beta';
    DC4_SUBFORMAT_VERSION_3 = '(De)Coder 4.1 Final (Cancelled)';
    DC4_SUBFORMAT_VERSION_4 = '(De)Coder 5.0';
  const
    DC4_SUBFORMAT_VERSION: array[Low(TDc4FormatVersion)..High(TDc4FormatVersion)] of string = (
      DC4_SUBFORMAT_VERSION_0,
      DC4_SUBFORMAT_VERSION_1,
      DC4_SUBFORMAT_VERSION_2,
      DC4_SUBFORMAT_VERSION_3,
      DC4_SUBFORMAT_VERSION_4
    );

  resourcestring
    INTEGRITY_CHECK_INFO_0 = 'DEC CalcMac';
    INTEGRITY_CHECK_INFO_1 = 'Hash of source data';
    INTEGRITY_CHECK_INFO_2 = 'Nested Hash of source data with password';
    INTEGRITY_CHECK_INFO_3 = 'Nested Hash of source data with password';
    INTEGRITY_CHECK_INFO_4 = 'Encrypt-then-HMAC';
  const
    INTEGRITY_CHECK_INFO: array[Low(TDc4FormatVersion)..High(TDc4FormatVersion)] of string = (
      INTEGRITY_CHECK_INFO_0,
      INTEGRITY_CHECK_INFO_1,
      INTEGRITY_CHECK_INFO_2,
      INTEGRITY_CHECK_INFO_3,
      INTEGRITY_CHECK_INFO_4
    );

  resourcestring
    KDF_VERSION_NAMES_0 = 'Unknown';
    KDF_VERSION_NAMES_1 = 'KDF1';
    KDF_VERSION_NAMES_2 = 'KDF2';
    KDF_VERSION_NAMES_3 = 'KDF3';
    KDF_VERSION_NAMES_4 = 'KDFx';
    KDF_VERSION_NAMES_5 = 'PBKDF2';
  const
    KDF_VERSION_NAMES: array[Low(TKdfVersion)..High(TKdfVersion)] of string = (
      KDF_VERSION_NAMES_0,
      KDF_VERSION_NAMES_1,
      KDF_VERSION_NAMES_2,
      KDF_VERSION_NAMES_3,
      KDF_VERSION_NAMES_4,
      KDF_VERSION_NAMES_5
    );

  resourcestring
    CIPHER_MODE_NAMES_0 = 'CTSx = double CBC, with CFS8 padding of truncated final block';
    CIPHER_MODE_NAMES_1 = 'CBCx = Cipher Block Chaining, with CFB8 padding of truncated final block';
    CIPHER_MODE_NAMES_2 = 'CFB8 = 8bit Cipher Feedback mode';
    CIPHER_MODE_NAMES_3 = 'CFBx = CFB on Blocksize of Cipher';
    CIPHER_MODE_NAMES_4 = 'OFB8 = 8bit Output Feedback mode';
    CIPHER_MODE_NAMES_5 = 'OFBx = OFB on Blocksize bytes';
    CIPHER_MODE_NAMES_6 = 'CFS8 = 8Bit CFS, double CFB';
    CIPHER_MODE_NAMES_7 = 'CFSx = CFS on Blocksize bytes';
    CIPHER_MODE_NAMES_8 = 'ECBx = Electronic Code Book';
    CIPHER_MODE_NAMES_9 = 'GCM = Galois Counter Mode';
  const
    CIPHER_MODE_NAMES: array[Low(TCipherMode)..High(TCipherMode)] of string = (
      CIPHER_MODE_NAMES_0,
      CIPHER_MODE_NAMES_1,
      CIPHER_MODE_NAMES_2,
      CIPHER_MODE_NAMES_3,
      CIPHER_MODE_NAMES_4,
      CIPHER_MODE_NAMES_5,
      CIPHER_MODE_NAMES_6,
      CIPHER_MODE_NAMES_7,
      CIPHER_MODE_NAMES_8,
      CIPHER_MODE_NAMES_9
    );

  resourcestring
    CIPHER_FILLMODE_NAMES_0 = 'Bytes';
  const
    CIPHER_FILLMODE_NAMES: array[Low(TBlockFillMode)..High(TBlockFillMode)] of string = (
      CIPHER_FILLMODE_NAMES_0
    );

  resourcestring
    SYes = 'Yes';
    SNo = 'No';
    SAuto = 'Auto';
    SUnknown = 'unknown';
    SNotAvailable = 'not available';

  function YesNo(b: boolean): string; overload;
  begin
    if b then exit(SYes) else exit(SNo);
  end;

  function YesNo(b: TYesNoAuto): string; overload;
  begin
    case b of
      Yes:   exit(SYes);
      No:    exit(SNo);
      Auto:  exit(SAuto);
    end;
  end;

resourcestring
  SFileFormat_S = 'File Format: %s';
  SSubFormat_S = 'Sub-Format: %s';
  SDC45EncryptedFile = '(De)Coder 4.x/5.x Encrypted File';
  SIsCompresseFolder_S = 'Is compressed folder: %s';
  S7zFormat = '7-Zip format';
  SZipFormat = 'ZIP format';
  SAdditionalZLibCompressed_S = 'Data additionally ZLib-compressed: %s';
  SOriginalFilename_S = 'Original filename: %s';
  SOriginalFilesize_S = 'Original filesize: %s';
  SOriginalDatetime_S = 'Original datetime: %s';
  SPasswordEncrypteedFileName_S = 'Password Encrypted File Name: %s';
  SKDF_S = 'Key Derivation Algorithm: %s';
  SPBKDFIterations_D = 'PBKDF Iterations: %d';
  SHashAlgo_S = 'Hashing Algorithm: %s';
  SHashDigestSize_D = 'Hash Digest Size: %d';
  SHashBlockSize_D = 'Hash Block Size: %d';
  SHashSeedSize_D = 'Hash Seed Size: %d';
  SEncAlgo_S = 'Encryption Algorithm: %s';
  SCipherKeySize_D = 'Cipher Key Size: %d';
  SCipherBlockSize_D = 'Cipher Block Size: %d';
  SCipherBufferSize_D = 'Cipher Buffer Size: %d';
  SCipherIvSize_D  = 'Cipher IV Size: %d';
  SCipherIvFillByte_S = 'Cipher IV Fill Byte: %s';
  SCipherMode_S = 'Cipher Mode: %s';
  SCipherBlockFillingMode_S = 'Cipher Block Filling Mode: %s';
  SGcmAuthTagSize_D = 'GCM Auth Tag Size: %d bits';
  SMAC_S = 'Message Authentication: %s';
begin
  sl.Add(Format(SFileFormat_S, [SDC45EncryptedFile]));
  sl.Add(Format(SSubFormat_S, [DC4_SUBFORMAT_VERSION[fi.Parameters.Dc4FormatVersion]]));
  if fi.IsCompressedFolder and (fi.Parameters.Dc4FormatVersion >= fvDc50) then
    sl.Add(Format(SIsCompresseFolder_S, [SYes + ' (' + S7zFormat + ')']))
  else if fi.IsCompressedFolder and (fi.Parameters.Dc4FormatVersion >= fvDc40) then
    sl.Add(Format(SIsCompresseFolder_S, [SYes + ' (' + SZipFormat + ')']))
  else
    sl.Add(Format(SIsCompresseFolder_S, [SNo]));
  sl.Add(Format(SAdditionalZLibCompressed_S, [YesNo(fi.IsZLibCompressed)]));
  if fi.OrigFileName = '' then
    sl.Add(Format(SOriginalFilename_S, [SUnknown]))
  else
    sl.Add(Format(SOriginalFilename_S, [fi.OrigFileName]));
  if fi.OrigFileSize = -1 then
    sl.Add(Format(SOriginalFilesize_S, [SUnknown]))
  else
    sl.Add(Format(SOriginalFilesize_S, [FileSizeHumanReadable(fi.OrigFileSize)]));
  if fi.OrigFileDate = -1 then
    sl.Add(Format(SOriginalDatetime_S, [SUnknown]))
  else
    sl.Add(Format(SOriginalDatetime_S, [DateTimeToStr(UnixToDateTime(fi.OrigFileDate, false))]));
  if fi.Parameters.Dc4FormatVersion = fvDc41FinalCancelled then
    sl.Add(Format(SPasswordEncrypteedFileName_S, [YesNo(fi.Parameters.ContainFileOrigName=fpEncryptWithUserKey)]))
  else
    sl.Add(Format(SPasswordEncrypteedFileName_S, [SNotAvailable]));
  sl.Add(Format(SKDF_S, [KDF_VERSION_NAMES[fi.Parameters.KDF]]));
  if fi.Parameters.KDF = kvPbkdf2 then
    sl.Add(Format(SPBKDFIterations_D, [fi.Parameters.PBKDF_Iterations]));
  sl.Add(Format(SHashAlgo_S, [StringReplace(fi.Parameters.HashClass.ClassName, 'THash_', '', [])]));
  sl.Add(Format(SHashDigestSize_D, [fi.Parameters.HashClass.DigestSize]));
  sl.Add(Format(SHashBlockSize_D, [fi.Parameters.HashClass.BlockSize]));
  sl.Add(Format(SHashSeedSize_D, [fi.Parameters.SeedSize]));
  sl.Add(Format(SEncAlgo_S, [StringReplace(fi.Parameters.CipherClass.ClassName, 'TCipher_', '', [])]));
  sl.Add(Format(SCipherKeySize_D, [fi.Parameters.CipherClass.Context.KeySize]));
  sl.Add(Format(SCipherBlockSize_D, [fi.Parameters.CipherClass.Context.BlockSize]));
  sl.Add(Format(SCipherBufferSize_D, [fi.Parameters.CipherClass.Context.BufferSize]));
  sl.Add(Format(SCipherIvSize_D, [fi.Parameters.IVSizeInBytes]));
  sl.Add(Format(SCipherIvFillByte_S, ['0x'+IntToHex(fi.Parameters.IvFillByte,2)]));
  sl.Add(Format(SCipherMode_S, [CIPHER_MODE_NAMES[fi.Parameters.CipherMode]]));
  // Commented out, because DEC currently doesn't use it
  //sl.Add(Format(SCipherBlockFillingMode_S, [CIPHER_FILLMODE_NAMES[fi.FillMode]]));
  if fi.Parameters.CipherMode = cmGCM then
    sl.Add(Format(SGcmAuthTagSize_D, [fi.Parameters.GCMAuthTagSizeInBytes*8]));
  sl.Add(Format(SMAC_S, [INTEGRITY_CHECK_INFO[fi.Parameters.Dc4FormatVersion]]));
end;

procedure DeCoder4X_EncodeFile(const AFileName, AOutput: String; const APassword: string; AParameters: TDC4Parameters; OnProgressProc: TDcProgressEvent=nil);
var
  tempstream: TFileStream;
  FileNameKey: TBytes;
  FilenamePassword: RawByteString;
  OrigNameEncrypted: RawByteString;
  HashResult: TBytes;
  FileNameUserPasswordEncrypted: Boolean;
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
  PbkdfIterations: long;
  V: TDc4FormatVersion;
  GCMAuthTagSizeInBytes: byte;
  tmpWS: WideString;
  outFileDidExist: boolean;
  PasswordRBS: RawByteString;
  (*
  IsFolder   IsZLib    SourceFile           ATempFileNameZLib       Source stream
  ---------------------------------------------------------------------------------------
  false      false     AFileName            undefined               AFileName
  false      true      AFileName===========>AFileName.z             AFileName_tmp.z
  true       false     AFileName_tmp.7z     undefined               AFileName_tmp.7z
  true       true      AFileName_tmp.7z====>AFileName.z             AFileName_tmp.z
  *)
  ATempFileNameZLib: string; // If IsZLibCompressed, then it is a temporary .dc5_tmp file, otherwise undefined
  SourceFile: string; // If IsFolder, then it is a temporary .7z file, otherwise it is AFileName
const
  // Measurement of 21367	files
  // ShanEntropy  AvgComprRatioZLibMax
  // 0.00-0.99    0.99908
  // 1.00-1.99    0.21721
  // 2.00-2.99    0.23161
  // 3.00-3.99    0.20000
  // 4.00-4.99    0.21339
  // 5.00-5.99    0.35107
  // 6.00-6.99    0.55397
  // 7.00-7.49    0.73129
  // 7.50-7.99    0.87920
  ShannonEntropyTreshold = 7.5;
resourcestring
  SOutputFilenameMustNotBeEmpty = 'Output filename must not be empty';
  SFileOrFolderNotFound = 'File or folder %s not found';
  SFolderEncryptionNotAllowed = 'Encryption of folders is not supported. Please pack the file contents using an external tool.';
  SFilenameTooLong = 'Filename too long';
  SEncodeStream = 'Encode stream';
  SCalcHash = 'Calc hash';
  SCalcHMac = 'Calc HMAC';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameMustNotBeEmpty);
  DeCoder4X_ValidateParameterBlock(AParameters);

  tempstream := nil;
  Source := nil;
  cipher := nil;
  ahash := nil;
  IsZLibCompressed := false;
  IsFolder := DirectoryExists(AFileName);
  if not IsFolder and not FileExists(AFileName) then
    raise Exception.CreateFmt(SFileOrFolderNotFound, [AFileName]);
  outFileDidExist := FileExists(AOutput);
  ATempFileNameZLib := '';
  try
    try
      {$REGION 'Encrypt folder? => Pack it to a file (version 1+)'}
      if IsFolder then
      begin
        if AParameters.Dc4FormatVersion >= fvDc50 then
        begin
          SourceFile := ExcludeTrailingPathDelimiter(AFileName) + '_Tmp_'+RandStringFileNameFriendly(10)+'.7z';
          SevenZipFolder(AFileName, SourceFile, OnProgressProc);
        end
        else if AParameters.Dc4FormatVersion >= fvDc40 then
        begin
          SourceFile := ExcludeTrailingPathDelimiter(AFileName) + '_Tmp_'+RandStringFileNameFriendly(10)+'.zip';
          SevenZipFolder(AFileName, SourceFile, OnProgressProc);
        end
        else
        begin
          raise Exception.Create(SFolderEncryptionNotAllowed);
        end;
      end
      else
      begin
        SourceFile := AFileName;
      end;
      {$ENDREGION}

      {$REGION 'Transfer parameters from AParameters to the working variables'}
      V := AParameters.Dc4FormatVersion;
      case AParameters.ShouldBeZLibCompressed of
        Yes:   IsZLibCompressed := true;
        No:    IsZLibCompressed := false;
        Auto:  IsZLibCompressed := not IsFolder and
                                   not IsCompressedFileType(SourceFile) and
                                   (ShannonEntropy(SourceFile, OnProgressProc) < ShannonEntropyTreshold);
      end;
      KdfVersion := AParameters.KDF;
      PbkdfIterations := AParameters.PBKDF_Iterations;
      if AParameters.IVSizeInBytes > 0 then
        IV := RandomBytes(AParameters.IVSizeInBytes)
      else
        SetLength(IV, 0);
      IvFillByte := AParameters.IvFillByte;
      Seed := BytesToRawByteString(RandomBytes(AParameters.SeedSize));

      HashClass := AParameters.HashClass;
      AHash := HashClass.Create;

      CipherClass := AParameters.CipherClass;
      Cipher := CipherClass.Create;
      Cipher.Mode := AParameters.CipherMode;
      Cipher.FillMode := AParameters.BlockFillMode;
      GCMAuthTagSizeInBytes := AParameters.GCMAuthTagSizeInBytes;

      FileNameUserPasswordEncrypted := AParameters.ContainFileOrigName = fpEncryptWithUserKey;
      {$ENDREGION}

      if V >= fvDc50 then
        PasswordRBS := UTF8Encode(APassword)  // version 4+: Password treated as UTF-8
      else
        PasswordRBS := AnsiString(APassword); // version 0..3: Password treated as ANSI

      {$REGION 'Compress stream if the file type is not already compressed'}
      if IsZLibCompressed and (V>=fvDc40) then
      begin
        ATempFileNameZLib := ChangeFileExt(AFileName, '_Tmp_' + RandStringFileNameFriendly(10) + '.z');
        ZLib_Compress(SourceFile, ATempFileNameZLib, OnProgressProc);
        Source := TFileStream.Create(ATempFileNameZLib, fmOpenRead or fmShareDenyWrite);
      end
      else
      begin
        Source := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
      end;
      {$ENDREGION}

      tempstream := TFileStream.Create(AOutput, fmCreate);

      {$REGION 'Generate key used by HMAC and Cipher'}
      // Note: "MGF1" is "KDF1" just without seed. So we do not need to implement MGF1 at all.
      if KDFVersion = kvKdfx then
        Key := TDECHashExtended(ahash).KDFx(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = kvKdf1 then
        Key := TDECHashExtended(ahash).KDF1(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = kvKdf2 then
        Key := TDECHashExtended(ahash).KDF2(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = kvKdf3 then
        Key := TDECHashExtended(ahash).KDF3(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
      else if KDFVersion = kvPbkdf2 then
        Key := TDECHashExtended(ahash).PBKDF2(BytesOf(PasswordRBS), BytesOf(Seed), PbkdfIterations, Cipher.Context.KeySize)
      else
        Assert(False);
      HMacKey := Key;
      {$ENDREGION}

      {$REGION '0.5 Magic Sequence (only version 4+)'}
      if (V>=fvDc50) then
        tempstream.WriteRawByteString('[' + RawByteString(DeCoder4X_GetOID(V)) + ']' + #13#10);
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

      {$REGION '2. Version (only version 1..3)'}
      if (V>=fvDc40) and (V<fvDc50) then
        tempstream.WriteByte(Ord(V));
      {$ENDREGION}

      {$REGION '3. Filename (version 1+)'}
      if V = fvDc40 then
      begin
        // Ver1: Clear text filename, terminated with "?"
        if AParameters.ContainFileOrigName = fpExpose then
          OrigName := RawByteString(ExtractFileName(AFileName)) // ANSI
        else
          OrigName := '';
        tempstream.WriteRawByteString(OrigName + '?');
      end
      else if V = fvDc41Beta then
      begin
        // Ver2: Base64 encoded filename, terminated with "?"
        if AParameters.ContainFileOrigName = fpExpose then
          OrigName := RawByteString(TNetEncoding.Base64.Encode(ExtractFileName(AFileName))) // ANSI
        else
          OrigName := '';
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
        if AParameters.ContainFileOrigName = fpHide then
          OrigName := ''
        else
        begin
          tmpWS := WideString(ExtractFileName(AFileName));
          SetLength(OrigName, Length(tmpWS)*SizeOf(WideChar));
          if Length(tmpWS) > 0 then
            Move(tmpWS[Low(tmpWS)], OrigName[Low(OrigName)], Length(tmpWS)*SizeOf(WideChar))
          else
            OrigName := '';
        end;
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
          OrigNameEncrypted := BytesToRawByteString(TDECFormattedCipher(Cipher).EncodeBytes(BytesOf(OrigName)));
        finally
          Cipher.Done;
        end;
        tempstream.WriteLongBE(Length(OrigNameEncrypted));
        tempstream.WriteRawByteString(OrigNameEncrypted);
      end
      else if V >= fvDc50 then
      begin
        // Ver4: Clear text filename, with length byte in front of it
        // Possible values:
        // - Original name in its entirety (example "foobar.txt")    <-- we choose this
        // - Just its extension (example "*.txt")
        // - Redacted (empty string "")
        if AParameters.ContainFileOrigName = fpExpose then
          OrigName := UTF8Encode(ExtractFileName(AFileName))
        else
          OrigName := '';
        if Length(OrigName) > 255 then
          raise Exception.Create(SFilenameTooLong);
        tempstream.WriteByte(Length(OrigName));
        tempstream.WriteRawByteString(OrigName);
      end;
      {$ENDREGION}

      {$REGION '3.1 File Size (version 4+)'}
      if V >= fvDc50 then
      begin
        if AParameters.ContainFileOrigSize and IsFolder then
          tempstream.WriteInt64(-1)  // TODO (not so important): Implement directory GetSize
        else if AParameters.ContainFileOrigSize then
          tempstream.WriteInt64(Int64(TFile.GetSize(AFileName)))
        else
          tempstream.WriteInt64(-1);
      end;
      {$ENDREGION}

      {$REGION '3.2 File Date/Time (version 4+)'}
      if V >= fvDc50 then
      begin
        if AParameters.ContainFileOrigDate and IsFolder then
          tempstream.WriteInt64(-1) // TODO (not so important): Implement directory last write time
        else if AParameters.ContainFileOrigDate then
          tempstream.WriteInt64(DateTimeToUnix(TFile.GetLastWriteTime(AFileName), false))
        else
          tempstream.WriteInt64(-1);
      end;
      {$ENDREGION}

      {$REGION '4. IdBase (only version 2 and 3)'}
      idBase := DC4_ID_BASES[V];
      if (V >= fvDc41Beta) and (V < fvDc50) then
        tempstream.WriteLongBE(idBase);
      {$ENDREGION}

      {$REGION '5. Cipher identity (version 0 and 2+)'}
      Assert(not CipherClass.InheritsFrom(TCipher_VtsDeCoderOldCipher)); // You must not use DC2 or DC3 ciphers in DC4/DC5 files
      if V <> fvDc40 then
        tempstream.WriteLongBE(DC_DEC_Identity(idBase, CipherClass.ClassName, V<fvDc50));
      {$ENDREGION}

      {$REGION '6. Cipher mode (version 0 and 2+)'}
      if V <> fvDc40 then tempstream.WriteByte(Ord(Cipher.Mode));
      {$ENDREGION}

      {$REGION '7. Hash identity (version 0 and 2+)'}
      if V <> fvDc40 then
        tempstream.WriteLongBE(DC_DEC_Identity(idBase, HashClass.ClassName, V<fvDc50));
      {$ENDREGION}

      {$REGION '7.5 IV (only version 4+)'}
      if V >= fvDc50 then
      begin
        tempstream.WriteByte(Length(IV));
        tempstream.WriteRawBytes(IV);
      end;
      {$ENDREGION}

      {$REGION '7.6 IV Fill Byte (only version 4+)'}
      if V >= fvDc50 then
      begin
        tempstream.WriteByte(IvFillByte);
      end;
      {$ENDREGION}

      {$REGION '7.7 Cipher block filling mode (only version 4+; currently unused by DEC)'}
      if V >= fvDc50 then
      begin
        tempstream.WriteByte(Ord(Cipher.FillMode));
      end;
      {$ENDREGION}

      {$REGION '8. Seed (only version 0 or version 2+)'}
      if V <> fvDc40 then tempstream.WriteByte(Length(Seed));
      tempstream.WriteRawByteString(Seed);
      {$ENDREGION}

      {$REGION '8.5 KDF version (only version 4+)'}
      if V >= fvDc50 then
      begin
        // 1=KDF1, 2=KDF2, 3=KDF3, 4=KDFx, 5=PBKDF2
        tempstream.WriteByte(Ord(KdfVersion));
      end;
      {$ENDREGION}

      {$REGION '8.6 PBKDF Iterations (only for KdfVersion=kvPbkdf2)'}
      if KdfVersion = kvPbkdf2 then
        tempstream.WriteInt32(PbkdfIterations);
      {$ENDREGION}

      {$REGION '8.7 GCM Tag length (only if GCM mode)'}
      if (V>=fvDc50) and (Cipher.Mode=cmGCM) then
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
          tempstream.WriteLongBE(Source.Size);
        TDECFormattedCipher(Cipher).EncodeStream(Source, tempstream, source.size, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SEncodeStream, TDcProgressState(State))
          end);
      finally
        Cipher.Done;
      end;
      {$ENDREGION}

      {$REGION '9.1 DEC CalcMAC (not if ECB mode)'}
      if ((V=fvHagenReddmannExample) or (V>=fvDc50)) and (Cipher.Mode<>cmECBx) then
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
      if (V>=fvDc50) and (Cipher.Mode=cmGCM) then
      begin
        tempstream.WriteRawBytes(TDECFormattedCipher(Cipher).CalculatedAuthenticationResult);
      end;
      {$ENDREGION}

      {$REGION '10. Hash/HMAC (version 1-3 hash on source, version 4+ hmac on encrypted file)'}
      if V = fvDc40 then
      begin
        Source.Position := 0;
        TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SCalcHash, TDcProgressState(State))
          end);
        tempstream.WriteRawBytes(HashResult);
      end
      else if V = fvDc41Beta then
      begin
        Source.position := 0;
        TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SCalcHash, TDcProgressState(State))
          end);
        HashResult2 := TDECHashExtended(ahash).CalcString(BytesToRawByteString(HashResult)+Seed+PasswordRBS, TFormat_Copy);
        tempstream.WriteRawByteString(HashResult2);
      end
      else if V = fvDc41FinalCancelled then
      begin
        Source.position := 0;
        TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SCalcHash, TDcProgressState(State))
          end);
        HashResult2 := TDECHashExtended(ahash).CalcString(
          BytesToRawByteString(HashResult) + Seed +
              TDECHashExtended(ahash).CalcString(
                Seed+TDECHashExtended(ahash).CalcString(Seed+PasswordRBS, TFormat_Copy)
              , TFormat_Copy)
        , TFormat_Copy);
        tempstream.WriteRawByteString(HashResult2);
      end
      else if V >= fvDc50 then
      begin
        tmp64 := tempstream.Position;
        tempstream.Position := 0;
        HashResult2 := BytesToRawByteString(TDECHashAuthentication(ahash).HMACStream(HMacKey, tempstream, tmp64, procedure(Size, Pos: Int64; State: TDECProgressState)
          begin
            if Assigned(OnProgressProc) then
              OnProgressProc(Size, Pos, SCalcHMac, TDcProgressState(State))
          end));
        tempstream.Position := tmp64;
        tempstream.WriteRawByteString(HashResult2);
      end;
      {$ENDREGION}

      {$REGION '11. File Terminus (only version 2 and 3)'}
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
      if IsFolder and (SourceFile<>'') and FileExists(SourceFile) then
        SecureDeleteFile(SourceFile); // delete temporary 7z/zip file
      if IsZLibCompressed and (ATempFileNameZLib<>'') and FileExists(ATempFileNameZLib) then
        SecureDeleteFile(ATempFileNameZLib);
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

function _DeCoder4X_DecodeFile(const AFileName: string; var AOutput: String; const APassword: string; OnProgressProc: TDcProgressEvent=nil): TDC4FileInfo;
var
  Source: TFileStream;
  OrigFileSize: Int64;
  OrigFileDate: Int64;
  ch: RawByteString;
  F: byte;
  V: TDc4FormatVersion;
  V_Detected: boolean;
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
  PbkdfIterations: Long;
  OrigNameEncrypted: RawByteString;
  iBlockFillMode: Byte;
  cMac: RawByteString;
  iTmp: integer;
  OnlyReadFileInfo: boolean;
  outFileDidExist: boolean;
  PasswordRBS: RawByteString;
  (*
  IsFolder   IsZLib    ATempFileNameZipOrDirectOutput   ATempFileNameZLib    tempstream
  ---------------------------------------------------------------------------------------
  false      false     AOutput                          undefined            AOutput
  false      true      AOutput                          AOutput_tmp.z        AOutput_tmp.z
  true       false     AOutput_tmp.7z                   undefined            AOutput_tmp.7z
  true       true      AOutput_tmp.7z                   AOutput_tmp.z        AOutput_tmp.z
  *)
  ATempFileNameZLib: string; // If ZLibCompressed, then =AOutput+.z, otherwise undefined
  ATempFileNameZipOrDirectOutput: string; // If IsFolder, then =AOutput+.7z, else =AOutput
resourcestring
  SFileOrFolderNotFound = 'File or folder %s not found';
  SFormatNotSupported = 'Format not supported';
  SUnsupportedFileFormatVersion = 'Unsupported file format version. Please try downloading the latest version of (De)Coder';
  SInvalidFileName = 'Invalid file name';
  SInvalidBlockFillingMode = 'Invalid block filling mode';
  SInvalidKdfVersion = 'Invalid KDF version';
  SEmptyPasswordNotAllowed = 'An empty password is not allowed';
  SVerifyHMac = 'Verify HMAC';
  SHMacMismatchPwdWrong = 'HMAC mismatch. The password is probably wrong or the file is corrupt.';
  SDecodeStream = 'Decode stream';
  SDecCalcMacMismatch = 'DEC CalcMAC mismatch';
  SGCMAuthTagMismatch = 'GCM Auth Tag mismatch';
  SVerifyHash = 'Verify hash';
  SHMacMismatch = 'HMAC mismatch. Password wrong or file corrupt.';
  SHashMismatch = 'Hash mismatch. Password wrong or file corrupt.';
  SFileTerminusWrong = 'File terminus wrong';
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt(SFileOrFolderNotFound, [AFileName]);
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  tempstream := nil;
  cipher := nil;
  ahash := nil;
  IsZLibCompressed := false;
  IsFolder := false;
  OnlyReadFileInfo := AOutput = '';
  ATempFileNameZLib := '';
  ATempFileNameZipOrDirectOutput := '';
  OrigFileSize := -1;
  OrigFileDate := -1;
  outFileDidExist := FileExists(AOutput);
  V_Detected := false;
  V := fvHagenReddmannExample; // to avoid that the compiler complains
  try
    try
      try
        {$REGION 'Is it the Hagen Reddmann example file format?'}
        CipherClass := DC_DEC_CipherById(DC4_ID_BASES[fvHagenReddmannExample], Source.ReadLongBE, true, true);
        if Assigned(CipherClass) then
        begin
          V := fvHagenReddmannExample;
          V_Detected := true;
        end
        else
        begin
          Source.Position := 0;
        end;
        {$ENDREGION}

        {$REGION '0.5 Magic sequence (version 4+)'}
        if not V_Detected then
        begin
          for V := fvDc50 to High(TDc4FormatVersion) do
          begin
            MagicSeq := '[' + RawByteString(DeCoder4X_GetOID(V)) + ']' + #13#10;
            if (Source.Size >= Length(MagicSeq)) and (Source.ReadRawByteString(Length(MagicSeq)) = MagicSeq) then
            begin
              V_Detected := true;
              break;
            end
            else
              Source.Position := 0;
          end;
        end;
        {$ENDREGION}

        {$REGION '1. Flags (version 1+)'}
        if not V_Detected or (V_Detected and (V>=fvDc40)) then
        begin
          // Bit 0:    [Ver1+] Is packed folder (1) or a regular file (0)?
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

        {$REGION '2. Version byte (only version 1..3)'}
        if not V_Detected then
        begin
          // 01 = (De)Coder 4.0
          // 02 = (De)Coder 4.1 Beta
          // 03 = (De)Coder 4.1 Final Cancelled (never released)
          iTmp := Source.ReadByte;
          if (iTmp < {Low(TDc4FormatVersion)}Ord(fvDc40)) or (iTmp > {High(TDc4FormatVersion)}Ord(fvDc41FinalCancelled)) then
            raise Exception.Create(SFormatNotSupported);
          V := TDc4FormatVersion(iTmp);
          V_Detected := true;
        end;
        {$ENDREGION}

        if not V_Detected then
          raise Exception.Create(SUnsupportedFileFormatVersion);

        {$REGION 'Create output stream'}
        if not OnlyReadFileInfo then
        begin
          if IsFolder then
          begin
            if V >= fvDc50 then
              ATempFileNameZipOrDirectOutput := ExcludeTrailingPathDelimiter(AOutput) + '_Tmp_'+RandStringFileNameFriendly(10)+'.7z'
            else if V >= fvDc50 then
              ATempFileNameZipOrDirectOutput := ExcludeTrailingPathDelimiter(AOutput) + '_Tmp_'+RandStringFileNameFriendly(10)+'.zip'
            else
              Assert(False);
          end
          else
          begin
            ATempFileNameZipOrDirectOutput := AOutput;
          end;

          if IsZLibCompressed then
          begin
            ATempFileNameZLib := ChangeFileExt(AOutput, '_Tmp_' + RandStringFileNameFriendly(10) + '.z');
            tempstream := TFileStream.Create(ATempFileNameZLib, fmCreate);
          end
          else
          begin
            tempstream := TFileStream.Create(ATempFileNameZipOrDirectOutput, fmCreate);
          end;
        end;
        {$ENDREGION}

        // Note in re Hagen Redmann example: From here, variable V can be used to check it

        if V >= fvDc50 then
          PasswordRBS := UTF8Encode(APassword)  // version 4+: Password treated as UTF-8
        else
          PasswordRBS := AnsiString(APassword); // version 0..3: Password treated as ANSI

        {$REGION 'Decide about file terminus'}
        if (V = fvHagenReddmannExample) or (V = fvDc40) then
        begin
          FileTerminus := '';
        end
        else if V = fvDc41Beta then
        begin
          FileTerminus := RawByteString(TNetEncoding.Base64.Encode('DCTERMINUS'));
        end
        else if V = fvDc41FinalCancelled then
        begin
          FileTerminus := RawByteString(#$63#$F3#$DF#$89#$B7#$27#$20#$EA);
        end
        else
        begin
          FileTerminus := '';
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
              if (Ord(ch[Low(ch)])<32){ or (Ord(ch[Low(ch)])>126)} then
                raise Exception.Create(SInvalidFileName);
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
            //     Encryption-Password = Hash->KDfx(User-Password, Seed)
            // if not encrypted with user-password, otherwise:
            //     Encryption-Password = Hash->KDfx(5Eh D1h 6Bh 12h 7Dh B4h C4h 3Ch, Seed)
            OrigNameEncrypted := Source.ReadRawByteString(Source.ReadLongBE); // will be decrypted below (after we initialized hash/cipher)
          end
          else if V >= fvDc50 then
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

        {$REGION '3.1 File Size (version 4+)'}
        if V >= fvDc50 then
        begin
          OrigFileSize := Source.ReadInt64;
        end;
        {$ENDREGION}

        {$REGION '3.2 File Date/Time (version 4+)'}
        if V >= fvDc50 then
        begin
          OrigFileDate := Source.ReadInt64;
        end;
        {$ENDREGION}

        {$REGION '4. IdBase (only version 2 and 3)'}
        if (V >= fvDc41Beta) and (V < fvDc50) then
          idBase := Source.ReadLongBE
        else
          idBase := DC4_ID_BASES[V];
        {$ENDREGION}

        {$REGION '5. Cipher identity (only version 0 or 2+)'}
        if V <> fvHagenReddmannExample then // If V=fvHagenReddmannExample, then we already checked it and the stream position should be 4.
        begin
          // Now query with the new actual fitting id base version (dependant on V)
          if V = fvDc40 then
            CipherClass := TCipher_AES
          else
            CipherClass := DC_DEC_CipherById(idBase, Source.ReadLongBE, V<fvDc50);
        end;
        if (V < fvDc50) and (CipherClass = TCipher_SCOP) then Cipherclass := TCipher_SCOP_DEC52; // unclear if it was faulty in DEC 5.2 or DEC 5.1c
        if (V < fvDc50) and (CipherClass = TCipher_XTEA) then Cipherclass := TCipher_XTEA_DEC52; // XTEA was not existing in DEC 5.1c, so it must be a DEC 5.2 problem only
        if (V < fvDc50) and (CipherClass = TCipher_Shark) then Cipherclass := TCipher_Shark_DEC52; // It didn't work in DEC 5.1c
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
          HashClass := DC_DEC_HashById(idBase, Source.ReadLongBE, V<fvDc50);
        AHash := HashClass.Create;
        {$ENDREGION}

        {$REGION '7.5 IV (only version 4+)'}
        if V >= fvDc50 then
          IV := Source.ReadRawBytes(Source.ReadByte)
        else
          SetLength(IV, 0);
        {$ENDREGION}

        {$REGION '7.6 IV Fill Byte (only version 4+)'}
        if V >= fvDc50 then
          IvFillByte := Source.ReadByte
        else
          IvFillByte := $FF;
        {$ENDREGION}

        {$REGION '7.7 Cipher block filling mode (only version 4+; currently unused by DEC)'}
        if V >= fvDc50 then
        begin
          iBlockFillMode := Source.ReadByte;
          if integer(iBlockFillMode) > Ord(High(TBlockFillMode)) then
            raise Exception.Create(SInvalidBlockFillingMode);
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
        if V >= fvDc50 then
        begin
          iTmp := Source.ReadByte;
          if (iTmp<Ord(Low(TKdfVersion))) or (iTmp>Ord(High(TKdfVersion))) then
            KdfVersion := kvUnknown
          else
            KdfVersion := TKdfVersion(iTmp);
        end
        else
          KdfVersion := kvKdfx;
        if KDFVersion = kvUnknown then {this will also be set if the value is too big}
          raise Exception.Create(SInvalidKdfVersion);
        {$ENDREGION}

        {$REGION '8.6 KDF Iterations (ONLY PRESENT for PBKDF2)'}
        if KDFVersion = kvPbkdf2 then
          PbkdfIterations := Source.ReadInt32
        else
          PbkdfIterations := 0;
        {$ENDREGION}

        {$REGION 'Generate key used by HMAC and Cipher'}
        if not OnlyReadFileInfo then
        begin
          if PasswordRBS = '' then
            raise Exception.Create(SEmptyPasswordNotAllowed);

          if KDFVersion = kvKdfx then
            Key := TDECHashExtended(ahash).KDFx(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvKdf1 then
            Key := TDECHashExtended(ahash).KDF1(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvKdf2 then
            Key := TDECHashExtended(ahash).KDF2(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvKdf3 then
            Key := TDECHashExtended(ahash).KDF3(BytesOf(PasswordRBS), BytesOf(Seed), Cipher.Context.KeySize)
          else if KDFVersion = kvPbkdf2 then
            Key := TDECHashExtended(ahash).PBKDF2(BytesOf(PasswordRBS), BytesOf(Seed), PbkdfIterations, Cipher.Context.KeySize)
          else
            Assert(False);

          HMacKey := Key;
        end;
        {$ENDREGION}

        {$REGION 'Verify HMAC of whole file before decrypting (version 4+)'}
        if not OnlyReadFileInfo and (V >= fvDc50) then
        begin
          bakSourcePosEncryptedData := Source.Position;
          Source.Position := 0;
          HashResult2 := BytesToRawByteString(TDECHashAuthentication(ahash).HMACStream(HMacKey, Source, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), procedure(Size, Pos: Int64; State: TDECProgressState)
            begin
              if Assigned(OnProgressProc) then
                OnProgressProc(Size, Pos, SVerifyHMac, TDcProgressState(State))
            end));
          Source.Position := Source.Size - ahash.DigestSize - Length(FileTerminus);
          if Source.ReadRawByteString(ahash.DigestSize) <> HashResult2 then
            raise Exception.Create(SHMacMismatchPwdWrong);
          Source.Position := bakSourcePosEncryptedData;
        end;
        {$ENDREGION}

        {$REGION '8.7 GCM Tag Length (only version 4+)'}
        if not OnlyReadFileInfo and (V>=fvDc50) and (Cipher.Mode = cmGCM) then
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
              TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, Source.ReadLongBE, procedure(Size, Pos: Int64; State: TDECProgressState)
                begin
                  if Assigned(OnProgressProc) then
                    OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
                end);
            end
            else
            begin
              iTmp := source.size - source.Position;
              if ((V=fvHagenReddmannExample) or (V>=fvDc50)) and (Cipher.Mode <> cmECBx) then Dec(iTmp, Cipher.Context.BlockSize{CalcMac}); // field 9.1
              if (V>=fvDc50) and (Cipher.Mode = cmGCM) then Dec(iTmp, TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3); // field 9.2
              Dec(iTmp, ahash.DigestSize{Hash/HMAC}); // field 10
              Dec(iTmp, Length(FileTerminus)); // field 11
              TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, iTmp, procedure(Size, Pos: Int64; State: TDECProgressState)
                begin
                  if Assigned(OnProgressProc) then
                    OnProgressProc(Size, Pos, SDecodeStream, TDcProgressState(State))
                end);
            end;
          finally
            Cipher.Done;
          end;
        end;
        {$ENDREGION}

        {$REGION '9.1 DEC CalcMAC (version 0 with length prefix, version 4+ without)'}
        if not OnlyReadFileInfo and ((V=fvHagenReddmannExample) or (V>=fvDc50)) and (Cipher.Mode <> cmECBx) then
        begin
          if V=fvHagenReddmannExample then
            cMac := Source.ReadRawByteString(Source.ReadByte)
          else
            cMac := Source.ReadRawByteString(Cipher.Context.BlockSize);
          if cMac <> Cipher.CalcMAC then
            raise Exception.Create(SDecCalcMacMismatch);
        end;
        {$ENDREGION}

        {$REGION '9.2 GCM Tag (only version 4+)'}
        if not OnlyReadFileInfo and (V>=fvDc50) and (Cipher.Mode = cmGCM) then
        begin
          if BytesToRawByteString(TDECFormattedCipher(Cipher).CalculatedAuthenticationResult) <> Source.ReadRawByteString(TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3) then
            raise Exception.Create(SGCMAuthTagMismatch);
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
              OrigNameEncrypted := BytesToRawByteString(TDECFormattedCipher(Cipher).DecodeBytes(BytesOf(OrigNameEncrypted)));
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
        if not OnlyReadFileInfo and (V >= fvDc40) and (V < fvDc50) then
        begin
          if V = fvDc40 then
          begin
            tempstream.position := 0;
            TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, procedure(Size, Pos: Int64; State: TDECProgressState)
              begin
                if Assigned(OnProgressProc) then
                  OnProgressProc(Size, Pos, SVerifyHash, TDcProgressState(State))
              end);
            HashResult2 := BytesToRawByteString(HashResult);
          end
          else if V = fvDc41Beta then
          begin
            tempstream.position := 0;
            TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, procedure(Size, Pos: Int64; State: TDECProgressState)
              begin
                if Assigned(OnProgressProc) then
                  OnProgressProc(Size, Pos, SVerifyHash, TDcProgressState(State))
              end);
            HashResult2 := TDECHashExtended(ahash).CalcString(BytesToRawByteString(HashResult)+Seed+PasswordRBS, TFormat_Copy);
          end
          else if V = fvDc41FinalCancelled then
          begin
            tempstream.position := 0;
            TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, procedure(Size, Pos: Int64; State: TDECProgressState)
              begin
                if Assigned(OnProgressProc) then
                  OnProgressProc(Size, Pos, SVerifyHash, TDcProgressState(State))
              end);
            HashResult2 := TDECHashExtended(ahash).CalcString(
              BytesToRawByteString(HashResult) + Seed +
                  TDECHashExtended(ahash).CalcString(
                    Seed+TDECHashExtended(ahash).CalcString(Seed+PasswordRBS, TFormat_Copy)
                  , TFormat_Copy)
            , TFormat_Copy);
          end
          else
            Assert(False);

          if Source.ReadRawByteString(ahash.DigestSize) <> HashResult2 then
          begin
            if V >= fvDc50 then
              raise Exception.Create(SHMacMismatch)
            else
              raise Exception.Create(SHashMismatch);
          end;
        end;
        {$ENDREGION}

        {$REGION '11. Terminus (only version 2 and 3)'}
        if FileTerminus <> '' then
        begin
          if OnlyReadFileInfo then Source.Position := Source.Size - Length(FileTerminus);
          if (Source.ReadRawByteString(Length(FileTerminus)) <> FileTerminus) then
            raise Exception.Create(SFileTerminusWrong);
        end;
        {$ENDREGION}

        {$REGION 'Decompress stuff'}
        if not OnlyReadFileInfo then
        begin
          FreeAndNil(tempstream);

          if IsZLibCompressed then
          begin
            // If IsFolder, then decompress AOutput_tmp.z to AOutput_tmp.7z
            //              else decompress AOutput_tmp.z to AOutput
            ZLib_Decompress(ATempFileNameZLib, ATempFileNameZipOrDirectOutput, OnProgressProc);
          end;

          if IsFolder then
          begin
            if V >= fvDc40 then
            begin
              try
                // version 1..3 = ZIP
                // version 4+ = 7zip
                SevenZipExtract(ATempFileNameZipOrDirectOutput, RelToAbs(AOutput), OnProgressProc);
              except
                // Can happen if the 7z.*.dll files are missing. Let the user unpack themselves
                // Remove the temp stuff from the name
                if RenameFile(ATempFileNameZipOrDirectOutput, AOutput + ExtractFileExt(ATempFileNameZipOrDirectOutput)) then
                begin
                  // and let the calling procedure know that the name is different
                  AOutput := AOutput + ExtractFileExt(ATempFileNameZipOrDirectOutput);
                end
                else
                begin
                  // and let the calling procedure know that the name is different
                  AOutput := ATempFileNameZipOrDirectOutput;
                end;
                // and avoid that the file is deleted (if renaming failed)
                ATempFileNameZipOrDirectOutput := '';
              end;
            end
            else
            begin
              Assert(False);
            end;
          end;
        end;
        {$ENDREGION}

        {$REGION 'Output file info and parameters'}
        ZeroMemory(@result, Sizeof(result));
        result.IsZLibCompressed := IsZLibCompressed;
        result.IsCompressedFolder := IsFolder;
        result.OrigFileName := OrigName;
        result.OrigFileSize := OrigFileSize;
        result.OrigFileDate := OrigFileDate;
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
        if (V = fvDc41FinalCancelled) and FileNameUserPasswordEncrypted then
          result.Parameters.ContainFileOrigName := fpEncryptWithUserKey
        else if (V >= fvDc50) and (OrigName = '') then
          result.Parameters.ContainFileOrigName := fpHide
        else
          result.Parameters.ContainFileOrigName := fpExpose;
        result.Parameters.ContainFileOrigSize := V >= fvDc50;
        result.Parameters.ContainFileOrigDate := V >= fvDc50;
        if (V >= fvDc50) and (result.Parameters.CipherMode = cmGCM) then
          result.Parameters.GCMAuthTagSizeInBytes := TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3
        else
          result.Parameters.GCMAuthTagSizeInBytes := 0;
        DeCoder4X_ValidateParameterBlock(result.Parameters); // Should always pass
        {$ENDREGION}
      except
        if Assigned(tempstream) then ProtectStream(tempstream);
        raise;
      end;
    finally
      if Assigned(Source) then FreeAndNil(Source);
      if Assigned(tempstream) then FreeAndNil(tempstream);
      if Assigned(Cipher) then FreeAndNil(Cipher);
      if Assigned(ahash) then FreeAndNil(ahash);
      if IsZLibCompressed and (ATempFileNameZLib<>'') and FileExists(ATempFileNameZLib) then
        SecureDeleteFile(ATempFileNameZLib);
      if IsFolder and (ATempFileNameZipOrDirectOutput<>'') and FileExists(ATempFileNameZipOrDirectOutput) then
        SecureDeleteFile(ATempFileNameZipOrDirectOutput);
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

function DeCoder4X_FileInfo(const AFileName: String; const APassword: string=''; OnProgressProc: TDcProgressEvent=nil): TDC4FileInfo;
var
  dummy: string;
begin
  dummy := '';
  result := _DeCoder4X_DecodeFile(AFileName, dummy, APassword, OnProgressProc);
end;

function DeCoder4X_DetectFile(const AFileName: string): boolean;
begin
  try
    DeCoder4X_FileInfo(AFileName);
    result := true;
  except
    result := false;
  end;
end;

function DeCoder4X_DecodeFile(const AFileName: string; var AOutput: String; const APassword: string; OnProgressProc: TDcProgressEvent=nil): TDC4FileInfo;
resourcestring
  SOutputFilenameEmpty = 'Output filename must not be empty';
begin
  if AOutput = '' then raise Exception.Create(SOutputFilenameEmpty);
  result := _DeCoder4X_DecodeFile(AFileName, AOutput, APassword, OnProgressProc);
end;

initialization
  RandomSeed;
end.
