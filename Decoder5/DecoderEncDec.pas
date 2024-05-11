unit DecoderEncDec;

interface

uses
  Windows, SysUtils, Classes, DECFormatBase, DECTypes,
  System.UITypes, DECCiphers, DECCipherBase, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECCipherFormats, 
  EncdDecd, System.NetEncoding, DECCRC, DECBaseClass, Generics.Collections,
  DECRandom, System.IOUtils;

type
  TDc4FormatVersion = (fvHagenReddmannExample, fvDc40, fvDc41Beta, fvDc41FinalCancelled, fvDc50Wip);

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

function DeCoder4X_GetDefaultParameters(V: TDc4FormatVersion): TDC4Parameters;
procedure DeCoder4X_ValidateParameterBlock(AParameters: TDC4Parameters);

procedure DeCoder4X_EncodeFile(const AFileName, AOutput: String; const APassword: RawByteString; AParameters: TDC4Parameters; OnProgressProc: TDECProgressEvent=nil);
function DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;

// Note: A password is only required for reading an user-key-encrypted filename (feature was only available in format version 3)
function DeCoder4X_FileInfo(const AFileName: String; const APassword: RawByteString=''; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

{$IFDEF Debug}
procedure Debug_ListHashAlgos(Lines: TStrings; V: TDc4FormatVersion);
procedure Debug_ListCipherAlgos(Lines: TStrings; V: TDc4FormatVersion);
{$ENDIF}

implementation

uses
  DecoderOldCiphers, DecoderFuncs, DateUtils, StrUtils, Math;

const
  DC4_ID_BASES: array[Low(TDc4FormatVersion)..High(TDc4FormatVersion)] of Int64 = (
    $84485225, // Hagen Reddmann Example (no .dc4 files)
    $59178954, // (De)Coder 4.0 (identities not used)
    $84671842, // (De)Coder 4.1 beta
    $19387612, // (De)Coder 4.1 final/cancelled
    $1259d82a  // (De)Coder 5.0 WIP
  );

  // This is the OID { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 37476 products(2) decoder(2) fileformat(1) dc4(4) }
  DC4_OID = '1.3.6.1.4.1.37476.2.2.1.4';

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

function DC_DEC_Identity(IdentityBase: Int64; ClassName: string; dc51compat: boolean): Int64;
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

    // You must not use DC2 or DC3 ciphers in DC4/DC5 files
    // Also, these ciphers/hashes were not available in older DEC, so we don't accept them
    if StartsText('TCipher_VtsDeCoder', cn) or
       not DC_DEC_ClassExistedInDEC51(cn) then
    begin
      Exit(-1); // raise Exception.Create('This DEC class did not exist in old DEC 5.1c');
    end;

    Signature := AnsiString   (StringOfChar(#$5A, 256 - Length(cn)) + AnsiUpperCase(cn));
    Result := CRC32(IdentityBase, Signature[1], Length(Signature));
  end
  else
  begin
    // Code copied from TDECObject.Identity
    // we cannot use TDECClassList.ClassByIdentity, because it does not allow switching of IdentifyBase
    Signature := RawByteString(StringOfChar(#$5A, 256 - Length(cn)) + UpperCase(cn));
    Result := CRC32(IdentityBase, Signature[Low(Signature)], Length(Signature) * SizeOf(Signature[Low(Signature)]));
  end;
end;

function DC_DEC_HashById(IdentityBase, Identity: Int64; dc51compat: boolean; NoException: boolean=false): TDECHashClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
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
    raise Exception.CreateFmt('Hash ID %.8x with base %.8x not found', [Identity, IdentityBase]);
end;

function DC_DEC_CipherById(IdentityBase, Identity: Int64; dc51compat: boolean; NoException: boolean=false): TDECCipherClass;
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
begin
  for p in TDECCipher.ClassList do
  begin
    c := p.Value;
    if (c <> nil) and
       not StartsText('TCipher_VtsDeCoder', c.ClassName) and
       (Identity = DC_DEC_Identity(IdentityBase, c.ClassName, dc51compat)) then
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

{$IFDEF Debug}
procedure Debug_ListHashAlgos(Lines: TStrings; V: TDc4FormatVersion);
var
  p: TPair<int64, TDECClass>;
  c: TDECClass;
  cn: string;
  sl: TStringList;
  addinfo: string;
begin
  sl := TStringList.Create;
  try
    for p in TDECHash.ClassList do
    begin
      c := p.Value;
      cn := c.ClassName;
      if (V<fvDc50Wip) and (cn='THash_Whirlpool0') then cn := 'THash_Whirlpool';
      if (V<fvDc50Wip) and (cn='THash_SHA0') then cn := 'THash_SHA';
      if (V<fvDc50Wip) and not DC_DEC_ClassExistedInDEC51(cn) then continue;
      addinfo := '';
      if (V>=fvDc50Wip) and (cn='THash_SHA3_512') then addinfo := ', default'
      else if (V>=fvDc40) and (V<fvDc50Wip) and (cn='THash_SHA512') then addinfo := ', default'
      else if (V=fvHagenReddmannExample) and (cn='THash_SHA1') then addinfo := ', default';
      sl.Add(
        '0x'+IntToHex(DC_DEC_Identity(DC4_ID_BASES[V], cn, V<fvDc50Wip), 8) + #9 +
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
  sl := TStringList.Create;
  try
    for p in TDECCipher.ClassList do
    begin
      c := p.Value;
      cn := c.ClassName;
      if StartsText('TCipher_VtsDeCoder', cn) then continue;
      if (V<fvDc50Wip) and (cn='TCipher_AES') then cn := 'TCipher_Rijndael';
      if (V<fvDc50Wip) and not DC_DEC_ClassExistedInDEC51(cn) then continue;
      addinfo := '';
      if (V<fvDc50Wip) and (cn='TCipher_Shark') then addinfo := ', faulty implementation';
      if (V<fvDc50Wip) and (cn='TCipher_SCOP') then addinfo := ', faulty implementation?';
      if (V<fvDc50Wip) and (cn='TCipher_Rijndael') then addinfo := ', default';
      if (V>=fvDc50Wip) and (cn='TCipher_AES') then addinfo := ', default';
      sl.Add(
        '0x'+IntToHex(DC_DEC_Identity(DC4_ID_BASES[V], cn, V<fvDc50Wip), 8) + #9 +
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

procedure DeCoder10_EncodeFile(const AFileName, AOutput: String; ForceUpperCase: boolean; OnProgressProc: TDECProgressEvent=nil);
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
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  tempstream := nil;
  try
    try
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, 0, TDECProgressState.Started);
      tempstream := TFileStream.Create(AOutput, fmCreate);
      tempstream.WriteRawByteString('COD'+#1#1);
      for I := 1 to 27 do
        let[I] := AnsiChar(Chr(199+I));
      while Source.Position < Source.Size do
      begin
        if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Position, TDECProgressState.Processing);
        rbsIn := Source.ReadRawByteString(Min(chunksize,Source.Size-Source.Position));
        rbsOut := '';
        for j := Low(rbsIn) to High(rbsIn) do
        begin
          ch := rbsIn[j];
          if (Ord(ch)<32) or (Ord(ch)>126) then
            raise Exception.Create('Only binary files can be encrypted with (De)Coder 1.0');
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
      tempstream.WriteRawByteString(#1#1#1);
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Size, TDECProgressState.Finished);
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
  foundchar: boolean;
  I, J: Integer;
  rbsIn: RawByteString;
  rbsOut: RawByteString;
  outFileDidExist: boolean;
const
  chunksize = 4096*3; // bigger = faster. Must be multiple of 3
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  tempstream := nil;
  try
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
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, 0, TDECProgressState.Started);
      for ch := 'A' to 'Z' do
        let[Ord(ch)-Ord('A')+1] := ch;
      for I := 1 to 27 do
        inl[I] := AnsiChar(Chr(199+I));
      SetLength(b,3);
      while Source.Position < Source.Size-3 do
      begin
        if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Position, TDECProgressState.Processing);
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
      if Assigned(OnProgressProc) then OnProgressProc(Source.Size, Source.Size, TDECProgressState.Finished);
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

procedure DeCoder20_EncodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder20_DecodeFile(const AFileName, AOutput: String; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder21_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder21_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder22_EncodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder22_DecodeFile(const AFileName, AOutput: String; Key: integer; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder30_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder30_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder32_EncodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

procedure DeCoder32_DecodeFile(const AFileName, AOutput: String; Key: AnsiString; OnProgressProc: TDECProgressEvent=nil);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  ssIn := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  outFileDidExist := FileExists(AOutput);
  try
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
  if V = fvDc41FinalCancelled then
    Result.ContainFileOrigName := fpEncryptWithUserKey // only available in version 3
  else if V < fvDc50Wip then
    Result.ContainFileOrigName := fpExpose
  else
    Result.ContainFileOrigName := fpHide; // default disabled for privacy
  if (V >= fvDc50Wip) and (result.CipherMode = cmGCM) then
    result.GCMAuthTagSizeInBytes := 128 shr 3
  else
    result.GCMAuthTagSizeInBytes := 0;
  result.ContainFileOrigSize := false; // available since ver4, but disabled by default
  result.ContainFileOrigDate := false; // available since ver4, but disabled by default
end;

procedure DeCoder4X_ValidateParameterBlock(AParameters: TDC4Parameters);
begin
  //if (AParameters.Dc4FormatVersion < fvDc40) and (AParameters.IsCompressedFolder) then
  //  raise Exception.Create('ZIP folder requires DC41beta+ version');

  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.HashClass <> THash_SHA512) then
    raise Exception.Create('Hash not accepted in DC40 version');
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.CipherClass <> TCipher_Rijndael) and (AParameters.CipherClass <> TCipher_AES) then
     raise Exception.Create('Cipher not accepted in DC40 version');
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.KDF <> kvKdfx) then
    raise Exception.Create('KDF version not accepted in DC40 version');
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.SeedSize <> 16) then
    raise Exception.Create('Seed size not accepted in DC40 version');
  if (AParameters.Dc4FormatVersion = fvDc40) and (AParameters.CipherMode <> cmCTSx) then
    raise Exception.Create('CiperMode not accepted in DC40 version');

  if (AParameters.Dc4FormatVersion < fvDc41Beta) and (AParameters.ShouldBeZLibCompressed <> No) then
    raise Exception.Create('ZLib requires DC41beta+ version');

  if (AParameters.Dc4FormatVersion < fvDc50Wip) and (AParameters.IvFillByte <> $FF) then
    raise Exception.Create('Indiv IvFillByte only accepted format version 4+');
  if (AParameters.Dc4FormatVersion < fvDc50Wip) and (AParameters.IVSizeInBytes <> 0) then
    raise Exception.Create('Indiv IV only accepted format version 4+');
  if (AParameters.Dc4FormatVersion < fvDc50Wip) and (AParameters.KDF <> kvKdfx) then
    raise Exception.Create('Indiv KDF only accepted format version 4+');

  if (AParameters.CipherMode <> cmGCM) and (AParameters.GCMAuthTagSizeInBytes <> 0) then
    raise Exception.Create('GCM Auth Tag only allowed for cipher mode GCM');
  if (AParameters.CipherMode = cmGCM) and (AParameters.GCMAuthTagSizeInBytes = 0) then
    raise Exception.Create('GCM Auth Tag required for cipher mode GCM');
  if (AParameters.CipherMode = cmGCM) and (AParameters.GCMAuthTagSizeInBytes < 4) then
    raise Exception.Create('GCM Auth Tag too small (choose at least 32 bits = 4 bytes)');
  if (AParameters.CipherMode = cmGCM) and (AParameters.GCMAuthTagSizeInBytes > 16) then
    raise Exception.Create('GCM Auth Tag too large (max 128 bits = 16 bytes)'); // This is the output size of CalcGaloisHash

  if (AParameters.KDF <> kvPbkdf2) and (AParameters.PBKDF_Iterations <> 0) then
    raise Exception.Create('PBKDF Iterations must be >0');
  if (AParameters.KDF = kvPbkdf2) and (AParameters.PBKDF_Iterations = 0) then
    raise Exception.Create('PBKDF Iterations must be =0');

  if (AParameters.Dc4FormatVersion <> fvDc41FinalCancelled) and (AParameters.ContainFileOrigName=fpEncryptWithUserKey) then
    raise Exception.Create('Encrypted Filename only accepted in DC41Final version');
  if (AParameters.Dc4FormatVersion < fvDc50Wip) and (AParameters.ContainFileOrigName=fpHide) then
    raise Exception.Create('Orig File Name can only be hidden in format version 4');
  if (AParameters.Dc4FormatVersion < fvDc50Wip) and AParameters.ContainFileOrigSize then
    raise Exception.Create('Orig FileSize only available in format version 4');
  if (AParameters.Dc4FormatVersion < fvDc50Wip) and AParameters.ContainFileOrigDate then
    raise Exception.Create('Orig FileDate only available in format version 4');
end;

procedure DeCoder4X_PrintFileInfo(fi: TDC4FileInfo; sl: TStrings);

  const
    DC4_SUBFORMAT_VERSION: array[Low(TDc4FormatVersion)..High(TDc4FormatVersion)] of string = (
      'Hagen Reddmann Example File',
      '(De)Coder 4.0',
      '(De)Coder 4.1 Beta',
      '(De)Coder 4.1 Final (Cancelled)',
      '(De)Coder 5.0 WIP'
    );

    INTEGRITY_CHECK_INFO: array[Low(TDc4FormatVersion)..High(TDc4FormatVersion)] of string = (
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
  if fi.OrigFileName = '' then
    sl.Add('Original filename: unknown')
  else
    sl.Add('Original filename: ' + fi.OrigFileName);
  if fi.OrigFileSize = -1 then
    sl.Add('Original filesize: unknown')
  else
    sl.Add('Original filesize: ' + FileSizeHumanReadable(fi.OrigFileSize));
  if fi.OrigFileDate = -1 then
    sl.Add('Original datetime: unknown')
  else
    sl.Add('Original datetime: ' + DateTimeToStr(UnixToDateTime(fi.OrigFileDate, false)));
  if fi.Parameters.Dc4FormatVersion = fvDc41FinalCancelled then
    sl.Add('Password Encrypted File Name: ' + YesNo(fi.Parameters.ContainFileOrigName=fpEncryptWithUserKey))
  else
    sl.Add('Password Encrypted File Name: not available');
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

procedure DeCoder4X_EncodeFile(const AFileName, AOutput: String; const APassword: RawByteString; AParameters: TDC4Parameters; OnProgressProc: TDECProgressEvent=nil);
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
  ATempFileName: string;
  PbkdfIterations: long;
  V: TDc4FormatVersion;
  GCMAuthTagSizeInBytes: byte;
  tmpWS: WideString;
  outFileDidExist: boolean;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  DeCoder4X_ValidateParameterBlock(AParameters);

  tempstream := nil;
  Source := nil;
  cipher := nil;
  ahash := nil;
  IsZLibCompressed := false;
  IsFolder := DirectoryExists(AFileName);
  outFileDidExist := FileExists(AOutput);

  try
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
        // 0.00-0.99    0.99908
        // 1.00-1.99    0.21721
        // 2.00-2.99    0.23161
        // 3.00-3.99    0.20000
        // 4.00-4.99    0.21339
        // 5.00-5.99    0.35107
        // 6.00-6.99    0.55397
        // 7.00-7.49    0.73129
        // 7.50-7.99    0.87920
        Auto:  IsZLibCompressed := not IsCompressedFileType(AFileName)
                                    or (ShannonEntropy(AFileName, OnProgressProc) < 7.5);
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
        tempstream.WriteRawByteString('[' + DC4_OID + ']');
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
          Move(tmpWS[Low(tmpWS)], OrigName[Low(OrigName)], Length(tmpWS)*SizeOf(WideChar));
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
      else if V >= fvDc50Wip then
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
          raise Exception.Create('Filename too long');
        tempstream.WriteByte(Length(OrigName));
        tempstream.WriteRawByteString(OrigName);
      end;
      {$ENDREGION}

      {$REGION '3.1 File Size (version 4+)'}
      if V >= fvDc50Wip then
      begin
        if AParameters.ContainFileOrigSize then
          tempstream.WriteInt64(Int64(TFile.GetSize(AFileName)))
        else
          tempstream.WriteInt64(-1);
      end;
      {$ENDREGION}

      {$REGION '3.2 File Date/Time (version 4+)'}
      if V >= fvDc50Wip then
      begin
        if AParameters.ContainFileOrigDate then
          tempstream.WriteInt64(DateTimeToUnix(TFile.GetLastWriteTime(AFileName), false))
        else
          tempstream.WriteInt64(-1);
      end;
      {$ENDREGION}

      {$REGION '4. IdBase (only version 2+)'}
      idBase := DC4_ID_BASES[V];
      if V >= fvDc41Beta then
        tempstream.WriteLongBE(idBase);
      {$ENDREGION}

      {$REGION '5. Cipher identity (version 0 and 2+)'}
      if V <> fvDc40 then
        tempstream.WriteLongBE(DC_DEC_Identity(idBase, CipherClass.ClassName, V<fvDc50Wip));
      {$ENDREGION}

      {$REGION '6. Cipher mode (version 0 and 2+)'}
      if V <> fvDc40 then tempstream.WriteByte(Ord(Cipher.Mode));
      {$ENDREGION}

      {$REGION '7. Hash identity (version 0 and 2+)'}
      if V <> fvDc40 then
        tempstream.WriteLongBE(DC_DEC_Identity(idBase, HashClass.ClassName, V<fvDc50Wip));
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
      if KdfVersion = kvPbkdf2 then
        tempstream.WriteInt32(PbkdfIterations);
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
          tempstream.WriteLongBE(Source.Size);
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
        HashResult2 := TDECHashExtended(ahash).CalcString(BytesToRawByteString(HashResult)+Seed+APassword, TFormat_Copy);
        tempstream.WriteRawByteString(HashResult2);
      end
      else if V = fvDc41FinalCancelled then
      begin
        Source.position := 0;
        TDECHashExtended(ahash).CalcStream(Source, Source.size, HashResult, OnProgressProc);
        HashResult2 := TDECHashExtended(ahash).CalcString(
          BytesToRawByteString(HashResult) + Seed +
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
        HashResult2 := BytesToRawByteString(TDECHashAuthentication(ahash).HMACStream(HMacKey, tempstream, tmp64, OnProgressProc));
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
      if IsZLibCompressed and (ATempFileName<>'') then
      begin
        SecureDeleteFile(ATempFileName);
      end;
    end;
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

function _DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
resourcestring
  SInfoUnzipNotImplemented = 'Note: Decrypting of folders is not possible. The archive was decrypted, but you must unpack it with an external tool';
var
  Source: TFileStream;
  OrigFileSize: Int64;
  OrigFileDate: Int64;
  ch: RawByteString;
  F: byte;
  V: TDc4FormatVersion;
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
  outFileDidExist: boolean;
begin
  Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  tempstream := nil;
  cipher := nil;
  ahash := nil;
  IsZLibCompressed := false;
  IsFolder := false;
  OnlyReadFileInfo := AOutput = '';
  ATempFileName := '';
  OrigFileSize := -1;
  OrigFileDate := -1;
  outFileDidExist := FileExists(AOutput);
  try
    try
      try
        // Is it the Hagen Reddmann example file format?
        CipherClass := DC_DEC_CipherById(DC4_ID_BASES[fvHagenReddmannExample], Source.ReadLongBE, true, true);
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
          V := TDc4FormatVersion(Source.ReadByte); // if too big, it will automatically be set to fvHagenReddmannExample
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
          MagicSeq := RawByteString('[' + DC4_OID + ']');
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
            OrigNameEncrypted := Source.ReadRawByteString(Source.ReadLongBE); // will be decrypted below (after we initialized hash/cipher)
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

        {$REGION '3.1 File Size (version 4+)'}
        if V >= fvDc50Wip then
        begin
          OrigFileSize := Source.ReadInt64;
        end;
        {$ENDREGION}

        {$REGION '3.2 File Date/Time (version 4+)'}
        if V >= fvDc50Wip then
        begin
          OrigFileDate := Source.ReadInt64;
        end;
        {$ENDREGION}

        {$REGION '4. IdBase (only version 2+)'}
        if (V = fvHagenReddmannExample) or (V = fvDc40) then
          idBase := DC4_ID_BASES[V] // hardcoded
        else
          idBase := Source.ReadLongBE;
        {$ENDREGION}

        {$REGION '5. Cipher identity (only version 0 or 2+)'}
        if V <> fvHagenReddmannExample then // If V=fvHagenReddmannExample, then we already checked it and the stream position should be 4.
        begin
          // Now query with the new actual fitting id base version (dependant on V)
          if V = fvDc40 then
            CipherClass := TCipher_AES
          else
            CipherClass := DC_DEC_CipherById(idBase, Source.ReadLongBE, V<fvDc50Wip);
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
          HashClass := DC_DEC_HashById(idBase, Source.ReadLongBE, V<fvDc50Wip);
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
          PbkdfIterations := Source.ReadInt32
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
          HashResult2 := BytesToRawByteString(TDECHashAuthentication(ahash).HMACStream(HMacKey, Source, source.size-source.Position-ahash.DigestSize-Length(FileTerminus), OnProgressProc));
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
              TDECFormattedCipher(Cipher).DecodeStream(Source, tempstream, Source.ReadLongBE, OnProgressProc);
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
          if BytesToRawByteString(TDECFormattedCipher(Cipher).CalculatedAuthenticationResult) <> Source.ReadRawByteString(TDECFormattedCipher(Cipher).AuthenticationResultBitLength shr 3) then
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
        if not OnlyReadFileInfo and (V >= fvDc40) and (V < fvDc50Wip) then
        begin
          if V = fvDc40 then
          begin
            tempstream.position := 0;
            TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
            HashResult2 := BytesToRawByteString(HashResult);
          end
          else if V = fvDc41Beta then
          begin
            tempstream.position := 0;
            TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
            HashResult2 := TDECHashExtended(ahash).CalcString(BytesToRawByteString(HashResult)+Seed+APassword, TFormat_Copy);
          end
          else if V = fvDc41FinalCancelled then
          begin
            tempstream.position := 0;
            TDECHashExtended(ahash).CalcStream(tempstream, tempstream.size, HashResult, OnProgressProc);
            HashResult2 := TDECHashExtended(ahash).CalcString(
              BytesToRawByteString(HashResult) + Seed +
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
          {$IFDEF Console}
          WriteLn(SInfoUnzipNotImplemented);
          {$ELSE}
          Windows.MessageBox(0, PChar(SInfoUnzipNotImplemented), '(De)Coder', MB_OK + MB_ICONINFORMATION);
          {$ENDIF}
        end;
        {$ENDREGION}

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
        else if (V >= fvDc50Wip) and (OrigName = '') then
          result.Parameters.ContainFileOrigName := fpHide
        else
          result.Parameters.ContainFileOrigName := fpExpose;
        result.Parameters.ContainFileOrigSize := V >= fvDc50Wip;
        result.Parameters.ContainFileOrigDate := V >= fvDc50Wip;
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
  except
    try
      if not outFileDidExist then SecureDeleteFile(AOutput);
    except
    end;
    raise;
  end;
end;

function DeCoder4X_FileInfo(const AFileName: String; const APassword: RawByteString=''; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
begin
  result := _DeCoder4X_DecodeFile(AFileName, '', APassword, OnProgressProc);
end;

function DeCoder4X_DecodeFile(const AFileName, AOutput: String; const APassword: RawByteString; OnProgressProc: TDECProgressEvent=nil): TDC4FileInfo;
begin
  if AOutput = '' then raise Exception.Create('Output filename must not be empty');
  result := _DeCoder4X_DecodeFile(AFileName, AOutput, APassword, OnProgressProc);
end;

initialization
  RandomSeed;
end.
