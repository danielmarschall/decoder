unit DecoderOldCiphers;

interface

uses
  SysUtils, DECCipherBase, DECCipherFormats, DECTypes;

type
  TCipher_RepeatingXorSequence = class abstract (TDECFormattedCipher)
  strict private
    FKey: PUInt8Array;
    FKeySize: Integer;
    FKeyPos: Integer;
  strict protected
    procedure DoInit(const Key; Size: Integer); override;
    procedure DoEncode(Source, Dest: Pointer; Size: Integer); override;
    procedure DoDecode(Source, Dest: Pointer; Size: Integer); override;
    procedure SecureErase; override;
  public
    class function Context: TCipherContext; override;
  end;

  // TCipher_VtsDeCoder10 does not exist as DEC Cipher,
  // because this is a cipher that changes the size of the blocks

  // XOR-Stream 00 01 02 03 ... FF 00 01 ...
  // No key provided
  TCipher_VtsDeCoder20 = class(TCipher_RepeatingXorSequence)
  strict protected
    procedure DoInit(const Key; Size: Integer); override;
  public
    class function Context: TCipherContext; override;
  end;

  // The exact implementation is not 100% sure because the binaries and
  // source of this version are lost.
  // It is porbably the same as Version 2.2, just with the key 0..255 instead 0..256
  TCipher_VtsDeCoder21 = class(TCipher_RepeatingXorSequence)
  strict protected
    procedure DoInit(const Key; Size: Integer); override;
  end;

  // XOR-Stream 60 61 62 ... FF 00 01 ...    if key is 60, etc.
  TCipher_VtsDeCoder22 = class(TCipher_RepeatingXorSequence)
  strict protected
    procedure DoInit(const Key; Size: Integer); override;
  end;

  // XOR-Stream repeated password, e.g. "foobarfoobarfoobar", etc.
  TCipher_VtsDeCoder30 = class(TCipher_RepeatingXorSequence)
  strict protected
    procedure DoInit(const Key; Size: Integer); override;
  end;

  // The exact implementation is not 100% sure because the binaries and
  // source of this version are lost.
  // Since Beta 3.3 has the same output as 3.0, it is very likely that
  // 3.1 was either like 3.0 or faulty like 3.2. One of these.
  (*
  TCipher_VtsDeCoder31 = class(TCipher_VtsDeCoder30);
  *)

  // (De)Coder 3.2 probably had a faulty implementation.
  // The XOR stream was "oobarr" instead of "foobar"
  // Version 3.0 and 3.3 beta were OK.  Unsure if 3.1 was OK
  TCipher_VtsDeCoder32 = class(TCipher_RepeatingXorSequence)
  strict protected
    procedure DoInit(const Key; Size: Integer); override;
  end;

implementation

uses
  DECUtil;

{ TCipher_RepeatingXorSequence }

class function TCipher_RepeatingXorSequence.Context: TCipherContext;
begin
  Result.KeySize                     := High(Integer);
  Result.BlockSize                   := 1;
  Result.BufferSize                  := 1;
  Result.AdditionalBufferSize        := 0;
  Result.NeedsAdditionalBufferBackup := False;
  Result.MinRounds                   := 1;
  Result.MaxRounds                   := 1;
  Result.CipherType                  := [ctBlock, ctSymmetric];
end;

procedure TCipher_RepeatingXorSequence.DoInit(const Key; Size: Integer);
begin
  inherited;
  FKey := ReallocMemory(FKey, Size);
  Move(Key, FKey^, Size);
  FKeySize := Size;
  FKeyPos := FKeySize-1; // Point to last character, not the first character, because there is one DoEncode round for IV in TDECCipher.Init, even if IV is empty
end;

procedure TCipher_RepeatingXorSequence.DoEncode(Source, Dest: Pointer; Size: Integer);
var
  i: integer;
begin
  if State = csDone then FKeyPos := 0;
  for i := 0 to Size-1 do
  begin
    if FKeySize = 0 then
    begin
      // Without key, this is a null cipher
      PByte(Dest)[i] := PByte(Source)[i];
    end
    else
    begin
      PByte(Dest)[i] := PByte(Source)[i] xor FKey^[FKeyPos];
      FKeyPos := (FKeyPos+1) mod FKeySize;
    end;
  end;
end;

procedure TCipher_RepeatingXorSequence.SecureErase;
begin
  ProtectBuffer(FKey^[0], FKeySize);
  FreeMem(FKey);
  FKeySize := 0;
  inherited;
end;

procedure TCipher_RepeatingXorSequence.DoDecode(Source, Dest: Pointer; Size: Integer);
begin
  DoEncode(Source, Dest, Size);
end;

{ TCipher_VtsDeCoder20 }

class function TCipher_VtsDeCoder20.Context: TCipherContext;
begin
  result := inherited Context;
  result.KeySize := 0;
end;

procedure TCipher_VtsDeCoder20.DoInit(const Key; Size: Integer);
var
  b: TBytes;
  i: integer;
begin
  SetLength(b, 256);
  try
    for i := $00 to $FF do b[i] := i;
    inherited DoInit(b[0], 256);
  finally
    ProtectBuffer(b[0], 256);
    SetLength(b,0);
  end;
end;

{ TCipher_VtsDeCoder21 }

procedure TCipher_VtsDeCoder21.DoInit(const Key; Size: Integer);
var
  b: TBytes;
  i: integer;
  KeyStr: AnsiString;
  iKey: integer;
const
  GuiLimMin = 1;
  GuiLimMax = 255;
resourcestring
  SInvalidDc21Key = 'Key must be a number between %d and %d';
begin
  SetLength(KeyStr, Size);
  Move(Key, KeyStr[Low(KeyStr)], Size);
  if not TryStrToInt(string(KeyStr), iKey) or (iKey<GuiLimMin) or (iKey>GuiLimMax) then
    raise EDECException.CreateFmt(SInvalidDc21Key, [GuiLimMin, GuiLimMax]);
  SetLength(b, 256);
  try
    for i := $00 to $FF do b[i] := (i + iKey) mod 256;
    inherited DoInit(b[0], 256);
  finally
    ProtectBuffer(b[0], 256);
    SetLength(b,0);
  end;
end;

{ TCipher_VtsDeCoder22 }

procedure TCipher_VtsDeCoder22.DoInit(const Key; Size: Integer);
var
  b: TBytes;
  i: integer;
  KeyStr: AnsiString;
  iKey: integer;
const
  GuiLimMin = 1;
  GuiLimMax = 256;
resourcestring
  SInvalidDc22Key = 'Key must be a number between %d and %d';
begin
  SetLength(KeyStr, Size);
  Move(Key, KeyStr[Low(KeyStr)], Size);
  if not TryStrToInt(string(KeyStr), iKey) or (iKey<GuiLimMin) or (iKey>GuiLimMax) then
    raise EDECException.CreateFmt(SInvalidDc22Key, [GuiLimMin, GuiLimMax]);
  SetLength(b, 256);
  try
    for i := $00 to $FF do b[i] := (i + iKey) mod 256;
    inherited DoInit(b[0], 256);
  finally
    ProtectBuffer(b[0], 256);
    SetLength(b,0);
  end;
end;

{ TCipher_VtsDeCoder30 }

procedure TCipher_VtsDeCoder30.DoInit(const Key; Size: Integer);
begin
  inherited DoInit(Key, Size);
end;

{ TCipher_VtsDeCoder32 }

procedure TCipher_VtsDeCoder32.DoInit(const Key; Size: Integer);
var
  b: TBytes;
begin
  // Convert "foobar" to "oobarr" (faulty implementation)
  SetLength(b, Size+1);
  Move(Key, b[0], Size);
  try
    b[Size] := b[Size-1];
    Delete(b, 0, 1);
    inherited DoInit(b[0], Size);
  finally
    ProtectBuffer(b[0], Size);
    SetLength(b,0);
  end;
end;

initialization
  TCipher_VtsDeCoder20.RegisterClass(TDECCipher.ClassList);
  TCipher_VtsDeCoder21.RegisterClass(TDECCipher.ClassList);
  TCipher_VtsDeCoder22.RegisterClass(TDECCipher.ClassList);
  TCipher_VtsDeCoder30.RegisterClass(TDECCipher.ClassList);
  //TCipher_VtsDeCoder31.RegisterClass(TDECCipher.ClassList);
  TCipher_VtsDeCoder32.RegisterClass(TDECCipher.ClassList);
end.
