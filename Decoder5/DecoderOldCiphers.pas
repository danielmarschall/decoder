unit DecoderOldCiphers;

interface

uses
  SysUtils, DECCipherBase, DECCipherFormats;

type
  TCipher_Dc30 = class(TDECFormattedCipher)
  private
    FKey: TBytes;
    FKeyPos: Integer;
  protected
    procedure DoInit(const Key; Size: Integer); override;
    procedure DoEncode(Source, Dest: Pointer; Size: Integer); override;
    procedure DoDecode(Source, Dest: Pointer; Size: Integer); override;
    procedure SecureErase; override;
  public
    class function Context: TCipherContext; override;
  end;

implementation

uses
  DECTypes, DECUtil;

{ TCipher_Dc30 }

class function TCipher_Dc30.Context: TCipherContext;
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

procedure TCipher_Dc30.DoInit(const Key; Size: Integer);
begin
  inherited;
  SetLength(FKey, Size);
  Move(Key, FKey[0], Size);
  FKeyPos := Length(FKey)-1; // Use "-1", not 0, because there is one DoEncode round for IV in TDECCipher.Init, even if IV is empty
end;

procedure TCipher_Dc30.DoEncode(Source, Dest: Pointer; Size: Integer);
var
  i: integer;
begin
  if State = csDone then FKeyPos := 0;
  for i := 0 to Size-1 do
  begin
    PByte(Dest)[i] := PByte(Source)[i] xor FKey[FKeyPos];
    FKeyPos := (FKeyPos+1) mod Length(FKey);
  end;
end;

procedure TCipher_Dc30.SecureErase;
begin
  ProtectBuffer(FKey, Length(FKey));
  SetLength(FKey, 0);
  inherited;
end;

procedure TCipher_Dc30.DoDecode(Source, Dest: Pointer; Size: Integer);
begin
  DoEncode(Source, Dest, Size);
end;

initialization
  TCipher_Dc30.RegisterClass(TDECCipher.ClassList);
end.
