unit DecoderMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

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
  DecoderEncDec, DECTypes, DecoderOldCiphers, DECCipherBase, DECCiphers,
  DECCipherFormats;

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

procedure TFormMain.Button1Click(Sender: TObject);
begin
end;

(*
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
*)

procedure TFormMain.Button2Click(Sender: TObject);
var
  Cipher: TDECCipher;
  s: RawByteString;
begin
  Cipher := TCipher_VtsDeCoder30.Create;
  Cipher.Init('', '', $FF);
  Cipher.Mode := cmECBx;
  s := Cipher.EncodeRawByteString('Hello');
  memo1.Lines.Add(s);
  Cipher.Done;

  Cipher.Init('', '', $FF);
  Cipher.Mode := cmECBx;
  memo1.Lines.Add(Cipher.DecodeRawByteString(s));
  Cipher.Done;
end;

procedure TFormMain.Button3Click(Sender: TObject);

  function Are2FilesEqual(const File1, File2: TFileName): Boolean;
  var
    ms1, ms2: TMemoryStream;
  begin
    Result := False;
    ms1 := TMemoryStream.Create;
    try
      ms1.LoadFromFile(File1);
      ms2 := TMemoryStream.Create;
      try
        ms2.LoadFromFile(File2);
        if ms1.Size = ms2.Size then
          Result := CompareMem(ms1.Memory, ms2.memory, ms1.Size);
      finally
        ms2.Free;
      end;
    finally
      ms1.Free;
    end
  end;
var
  fi: TDC4FileInfo;

begin
  Memo1.Lines.Clear;

  DeCoder20_EncodeFile('TestData\dc20_256zero_in.txt', 'TestData\dc20_256zero_out.tmp', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc20_256zero_out.txt', 'TestData\dc20_256zero_out.tmp'));
  DeleteFile('TestData\dc20_256zero_out.tmp');
  DeCoder20_EncodeFile('TestData\dc20_test_in.txt', 'TestData\dc20_test_out.tmp', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc20_test_out.txt', 'TestData\dc20_test_out.tmp'));
  DeleteFile('TestData\dc20_test_out.tmp');
  Memo1.Lines.Add('DC20 Encode OK');
  Memo1.Lines.Add('');

  DeCoder22_EncodeFile('TestData\dc22_256zero_in.txt', 'TestData\dc22_256zero_out_61.tmp', 61, OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc22_256zero_out_61.txt', 'TestData\dc22_256zero_out_61.tmp'));
  DeleteFile('TestData\dc22_256zero_out_61.tmp');
  DeCoder22_EncodeFile('TestData\dc22_test_in.txt', 'TestData\dc22_test_out_61.tmp', 61, OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc22_test_out_61.txt', 'TestData\dc22_test_out_61.tmp'));
  DeleteFile('TestData\dc22_test_out_61.tmp');
  Memo1.Lines.Add('DC22 Encode OK');
  Memo1.Lines.Add('');

  DeCoder30_EncodeFile('TestData\dc30_256zero_in.txt', 'TestData\dc30_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc30_256zero_out_foobar.txt', 'TestData\dc30_256zero_out_foobar.tmp'));
  DeleteFile('TestData\dc30_256zero_out_foobar.tmp');
  DeCoder30_EncodeFile('TestData\dc30_test_in.txt', 'TestData\dc30_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc30_test_out_foobar.txt', 'TestData\dc30_test_out_foobar.tmp'));
  DeleteFile('TestData\dc30_test_out_foobar.tmp');
  Memo1.Lines.Add('DC30 Encode OK');
  Memo1.Lines.Add('');

  DeCoder32_EncodeFile('TestData\dc32_256zero_in.txt', 'TestData\dc32_256zero_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc32_256zero_out_foobar.txt', 'TestData\dc32_256zero_out_foobar.tmp'));
  DeleteFile('TestData\dc32_256zero_out_foobar.tmp');
  DeCoder32_EncodeFile('TestData\dc32_256zero_in.txt', 'TestData\dc32_256zero_out_abcdefg.tmp', 'abcdefg', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc32_256zero_out_abcdefg.txt', 'TestData\dc32_256zero_out_abcdefg.tmp'));
  DeleteFile('TestData\dc32_256zero_out_abcdefg.tmp');
  DeCoder32_EncodeFile('TestData\dc32_test_in.txt', 'TestData\dc32_test_out_foobar.tmp', 'foobar', OnProgressProc);
  Assert(Are2FilesEqual('TestData\dc32_test_out_foobar.txt', 'TestData\dc32_test_out_foobar.tmp'));
  DeleteFile('TestData\dc32_test_out_foobar.tmp');
  Memo1.Lines.Add('DC32 Encode OK');
  Memo1.Lines.Add('');

  DeCoder4X_DecodeFile('TestData\schloss.dc4', 'schloss_decoded.bmp', 'test', false, OnProgressProc);
  Memo1.Lines.Add('Decode DC41 Beta OK');
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver0.dc5', 'test', OnProgressProc, fvHagenReddmannExample);
  DeCoder4X_DecodeFile('schloss_ver0.dc5', 'schloss_decoded_dc5_ver0.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver0.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver0.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver0.bmp');
  DeleteFile('schloss_ver0.dc5');
  Memo1.Lines.Add('Hagen Example OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver1.dc5', 'test', OnProgressProc, fvDc40);
  DeCoder4X_DecodeFile('schloss_ver1.dc5', 'schloss_decoded_dc5_ver1.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver1.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver1.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver1.bmp');
  DeleteFile('schloss_ver1.dc5');
  Memo1.Lines.Add('DC40 OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver2.dc5', 'test', OnProgressProc, fvDc41Beta);
  DeCoder4X_DecodeFile('schloss_ver2.dc5', 'schloss_decoded_dc5_ver2.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver2.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver2.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver2.bmp');
  DeleteFile('schloss_ver2.dc5');
  Memo1.Lines.Add('DC41 Beta OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver3.dc5', 'test', OnProgressProc, fvDc41FinalCancelled);
  DeCoder4X_DecodeFile('schloss_ver3.dc5', 'schloss_decoded_dc5_ver3.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver3.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver3.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver3.bmp');
  DeleteFile('schloss_ver3.dc5');
  Memo1.Lines.Add('DC41 Final OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeCoder4X_EncodeFile('schloss_decoded.bmp', 'schloss_ver4.dc5', 'test', OnProgressProc, fvDc50Wip);
  DeCoder4X_DecodeFile('schloss_ver4.dc5', 'schloss_decoded_dc5_ver4.bmp', 'test', false, OnProgressProc);
  Assert(Are2FilesEqual('schloss_decoded.bmp', 'schloss_decoded_dc5_ver4.bmp'));
  fi := DeCoder4X_DecodeFile('schloss_ver4.dc5', '', '', true);
  DeleteFile('schloss_decoded_dc5_ver4.bmp');
  DeleteFile('schloss_ver4.dc5');
  Memo1.Lines.Add('DC50 OK:');
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
  Memo1.Lines.Add('');

  DeleteFile('schloss_decoded.bmp');

  ShowMessage('Alles OK');
end;

end.
