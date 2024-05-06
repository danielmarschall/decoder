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
var
  fi: TDC4FileInfo;
begin


  DeleteFile('schloss_decoded.bmp');

  DeCoder4X_DecodeFile('schloss.dc4', 'schloss_decoded.bmp', 'test', false, OnProgressProc);
  ShowMessage('ok');

  DeCoder4X_EncodeFile_Ver4('schloss_decoded.bmp', 'schloss.dc5', 'test', OnProgressProc);
  ShowMessage('ok1');
  DeCoder4X_DecodeFile('schloss.dc5', 'schloss_decoded_dc5.bmp', 'test', false, OnProgressProc);
  ShowMessage('ok2');



  fi := DeCoder4X_DecodeFile('schloss.dc4', '', '', true);
  ShowMessage('ok');

  fi := DeCoder4X_DecodeFile('schloss.dc5', '', '', true);
  ShowMessage('ok');

  Memo1.Lines.Clear;
  DeCoder4X_PrintFileInfo(fi, Memo1.Lines);
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
  Cipher := TCipher_Dc30.Create;
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

function Convert(const Bytes: TBytes): RawByteString;
begin
  SetLength(Result, Length(Bytes));
  Move(Bytes[0], Result[1], Length(Bytes))
end;

procedure TFormMain.Button3Click(Sender: TObject);
var
  ssIn: TFileStream;
  ssOut: TFileStream;
  Cipher: TDECCipher;
begin
  ssIn := TFileStream.Create('c:\SVN\Decoder\trunk\History\Decoder30\test_in.txt', fmOpenRead);
  ssOut := TFileStream.Create('c:\SVN\Decoder\trunk\History\Decoder30\test_out_foobar_2.txt', fmCreate);
  Cipher := TCipher_Dc30.Create;
  Cipher.Init(AnsiString('foobar'), AnsiString(''), $FF);
  Cipher.Mode := cmECBx;
  TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size);
  Cipher.Done;
  ssIn.Free;
  ssOut.Free;

  ssIn := TFileStream.Create('c:\SVN\Decoder\trunk\History\Decoder30\256zero_in.txt', fmOpenRead);
  ssOut := TFileStream.Create('c:\SVN\Decoder\trunk\History\Decoder30\256zero_out_foobar_2.txt', fmCreate);
//  Cipher := TCipher_Dc30.Create;
//  Cipher.Init(AnsiString('foobar'), AnsiString(''), $FF);
  Cipher.Mode := cmECBx;
  TDECFormattedCipher(Cipher).EncodeStream(ssIn, ssOut, ssIn.Size);
  Cipher.Done;
  ssIn.Free;
  ssOut.Free;

  Cipher.Free;
end;

end.
