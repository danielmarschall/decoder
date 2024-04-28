object DlgForm: TDlgForm
  Left = 344
  Top = 280
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '(De)Coder'
  ClientHeight = 105
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnMouseMove = BackgroundMouseMove
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DialogImg: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
    AutoSize = True
    Transparent = True
    OnClick = OKBtnClick
  end
  object DialogLbl: TElLabel
    Left = 56
    Top = 8
    Width = 3
    Height = 13
  end
  object OKBtn: TElPopupButton
    Left = 200
    Top = 72
    Width = 91
    Height = 25
    DrawDefaultFrame = False
    Cancel = True
    Default = True
    ModalResult = 1
    NumGlyphs = 1
    Caption = 'OK'
    TabOrder = 0
    OnClick = OKBtnClick
    OnMouseMove = OKBtnMouseMove
    DockOrientation = doNoOrient
    DoubleBuffered = False
  end
end
