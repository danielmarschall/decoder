object InfoForm: TInfoForm
  Left = 273
  Top = 210
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '(De)Coder QuickStarter'
  ClientHeight = 155
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseMove = BackgroundMouseMove
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TElPopupButton
    Left = 216
    Top = 120
    Width = 91
    Height = 25
    DrawDefaultFrame = False
    NumGlyphs = 1
    Caption = '&OK'
    TabOrder = 0
    OnClick = OKBtnClick
    OnMouseMove = OKBtnMouseMove
    DockOrientation = doNoOrient
    DoubleBuffered = False
  end
  object ElPanel1: TElPanel
    Left = 8
    Top = 8
    Width = 297
    Height = 105
    Align = alNone
    MouseCapture = False
    TabOrder = 1
    OnMouseMove = BackgroundMouseMove
    DockOrientation = doNoOrient
    DoubleBuffered = False
    object InfoLbl5: TElLabel
      Left = 56
      Top = 80
      Width = 117
      Height = 13
      Caption = 'Alle Rechte vorbehalten!'
      OnMouseMove = BackgroundMouseMove
    end
    object InfoLbl4: TElLabel
      Left = 56
      Top = 64
      Width = 201
      Height = 13
      Caption = '(C)Copyright 2001 - 2002 Daniel Marschall.'
      OnMouseMove = BackgroundMouseMove
    end
    object InfoLbl2: TElLabel
      Left = 56
      Top = 24
      Width = 110
      Height = 13
      Caption = '(De)Coder QuickStarter'
      OnMouseMove = BackgroundMouseMove
    end
    object InfoLbl1: TElLabel
      Left = 56
      Top = 8
      Width = 166
      Height = 13
      Caption = 'Daniel Marschall Computersoftware'
      OnMouseMove = BackgroundMouseMove
    end
    object InfoLbl3: TElLabel
      Left = 232
      Top = 24
      Width = 53
      Height = 13
      Caption = 'Version 3.3'
      OnMouseMove = BackgroundMouseMove
    end
    object InfoImg: TImage
      Left = 16
      Top = 16
      Width = 32
      Height = 32
      AutoSize = True
      Transparent = True
      OnMouseMove = BackgroundMouseMove
    end
  end
end
