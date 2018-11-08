object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '#'
  ClientHeight = 232
  ClientWidth = 361
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = form_create
  PixelsPerInch = 96
  TextHeight = 13
  object spe_leftshape: TShape
    Left = 0
    Top = 0
    Width = 105
    Height = 233
    Brush.Color = clBtnFace
    Pen.Style = psClear
  end
  object img_schloss: TImage
    Left = 8
    Top = 8
    Width = 89
    Height = 89
    AutoSize = True
    Transparent = True
  end
  object lbl_name: TLabel
    Left = 112
    Top = 8
    Width = 9
    Height = 13
    Caption = '#'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object lbl_version: TLabel
    Left = 112
    Top = 24
    Width = 7
    Height = 13
    Caption = '#'
    Transparent = True
  end
  object lbl_leader: TLabel
    Left = 112
    Top = 40
    Width = 7
    Height = 13
    Caption = '#'
    Transparent = True
  end
  object lbl_copyright: TLabel
    Left = 112
    Top = 64
    Width = 7
    Height = 13
    Caption = '#'
    Transparent = True
  end
  object lbl_url: TLabel
    Left = 112
    Top = 80
    Width = 7
    Height = 13
    Cursor = crHandPoint
    Caption = '#'
    Transparent = True
    OnClick = lbl_urlClick
  end
  object gbx_lang: TGroupBox
    Left = 112
    Top = 104
    Width = 241
    Height = 89
    Caption = '#'
    TabOrder = 0
    object lbl_lang_value: TLabel
      Left = 64
      Top = 24
      Width = 7
      Height = 13
      Caption = '#'
    end
    object lbl_translator_value: TLabel
      Left = 64
      Top = 40
      Width = 7
      Height = 13
      Caption = '#'
    end
    object lbl_revision_value: TLabel
      Left = 64
      Top = 56
      Width = 7
      Height = 13
      Caption = '#'
    end
    object lbl_lang_name: TLabel
      Left = 16
      Top = 24
      Width = 7
      Height = 13
      Caption = '#'
    end
    object lbl_translator_name: TLabel
      Left = 16
      Top = 40
      Width = 7
      Height = 13
      Caption = '#'
    end
    object lbl_revision_name: TLabel
      Left = 16
      Top = 56
      Width = 7
      Height = 13
      Caption = '#'
    end
  end
  object btn_close: TButton
    Left = 240
    Top = 200
    Width = 115
    Height = 25
    Cancel = True
    Caption = '#'
    Default = True
    TabOrder = 1
    OnClick = btn_closeClick
  end
end
