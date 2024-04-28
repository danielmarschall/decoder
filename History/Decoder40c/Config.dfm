object ConfigForm: TConfigForm
  Left = 247
  Top = 168
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 265
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btn_close: TButton
    Left = 128
    Top = 232
    Width = 145
    Height = 25
    Cancel = True
    Caption = '#'
    Default = True
    TabOrder = 0
    OnClick = btn_close_click
  end
  object grp_system: TGroupBox
    Left = 8
    Top = 8
    Width = 385
    Height = 217
    TabOrder = 1
    object lbl_label4: TLabel
      Left = 16
      Top = 168
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_label3: TLabel
      Left = 16
      Top = 112
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_label2: TLabel
      Left = 16
      Top = 56
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_label1: TLabel
      Left = 16
      Top = 24
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object btn_reg2: TButton
      Left = 16
      Top = 136
      Width = 161
      Height = 25
      Caption = '#'
      TabOrder = 2
      OnClick = btn_reg2_click
    end
    object btn_unreg2: TButton
      Left = 208
      Top = 136
      Width = 161
      Height = 25
      Caption = '#'
      TabOrder = 3
      OnClick = btn_unreg2_click
    end
    object btn_unreg1: TButton
      Left = 208
      Top = 80
      Width = 161
      Height = 25
      Caption = '#'
      TabOrder = 1
      OnClick = btn_unreg1_click
    end
    object btn_reg1: TButton
      Left = 16
      Top = 80
      Width = 161
      Height = 25
      Caption = '#'
      TabOrder = 0
      OnClick = btn_reg1_click
    end
  end
end
