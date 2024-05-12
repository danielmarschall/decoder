object WartenForm: TWartenForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Bitte warten...'
  ClientHeight = 149
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_wait: TLabel
    Left = 8
    Top = 19
    Width = 91
    Height = 29
    Caption = 'lbl_wait'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object lbl_info1: TLabel
    Left = 8
    Top = 104
    Width = 7
    Height = 13
    Caption = '#'
    Transparent = True
  end
  object lbl_info2: TLabel
    Left = 8
    Top = 120
    Width = 7
    Height = 13
    Caption = '#'
    Transparent = True
  end
  object pbr_progress: TProgressBar
    Left = 8
    Top = 64
    Width = 409
    Height = 33
    Max = 10000
    Smooth = True
    TabOrder = 0
  end
  object btn_escape: TButton
    Left = 296
    Top = 116
    Width = 121
    Height = 25
    Cancel = True
    Caption = '#'
    TabOrder = 1
    Visible = False
    OnClick = btn_escapeClick
  end
end
