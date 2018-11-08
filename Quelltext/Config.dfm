object ConfigForm: TConfigForm
  Left = 247
  Top = 168
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '#'
  ClientHeight = 321
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = form_create
  OnHide = form_hide
  OnShow = form_show
  PixelsPerInch = 96
  TextHeight = 13
  object btn_close: TButton
    Left = 160
    Top = 288
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
    Width = 457
    Height = 169
    TabOrder = 1
    object lbl_info2: TLabel
      Left = 40
      Top = 128
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_info1: TLabel
      Left = 16
      Top = 24
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object img_information: TImage
      Left = 16
      Top = 128
      Width = 16
      Height = 16
      Transparent = True
    end
    object sys_checkbox1: TCheckBox
      Left = 16
      Top = 48
      Width = 425
      Height = 17
      Caption = '#'
      TabOrder = 0
      OnClick = sys_checkbox1_click
    end
    object sys_checkbox2: TCheckBox
      Left = 16
      Top = 64
      Width = 425
      Height = 17
      Caption = '#'
      TabOrder = 1
      OnClick = sys_checkbox2_click
    end
    object sys_checkbox3: TCheckBox
      Left = 16
      Top = 80
      Width = 425
      Height = 17
      Caption = '#'
      TabOrder = 2
      OnClick = sys_checkbox3_click
    end
    object sys_checkbox4: TCheckBox
      Left = 16
      Top = 96
      Width = 425
      Height = 17
      Caption = '#'
      TabOrder = 3
      OnClick = sys_checkbox4_click
    end
  end
  object grp_keylogger: TGroupBox
    Left = 8
    Top = 184
    Width = 457
    Height = 97
    Caption = '#'
    TabOrder = 2
    object lbl_key_info: TLabel
      Left = 16
      Top = 24
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_bytes: TLabel
      Left = 80
      Top = 72
      Width = 7
      Height = 13
      Caption = '#'
    end
    object edt_garbarge: TEdit
      Left = 16
      Top = 64
      Width = 41
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = '0'
    end
    object upd_garbarge: TUpDown
      Left = 56
      Top = 64
      Width = 17
      Height = 25
      TabOrder = 1
      OnClick = upd_garbargeClick
    end
  end
  object tmr_timer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = timer_timer
    Left = 424
    Top = 24
  end
end
