object MainForm: TMainForm
  Left = 224
  Top = 142
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 395
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_file: TLabel
    Left = 64
    Top = 8
    Width = 473
    Height = 13
    AutoSize = False
    Caption = '#'
    ParentShowHint = False
    ShowHint = True
    Transparent = True
  end
  object lbl_label1: TLabel
    Left = 8
    Top = 8
    Width = 7
    Height = 13
    Caption = '#'
    Transparent = True
  end
  object vle: TValueListEditor
    Left = 8
    Top = 32
    Width = 529
    Height = 217
    Ctl3D = True
    DefaultRowHeight = 16
    KeyOptions = [keyEdit]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goRowSelect, goThumbTracking]
    ParentCtl3D = False
    ParentShowHint = False
    ScrollBars = ssNone
    ShowHint = True
    TabOrder = 0
    ColWidths = (
      150
      373)
  end
  object btn_config: TBitBtn
    Left = 544
    Top = 144
    Width = 49
    Height = 49
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = btn_config_click
    NumGlyphs = 2
  end
  object btn_open: TBitBtn
    Left = 544
    Top = 32
    Width = 49
    Height = 49
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = btn_open_click
    NumGlyphs = 2
  end
  object btn_folder: TBitBtn
    Left = 544
    Top = 88
    Width = 49
    Height = 49
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btn_folder_click
    NumGlyphs = 2
  end
  object btn_help: TBitBtn
    Left = 544
    Top = 200
    Width = 49
    Height = 49
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = btn_help_click
    NumGlyphs = 2
  end
  object btn_dec: TBitBtn
    Left = 544
    Top = 264
    Width = 49
    Height = 49
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Visible = False
    OnClick = btn_dec_click
    NumGlyphs = 2
  end
  object btn_enc: TBitBtn
    Left = 544
    Top = 320
    Width = 49
    Height = 49
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    Visible = False
    OnClick = btn_enc_click
    NumGlyphs = 2
  end
  object grp_enc: TGroupBox
    Left = 8
    Top = 256
    Width = 529
    Height = 113
    TabOrder = 7
    Visible = False
    object lbl_enc_label1: TLabel
      Left = 16
      Top = 28
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_enc_label2: TLabel
      Left = 16
      Top = 52
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object lbl_enc_entropy: TLabel
      Left = 464
      Top = 27
      Width = 9
      Height = 16
      Caption = '#'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
    end
    object lbl_enc_equal: TLabel
      Left = 464
      Top = 51
      Width = 9
      Height = 16
      Caption = '#'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
    end
    object img_enc_warning: TImage
      Left = 8
      Top = 88
      Width = 16
      Height = 16
      ParentShowHint = False
      ShowHint = True
      Transparent = True
      Visible = False
    end
    object lbl_enc_warning: TLabel
      Left = 32
      Top = 90
      Width = 9
      Height = 13
      Caption = '#'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object edt_enc_password: TEdit
      Left = 120
      Top = 24
      Width = 337
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      PasswordChar = '*'
      TabOrder = 0
      OnChange = edt_enc_password_change
      OnKeyPress = edt_enc_password_keypress
    end
    object edt_enc_password2: TEdit
      Left = 120
      Top = 48
      Width = 337
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      PasswordChar = '*'
      TabOrder = 1
      OnChange = edt_enc_password2_change
      OnKeyPress = edt_enc_password2_keypress
    end
    object chk_enc_securedelete: TCheckBox
      Left = 120
      Top = 80
      Width = 377
      Height = 17
      Caption = '#'
      Enabled = False
      TabOrder = 2
    end
  end
  object grp_dec: TGroupBox
    Left = 8
    Top = 256
    Width = 529
    Height = 89
    TabOrder = 5
    Visible = False
    object lbl_dec_label1: TLabel
      Left = 16
      Top = 28
      Width = 7
      Height = 13
      Caption = '#'
      Transparent = True
    end
    object img_dec_warning: TImage
      Left = 8
      Top = 65
      Width = 16
      Height = 16
      ParentShowHint = False
      ShowHint = True
      Transparent = True
      Visible = False
    end
    object lbl_dec_warning: TLabel
      Left = 32
      Top = 67
      Width = 9
      Height = 13
      Caption = '#'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object edt_dec_password: TEdit
      Left = 120
      Top = 24
      Width = 345
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      PasswordChar = '*'
      TabOrder = 0
      OnKeyPress = edt_dec_password_keypress
    end
    object chk_dec_securedelete: TCheckBox
      Left = 120
      Top = 56
      Width = 377
      Height = 17
      Caption = '#'
      Enabled = False
      TabOrder = 1
    end
  end
  object statusbar: TStatusBar
    Left = 0
    Top = 376
    Width = 601
    Height = 19
    Panels = <
      item
        Width = 531
      end
      item
        Alignment = taRightJustify
        Width = 70
      end>
  end
  object dlg_open: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 16
    Top = 216
  end
  object dlg_save_dec: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 48
    Top = 216
  end
  object dlg_save_enc: TSaveDialog
    DefaultExt = 'dc4'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 80
    Top = 216
  end
  object xp: TXPManifest
    Left = 144
    Top = 216
  end
  object tmr_wait: TTimer
    Interval = 10
    OnTimer = tmr_wait_timer
    Left = 112
    Top = 216
  end
end
