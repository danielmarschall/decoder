object MainForm: TMainForm
  Left = 357
  Top = 250
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = '(De)Coder 4.0'
  ClientHeight = 366
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Visible = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BgPanel: TElPanel
    Left = 0
    Top = 29
    Width = 519
    Height = 337
    BackgroundType = bgtVertGradient
    GradientEndColor = clSilver
    GradientStartColor = clSilver
    GradientSteps = 128
    Align = alClient
    UseXPThemes = False
    TabOrder = 0
    DockOrientation = doNoOrient
    object ULab1: TElLabel
      Left = 8
      Top = 8
      Width = 153
      Height = 30
      Caption = '(De)Coder 4.0'
      Effect = lesShadow
      ShadowColor = 12900
      XOffset = 1
      YOffset = 1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -24
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object CLab1: TElHTMLLabel
      Left = 8
      Top = 304
      Width = 341
      Height = 26
      Cursor = crDefault
      Caption = 
        '<font color="#643200">&copy; Copyright 2001 - 2004 Daniel Marsch' +
        'all. Alle Rechte vorbehalten!<br>'#13#10'Webseite: <a href="home">http' +
        '://www.d-m-home.de/</a> - E-Mail: <a href="email">info@daniel-ma' +
        'rschall.de</a></font>'
      IsHTML = True
      WordWrap = False
      LinkColor = clRed
      LinkStyle = [fsUnderline]
      OnLinkClick = CLab1LinkClick
    end
    object DLab1: TElLabel
      Left = 72
      Top = 56
      Width = 100
      Height = 13
      Caption = 'Keine Datei ge'#246'ffnet.'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DImg: TImage
      Left = 32
      Top = 56
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        07544269746D6170360C0000424D360C00000000000036000000280000002000
        0000200000000100180000000000000C0000C40E0000C40E0000000000000000
        0000FF00FFFF00FFFF00FF000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFFFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080C0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080808080808080808080808080808080808080
        8080808080808080808080808080808080808080808080808080808080808080
        80808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
      Visible = False
    end
    object DLab3: TElLabel
      Left = 32
      Top = 96
      Width = 57
      Height = 13
      Caption = 'Verzeichnis:'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DLab3b: TElLabel
      Left = 48
      Top = 112
      Width = 281
      Height = 13
      Caption = 'Verzeichnis'
      ShadowColor = clBlack
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DLab2: TElLabel
      Left = 72
      Top = 72
      Width = 42
      Height = 13
      Caption = 'Dateityp:'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DLab1b: TElLabel
      Left = 160
      Top = 56
      Width = 12
      Height = 13
      Caption = '1b'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DLab2b: TElLabel
      Left = 160
      Top = 72
      Width = 12
      Height = 13
      Caption = '2b'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DLab4: TElHTMLLabel
      Left = 32
      Top = 136
      Width = 54
      Height = 13
      Cursor = crDefault
      Caption = 'Dateistatus'
      IsHTML = True
      WordWrap = False
      LinkColor = clMaroon
      LinkStyle = [fsUnderline]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 2610
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DImg2: TImage
      Left = 32
      Top = 56
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        07544269746D6170360C0000424D360C00000000000036000000280000002000
        0000200000000100180000000000000C0000C40E0000C40E0000000000000000
        0000FF00FFFF00FFFF00FF000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000000FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000
        00000000000000000000000000000000FFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0080800080800080800080800080800080
        80008080008080008080008080008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008080FFFFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008080FFFFFF0080800080800080800080
        80008080008080008080C0C0C0008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008080FFFFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008080FFFFFF0080800080800080800080
        80008080008080008080C0C0C0008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008080FFFFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008080FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0080800080800080800080800080
        80008080008080008080008080008080000000C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00080800000000080800080
        80008080008080008080000000008080FFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008080000000FFFFFFFFFF
        FFFFFFFFFFFFFF008080000000FFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008080000000C0C0C0C0C0
        C0C0C0C0C0C0C0008080000000FFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008080C0C0C0000000FFFF
        FFFFFFFF008080008080000000FFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0008080C0C0C00000
        00000000008080000000FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0080800080
        80008080008080FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFFFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080C0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080808080808080808080808080808080808080
        8080808080808080808080808080808080808080808080808080808080808080
        80808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
      Visible = False
    end
    object DImg3: TImage
      Left = 32
      Top = 56
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        07544269746D6170360C0000424D360C00000000000036000000280000002000
        0000200000000100180000000000000C0000C40E0000C40E0000000000000000
        0000FF00FFFF00FFFF00FF000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
        80000080000080000080000080868686868686C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000800000
        FF0000FF0000FF0000FF0000FF000080000080868686868686FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000FF0000FF0000
        FF0000FF0000FF0000FF0000FF0000FF0000FF000080868686868686FF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000FF0000FFF8F8F80000
        FF0000FF0000FF0000FF0000FFF8F8F80000FF0000FF000080868686FF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000800000FFF8F8F8F8F8F8F8F8
        F80000FF0000FF0000FFF8F8F8F8F8F8F8F8F80000FF000080868686868686FF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000FF0000FF0000FFF8F8F8F8F8
        F8F8F8F80000FFF8F8F8F8F8F8F8F8F80000FF0000FF0000FF000080868686FF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000800000FF0000FF0000FF0000FFF8F8
        F8F8F8F8F8F8F8F8F8F8F8F8F80000FF0000FF0000FF0000FF000080868686FF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000FF0000FF0000FF0000FF0000
        FFF8F8F8F8F8F8F8F8F80000FF0000FF0000FF0000FF0000FF000080868686FF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000800000FF0000FF0000FF0000FFF8F8
        F8F8F8F8F8F8F8F8F8F8F8F8F80000FF0000FF0000FF0000FF000080868686FF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000FF0000FF0000FFF8F8F8F8F8
        F8F8F8F80000FFF8F8F8F8F8F8F8F8F80000FF0000FF0000FF000080FF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000800000FFF8F8F8F8F8F8F8F8
        F80000FF0000FF0000FFF8F8F8F8F8F8F8F8F80000FF000080868686FF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000FF0000FFF8F8F80000
        FF0000FF0000FF0000FF0000FFF8F8F80000FF0000FF000080FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000800000FF0000FF0000
        FF0000FF0000FF0000FF0000FF0000FF0000FF000080000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000800000800000
        FF0000FF0000FF0000FF0000FF000080000080C0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
        80000080000080000080000080FFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFFFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFFFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080FFFFFFC0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080C0C0C0808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FF808080808080808080808080808080808080808080
        8080808080808080808080808080808080808080808080808080808080808080
        80808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
    end
    object Label1: TLabel
      Left = 32
      Top = 120
      Width = 213
      Height = 26
      Caption = 
        'Klicken Sie auf "Datei '#246'ffnen", um eine Datei'#13#10'zu ver- oder ents' +
        'chl'#252'sseln.'
      Transparent = True
    end
    object MenuPanel: TElPanel
      Left = 360
      Top = 16
      Width = 145
      Height = 305
      BackgroundType = bgtVertGradient
      GradientEndColor = clSilver
      GradientStartColor = clSilver
      GradientSteps = 128
      Align = alNone
      UseXPThemes = False
      TabOrder = 0
      DockOrientation = doNoOrient
      object MBev1: TBevel
        Left = 8
        Top = 80
        Width = 129
        Height = 2
        Shape = bsBottomLine
      end
      object MBev2: TBevel
        Left = 8
        Top = 120
        Width = 129
        Height = 2
        Shape = bsBottomLine
      end
      object MBev3: TBevel
        Left = 8
        Top = 192
        Width = 129
        Height = 2
        Shape = bsBottomLine
      end
      object OpenBtn: TElPanel
        Left = 8
        Top = 16
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 0
        Caption = 'Datei '#246'ffnen'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object CloseBtn: TElPanel
        Left = 8
        Top = 48
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 1
        Caption = 'Datei schlie'#223'en'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object EncBtn: TElPanel
        Left = 8
        Top = 128
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 2
        Caption = 'Verschl'#252'sseln'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object DecBtn: TElPanel
        Left = 8
        Top = 160
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 3
        Caption = 'Entschl'#252'sseln'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object HelpBtn: TElPanel
        Left = 8
        Top = 232
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 4
        Caption = 'Hilfe'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object ExitBtn: TElPanel
        Left = 8
        Top = 264
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 5
        Caption = 'Beenden'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object InfoBtn: TElPanel
        Left = 8
        Top = 88
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 6
        Caption = 'Dateiinformationen'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
      object OptionBtn: TElPanel
        Left = 8
        Top = 200
        Width = 129
        Height = 25
        BackgroundType = bgtVertGradient
        GradientEndColor = clSilver
        GradientStartColor = clSilver
        GradientSteps = 128
        Align = alNone
        UseXPThemes = False
        Enabled = False
        TabOrder = 7
        Caption = 'Einstellungen'
        OnMouseDown = BtnMouseDown
        OnMouseUp = BtnMouseUp
        OnMouseEnter = MouseEnter
        OnMouseLeave = MouseLeave
        DockOrientation = doNoOrient
      end
    end
    object VBox: TElPanel
      Left = 14
      Top = 176
      Width = 331
      Height = 113
      BackgroundType = bgtVertGradient
      GradientEndColor = clSilver
      GradientStartColor = clSilver
      GradientSteps = 128
      Align = alNone
      UseXPThemes = False
      TabOrder = 1
      Visible = False
      DockOrientation = doNoOrient
      object VLab1: TElLabel
        Left = 8
        Top = 12
        Width = 41
        Height = 13
        Caption = 'Meldung'
        ShadowColor = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object VLab2: TElHTMLLabel
        Left = 16
        Top = 71
        Width = 270
        Height = 26
        Cursor = crDefault
        Caption = 
          'Anmerkung: Es wird zwischen Gro'#223'- und Kleinschreibung<br>untersc' +
          'hieden!'
        IsHTML = True
        WordWrap = False
        LinkColor = clRed
        LinkStyle = [fsUnderline]
        OnLinkClick = CLab1LinkClick
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object PwdEdit: TElEdit
        Left = 16
        Top = 36
        Width = 265
        Height = 21
        Cursor = crIBeam
        BorderSides = [ebsLeft, ebsRight, ebsTop, ebsBottom]
        PasswordChar = '*'
        RightMargin = 1
        TopMargin = 0
        BorderStyle = bsSingle
        LineBorderActiveColor = clBlack
        LineBorderInactiveColor = clBlack
        MaxUndoLevel = 0
        Ctl3D = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
      end
    end
    object TrackBar1: TTrackBar
      Left = 128
      Top = 96
      Width = 57
      Height = 45
      Max = 239
      Min = -239
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      Visible = False
      OnChange = TrackBar3Change
    end
    object TrackBar2: TTrackBar
      Left = 192
      Top = 96
      Width = 57
      Height = 45
      Max = 240
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 3
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      Visible = False
      OnChange = TrackBar3Change
    end
    object TrackBar3: TTrackBar
      Left = 256
      Top = 96
      Width = 57
      Height = 45
      Max = 240
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      Visible = False
      OnChange = TrackBar3Change
    end
    object Button1: TButton
      Left = 200
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 5
      Visible = False
      OnClick = Button1Click
    end
  end
  object Capt: TElPanel
    Left = 0
    Top = 0
    Width = 519
    Height = 29
    BackgroundType = bgtVertGradient
    GradientEndColor = clSilver
    GradientStartColor = clSilver
    GradientSteps = 128
    Align = alTop
    UseXPThemes = False
    TabOrder = 1
    OnMouseDown = CaptMouseDown
    DockOrientation = doNoOrient
    object CaptLabel: TElLabel
      Left = 8
      Top = 7
      Width = 97
      Height = 16
      Caption = '(De)Coder 4.0'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnMouseDown = CaptMouseDown
    end
    object ExBtn: TElPanel
      Tag = 1
      Left = 490
      Top = 0
      Width = 29
      Height = 29
      BackgroundType = bgtVertGradient
      GradientEndColor = clSilver
      GradientStartColor = clSilver
      GradientSteps = 128
      Align = alNone
      UseXPThemes = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      Caption = 'X'
      OnMouseDown = BtnMouseDown
      OnMouseUp = BtnMouseUp
      OnMouseEnter = ExBtnMouseEnter
      OnMouseLeave = ExBtnMouseLeave
      DockOrientation = doNoOrient
    end
  end
  object BlinkTimer: TTimer
    Enabled = False
    Interval = 175
    OnTimer = BlinkTimerTimer
    Left = 320
    Top = 72
  end
  object WaitTmr1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = WaitTmr1Timer
    Left = 288
    Top = 72
  end
  object OpenDlg: TOpenDialog
    Filter = 'Alle Dateien (*.*)|*.*|Verschl'#252'sselte Dateien (*.dc4)|*.dc4'
    Options = [ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 256
    Top = 40
  end
  object CipherManager1: TCipherManager
    Mode = cmCTS
    HashManager = HashManager1
    Left = 288
    Top = 40
    Cipher = 'TCipher_Blowfish'
  end
  object HashManager1: THashManager
    Left = 320
    Top = 40
    Hash = 'THash_RipeMD256'
  end
end
