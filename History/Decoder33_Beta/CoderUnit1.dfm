object MainForm: TMainForm
  Left = 251
  Top = 139
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = '(De)Coder'
  ClientHeight = 340
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseMove = BackgroundMouseMove
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 321
    Width = 353
    Height = 19
    Cursor = crHandPoint
    Hint = 'Besuchen Sie die Webseite der DMCS!'
    Panels = <
      item
        Text = 'Besuchen Sie uns:'
        Width = 210
      end
      item
        Text = 'http://www.d-m-home.de'
        Width = 143
      end>
    ParentShowHint = False
    ShowHint = True
    SimplePanel = False
    OnClick = StatusBarClick
    OnMouseMove = BackgroundMouseMove
  end
  object BgPnl: TElPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 321
    BackgroundType = bgtVertGradient
    GradientEndColor = clWhite
    GradientStartColor = 13160660
    Align = alNone
    UseXPThemes = False
    MouseCapture = False
    TabOrder = 1
    DockOrientation = doNoOrient
    DoubleBuffered = False
    object CopyrightLbl1: TLabel
      Left = 8
      Top = 277
      Width = 301
      Height = 18
      Caption = '(C)Copyright 2001 - 2003 Daniel Marschall.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'LcdD'
      Font.Style = []
      ParentFont = False
      Transparent = True
      OnMouseMove = BackgroundMouseMove
    end
    object CopyrightLbl2: TLabel
      Left = 8
      Top = 298
      Width = 157
      Height = 17
      Caption = 'Alle Rechte vorbehalten.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'LcdD'
      Font.Style = []
      ParentFont = False
      Transparent = True
      OnMouseMove = BackgroundMouseMove
    end
    object StatusLbl1: TLabel
      Left = 8
      Top = 237
      Width = 48
      Height = 18
      Caption = 'Status:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -16
      Font.Name = 'LcdD'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      Transparent = True
      OnMouseMove = BackgroundMouseMove
    end
    object StatusLbl2: TLabel
      Left = 120
      Top = 237
      Width = 123
      Height = 18
      Hint = 'Der Status der Programms'
      Caption = 'Programm bereit!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -16
      Font.Name = 'LcdD'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
      OnMouseMove = BackgroundMouseMove
    end
    object CopyrightBvl: TBevel
      Left = 8
      Top = 264
      Width = 337
      Height = 2
      Shape = bsBottomLine
    end
    object TitelLbl: TElLabel
      Left = 90
      Top = 2
      Width = 124
      Height = 29
      Caption = '(De)Coder'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -24
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Effect = lesShadow
      ShadowColor = clGray
      XOffset = -2
      YOffset = -2
    end
    object ProgressPnl: TPanel
      Left = 7
      Top = 199
      Width = 339
      Height = 27
      BevelOuter = bvLowered
      TabOrder = 0
      object ProgressGge: TGauge
        Left = 1
        Top = 1
        Width = 337
        Height = 25
        Hint = 'Fortschritt'
        BackColor = clTeal
        Color = clWindowText
        Enabled = False
        ForeColor = clMaroon
        ParentColor = False
        ParentShowHint = False
        Progress = 0
        ShowHint = False
      end
    end
    object OpenBtn: TElPopupButton
      Left = 8
      Top = 40
      Width = 89
      Height = 25
      DrawDefaultFrame = False
      NumGlyphs = 1
      Caption = 'Datei &öffnen'
      TabOrder = 1
      OnClick = OpenBtnClick
      OnMouseMove = OpenBtnMouseMove
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object ExitBtn: TElPopupButton
      Left = 256
      Top = 40
      Width = 89
      Height = 25
      DrawDefaultFrame = False
      NumGlyphs = 1
      Caption = '&Beenden'
      TabOrder = 2
      OnClick = ExitBtnClick
      OnMouseMove = ExitBtnMouseMove
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object CloseBtn: TElPopupButton
      Left = 8
      Top = 72
      Width = 89
      Height = 25
      DrawDefaultFrame = False
      NumGlyphs = 1
      Caption = 'Datei &schließen'
      Enabled = False
      TabOrder = 3
      OnClick = CloseBtnClick
      OnMouseMove = CloseBtnMouseMove
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object CryptBtn: TElPopupButton
      Left = 104
      Top = 72
      Width = 241
      Height = 25
      DrawDefaultFrame = False
      NumGlyphs = 1
      Caption = '&Verschlüsseln / Entschlüsseln'
      Enabled = False
      TabOrder = 4
      OnClick = CryptBtnClick
      OnMouseMove = CryptBtnMouseMove
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object KeyPnl: TElPanel
      Left = 8
      Top = 112
      Width = 337
      Height = 73
      BackgroundType = bgtVertGradient
      GradientEndColor = clWhite
      GradientStartColor = 13160660
      Align = alNone
      BevelSpaceColor = 13160660
      UseXPThemes = False
      Color = 13160660
      MouseCapture = False
      TabOrder = 5
      DockOrientation = doNoOrient
      DoubleBuffered = False
      object KeyLbl: TLabel
        Left = 8
        Top = 6
        Width = 188
        Height = 18
        Caption = 'Verschlüsselungspasswort:'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'LcdD'
        Font.Style = []
        ParentFont = False
        Transparent = True
        OnMouseMove = BackgroundMouseMove
      end
      object KeyEdt: TElEdit
        Left = 16
        Top = 40
        Width = 305
        Height = 21
        Cursor = crIBeam
        VertScrollBarStyles.ShowTrackHint = False
        VertScrollBarStyles.Width = 16
        VertScrollBarStyles.ButtonSize = 16
        HorzScrollBarStyles.ShowTrackHint = False
        HorzScrollBarStyles.Width = 16
        HorzScrollBarStyles.ButtonSize = 16
        UseCustomScrollBars = True
        Alignment = taLeftJustify
        BorderSides = [ebsLeft, ebsRight, ebsTop, ebsBottom]
        RTLContent = False
        PasswordChar = '*'
        Transparent = False
        TopMargin = 3
        BorderStyle = bsSingle
        LineBorderActiveColor = clBlack
        LineBorderInactiveColor = clBlack
        WordWrap = False
        Ctl3D = True
        Enabled = False
        ParentColor = False
        ParentCtl3D = False
        TabOrder = 0
        TabStop = True
      end
    end
    object FileNameEdt: TEdit
      Left = 104
      Top = 40
      Width = 145
      Height = 21
      Enabled = False
      ReadOnly = True
      TabOrder = 6
      Text = 'Keine Datei geöffnet'
    end
  end
  object WaitTmr2: TTimer
    Enabled = False
    Interval = 500
    OnTimer = WaitTmr2Timer
    Left = 224
    Top = 288
  end
  object BlinkTmr: TTimer
    Enabled = False
    Interval = 150
    OnTimer = BlinkTmrTimer
    Left = 160
    Top = 288
  end
  object DragDrop: TDropFileTarget
    Dragtypes = [dtCopy]
    GetDataOnEnter = False
    OnDrop = DragDropDrop
    ShowImage = True
    Left = 256
    Top = 288
  end
  object WaitTmr1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = WaitTmr1Timer
    Left = 192
    Top = 288
  end
  object OpenDlg: TOpenDialogEx
    Filter = 
      'Alle Dateien (*.*)|*.*|Textdateien (*.txt)|*.txt|Dokumente (*.do' +
      'c)|*.doc|Klangdateien (*.wav)|*.wav|Bitmap-Dateien (*.bmp)|*.bmp' +
      '|Anwendungen (*.exe)|*.exe|Musikdateien (*.mid)|*.mid'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Datei öffnen...'
    Left = 320
    Top = 288
  end
  object SaveDlg: TSaveDialogEx
    Filter = 
      'Alle Dateien (*.*)|*.*|Textdateien (*.txt)|*.txt|Dokumente (*.do' +
      'c)|*.doc|Klangdateien (*.wav)|*.wav|Bitmap-Dateien (*.bmp)|*.bmp' +
      '|Anwendungen (*.exe)|*.exe|Musikdateien (*.mid)|*.mid'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Datei speichern...'
    Left = 288
    Top = 288
  end
end
