object MainForm: TMainForm
  Left = 247
  Top = 133
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'QuickStarter'
  ClientHeight = 105
  ClientWidth = 302
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
  OnHide = FormHide
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
    OnMouseMove = BackgroundMouseMove
  end
  object DialogLbl: TElLabel
    Left = 56
    Top = 8
    Width = 3
    Height = 13
  end
  object NoBtn: TElPopupButton
    Left = 208
    Top = 72
    Width = 83
    Height = 25
    DrawDefaultFrame = False
    Cancel = True
    Default = True
    NumGlyphs = 1
    Caption = '&Nein'
    TabOrder = 0
    OnClick = NoBtnClick
    OnMouseMove = NoBtnMouseMove
    DockOrientation = doNoOrient
    DoubleBuffered = False
  end
  object YesBtn: TElPopupButton
    Left = 120
    Top = 72
    Width = 83
    Height = 25
    DrawDefaultFrame = False
    NumGlyphs = 1
    Caption = '&Ja'
    TabOrder = 1
    OnClick = YesBtnClick
    OnMouseMove = YesBtnMouseMove
    DockOrientation = doNoOrient
    DoubleBuffered = False
  end
  object TrayPopup: TPopupMenu
    Left = 40
    Top = 72
    object DeCoderstarten1: TMenuItem
      Caption = '(De)Coder &starten!'
      Default = True
      OnClick = TrayIconClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object QuickStarterinfo: TMenuItem
      Caption = '&Informationen'
      OnClick = QuickStarterinfo2Click
    end
    object QuickStarterbeenden1: TMenuItem
      Caption = 'QuickStarter &beenden!'
      OnClick = QuickStarterbeenden1Click
    end
  end
  object TrayIcon: TantTaskbarIcon
    Hint = '(De)Coder 3.3 QucikStarter!'
    PopupMenu = TrayPopup
    OnClick = TrayIconClick
    Left = 8
    Top = 72
  end
end
