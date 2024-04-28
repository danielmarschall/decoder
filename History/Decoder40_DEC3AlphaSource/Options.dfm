object OptionsForm: TOptionsForm
  Left = 375
  Top = 213
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'Einstellungen'
  ClientHeight = 233
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Capt: TElPanel
    Left = 0
    Top = 0
    Width = 425
    Height = 29
    BackgroundType = bgtVertGradient
    GradientEndColor = clSilver
    GradientStartColor = clSilver
    GradientSteps = 128
    Align = alTop
    UseXPThemes = False
    TabOrder = 0
    OnMouseDown = CaptMouseDown
    DockOrientation = doNoOrient
    object CaptLabel: TElLabel
      Left = 8
      Top = 7
      Width = 94
      Height = 16
      Caption = 'Einstellungen'
      ShadowColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ExBtn: TElPanel
      Tag = 1
      Left = 396
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
      OnMouseDown = ExBtnMouseDown
      OnMouseUp = ExBtnMouseUp
      OnMouseEnter = ExBtnMouseEnter
      OnMouseLeave = ExBtnMouseLeave
      DockOrientation = doNoOrient
    end
  end
  object MainPanel: TElPanel
    Left = 0
    Top = 29
    Width = 425
    Height = 204
    BackgroundType = bgtVertGradient
    GradientEndColor = clSilver
    GradientStartColor = clSilver
    GradientSteps = 128
    Align = alClient
    UseXPThemes = False
    TabOrder = 1
    DockOrientation = doNoOrient
    object UserLbl: TLabel
      Left = 104
      Top = 16
      Width = 219
      Height = 13
      Caption = 'Folgenden Namen in meine Dateien eintragen:'
      Transparent = True
    end
    object UserEdt: TElEdit
      Left = 104
      Top = 32
      Width = 257
      Height = 21
      Cursor = crIBeam
      BorderSides = [ebsLeft, ebsRight, ebsTop, ebsBottom]
      RightMargin = 1
      TopMargin = 2
      BorderStyle = bsSingle
      LineBorderActiveColor = clBlack
      LineBorderInactiveColor = clBlack
      MaxUndoLevel = 0
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
    end
    object OKBtn: TElPanel
      Left = 288
      Top = 168
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
      Caption = 'OK'
      OnMouseDown = BtnMouseDown
      OnMouseUp = BtnMouseUp
      OnMouseEnter = BtnMouseEnter
      OnMouseLeave = BtnMouseLeave
      DockOrientation = doNoOrient
    end
    object ElSideBar1: TElSideBar
      Left = 1
      Top = 1
      Width = 90
      Height = 202
      Sections = <
        item
          Index = 0
          Caption = 'Einstellungen'
          Items = <
            item
              Index = 0
              Caption = 'Sprache'
            end
            item
              Index = 1
              Caption = 'Lizenz'
            end
            item
              Index = 2
              ImageIndex = 1
              Caption = 'Algorithmen'
            end>
        end>
      SectionIndex = 0
      ItemIndex = 0
      ItemTracking = False
      UnderlineTracked = False
      FlatActiveItem = True
      Flat = True
      ItemsFont.Charset = DEFAULT_CHARSET
      ItemsFont.Color = clWindow
      ItemsFont.Height = -11
      ItemsFont.Name = 'MS Sans Serif'
      ItemsFont.Style = []
      SectionsFont.Charset = DEFAULT_CHARSET
      SectionsFont.Color = clWindowText
      SectionsFont.Height = -11
      SectionsFont.Name = 'MS Sans Serif'
      SectionsFont.Style = []
      TrackSectionFontColor = clBtnText
      TrackItemFontColor = clBtnText
      TrackItemBkColor = clBackground
      ScrollButtonColor = clBackground
      TransparentItems = True
      ArrowColor = clBlack
      Align = alLeft
      BorderStyle = bsSingle
      Color = clSilver
      TabOrder = 2
      object TElSideBarContainerPanel
        UseXPThemes = False
        TabOrder = 0
      end
    end
  end
end
