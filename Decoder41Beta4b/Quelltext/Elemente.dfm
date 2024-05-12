object ElementeForm: TElementeForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = '#'
  ClientHeight = 286
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = form_create
  OnDestroy = form_destroy
  OnResize = form_resize
  OnShow = form_show
  PixelsPerInch = 96
  TextHeight = 13
  object file_box: TListBox
    Left = 0
    Top = 0
    Width = 426
    Height = 245
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = popup
    Sorted = True
    TabOrder = 0
  end
  object bottom_panel: TPanel
    Left = 0
    Top = 245
    Width = 426
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btn_close: TButton
      Left = 8
      Top = 8
      Width = 129
      Height = 25
      Cancel = True
      Caption = '#'
      TabOrder = 0
      OnClick = btn_close_click
    end
    object btn_remove: TButton
      Left = 288
      Top = 8
      Width = 129
      Height = 25
      Caption = '#'
      TabOrder = 1
      OnClick = btn_remove_click
    end
  end
  object popup: TPopupMenu
    Left = 8
    Top = 8
    object add_file: TMenuItem
      Caption = 'add_file'
      ImageIndex = 17
      OnClick = add_file_click
    end
    object add_folder: TMenuItem
      Caption = 'add_folder'
      ImageIndex = 18
      OnClick = add_folder_click
    end
    object seperator: TMenuItem
      Caption = '-'
    end
    object remove: TMenuItem
      Caption = 'remove'
      ImageIndex = 3
      OnClick = btn_remove_click
    end
  end
  object tmr_refresh: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmr_refreshTimer
    Left = 40
    Top = 8
  end
end
