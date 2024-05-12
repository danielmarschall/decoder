object DataModuleDragDropHandler: TDataModuleDragDropHandler
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 156
  Width = 215
  object PopupMenu1: TPopupMenu
    Left = 24
    Top = 16
    object MenuEncrypt: TMenuItem
      Caption = '?'
      Hint = '?'
      OnClick = MenuEncryptClick
    end
    object MenuLine1: TMenuItem
      Caption = '-'
    end
  end
end
