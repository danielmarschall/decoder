object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'ProgressTest'
  ClientHeight = 467
  ClientWidth = 635
  Color = clBtnFace
  Constraints.MinHeight = 226
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    635
    467)
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 64
    Width = 619
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 619
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    TabOrder = 1
  end
end
