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
  object Button1: TButton
    Left = 8
    Top = 96
    Width = 129
    Height = 49
    Caption = 'Decrypt'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 64
    Width = 619
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 619
    Height = 169
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button2: TButton
    Left = 216
    Top = 96
    Width = 145
    Height = 49
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 384
    Top = 96
    Width = 137
    Height = 49
    Caption = 'Button3'
    TabOrder = 4
    OnClick = Button3Click
  end
end
