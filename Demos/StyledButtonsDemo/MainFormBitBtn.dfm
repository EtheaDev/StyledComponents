object fmBitBtn: TfmBitBtn
  Left = 0
  Top = 0
  Caption = 'BitButtons - Kind and ModalResult Demo'
  ClientHeight = 365
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object btOK: TBitBtn
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
    OnClick = ButtonClick
    OnMouseEnter = ButtonMouseEnter
    OnMouseLeave = ButtonMouseLeave
  end
  object btCancel: TBitBtn
    Left = 16
    Top = 47
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
    OnClick = ButtonClick
  end
  object btClose: TBitBtn
    Left = 16
    Top = 78
    Width = 75
    Height = 25
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 2
    OnClick = ButtonClick
  end
  object btYes: TBitBtn
    Left = 16
    Top = 109
    Width = 75
    Height = 25
    Kind = bkYes
    NumGlyphs = 2
    TabOrder = 3
    OnClick = ButtonClick
  end
  object btNo: TBitBtn
    Left = 16
    Top = 140
    Width = 75
    Height = 25
    Kind = bkNo
    NumGlyphs = 2
    TabOrder = 4
    OnClick = ButtonClick
  end
  object btAbort: TBitBtn
    Left = 16
    Top = 171
    Width = 75
    Height = 25
    Kind = bkAbort
    NumGlyphs = 2
    TabOrder = 5
    OnClick = ButtonClick
  end
  object btRetry: TBitBtn
    Left = 16
    Top = 202
    Width = 75
    Height = 25
    Kind = bkRetry
    NumGlyphs = 2
    TabOrder = 6
    OnClick = ButtonClick
  end
  object btIgnore: TBitBtn
    Left = 16
    Top = 233
    Width = 75
    Height = 25
    Kind = bkIgnore
    NumGlyphs = 2
    TabOrder = 7
    OnClick = ButtonClick
  end
  object btHelp: TBitBtn
    Left = 16
    Top = 264
    Width = 75
    Height = 25
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 8
    OnClick = ButtonClick
  end
  object btAll: TBitBtn
    Left = 16
    Top = 295
    Width = 75
    Height = 25
    Kind = bkAll
    NumGlyphs = 2
    TabOrder = 9
    OnClick = ButtonClick
  end
  object btCustom: TBitBtn
    Left = 16
    Top = 326
    Width = 75
    Height = 25
    Caption = 'Custom'
    TabOrder = 10
    OnClick = ButtonClick
  end
end
