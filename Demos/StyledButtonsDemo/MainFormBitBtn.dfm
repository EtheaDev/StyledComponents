object fmBitBtn: TfmBitBtn
  Left = 0
  Top = 0
  Caption = 'BitButtons - Kind and ModalResult Demo'
  ClientHeight = 364
  ClientWidth = 745
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
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
      33333337777FF377FF3333993370739993333377FF373F377FF3399993000339
      993337777F777F3377F3393999707333993337F77737333337FF993399933333
      399377F3777FF333377F993339903333399377F33737FF33377F993333707333
      399377F333377FF3377F993333101933399377F333777FFF377F993333000993
      399377FF3377737FF7733993330009993933373FF3777377F7F3399933000399
      99333773FF777F777733339993707339933333773FF7FFF77333333999999999
      3333333777333777333333333999993333333333377777333333}
    NumGlyphs = 2
    TabOrder = 10
    OnClick = ButtonClick
  end
end
