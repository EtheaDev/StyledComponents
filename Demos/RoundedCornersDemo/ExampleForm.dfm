object RoundedCornerExamples: TRoundedCornerExamples
  Left = 0
  Top = 0
  ActiveControl = cbChangeStyle
  Caption = 'Rounded Corners Examples'
  ClientHeight = 240
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object NoTopLeftButton: TStyledButton
    Left = 24
    Top = 16
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopRight, rcBottomRight, rcBottomLeft]'
    Caption = 'No Top Left'
    TabOrder = 0
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopRight, rcBottomRight, rcBottomLeft]
    StyleDrawType = btRounded
    StyleClass = 'Aqua Graphite'
  end
  object NoTopButton: TStyledButton
    Left = 184
    Top = 16
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcBottomRight, rcBottomLeft]'
    Caption = 'No Top'
    TabOrder = 1
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcBottomRight, rcBottomLeft]
    StyleDrawType = btRounded
    StyleClass = 'Calypso'
  end
  object TopLeftBottomRightButton: TStyledButton
    Left = 352
    Top = 16
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopLeft, rcBottomRight]'
    Caption = 'Top Left + Bottom Right'
    TabOrder = 2
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopLeft, rcBottomRight]
    StyleDrawType = btRounded
    StyleClass = 'Emerald'
  end
  object NoTopRightButton: TStyledButton
    Left = 24
    Top = 62
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopLeft, rcBottomRight, rcBottomLeft]'
    Caption = 'No Top Right'
    TabOrder = 3
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopLeft, rcBottomRight, rcBottomLeft]
    StyleDrawType = btRounded
    StyleClass = 'Golden Graphite'
  end
  object NoLeftButton: TStyledButton
    Left = 184
    Top = 62
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopRight, rcBottomRight]'
    Caption = 'No Left'
    TabOrder = 4
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopRight, rcBottomRight]
    StyleDrawType = btRounded
    StyleClass = 'Lucky Point'
  end
  object BottomLeftTopRightButton: TStyledButton
    Left = 352
    Top = 62
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopRight, rcBottomLeft]'
    Caption = 'Bottom Left + Top Right'
    TabOrder = 5
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopRight, rcBottomLeft]
    StyleDrawType = btRounded
    StyleClass = 'Ruby Graphite'
  end
  object NoBottomLeftButton: TStyledButton
    Left = 24
    Top = 108
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopLeft, rcTopRight, rcBottomRight]'
    Caption = 'No Bottom Left'
    TabOrder = 6
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopLeft, rcTopRight, rcBottomRight]
    StyleDrawType = btRounded
    StyleClass = 'Windows10 Purple'
  end
  object NoRightButton: TStyledButton
    Left = 184
    Top = 108
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopLeft, rcBottomLeft]'
    Caption = 'No Right'
    TabOrder = 7
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopLeft, rcBottomLeft]
    StyleDrawType = btRounded
    StyleClass = 'Coral'
  end
  object RoundedButton: TStyledButton
    Left = 352
    Top = 108
    Width = 140
    Height = 40
    Hint = 
      'StyleRoundedCorners = [rcTopLeft,rcTopRight,rcBottomRight,rcBott' +
      'omLeft]'
    Caption = 'All Rounded Corners'
    TabOrder = 8
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleDrawType = btRounded
    StyleClass = 'Iceberg Classico'
  end
  object NoBottomRightButton: TStyledButton
    Left = 24
    Top = 154
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopLeft, rcTopRight, rcBottomLeft]'
    Caption = 'No Bottom Right'
    TabOrder = 9
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopLeft, rcTopRight, rcBottomLeft]
    StyleDrawType = btRounded
    StyleClass = 'Calypso'
  end
  object NoBottomButton: TStyledButton
    Left = 184
    Top = 154
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = [rcTopLeft, rcTopRight]'
    Caption = 'No Bottom'
    TabOrder = 10
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = [rcTopLeft, rcTopRight]
    StyleDrawType = btRounded
    StyleClass = 'Luna'
  end
  object NoRoundedButton: TStyledButton
    Left = 352
    Top = 154
    Width = 140
    Height = 40
    Hint = 'StyleRoundedCorners = []'
    Caption = 'No Rounded Corners'
    TabOrder = 11
    WordWrap = True
    StyleElements = [seFont, seBorder]
    OnClick = ButtonClick
    StyleRadius = 24
    StyleRoundedCorners = []
    StyleDrawType = btRounded
    StyleClass = 'Windows10 Green'
  end
  object StylePanel: TPanel
    Left = 0
    Top = 207
    Width = 518
    Height = 33
    Align = alBottom
    TabOrder = 12
    object StyleLabel: TLabel
      Left = 30
      Top = 9
      Width = 133
      Height = 15
      Caption = 'Change application style:'
    end
    object cbChangeStyle: TComboBox
      Left = 169
      Top = 6
      Width = 174
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnSelect = cbChangeStyleSelect
    end
  end
end
