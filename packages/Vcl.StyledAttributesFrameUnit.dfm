object AttributesFrame: TAttributesFrame
  Left = 0
  Top = 0
  Width = 364
  Height = 334
  TabOrder = 0
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 364
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonStyleLabel: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 60
      Height = 37
      Margins.Left = 10
      Margins.Top = 10
      Align = alLeft
      Caption = 'Style Normal'
      ExplicitHeight = 13
    end
    object StyledButton: TStyledButton
      Left = 162
      Top = 7
      Width = 129
      Height = 41
      Caption = 'StyledButton'
      TabOrder = 0
    end
  end
  object ButtonDrawingStyleGroupBox: TGroupBox
    Left = 0
    Top = 117
    Width = 364
    Height = 65
    Align = alTop
    Caption = 'Button Drawing Style'
    TabOrder = 2
    object ButtonDrawTypePanel: TPanel
      Left = 2
      Top = 15
      Width = 150
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object StyleDrawTypeLabel: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 0
        Width = 130
        Height = 13
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 10
        Margins.Bottom = 0
        Align = alTop
        Caption = 'DrawType:'
        ExplicitWidth = 53
      end
      object StyleDrawTypeComboBox: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 16
        Width = 144
        Height = 21
        Align = alClient
        Style = csDropDownList
        TabOrder = 0
      end
    end
    object RadiusPanel: TPanel
      Left = 152
      Top = 15
      Width = 210
      Height = 48
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object StyleRadiusLabel: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 0
        Width = 190
        Height = 13
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 10
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Radius:'
        ExplicitWidth = 36
      end
      object RadiusTrackBar: TTrackBar
        Left = 0
        Top = 13
        Width = 210
        Height = 35
        Align = alClient
        Max = 30
        Min = 1
        Position = 1
        TabOrder = 0
      end
    end
  end
  object BorderGroupBox: TGroupBox
    Left = 0
    Top = 182
    Width = 364
    Height = 67
    Align = alTop
    Caption = 'Border'
    TabOrder = 3
    object BorderColorPanel: TPanel
      Left = 155
      Top = 15
      Width = 207
      Height = 50
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object BorderColorLabel: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 195
        Height = 13
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Color'
        FocusControl = BorderColorBox
        ExplicitWidth = 25
      end
      object BorderColorBox: TColorBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 201
        Height = 22
        Align = alClient
        TabOrder = 0
      end
    end
    object BorderDrawStyleRadioGroup: TRadioGroup
      AlignWithMargins = True
      Left = 58
      Top = 18
      Width = 94
      Height = 44
      Align = alLeft
      Caption = 'Draw Style'
      Columns = 2
      Items.Strings = (
        'Solid'
        'Clear')
      TabOrder = 0
    end
    object BorderWidthPanel: TPanel
      Left = 2
      Top = 15
      Width = 53
      Height = 50
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object BorderDrawWidthLabel: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 3
        Width = 41
        Height = 13
        Margins.Left = 6
        Margins.Right = 6
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Width'
        FocusControl = BorderColorBox
        ExplicitTop = 6
        ExplicitWidth = 28
      end
      object BorderWidthSpinEdit: TSpinEdit
        Left = 3
        Top = 18
        Width = 45
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
    end
  end
  object FontGroupBox: TGroupBox
    Left = 0
    Top = 249
    Width = 364
    Height = 75
    Align = alTop
    Caption = 'Font'
    TabOrder = 4
    object FontColorPanel: TPanel
      Left = 152
      Top = 15
      Width = 210
      Height = 58
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 2
      ExplicitWidth = 360
      object FontColorLabel: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 198
        Height = 13
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Color'
        FocusControl = FontColorBox
        ExplicitWidth = 25
      end
      object FontColorBox: TColorBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 204
        Height = 22
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 354
      end
    end
    object FontStyleGroupBox: TGroupBox
      Left = 2
      Top = 15
      Width = 150
      Height = 58
      Align = alLeft
      Caption = 'Style'
      TabOrder = 0
      object FontBoldCheckBox: TCheckBox
        Left = 6
        Top = 16
        Width = 70
        Height = 17
        Caption = 'Bold'
        TabOrder = 0
      end
      object UnderlineCheckBox: TCheckBox
        Left = 6
        Top = 33
        Width = 70
        Height = 17
        Caption = 'Underline'
        TabOrder = 1
      end
      object ItalicCheckBox: TCheckBox
        Left = 78
        Top = 16
        Width = 70
        Height = 17
        Caption = 'Italic'
        TabOrder = 2
      end
      object StrikeoutCheckBox: TCheckBox
        Left = 78
        Top = 33
        Width = 70
        Height = 17
        Caption = 'Strikeout'
        TabOrder = 3
      end
    end
  end
  object ButtonFaceGroupBox: TGroupBox
    Left = 0
    Top = 50
    Width = 364
    Height = 67
    Align = alTop
    Caption = 'Button Face'
    TabOrder = 1
    object ButtonColorPanel: TPanel
      Left = 155
      Top = 15
      Width = 207
      Height = 50
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object ButtonColorLabel: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 195
        Height = 13
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Button Color'
        ExplicitWidth = 60
      end
      object ButtonColorBox: TColorBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 201
        Height = 22
        Align = alTop
        TabOrder = 0
      end
    end
    object ButtonDrawStyleRadioGroup: TRadioGroup
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 147
      Height = 44
      Align = alLeft
      Caption = 'Draw Style'
      Columns = 2
      Items.Strings = (
        'Solid'
        'Clear')
      TabOrder = 0
    end
  end
end
