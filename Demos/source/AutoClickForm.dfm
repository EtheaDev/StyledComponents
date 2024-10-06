object fmAutoClick: TfmAutoClick
  Left = 0
  Top = 0
  ActiveControl = AutoClickButtonRounded
  Caption = 'AutoClick Demo'
  ClientHeight = 321
  ClientWidth = 413
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object lbAutoClickDelay: TLabel
    Left = 266
    Top = 16
    Width = 59
    Height = 15
    Caption = 'Delay (ms):'
  end
  object Label1: TLabel
    Left = 266
    Top = 72
    Width = 59
    Height = 15
    Caption = 'Delay (ms):'
  end
  object Label2: TLabel
    Left = 266
    Top = 132
    Width = 59
    Height = 15
    Caption = 'Delay (ms):'
  end
  object AutoClickButtonRounded: TStyledButton
    Left = 8
    Top = 16
    Width = 239
    Height = 41
    Caption = 'Bootstrap Auto Click Button'
    ImageIndex = 11
    Images = VirtualImageList32
    TabOrder = 0
    OnClick = AutoClick
    StyleDrawType = btRounded
    StyleFamily = 'Bootstrap'
  end
  object AutoClickButtonRoundRect: TStyledButton
    Left = 8
    Top = 72
    Width = 239
    Height = 41
    Caption = 'Classic Auto Click Button'
    ImageIndex = 8
    Images = VirtualImageList32
    TabOrder = 1
    OnClick = AutoClick
    StyleRadius = 20
  end
  object AutoClickButtonRect: TStyledButton
    Left = 8
    Top = 132
    Width = 239
    Height = 41
    Caption = 'DarkRed Auto Click Button'
    ImageIndex = 4
    Images = VirtualImageList32
    TabOrder = 2
    OnClick = AutoClick
    StyleFamily = 'SVG-Colors'
    StyleClass = 'Darkred'
  end
  object StartStyledButton: TStyledButton
    Left = 8
    Top = 188
    Width = 117
    Height = 37
    Caption = 'Start Test...'
    TabOrder = 3
    OnClick = StartStyledButtonClick
  end
  object RoundedSpinEdit: TSpinEdit
    Left = 266
    Top = 33
    Width = 62
    Height = 24
    Increment = 500
    MaxValue = 100000
    MinValue = 1000
    TabOrder = 4
    Value = 3000
  end
  object RoundRectSpinEdit: TSpinEdit
    Left = 266
    Top = 89
    Width = 62
    Height = 24
    Increment = 500
    MaxValue = 100000
    MinValue = 1000
    TabOrder = 5
    Value = 6000
  end
  object RectSpinEdit: TSpinEdit
    Left = 266
    Top = 149
    Width = 62
    Height = 24
    Increment = 500
    MaxValue = 100000
    MinValue = 1000
    TabOrder = 6
    Value = 9000
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 240
    Width = 342
    Height = 78
    Caption = 'Test Task Dialogs with AutoClick'
    TabOrder = 7
    object Label3: TLabel
      Left = 96
      Top = 27
      Width = 59
      Height = 15
      Caption = 'Delay (ms):'
    end
    object TestDialogsStyledButton: TStyledButton
      Left = 231
      Top = 25
      Width = 103
      Height = 37
      Caption = 'Test Dialogs...'
      TabOrder = 3
      OnClick = TestDialogsStyledButtonClick
    end
    object DialogsSpinEdit: TSpinEdit
      Left = 167
      Top = 24
      Width = 54
      Height = 24
      Increment = 500
      MaxValue = 100000
      MinValue = 1000
      TabOrder = 1
      Value = 3000
    end
    object cbAutoClick: TCheckBox
      Left = 12
      Top = 27
      Width = 74
      Height = 17
      Caption = 'AutoClick'
      TabOrder = 0
    end
    object cbUseCommandLinks: TCheckBox
      Left = 12
      Top = 50
      Width = 145
      Height = 17
      Caption = 'Use Command Links'
      TabOrder = 2
    end
  end
  object VirtualImageList32: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'home-deeppurple'
        Name = 'home-deeppurple'
      end
      item
        CollectionIndex = 1
        CollectionName = 'home-indigo'
        Name = 'home-indigo'
      end
      item
        CollectionIndex = 2
        CollectionName = 'home-pink'
        Name = 'home-pink'
      end
      item
        CollectionIndex = 3
        CollectionName = 'home-purple'
        Name = 'home-purple'
      end
      item
        CollectionIndex = 4
        CollectionName = 'menu-amber'
        Name = 'menu-amber'
      end
      item
        CollectionIndex = 5
        CollectionName = 'menu-pink'
        Name = 'menu-pink'
      end
      item
        CollectionIndex = 6
        CollectionName = 'menu-Blue-grey'
        Name = 'menu-Blue-grey'
      end
      item
        CollectionIndex = 7
        CollectionName = 'menu-green'
        Name = 'menu-green'
      end
      item
        CollectionIndex = 8
        CollectionName = 'heart'
        Name = 'heart'
      end
      item
        CollectionIndex = 9
        CollectionName = 'dots-vertical-white'
        Name = 'dots-vertical-white'
      end
      item
        CollectionIndex = 10
        CollectionName = 'dots-vertical-black'
        Name = 'dots-vertical-black'
      end
      item
        CollectionIndex = 11
        CollectionName = 'launch-white'
        Name = 'launch-white'
      end
      item
        CollectionIndex = 12
        CollectionName = 'launch-black'
        Name = 'launch-black'
      end
      item
        CollectionIndex = 13
        CollectionName = 'trash-white'
        Name = 'trash-white'
      end
      item
        CollectionIndex = 14
        CollectionName = 'trash-black'
        Name = 'trash-black'
      end
      item
        CollectionIndex = 15
        CollectionName = 'bookmark-white'
        Name = 'bookmark-white'
      end
      item
        CollectionIndex = 16
        CollectionName = 'bookmark-black'
        Name = 'bookmark-black'
      end
      item
        CollectionIndex = 17
        CollectionName = 'home-white'
        Name = 'home-white'
      end
      item
        CollectionIndex = 18
        CollectionName = 'home-black'
        Name = 'home-black'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Angular-logo'
        Name = 'Angular-logo'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Bootstrap-logo'
        Name = 'Bootstrap-logo'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Delphi-Logo'
        Name = 'Delphi-Logo'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Basic-Color'
        Name = 'Basic-Color'
      end
      item
        CollectionIndex = 23
        CollectionName = 'SVG-Color'
        Name = 'SVG-Color'
      end>
    ImageCollection = dmResources.ImageCollection
    PreserveItems = True
    Width = 32
    Height = 32
    Left = 317
    Top = 193
  end
end
