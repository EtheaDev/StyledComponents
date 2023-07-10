object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'StyledToolbar Test'
  ClientHeight = 534
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnClick = ToolButtonclick
  OnCreate = FormCreate
  TextHeight = 15
  object ToolBar1: TToolBar
    AlignWithMargins = True
    Left = 3
    Top = 69
    Width = 735
    Height = 58
    AutoSize = True
    ButtonHeight = 58
    ButtonWidth = 49
    Caption = 'ToolBar1'
    Images = VirtualImageList
    ShowCaptions = True
    TabOrder = 1
    TabStop = True
    OnClick = ToolBarClick
    object ToolButton2: TToolButton
      Left = 0
      Top = 0
      Hint = 'Open Button'
      AllowAllUp = True
      Caption = 'Open'
      Down = True
      Grouped = True
      ImageIndex = 12
      Style = tbsCheck
    end
    object ToolButton1: TToolButton
      Left = 49
      Top = 0
      Hint = 'Menu button'
      Caption = 'Menu'
      Grouped = True
      ImageIndex = 4
      Style = tbsCheck
    end
    object ToolButton5: TToolButton
      Left = 98
      Top = 0
      Width = 10
      Hint = 'Hint of Sep'
      Caption = 'Sep'
      ImageIndex = 11
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 108
      Top = 0
      Hint = 'Like Button'
      Caption = 'Like'
      Down = True
      Grouped = True
      ImageIndex = 8
      Style = tbsCheck
    end
    object ToolButton4: TToolButton
      Left = 157
      Top = 0
      Hint = 'Options button'
      Caption = 'Options'
      Grouped = True
      ImageIndex = 10
      Style = tbsCheck
    end
    object ToolButton6: TToolButton
      Left = 206
      Top = 0
      Width = 10
      Caption = 'ToolButton6'
      ImageIndex = 9
      Style = tbsDivider
    end
  end
  object StyledToolbar1: TStyledToolbar
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 735
    Height = 60
    AutoSize = True
    Caption = 'Control: Open - Top:-3 - Left:336 - Width: 61 - Height: 60'
    DragMode = dmAutomatic
    ParentColor = True
    TabOrder = 0
    ButtonHeight = 60
    ButtonWidth = 60
    Images = VirtualImageList
    Indent = 3
    ShowCaptions = True
    StyleFamily = 'Bootstrap'
    StyleAppearance = 'Outline'
    object StyledToolButton1: TStyledToolButton
      Left = 0
      Top = 0
      Hint = ''
      DragMode = dmAutomatic
      Caption = 'Open'
      Images = VirtualImageList
      ImageIndex = 12
      StyleFamily = 'Bootstrap'
      StyleClass = 'Success'
      StyleAppearance = 'Outline'
      AllowAllUp = True
      Down = True
      Grouped = True
      Style = tbsStyledCheck
    end
    object StyledToolButton2: TStyledToolButton
      Left = 60
      Top = 0
      Hint = ''
      DragMode = dmAutomatic
      Caption = 'Menu'
      Images = VirtualImageList
      ImageIndex = 4
      StyleFamily = 'Bootstrap'
      StyleAppearance = 'Outline'
      Grouped = True
      Style = tbsStyledCheck
    end
    object SSep1: TStyledToolButton
      Left = 120
      Top = 0
      Width = 10
      Hint = ''
      DragMode = dmAutomatic
      Enabled = False
      Caption = ''
      StyleFamily = 'Bootstrap'
      StyleAppearance = 'Outline'
      Style = tbsStyledSeparator
    end
    object StyledToolButton3: TStyledToolButton
      Left = 130
      Top = 0
      Hint = ''
      DragMode = dmAutomatic
      Caption = 'Like'
      Images = VirtualImageList
      ImageIndex = 8
      StyleFamily = 'Bootstrap'
      StyleAppearance = 'Outline'
      Down = True
      Grouped = True
      Style = tbsStyledCheck
    end
    object StyledToolButton5: TStyledToolButton
      Left = 190
      Top = 0
      Hint = ''
      DragMode = dmAutomatic
      Caption = 'Options'
      Images = VirtualImageList
      ImageIndex = 10
      StyleFamily = 'Bootstrap'
      StyleAppearance = 'Outline'
      Grouped = True
      Style = tbsStyledCheck
    end
    object StyledToolButton6: TStyledToolButton
      Left = 250
      Top = 0
      Width = 10
      Hint = ''
      Enabled = False
      Caption = 'StyledToolButton6'
      StyleFamily = 'Bootstrap'
      StyleAppearance = 'Outline'
      Style = tbsStyledSeparator
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 456
    Width = 741
    Height = 78
    Align = alBottom
    TabOrder = 2
    object LeftPanel: TPanel
      Left = 1
      Top = 1
      Width = 122
      Height = 76
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object CreateButton: TStyledButton
        Left = 8
        Top = 14
        Width = 108
        Height = 41
        Hint = 'Create Toolbar "runtime"'
        OnClick = CreateButtonClick
        Caption = 'Create Toolbars'
        TabOrder = 0
      end
    end
    object BottomClientPanel: TPanel
      Left = 123
      Top = 1
      Width = 617
      Height = 76
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      object WidthLabel: TLabel
        Left = 20
        Top = 15
        Width = 35
        Height = 15
        Alignment = taRightJustify
        Caption = 'Width:'
      end
      object HeightLabel: TLabel
        Left = 16
        Top = 39
        Width = 39
        Height = 15
        Alignment = taRightJustify
        Caption = 'Height:'
      end
      object tbWidth: TTrackBar
        Left = 57
        Top = 5
        Width = 191
        Height = 30
        Hint = 'Change Toolbar.ButtonWidth'
        Max = 100
        Frequency = 5
        Position = 34
        TabOrder = 0
        OnChange = UpdateToolbars
      end
      object tbHeight: TTrackBar
        Left = 57
        Top = 36
        Width = 191
        Height = 30
        Hint = 'Change Toolbar.ButtonHeight'
        Max = 100
        Frequency = 5
        Position = 40
        TabOrder = 1
        OnChange = UpdateToolbars
      end
      object ShowCaptionCheckBox: TCheckBox
        Left = 266
        Top = 13
        Width = 110
        Height = 17
        Caption = 'Show Captions'
        TabOrder = 2
        OnClick = UpdateToolbars
      end
      object ListCheckBox: TCheckBox
        Left = 266
        Top = 36
        Width = 110
        Height = 17
        Caption = 'List'
        TabOrder = 3
        OnClick = UpdateToolbars
      end
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 130
    Width = 741
    Height = 326
    Align = alClient
    TabOrder = 3
  end
  object VirtualImageList: TVirtualImageList
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
      end>
    ImageCollection = dmResources.ImageCollection
    Width = 36
    Height = 36
    Left = 520
    Top = 392
  end
end
