object fmStyledCategoryButtons: TfmStyledCategoryButtons
  Left = 0
  Top = 0
  Caption = 'StyledCategoryButtons Test'
  ClientHeight = 534
  ClientWidth = 906
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object BottomPanel: TPanel
    Left = 0
    Top = 456
    Width = 906
    Height = 78
    Align = alBottom
    TabOrder = 1
    object LeftPanel: TPanel
      Left = 1
      Top = 1
      Width = 160
      Height = 76
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object CreateButton: TStyledButton
        AlignWithMargins = True
        Left = 10
        Top = 10
        Width = 140
        Height = 56
        Hint = 'Create CategoryButtons "runtime"'
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alClient
        Caption = 'Create CategoryButtons'
        TabOrder = 0
        StyleElements = [seFont, seBorder]
        OnClick = CreateButtonClick
        StyleDrawType = btRounded
        StyleClass = 'Windows10'
      end
    end
    object BottomClientPanel: TPanel
      Left = 161
      Top = 1
      Width = 744
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
        Left = 61
        Top = 5
        Width = 191
        Height = 30
        Hint = 'Change CategoryButtons.ButtonWidth'
        Max = 150
        Min = 50
        Frequency = 5
        Position = 50
        TabOrder = 0
        OnChange = UpdateCategoryButtons
      end
      object tbHeight: TTrackBar
        Left = 61
        Top = 36
        Width = 191
        Height = 30
        Hint = 'Change CategoryButtons.ButtonHeight'
        Max = 100
        Frequency = 5
        Position = 40
        TabOrder = 1
        OnChange = UpdateCategoryButtons
      end
      object ShowCaptionCheckBox: TCheckBox
        Left = 266
        Top = 7
        Width = 110
        Height = 17
        Caption = 'Show Captions'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = UpdateCategoryButtons
      end
      object FullSizeCheckBox: TCheckBox
        Left = 266
        Top = 30
        Width = 110
        Height = 17
        Caption = 'Full Size Buttons'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = UpdateCategoryButtons
      end
      object ShowIconCheckBox: TCheckBox
        Left = 266
        Top = 52
        Width = 110
        Height = 17
        Caption = 'Show Icons'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = UpdateCategoryButtons
      end
      object cbCaptionAlignment: TComboBox
        Left = 382
        Top = 4
        Width = 139
        Height = 23
        Style = csDropDownList
        TabOrder = 5
        OnSelect = cbCaptionAlignmentSelect
      end
      object cbImageAlignment: TComboBox
        Left = 382
        Top = 49
        Width = 139
        Height = 23
        Style = csDropDownList
        TabOrder = 6
        OnSelect = cbImageAlignmentSelect
      end
    end
  end
  object CategoryButtons: TCategoryButtons
    Left = 0
    Top = 0
    Width = 177
    Height = 456
    Align = alLeft
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions]
    Categories = <
      item
        Caption = 'VCL First'
        Color = 16771839
        Collapsed = False
        Items = <
          item
            Caption = 'one'
            ImageIndex = 4
          end
          item
            Caption = 'two'
            ImageIndex = 8
          end
          item
            Caption = 'three'
            ImageIndex = 12
          end>
      end
      item
        Caption = 'VCL Second'
        Color = 16053492
        Collapsed = False
        Items = <
          item
            Caption = 'four'
            ImageIndex = 14
          end
          item
            Caption = 'five'
            ImageIndex = 19
          end
          item
            Caption = 'six'
            ImageIndex = 1
          end>
      end>
    Images = VirtualImageList
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 2
    OnButtonClicked = CategoryButtonsButtonClicked
  end
  object StyledCategoryButtons: TStyledCategoryButtons
    Left = 177
    Top = 0
    Width = 177
    Height = 456
    Align = alLeft
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions]
    Categories = <
      item
        Caption = 'Styled First'
        Color = 16771839
        Collapsed = False
        Items = <
          item
            Caption = 'one'
            ImageIndex = 4
          end
          item
            Caption = 'two'
            ImageIndex = 8
          end
          item
            Caption = 'three'
            ImageIndex = 12
          end>
      end
      item
        Caption = 'Styled Second'
        Color = 16053492
        Collapsed = False
        Items = <
          item
            Caption = 'four'
            ImageIndex = 14
          end
          item
            Caption = 'five'
            ImageIndex = 19
          end
          item
            Caption = 'six'
            ImageIndex = 1
          end>
      end>
    Images = VirtualImageList
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
    OnButtonClicked = CategoryButtonsButtonClicked
  end
  object StyledCategoryButtonsBootstrap: TStyledCategoryButtons
    Left = 354
    Top = 0
    Width = 177
    Height = 456
    Align = alLeft
    ButtonFlow = cbfVertical
    ButtonHeight = 36
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions]
    Categories = <
      item
        Caption = 'Styled Rounded First'
        Color = 16771839
        Collapsed = False
        Items = <
          item
            Caption = 'one'
            ImageIndex = 4
          end
          item
            Caption = 'two'
            ImageIndex = 8
            StyleFamily = 'Bootstrap'
            StyleClass = 'Secondary'
            StyleAppearance = 'Normal'
          end
          item
            Caption = 'three'
            ImageIndex = 12
            StyleFamily = 'Bootstrap'
            StyleClass = 'Success'
            StyleAppearance = 'Normal'
          end>
      end
      item
        Caption = 'Styled Rounded Second'
        Color = 16053492
        Collapsed = False
        Items = <
          item
            Caption = 'four'
            ImageIndex = 14
            StyleFamily = 'Bootstrap'
            StyleClass = 'Danger'
            StyleAppearance = 'Normal'
          end
          item
            Caption = 'five'
            ImageIndex = 19
            StyleFamily = 'Bootstrap'
            StyleClass = 'Warning'
            StyleAppearance = 'Normal'
          end
          item
            Caption = 'six'
            ImageIndex = 1
            StyleFamily = 'Bootstrap'
            StyleClass = 'Info'
            StyleAppearance = 'Normal'
          end>
      end>
    Images = VirtualImageList
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 3
    OnButtonClicked = CategoryButtonsButtonClicked
    StyleDrawType = btRounded
    StyleFamily = 'Bootstrap'
    OnGetNotificationBadgeInfo = StyledCategoryButtonsBootstrapGetNotificationBadgeInfo
  end
  object BadgeTimer: TTimer
    Interval = 5000
    OnTimer = BadgeTimerTimer
    Left = 608
    Top = 272
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
    Left = 608
    Top = 360
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 472
    Top = 168
    object acFileOpen: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 12
      ShortCut = 16463
    end
  end
end
