object fmRoundedCorners: TfmRoundedCorners
  Left = 0
  Top = 0
  Caption = 'Styled RoundedCorners Demo'
  ClientHeight = 418
  ClientWidth = 817
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
    Top = 346
    Width = 817
    Height = 72
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 313
    object WidthLabel: TLabel
      Left = 284
      Top = 10
      Width = 35
      Height = 15
      Alignment = taRightJustify
      Caption = 'Width:'
    end
    object HeightLabel: TLabel
      Left = 280
      Top = 40
      Width = 39
      Height = 15
      Alignment = taRightJustify
      Caption = 'Height:'
    end
    object ShowCaptionCheckBox: TCheckBox
      Left = 10
      Top = 6
      Width = 110
      Height = 17
      Caption = 'Show Captions'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = ShowCaptionCheckBoxClick
    end
    object ShowIconCheckBox: TCheckBox
      Left = 10
      Top = 36
      Width = 110
      Height = 17
      Caption = 'Show Icons'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = ShowIconCheckBoxClick
    end
    object cbCaptionAlignment: TComboBox
      Left = 126
      Top = 5
      Width = 139
      Height = 23
      Style = csDropDownList
      TabOrder = 2
      OnSelect = cbCaptionAlignmentSelect
    end
    object cbImageAlignment: TComboBox
      Left = 126
      Top = 33
      Width = 139
      Height = 23
      Style = csDropDownList
      TabOrder = 3
      OnSelect = cbImageAlignmentSelect
    end
    object tbWidth: TTrackBar
      Left = 325
      Top = 6
      Width = 191
      Height = 30
      Hint = 'Change ButtonGroup.ButtonWidth'
      Max = 200
      Min = 50
      Frequency = 5
      Position = 100
      TabOrder = 4
      OnChange = tbWidthChange
    end
    object tbHeight: TTrackBar
      Left = 325
      Top = 37
      Width = 191
      Height = 40
      Hint = 'Change ButtonGroup.ButtonHeight'
      Max = 60
      Min = 30
      Frequency = 5
      Position = 40
      TabOrder = 5
      OnChange = tbHeightChange
    end
  end
  object OptionsPanel: TPanel
    Left = 0
    Top = 0
    Width = 817
    Height = 346
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 313
    object ButtonGroup: TStyledButtonGroup
      Left = 1
      Top = 41
      Width = 120
      Height = 304
      Align = alLeft
      BevelInner = bvNone
      BevelOuter = bvNone
      ButtonHeight = 40
      ButtonWidth = 100
      ButtonOptions = [gboFullSize, gboShowCaptions]
      Images = VirtualImageList
      Items = <
        item
          Action = acFileOpen
        end
        item
          Caption = 'Close'
          ImageIndex = 0
          StyleFamily = 'Bootstrap'
          StyleClass = 'Warning'
          StyleAppearance = 'Normal'
        end
        item
          Caption = 'Button'
          ImageIndex = 6
          StyleFamily = 'Bootstrap'
          StyleClass = 'Success'
          StyleAppearance = 'Normal'
        end>
      TabOrder = 0
      StyleDrawType = btRounded
      StyleRoundedCorners = [rcTopRight, rcBottomRight]
      StyleFamily = 'Bootstrap'
      ExplicitHeight = 271
    end
    object Toolbar: TStyledToolbar
      Left = 1
      Top = 1
      Width = 815
      Height = 40
      ButtonHeight = 40
      ButtonWidth = 120
      Images = VirtualImageList
      ShowCaptions = True
      TabOrder = 1
      StyleDrawType = btRounded
      object tlbHome: TStyledToolButton
        Left = 0
        Top = 0
        Caption = 'Home'
        ImageAlignment = iaLeft
        ImageIndex = 2
        ImageMargins.Left = 8
        ImageMargins.Right = 8
        StyleFamily = 'SVG-Colors'
        StyleClass = 'Aquamarine'
      end
      object tlbSeparator: TStyledToolButton
        Left = 120
        Top = 0
        ImageIndex = 1
        StyleDrawType = btRoundRect
        Style = tbsSeparator
      end
      object tlbLeft: TStyledToolButton
        Left = 126
        Top = 0
        Down = True
        Caption = 'Left'
        ImageAlignment = iaLeft
        ImageIndex = 24
        PressedImageIndex = 27
        SelectedImageIndex = 27
        ImageMargins.Left = 8
        ImageMargins.Right = 8
        StyleRoundedCorners = [rcTopLeft, rcBottomLeft]
        StyleFamily = 'SVG-Colors'
        StyleClass = 'Aliceblue'
        Grouped = True
        Style = tbsCheck
      end
      object tlbCenter: TStyledToolButton
        Left = 246
        Top = 0
        Caption = 'Center'
        ImageAlignment = iaLeft
        ImageIndex = 25
        PressedImageIndex = 27
        SelectedImageIndex = 27
        ImageMargins.Left = 8
        ImageMargins.Right = 8
        StyleRoundedCorners = []
        StyleFamily = 'SVG-Colors'
        StyleClass = 'Aliceblue'
        Grouped = True
        Style = tbsCheck
      end
      object tlbRight: TStyledToolButton
        Left = 366
        Top = 0
        Caption = 'Right'
        ImageAlignment = iaLeft
        ImageIndex = 26
        PressedImageIndex = 27
        SelectedImageIndex = 27
        ImageMargins.Left = 8
        ImageMargins.Right = 8
        StyleRoundedCorners = [rcTopRight, rcBottomRight]
        StyleFamily = 'SVG-Colors'
        StyleClass = 'Aliceblue'
        Grouped = True
        Style = tbsCheck
      end
    end
    object CategoryButtons: TStyledCategoryButtons
      Left = 696
      Top = 41
      Width = 120
      Height = 304
      Align = alRight
      BevelInner = bvNone
      BevelOuter = bvNone
      ButtonFlow = cbfVertical
      ButtonHeight = 40
      ButtonOptions = [boFullSize, boGradientFill, boShowCaptions]
      Categories = <
        item
          Caption = 'Functions'
          Color = 16777194
          Collapsed = False
          Items = <
            item
              Caption = 'Home'
              ImageIndex = 2
              StyleFamily = 'Angular-Light'
              StyleClass = 'DeepPurple'
              StyleAppearance = 'Raised'
            end>
        end
        item
          Caption = 'Alignment'
          Color = 15395839
          Collapsed = False
          Items = <
            item
              Caption = 'Left'
              ImageIndex = 24
              StyleRoundedCorners = [rcTopLeft, rcTopRight]
            end
            item
              Caption = 'Center'
              ImageIndex = 25
              StyleRoundedCorners = []
            end
            item
              Caption = 'Right'
              ImageIndex = 26
              StyleRoundedCorners = [rcBottomRight, rcBottomLeft]
            end>
        end>
      Images = VirtualImageList
      RegularButtonColor = clWhite
      SelectedButtonColor = 15132390
      TabOrder = 2
      StyleRadius = 26
      StyleDrawType = btRoundRect
      StyleFamily = 'Angular-Light'
      StyleClass = 'Amber'
      StyleAppearance = 'Raised'
      ExplicitHeight = 271
    end
    object CenterPanel: TPanel
      Left = 121
      Top = 41
      Width = 575
      Height = 304
      Align = alClient
      TabOrder = 3
      ExplicitHeight = 271
      object TopPanel: TPanel
        Left = 1
        Top = 1
        Width = 573
        Height = 42
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 0
        object AngularButton: TStyledButton
          Left = 1
          Top = 1
          Width = 110
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Down = True
          Caption = 'Angular'
          ImageIndex = 19
          Images = VirtualImageList
          TabOrder = 0
          StyleRoundedCorners = [rcTopLeft, rcBottomLeft]
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
        object BootstrapButton: TStyledButton
          Left = 111
          Top = 1
          Width = 116
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Caption = 'Bootstrap'
          ImageIndex = 20
          Images = VirtualImageList
          TabOrder = 1
          StyleRoundedCorners = []
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
        object ColoredButton: TStyledButton
          Left = 227
          Top = 1
          Width = 116
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Caption = 'Colored'
          ImageIndex = 23
          Images = VirtualImageList
          TabOrder = 2
          StyleRoundedCorners = []
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
        object ClassicButton: TStyledButton
          Left = 343
          Top = 1
          Width = 116
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Caption = 'Classic'
          ImageIndex = 21
          Images = VirtualImageList
          TabOrder = 3
          StyleRoundedCorners = [rcTopRight, rcBottomRight]
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
      end
      object GraphicPanel: TPanel
        Left = 1
        Top = 43
        Width = 573
        Height = 42
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 1
        object GraphicAngularButton: TStyledGraphicButton
          Left = 1
          Top = 1
          Width = 110
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Transparent = True
          Caption = 'Angular'
          Images = VirtualImageList
          ImageIndex = 19
          StyleRoundedCorners = [rcTopLeft, rcBottomLeft]
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
        object GraphicBootstrapButton: TStyledGraphicButton
          Left = 111
          Top = 1
          Width = 116
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Transparent = True
          Caption = 'Bootstrap'
          Images = VirtualImageList
          ImageIndex = 20
          StyleRoundedCorners = []
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
        object GraphicClassicButton: TStyledGraphicButton
          Left = 343
          Top = 1
          Width = 116
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Down = True
          Transparent = True
          Caption = 'Classic'
          Images = VirtualImageList
          ImageIndex = 21
          StyleRoundedCorners = [rcTopRight, rcBottomRight]
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
        object GraphicColoredButton: TStyledGraphicButton
          Left = 227
          Top = 1
          Width = 116
          Height = 40
          Align = alLeft
          GroupIndex = 1
          Transparent = True
          Caption = 'Colored'
          Images = VirtualImageList
          ImageIndex = 23
          StyleRoundedCorners = []
          StyleDrawType = btRounded
          StyleAppearance = 'Outline'
        end
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 4
        Top = 88
        Width = 567
        Height = 87
        Align = alTop
        Caption = 'FAB'
        TabOrder = 2
        object btn_FABTrash: TStyledButton
          AlignWithMargins = True
          Left = 22
          Top = 20
          Width = 64
          Height = 62
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 13
          Images = VirtualImageList
          TabOrder = 0
          StyleRadius = 30
          StyleRoundedCorners = [rcBottomRight, rcBottomLeft]
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
        end
        object btn_FABBookmark: TStyledButton
          AlignWithMargins = True
          Left = 126
          Top = 20
          Width = 64
          Height = 62
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 16
          Images = VirtualImageList
          TabOrder = 1
          StyleRoundedCorners = [rcTopRight, rcBottomLeft]
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'Amber'
        end
        object btn_FABHome: TStyledButton
          AlignWithMargins = True
          Left = 230
          Top = 20
          Width = 64
          Height = 62
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 17
          Images = VirtualImageList
          TabOrder = 2
          StyleRoundedCorners = [rcTopLeft, rcBottomRight]
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
        end
        object btn_FABHeartDisabled: TStyledButton
          AlignWithMargins = True
          Left = 334
          Top = 20
          Width = 64
          Height = 62
          Margins.Left = 20
          Margins.Right = 20
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 8
          Images = VirtualImageList
          TabOrder = 3
          StyleRoundedCorners = [rcTopLeft, rcTopRight]
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
        end
      end
      object IconButtonsGroupBox: TGroupBox
        AlignWithMargins = True
        Left = 4
        Top = 181
        Width = 567
        Height = 68
        Align = alTop
        Caption = 'Icon Buttons'
        TabOrder = 3
        object btn_IconHome: TStyledButton
          AlignWithMargins = True
          Left = 47
          Top = 20
          Width = 45
          Height = 43
          Margins.Left = 0
          Margins.Right = 0
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 0
          Images = VirtualImageList
          NotificationBadge.Color = clFuchsia
          NotificationBadge.NotificationCount = 100
          TabOrder = 1
          StyleRoundedCorners = []
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
        end
        object btn_IconDots: TStyledButton
          AlignWithMargins = True
          Left = 2
          Top = 20
          Width = 45
          Height = 43
          Margins.Left = 0
          Margins.Right = 0
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 10
          Images = VirtualImageList
          NotificationBadge.CustomText = '!'
          TabOrder = 0
          StyleRoundedCorners = [rcTopLeft, rcBottomLeft]
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
        end
        object btn_IconMenu: TStyledButton
          AlignWithMargins = True
          Left = 92
          Top = 20
          Width = 45
          Height = 43
          Margins.Left = 0
          Margins.Right = 0
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 4
          Images = VirtualImageList
          NotificationBadge.NotificationCount = 5
          NotificationBadge.Size = nbsSmallDot
          TabOrder = 2
          StyleRoundedCorners = [rcTopRight, rcBottomRight]
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'DeepPurple'
          StyleAppearance = 'Basic'
        end
        object btn_trash: TStyledButton
          AlignWithMargins = True
          Left = 182
          Top = 20
          Width = 45
          Height = 43
          Margins.Left = 0
          Margins.Right = 0
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 14
          Images = VirtualImageList
          NotificationBadge.Color = clTeal
          NotificationBadge.NotificationCount = 5
          TabOrder = 3
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
        end
        object btn_IconLaunchDisabled: TStyledButton
          AlignWithMargins = True
          Left = 227
          Top = 20
          Width = 45
          Height = 43
          Margins.Left = 0
          Margins.Right = 0
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 12
          Images = VirtualImageList
          TabOrder = 4
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
        end
        object btn_IconHeart: TStyledButton
          AlignWithMargins = True
          Left = 137
          Top = 20
          Width = 45
          Height = 43
          Margins.Left = 0
          Margins.Right = 0
          Align = alLeft
          ImageAlignment = iaCenter
          ImageIndex = 8
          Images = VirtualImageList
          NotificationBadge.Color = clOlive
          NotificationBadge.NotificationCount = 12
          TabOrder = 5
          StyleDrawType = btRounded
          StyleFamily = 'Angular-Light'
          StyleClass = 'Warn'
          StyleAppearance = 'Basic'
        end
      end
    end
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
      end
      item
        CollectionIndex = 24
        CollectionName = 'ic_format_align_left_black'
        Name = 'ic_format_align_left_black'
      end
      item
        CollectionIndex = 25
        CollectionName = 'ic_format_align_center_black'
        Name = 'ic_format_align_center_black'
      end
      item
        CollectionIndex = 26
        CollectionName = 'ic_format_align_right_black'
        Name = 'ic_format_align_right_black'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ic_check_black'
        Name = 'ic_check_black'
      end>
    ImageCollection = dmResources.ImageCollection
    Width = 36
    Height = 36
    Left = 608
    Top = 360
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 584
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
