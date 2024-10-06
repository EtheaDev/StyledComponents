object frmMain: TfrmMain
  Left = 254
  Top = 225
  Caption = 'Styled Components Demos'
  ClientHeight = 661
  ClientWidth = 1059
  Color = clWindow
  Constraints.MinHeight = 690
  Constraints.MinWidth = 1000
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 13
  object WorkPanel: TPanel
    Left = 200
    Top = 44
    Width = 459
    Height = 617
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object WorkClientPanel: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 459
      Height = 617
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object WorkTitlePanel: TPanel
        Left = 0
        Top = 0
        Width = 459
        Height = 27
        Align = alTop
        TabOrder = 0
        object WorkTitleLabel: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 5
          Width = 57
          Height = 13
          Caption = 'Workspace'
        end
      end
    end
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 44
    Width = 200
    Height = 617
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object catMenuItems: TStyledCategoryButtons
      Left = 0
      Top = 0
      Width = 200
      Height = 617
      Align = alClient
      BackgroundGradientDirection = gdVertical
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 40
      ButtonWidth = 36
      ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions, boCaptionOnlyBorder]
      Categories = <
        item
          Caption = 'Styled Buttons'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acFamilyClassAppearance
            end
            item
              Action = acStyledBitBtn
            end
            item
              Action = acStyledButtonVCLStyles
            end
            item
              Action = acControlList
            end>
        end
        item
          Caption = 'Toolbar and Navigators'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acStyledToolbar
              Caption = 'Toolbar'
            end
            item
              Action = acStyledDbNavigator
            end>
        end
        item
          Caption = 'Grouped Buttons'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acStyledButtonGroup
            end
            item
              Action = acStyledCategoryButtons
            end>
        end
        item
          Caption = 'Dialogs'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acStyledTaskDialog
            end>
        end
        item
          Caption = 'Special options'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acAutoClick
            end
            item
              Action = acRoundedCorners
            end
            item
              Action = acAnimatedStyledButton
            end>
        end>
      HotButtonColor = 12500670
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 0
    end
  end
  object panlTop: TPanel
    Left = 0
    Top = 0
    Width = 1059
    Height = 44
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 53
      Top = 3
      Width = 138
      Height = 38
      Align = alLeft
      Caption = 'Styled Components Demos'
      Layout = tlCenter
    end
    object SettingsToolBar: TStyledToolbar
      AlignWithMargins = True
      Left = 926
      Top = 2
      Width = 131
      Height = 40
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      ButtonHeight = 40
      ButtonWidth = 40
      Indent = 2
      TabOrder = 0
      object AboutToolButton: TStyledToolButton
        Left = 0
        Top = 0
        Action = acAbout
      end
      object QuitToolButton: TStyledToolButton
        Left = 40
        Top = 0
        Action = acQuit
      end
      object SepToolButton: TStyledToolButton
        Left = 80
        Top = 0
        Style = tbsSeparator
      end
      object ColorSettingsToolButton: TStyledToolButton
        Left = 86
        Top = 0
        Action = actSettings
      end
    end
    object MenuButtonToolbar: TStyledToolbar
      AlignWithMargins = True
      Left = 2
      Top = 2
      Width = 46
      Height = 40
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alLeft
      ButtonHeight = 40
      ButtonWidth = 40
      Indent = 2
      TabOrder = 1
      object MenuToolButton: TStyledToolButton
        Left = 0
        Top = 0
        Action = actMenu
      end
    end
  end
  object RightPanel: TPanel
    Left = 659
    Top = 44
    Width = 400
    Height = 617
    Align = alRight
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    object pc: TPageControl
      Left = 0
      Top = 0
      Width = 400
      Height = 576
      ActivePage = tsFont
      Align = alClient
      TabOrder = 0
      OnChange = pcChange
      object tsFont: TTabSheet
        Caption = 'Font'
        ImageIndex = 1
        DesignSize = (
          392
          548)
        object FontLabel: TLabel
          Left = 8
          Top = 33
          Width = 55
          Height = 13
          Caption = 'Font name'
        end
        object SizeLabel: TLabel
          Left = 8
          Top = 79
          Width = 20
          Height = 13
          Caption = 'Size'
        end
        object CbFont: TComboBox
          Left = 8
          Top = 50
          Width = 381
          Height = 22
          Style = csOwnerDrawFixed
          Anchors = [akLeft, akTop, akRight]
          Sorted = True
          TabOrder = 0
          OnDrawItem = CbFontDrawItem
          OnSelect = CbFontSelect
        end
        object EditFontSize: TEdit
          Left = 8
          Top = 96
          Width = 34
          Height = 21
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 1
          Text = '12'
          OnChange = EditFontSizeChange
        end
        object FontSizeUpDown: TUpDown
          Left = 42
          Top = 96
          Width = 16
          Height = 21
          Associate = EditFontSize
          Min = 8
          Max = 30
          Position = 12
          TabOrder = 2
        end
        object PanelTopFont: TPanel
          Left = 0
          Top = 0
          Width = 392
          Height = 25
          Align = alTop
          Caption = 'FONT SELECTION AND SIZE'
          TabOrder = 3
        end
      end
      object stTheme: TTabSheet
        Caption = 'Theme'
        ImageIndex = 2
        object PanelTopTheme: TPanel
          Left = 0
          Top = 0
          Width = 392
          Height = 25
          Align = alTop
          Caption = 'THEME SELECTION'
          TabOrder = 0
        end
        object ThemesRadioGroup: TRadioGroup
          Left = 0
          Top = 25
          Width = 392
          Height = 118
          Align = alTop
          Caption = 'Theme Options (Dark or Light)'
          ItemIndex = 0
          Items.Strings = (
            'Show Themes compatible with current Windows preferences'
            'Show all Dark Themes'
            'Show all Light Themes')
          TabOrder = 1
          OnClick = ThemesRadioGroupClick
        end
        object SelectThemeRadioGroup: TRadioGroup
          Left = 0
          Top = 143
          Width = 392
          Height = 405
          Align = alClient
          Caption = 'Theme Selection'
          Columns = 2
          TabOrder = 2
          OnClick = SelectThemeRadioGroupClick
        end
      end
      object stGeneral: TTabSheet
        Caption = 'Rendering'
        ImageIndex = 3
        object PanelTopPreviewSettings: TPanel
          Left = 0
          Top = 0
          Width = 392
          Height = 25
          Align = alTop
          Caption = 'RENDERING OPTIONS'
          TabOrder = 0
        end
        object RoundedButtonsGroupBox: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 386
          Height = 97
          Align = alTop
          Caption = 'Rounded Buttons'
          TabOrder = 1
          object ToolbarRoundedCheckBox: TCheckBox
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 376
            Height = 17
            Align = alTop
            Caption = 'Apply to Toolbars'
            TabOrder = 0
            OnClick = ToolbarRoundedCheckBoxClick
          end
          object ButtonsRoundedCheckBox: TCheckBox
            AlignWithMargins = True
            Left = 5
            Top = 41
            Width = 376
            Height = 17
            Align = alTop
            Caption = 'Apply to Buttons'
            TabOrder = 1
            OnClick = ButtonsRoundedCheckBoxClick
          end
          object MenuRoundedCheckBox: TCheckBox
            AlignWithMargins = True
            Left = 5
            Top = 64
            Width = 376
            Height = 17
            Align = alTop
            Caption = 'Apply to Menu Buttons'
            TabOrder = 2
            OnClick = MenuRoundedCheckBoxClick
          end
        end
        object ShowFormGroupBox: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 131
          Width = 386
          Height = 62
          Align = alTop
          Caption = 'Show Forms'
          TabOrder = 2
          object cbEmbeddedForms: TCheckBox
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 376
            Height = 30
            Align = alTop
            Caption = 'Embedded Forms'
            Checked = True
            State = cbChecked
            TabOrder = 0
          end
        end
      end
    end
    object SettingsButtonsPanel: TPanel
      Left = 0
      Top = 576
      Width = 400
      Height = 41
      Align = alBottom
      TabOrder = 1
      object ApplySettingsButton: TStyledButton
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 33
        Action = acApplySettings
        Align = alLeft
        TabOrder = 0
      end
      object CancelSettingsButton: TStyledButton
        AlignWithMargins = True
        Left = 170
        Top = 4
        Width = 140
        Height = 33
        Action = acCancelSettings
        Align = alLeft
        TabOrder = 1
      end
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 240
    Top = 88
    object acQuit: TAction
      Category = 'File'
      Caption = 'Quit'
      Hint = 'Close application'
      ImageIndex = 44
      ShortCut = 16465
      OnExecute = acQuitExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About ...'
      Hint = 'About this Demo...'
      ImageIndex = 43
      OnExecute = acAboutExecute
    end
    object actSettings: TAction
      Category = 'Settings'
      Caption = 'Settings'
      Hint = 'Open Settings (Font, Theme  and Appearance)'
      ImageIndex = 40
      OnExecute = actSettingsExecute
    end
    object actMenu: TAction
      Caption = 'Collapse'
      Hint = 'Collapse'
      ImageIndex = 7
      OnExecute = actMenuExecute
    end
    object acStyledToolbar: TAction
      Category = 'Example'
      Caption = 'Toolbar examples'
      Hint = 'Shows the use of StyledToolbar and run-time creation'
      ImageIndex = 38
      OnExecute = acExampleExecute
    end
    object acFamilyClassAppearance: TAction
      Category = 'Example'
      Caption = 'Family/Class/Appearance'
      Hint = 'Use StyledButtons with different Family/Class/Appearance.'
      ImageIndex = 19
      OnExecute = acExampleExecute
    end
    object acStyledBitBtn: TAction
      Category = 'Example'
      Caption = 'StyledBitBtn'
      Hint = 'Use StyledBitBtn with different Appearance based on ModalResult'
      ImageIndex = 39
      OnExecute = acExampleExecute
    end
    object acAutoClick: TAction
      Category = 'Example'
      Caption = 'Auto Click Buttons and Dialogs'
      Hint = 
        'Demonstrate use of AutoClick option for StyledButtons and Styled' +
        ' Dialogs'
      ImageIndex = 8
      OnExecute = acExampleExecute
    end
    object acStyledButtonVCLStyles: TAction
      Category = 'Example'
      Caption = 'Styled Buttons vs VCL Styles'
      Hint = 
        'Compare standard VCL Button with "per-control" Style and same VC' +
        'L style applied to StyledButtons'
      ImageIndex = 21
      OnExecute = acExampleExecute
    end
    object acControlList: TAction
      Category = 'Example'
      Caption = 'ControlList with StyledButtons'
      Hint = 'Demonstrate the use of StyledButtons inside a ControlListView'
      ImageIndex = 45
      OnExecute = acExampleExecute
    end
    object acStyledDbNavigator: TAction
      Category = 'Example'
      Caption = 'DbNavigator and BindNavigator'
      Hint = 
        'Shows the use of Styled DbNavigator and BindNavigator with capti' +
        'ons and custom images'
      ImageIndex = 34
      OnExecute = acExampleExecute
    end
    object acStyledButtonGroup: TAction
      Category = 'Example'
      Caption = 'ButtonGroup'
      Hint = 
        'Shows Styled ButtonGroup with Caption alignment  and Notificatio' +
        'n badge'
      ImageIndex = 37
      OnExecute = acExampleExecute
    end
    object acStyledCategoryButtons: TAction
      Category = 'Example'
      Caption = 'CategoryButtons'
      Hint = 
        'Shows Styled CategoryButtons with Caption alignment  and Notific' +
        'ation badge'
      ImageIndex = 31
      OnExecute = acExampleExecute
    end
    object acStyledTaskDialog: TAction
      Category = 'Example'
      Caption = 'Task and Message Dialogs'
      Hint = 'Shows Styled TaskDialog and Message Dialogs'
      ImageIndex = 36
      OnExecute = acExampleExecute
    end
    object acRoundedCorners: TAction
      Category = 'Example'
      Caption = 'Rounded Corners'
      Hint = 
        'Demonstrate use of Rounded Corners in different Styled Component' +
        's'
      ImageIndex = 32
      OnExecute = acExampleExecute
    end
    object acApplySettings: TAction
      Category = 'Settings'
      Caption = 'Apply Settings'
      Hint = 'Apply Settings to application'
      ImageIndex = 41
      OnExecute = acApplySettingsExecute
      OnUpdate = acSettingsChangedUpdate
    end
    object acCancelSettings: TAction
      Category = 'Settings'
      Caption = 'Cancel Settings'
      Hint = 'Cancel Settings (revert to previous)'
      ImageIndex = 42
      OnExecute = acCancelSettingsExecute
      OnUpdate = acSettingsChangedUpdate
    end
    object acAnimatedStyledButton: TAction
      Category = 'Example'
      Caption = 'Animated StyledButtons'
      Hint = 'Shows Animated StyledButton (using SKIA support for animations)'
      ImageIndex = 28
      OnExecute = acExampleExecute
    end
  end
end
